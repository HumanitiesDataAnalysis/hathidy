#' Hathidy: Work with wordcount data for 15 million books.
#'
#' @description In actual research, you shouldn't download files multile times, even if you work in multiple files. This package
#' uses a standard location.
#'
#'
#' @docType package
#'
#' @name hathidy
#'
#' @import dplyr
#' @import stringr
#' @import purrr
NULL


#' Add a set of 'chunks' to a tibble with page-level counts.
#'
#' @param frame A set of counts
#' @param count The field containing wordcounts
#' @param length The desired chunk size. If smaller than 500 or so,
#'  this will probably break against the existing pages.
#'
#' @return The input frame with a new column 'chunk'.
#' @export
add_chunks = function(frame, count, length=2000) {
  # Breaks a book into fixed length
  count = enquo(count)
  grouping = c()
  frame %>%
    group_by(page, add = TRUE) %>%
    {grouping <<- group_vars(.); .} %>%
    summarize(count=sum(count)) %>%
    mutate(tally = cumsum(count), chunk = 1 + tally %/% length) %>%
    select(page, chunk) %>%
    inner_join(frame, by = c(grouping))
}

# Clean ids don't have colons or slashes
id_clean = function(htid) htid %>% str_replace_all(":", "+") %>% str_replace_all("/", "=")

# Encoded ids for urls also replace periods with commas.
id_encode = function(htid) htid %>% id_clean %>% str_replace_all("\\.", ",")

pairtree = function(htid) {
  splitted = str_split(htid, "\\.", n = 2)[[1]]
  breaks = seq(1, nchar(splitted[2]), by = 2)
  cleaned = splitted[2] %>% id_encode
  slashes = str_sub(cleaned, breaks, breaks + 1) %>%  str_c(sep="/", collapse="/")
  str_c(splitted[1], "pairtree_root", slashes, cleaned, sep="/")
}

local_loc = function(htid, suffix="json") {
  clean = htid %>% id_clean
  pairtr = pairtree(htid)
  str_glue("{hathidy_dir()}/{pairtr}/{clean}.{suffix}")
}

download_http = function(htid) {
  local_name = local_loc(htid)
  url = str_glue("https://data.analytics.hathitrust.org/htrc-ef-access/get?action=download-ids&id={htid}&output=json")
#  message("Downloading", htid)
  dir.create(dirname(local_name), showWarnings = FALSE, recursive = TRUE)
  download.file(url = url, destfile = local_name)
}

hathidy_dir <- function() {
  if(is.null(getOption("hathidy_dir"))) {
    message(
      stringr::str_glue("Saving downloaded books to {tempdir()};",
      " to speed up subsequent runs and be polite to the HTRC servers",
      ", set a permanent local cache like 'options('hathidy_dir' = {path.expand(c('~/hathi-ef'))})'",
      ".")
    )
    options("hathidy_dir"=tempdir())
  }
  return(getOption("hathidy_dir"))
}

load_json = function(htid, check_suffixes = c("json","json.bz2","json.gz")) {
  for (suffix in check_suffixes) {
    fname = local_loc(htid, suffix = suffix)
    if (file.exists(fname)) {
      tryCatch(
        return(jsonlite::read_json(fname)),
        error = function(e) {stop(
          str_glue("Unable to load features from {fname}: possibly a partially downloaded file that must be removed?")
        )}
      )}
  }
  NULL
}

.get_metadata = function(htid) {
  # Expand this out to use a cache? This is expensive.
  load_json(htid)[[2]]
}

#' Add TF-IDF summary based on groupings.
#'
#' @description The TF-IDF function in tidytext requires an explicit 'doc'
#' parameter; this applies it on the existing dataset groups.
#'
#' @param frame A grouped data from
#' @param word The unquoted variable name storing terms for frequency
#' @param count The unquoted variable storing the count
#'
#' @return A data_frame with a column tfidf added.
#' @export
grouped_tfidf = function(frame, word, count) {
  token = enquo(word)
  count = enquo(count)
  groupings = groups(f)
  n_docs = frame %>% distinct(!!!groupings) %>% nrow
  frame %>%
    distinct(!!!groupings, token) %>%
    group_by(!!token) %>%
    summarize(idf = log(n_docs/n())) %>%
    inner_join(frame) %>%
    group_by(!!!groupings) %>%
    mutate(doc_total = sum(!!count)) %>%
    group_by(!!token, add = TRUE) %>%
    summarize(count = sum(!!count), tfidf = count/doc_total[1]*idf[1])
}


.export = function(frame, htid, quoted, metadata, only_body = TRUE) {
  output = frame %>%
    filter(ifelse(only_body, section=="body", TRUE)) %>%
    group_by(!!!quoted) %>%
    summarize(count=sum(count)) %>%
    mutate(htid = htid)
  metas = .get_metadata(htid)
  if (nrow(output) > 0) {
    for (meta in metadata) {
      output[[meta]] = metas[[meta]]
    }
  }
  output
}

#' Return Hathi Trust Extended Feature counts.
#'
#' @param htid A Hathi Trust volume identifier, or a list of several.
#' @param cols The level of aggregation to return. Possible values: "page" (sequence in book), "token" (word), "POS"
#'  (tagged part of speech), and "section." Default is c("page", "token"). If section is not requested, values are returned
#'  only for the books body.
#' @param metadata Supplements the returned frame with volume-level metadata.
#'  Possible values are:  c("schemaVersion", "dateCreated", "volumeIdentifier", "accessProfile",
#' "rightsAttributes", "hathitrustRecordNumber", "enumerationChronology",
#' "sourceInstitution", "sourceInstitutionRecordNumber", "oclc", "isbn",
#' "issn", "lccn", "title", "imprint", "lastUpdateDate", "governmentDocument",
#' "pubDate", "pubPlace", "language", "bibliographicFormat", "genre", "issuance",
#' "typeOfResource", "classification", "names", "htBibUrl", "handleUrl")
#'
#' @return a tibble, with columns created by the call.
#' @export
hathi_counts = function(htid, cols = c("page", "token"), metadata = c()) {
  cache_csv = TRUE
  if (length(htid) > 1) {
    # Do a list of them with a progress bar.
    pb = progress_estimated(length(htid))
    return(htid %>% map(function(htid_) {
      pb$tick()$print()
      hathi_counts(htid_, cols, metadata)
    }) %>% bind_rows)
  }

  metadata = intersect(metadata,
    c("schemaVersion", "dateCreated", "volumeIdentifier", "accessProfile",
    "rightsAttributes", "hathitrustRecordNumber", "enumerationChronology",
    "sourceInstitution", "sourceInstitutionRecordNumber", "oclc", "isbn",
    "issn", "lccn", "title", "imprint", "lastUpdateDate", "governmentDocument",
    "pubDate", "pubPlace", "language", "bibliographicFormat", "genre", "issuance",
    "typeOfResource", "classification", "names", "htBibUrl", "handleUrl"))

  quoted = rlang::syms(intersect(cols, c("page", "token", "POS", "section")))



  local_csv = local_loc(htid, suffix = "csv.gz")
  if (cache_csv && file.exists(local_csv)) {
    return(readr::read_csv(local_csv, col_types = "ccici", progress = FALSE) %>% .export(htid, quoted, metadata, only_body = !"section" %in% cols))
  }

  listified_version = load_json(htid, check_suffixes = c("json", "json.bz2", "json.gz"))
  if (is.null(listified_version)) {
    # Download if none of the files exist.
    download_http(htid)
    listified_version = load_json(htid, check_suffixes = c("json"))
  }

  tibble = listified_version %>% parse_listified_book
  if (cache_csv) {tibble %>% mutate(page = as.integer(page), count = as.integer(count)) %>% readr::write_csv(local_csv)}
  return(tibble %>% .export(htid, quoted, metadata, only_body = !"section" %in% cols ))
}


download_book = function(htid) {
  # Not used; http download is easier.
  pairtr = pairtree(htid)
  clean = htid %>% id_clean
  hathi_url = str_glue("data.analytics.hathitrust.org::features/{pairtr}/{clean}.json.bz2")
  local_url = local_loc(htid)
  if (!file.exists(local_url)) {
    dir.create(dirname(local_url), showWarnings = FALSE, recursive = TRUE)
    syncr::rsync(src = hathi_url, dest = local_url)
  }
}

parse_section = function(page, section) {
  d = page[[section]]$tokenPosCount
  if (length(d)) {
    lens = sapply(d, length)
    poses = lapply(d, names) %>% unlist
    return(
      data_frame(token = rep(names(d), times = lens), POS = poses, count = unlist(d), section = section)
      )
  }
  return(NULL)
}

parse_page = function(page) {
  parts = c("body", "header", "footer")
  seq = as.numeric(page$seq)
  body = parts %>% purrr::map(~parse_section(page, .x)) %>% purrr::discard(is.null) %>% bind_rows
  if (nrow(body)) {
    body$page = seq
  }
  body
}


parse_listified_book = function(listified_version) {
  listified_version$features$pages %>% map(parse_page) %>%
    bind_rows %>%
    mutate(page = as.numeric(page))
}

