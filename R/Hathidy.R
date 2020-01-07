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

# Clean ids don't have colons or slashes, because those mess up file paths.
id_clean <- function(htid) htid %>% str_replace_all(":", "+") %>% str_replace_all("/", "=")

# Encoded ids for urls also replace periods with commas, because those mess up URLs.
id_encode <- function(htid) htid %>% id_clean() %>% str_replace_all("\\.", ",")


pairtree <- function(htid) {
  splitted <- str_split(htid, "\\.", n = 2)[[1]]
  if (length(splitted) == 1) {
    stop(str_glue("malformed htid {htid}: Hathi ids should contain a period"))
  }
  breaks <- seq(1, nchar(splitted[2]), by = 2)
  cleaned <- splitted[2] %>% id_encode()
  slashes <- str_sub(cleaned, breaks, breaks + 1) %>% str_c(sep = "/", collapse = "/")
  str_c(splitted[1], "pairtree_root", slashes, cleaned, sep = "/")
}

local_loc <- function(htid, suffix = "json") {
  clean <- htid %>% id_clean()
  pairtr <- pairtree(htid)
  str_glue("{hathidy_dir()}/{pairtr}/{clean}.{suffix}")
}

ef_check <- function(htid) {
  url <- str_glue("https://data.analytics.hathitrust.org/htrc-ef-access/get?action=check-exists&ids={htid}")
  return(jsonlite::fromJSON(url))
}

download_http <- function(htid) {
  local_name <- local_loc(htid)
  url <- str_glue("https://data.analytics.hathitrust.org/htrc-ef-access/get?action=download-ids&id={htid}&output=json")
  dir.create(dirname(local_name), showWarnings = FALSE, recursive = TRUE)
  utils::download.file(url = url, destfile = local_name)
}

hathidy_dir <- function() {
  if (is.null(getOption("hathidy_dir"))) {
    message(
      stringr::str_glue(
        "Saving downloaded books to {tempdir()};",
        " to speed up subsequent runs and be polite to the HTRC servers,",
        " set a permanent local cache like 'options('hathidy_dir' = {path.expand(c('~/hathi-ef'))})'",
        "."
      )
    )
    options("hathidy_dir" = tempdir())
  }
  return(getOption("hathidy_dir"))
}

load_json <- function(htid, check_suffixes = c("json", "json.bz2", "json.gz")) {
  for (suffix in check_suffixes) {
    fname <- local_loc(htid, suffix = suffix)
    if (file.exists(fname)) {
      tryCatch(
        return(jsonlite::read_json(fname)),
        error = function(e) {
          stop(
            str_glue("Unable to load features from {fname}: possibly a partially downloaded file that must be removed?")
          )
        }
      )
    }
  }
  NULL
}

.get_metadata <- function(htid) {
  # Expand this out to use a cache? This is expensive.
  load_json(htid)[[2]]
}

.export <- function(frame, htid, quoted, metadata, only_body = TRUE) {
  output <- frame %>%
    filter(ifelse(only_body, section == "body", TRUE)) %>%
    group_by(!!!quoted) %>%
    summarize(count = sum(count)) %>%
    mutate(htid = htid) %>%
    ungroup

  metas <- .get_metadata(htid)
  if (nrow(output) > 0) {
    for (meta in metadata) {
      output[[meta]] <- metas[[meta]]
    }
  }
  output
}

multi_download =  function(htid, cols, metadata, cache)  {
  # Do a list of them with a progress bar.
  pb <- progress_estimated(length(htid))

  download <- function(htid_) {
    pb$tick()$print()
    hathi_counts(htid_, cols, metadata, cache)
  }

  #if (length(errors) > 0) {
  #  warning("Errors on the following htid(s):", errors)
  #}

  value = htid %>% map(possibly(download, otherwise = tibble(), quiet = FALSE)) %>% bind_rows()

  diff = setdiff((value$htid), htid)
  if (length(diff) > 0) warning("Unable to return Hathi IDs", diff)
  value
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
#' @param cache Store a copy of the data for fast access next time. Default format is "csv".
#' "Support planned for 'parquet'.
#' @param path A direct filepath to a json dump. Use this if you are not using a pairtree to store files. If this is entered, 'htid' is ignored.
#' @return a tibble, with columns created by the call.
#' @export
hathi_counts <- function(htid, cols = c("page", "token"), metadata = c(), cache = "csv", path = FALSE) {

  if (path) {
    htid = NULL
    cache = FALSE
  }

  if (length(htid) > 1) {
    return(multi_download(htid, cols, metadata, cache))
  }

  # TODO raise on bad meta field.
  metadata <- intersect(
    metadata,
    c(
      "schemaVersion", "dateCreated", "volumeIdentifier", "accessProfile",
      "rightsAttributes", "hathitrustRecordNumber", "enumerationChronology",
      "sourceInstitution", "sourceInstitutionRecordNumber", "oclc", "isbn",
      "issn", "lccn", "title", "imprint", "lastUpdateDate", "governmentDocument",
      "pubDate", "pubPlace", "language", "bibliographicFormat", "genre", "issuance",
      "typeOfResource", "classification", "names", "htBibUrl", "handleUrl"
    )
  )

  quoted <- rlang::syms(intersect(cols, c("page", "token", "POS", "section")))

  if (path == FALSE) {
    local_csv <- local_loc(htid, suffix = "csv.gz")
    if (cache == "csv" && file.exists(local_csv)) {
      return(readr::read_csv(local_csv, col_types = "ccici", progress = FALSE) %>% .export(htid, quoted, metadata, only_body = !"section" %in% cols))
    }
  }

  if (path) {
    listified_version <- jsonlite::read_json(path)
  } else {
    listified_version <- load_json(htid, check_suffixes = c("json", "json.bz2", "json.gz"))
  }

  if (is.null(listified_version)) {
    # Download if none of the files exist.
    download_http(htid)
    listified_version <- load_json(htid, check_suffixes = c("json"))
  }

  tibble <- listified_version %>% parse_listified_book()

  if (cache == "csv") {
    tibble %>% mutate(page = as.integer(page), count = as.integer(count)) %>% readr::write_csv(local_csv)
  }

  return(tibble %>% .export(htid, quoted, metadata, only_body = !"section" %in% cols))

}

parse_section <- function(page, section) {
  d <- page[[section]]$tokenPosCount
  if (length(d)) {
    lens <- sapply(d, length)
    poses <- lapply(d, names) %>% unlist()
    return(
      tibble(token = rep(names(d), times = lens), POS = poses, count = unlist(d), section = section)
    )
  }
  return(NULL)
}

parse_page <- function(page) {
  parts <- c("body", "header", "footer")
  seq <- as.numeric(page$seq)

  body <- parts %>%
    purrr::map(~ parse_section(page, .x)) %>%
    purrr::discard(is.null) %>%
    map(~{.x$page = seq; .x})
  # Don't yet flatten to list; that waits until later.
  body
}


parse_listified_book <- function(listified_version) {
  listified_version$features$pages %>%
    map(parse_page) %>%
    rlang::flatten() %>%
    bind_rows() %>%
    mutate(page = as.numeric(page))
}
