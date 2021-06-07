#' Hathidy: Work with wordcount data for 17 million books from the Hathi Trust
#'
#' @description An R interface for transparently downloading and analyzing page-level wordcount
#' data from the HathiTrust for over 17 million books.
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

stubbytree <- function(htid) {
  splitted <- str_split(htid, "\\.", n = 2)[[1]]
  if (length(splitted) == 1) {
    stop(str_glue("malformed htid {htid}: Hathi ids should contain a period"))
  }
  splitted[2]
  cleaned = splitted[2] %>% id_clean()
  breaks = seq(1, by=3, length.out = nchar(cleaned)/3)
  stubbydir = str_sub(cleaned, breaks, breaks) %>% str_c(collapse="")
  str_c(splitted[1], stubbydir, sep="/")
}

local_loc <- function(htid, suffix = "json") {
  clean <- htid %>% id_clean()
  stub <- stubbytree(htid)
  str_glue("{hathidy_dir()}/{stub}/{clean}.{suffix}")
}

download_http <- function(htid) {
  local_name <- local_loc(htid, suffix = "json.bz2")
  tree = stubbytree(htid)
  clean = id_clean(htid)
  url <- str_glue("http://data.analytics.hathitrust.org/features-2020.03/{tree}/{clean}.json.bz2")
  dir.create(dirname(local_name), showWarnings = FALSE, recursive = TRUE)
  utils::download.file(url = url, destfile = local_name)
  local_name
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

load_json <- function(htid, check_suffixes = c("json", "json.bz2")) {
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

.export <- function(frame, cols, metadata, sections, metadata_object) {
  if (length(sections) < 3) {
    frame <- frame %>% filter(section %in% sections)
  }
  output <- frame %>%
    group_by(!!!rlang::syms(cols)) %>%
    summarize(count = sum(count), .groups = "drop")

  metas <- metadata_object[metadata]
  if (nrow(output) > 0) {
    for (meta in metadata) {
      output[[meta]] <- metas[[meta]]
    }
  }
  output
}

multi_download =  function(htid, cols, sections, metadata, cache)  {
  # Do a list of them with a progress bar.
  pb <- progress_estimated(length(htid))

  download <- function(htid_) {
    pb$tick()$print()
    hathi_counts(htid_, cols, sections, metadata, cache)
  }

  value = htid %>% map_dfr(possibly(download, otherwise = tibble(), quiet = FALSE))

  diff = setdiff((value$htid), htid)
  if (length(diff) > 0) warning("Unable to return Hathi IDs", diff)
  value
}

load_feather = function(path, cols, metadata, sections) {
  table = arrow::read_feather(path, as_data_frame = FALSE, col_select = all_of(c(cols, "section", "count")))
  metadata_fields = jsonlite::parse_json(table$metadata$meta)
  value = .export(table, cols, metadata, sections = sections, metadata_object = metadata_fields)
  return(value)
}


#' Return Hathi Trust Extended Feature counts.
#'
#' @param htid A Hathi Trust volume identifier, or a list of several.
#' @param cols The level of aggregation to return. Possible values: "page" (sequence in book), "token" (word), "POS"
#'  (tagged part of speech), and "section." Default is c("page", "token"). If section is not requested, values are returned
#'  only for the book's body (not headers and footers.)
#'
#' @param sections Hathi divides books into 'header', 'footer', and 'body.' If you request 'section' as one of your
#' cols, all will be returned; but if you do not, only the counts for the body will be returned unless you manually
#' specificy, for example, (sections = c("header", "footer", "body"))
#' @param metadata Supplements the returned frame with volume-level metadata.
#'  Possible values are:  c("schemaVersion", "dateCreated", "volumeIdentifier", "accessProfile",
#' "rightsAttributes", "hathitrustRecordNumber", "enumerationChronology",
#' "sourceInstitution", "sourceInstitutionRecordNumber", "oclc", "isbn",
#' "issn", "lccn", "title", "imprint", "lastUpdateDate", "governmentDocument",
#' "pubDate", "pubPlace", "language", "bibliographicFormat", "genre", "issuance",
#' "typeOfResource", "classification", "names", "htBibUrl", "handleUrl")
#' @param cache Store a copy of the data for fast access next time. Default format is "csv".
#' "Support planned for 'parquet'.
#' @param path A direct filepath to a json dump. Use this if you are not using a stubbytree to store files.
#'             If this is entered, 'htid' is ignored.
#' @return a tibble, with columns created by the call.
#' @export

hathi_counts <- function(htid, cols = c("page", "token"), sections = NULL, metadata = c("htid"), cache = "feather", path = FALSE) {

  if (path) {
    htid = NULL
    cache = FALSE
  }

  if (is.null(sections)) {
    if ("section" %in% cols) {
      sections = c("header", "footer", "body")
    } else {
      sections = c("body")
    }
  }

  if (length(htid) > 1) {
    return(multi_download(htid, cols, sections, metadata, cache))
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
      "typeOfResource", "classification", "names", "htBibUrl", "handleUrl", "id", "htid"
    )
  )


  if (path == FALSE) {
    local_feather <- local_loc(htid, suffix = "feather")
    if (cache == "feather" && file.exists(local_feather)) {
      return(load_feather(local_feather, cols, metadata, sections))
    }
  }

  if (path) {
    listified_version <- jsonlite::read_json(path)
  } else {
    listified_version <- load_json(htid, check_suffixes = c("json", "json.bz2", "json.gz"))
  }

  if (is.null(listified_version)) {
    # Download if none of the files exist.
    file_name = download_http(htid)
    listified_version <- load_json(htid, check_suffixes = c("json", "json.bz2"))
  }

  tibble <- listified_version %>% parse_listified_book()

  listified_version[["metadata"]][["htid"]] = htid

  if (cache == "feather") {
    if (!require(arrow)) {stop("You must install the Arrow package for caching.")}
    intermediate = local_loc(htid, suffix = "feather")
    final = local_loc(htid, suffix = "feather")
    meta_as_json = listified_version[["metadata"]] %>% jsonlite::toJSON(auto_unbox = TRUE, na = "null")
    data = tibble
    schema = arrow::schema(
                           page=arrow::uint16(),
                           section = arrow::utf8(),
                           token = arrow::utf8(),
                           POS = arrow::utf8(),
                           count = arrow::uint16())
    schema = schema$WithMetadata(list(meta = meta_as_json))
    table = with(data, {
      arrow::Table$create(page = as.integer(page),
                          section = section,
                          token = token,
                          POS = POS,
                          count = as.integer(count),
                          schema = schema)
    })
    table$metadata$meta = meta_as_json
    table %>% arrow::write_feather(intermediate, compression = "zstd")
  }

  return(tibble %>%.export(cols, metadata, sections, listified_version[['metadata']]))

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
