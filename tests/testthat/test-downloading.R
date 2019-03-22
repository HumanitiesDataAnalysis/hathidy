context("test-downloading")
library(hathidy)
library(testthat)
library(dplyr)

# options(hathidy_dir = "/drobo/feature-counts/")

if (FALSE) {
  test_that("Empty Test is filled", {
    htid <- "mdp.39015022276334"
    dir <- hathidy:::hathidy_dir()
    l <- hathi_counts(htid)
  })
}

test_that("multi_loads", {
  htid = c("inu.30000125628176", "mdp.39015022276334")
  c <- hathi_counts(htid)
})

test_that("Russian text works isn't bonkers", {
  htid <- "mdp.39015022276334"
  listified_version <- hathi_counts(htid, cache = FALSE)
  listified_version %>%
    count(token) %>%
    filter(token=="вед") %>%
    transmute(`Count of вед in Russian text` = n) %>%
    with(assertthat::assert_that(`Count of вед in Russian text` > 20))
})
