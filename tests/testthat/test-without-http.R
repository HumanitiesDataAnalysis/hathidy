library(hathidy)
library(testthat)
context("test-without-http")

test_that("Empty Test is filled", {
  expect_error({
    hathidy:::local_loc('gahbarbar')
  }, "period")
})

