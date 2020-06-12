library(hathidy)
library(testthat)
context("test-without-http")

test_that("Empty Test is filled", {
  expect_error({
    hathidy:::local_loc('gahbarbar')
  }, "period")
})

test_stubbytree({
  expect_equal(
    hathidy:::stubbytree("aeu.ark:/13960/t00008f0x"),
    "aeu/a+30000/aeu.ark+=13960=t00008f0x"
  )
})

