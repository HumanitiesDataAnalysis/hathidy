context("test-downloading")
library(hathidy)
library(testthat)

options(hathidy_dir = "/drobo/feature-counts/")

if (FALSE) {
test_that("Empty Test is filled", {
  htid = "mdp.39015022276334"
  dir = hathidy:::hathidy_dir()
  l = hathi_counts(htid)
})
}

test_that("multi_loads") {
  hathi_counts(c("inu.30000125628176", "mdp.39015022276334"))
}
local_loc(htid)

test_that("Arabic", {
htid = "inu.30000125628176"
listified_version = load_json(htid)
page = listified_version$features$pages[[12]]
page %>% parse_section("body")
hathi_counts(htid)
}

#listified_version$metadata

ggplot(complete) +
  # This could be geom_point, or geom_line, or whatever
  geom_point() +
  # The aesthetic mapping. Choose what columns you want.
  aes(x = year, y = whatever, fill = otherwhatever) +
  # Gotta have titles for a graph to be good!
  labs(title = "A succinct explanation", subtitle="With a caveat", caption = "and an attribution")
