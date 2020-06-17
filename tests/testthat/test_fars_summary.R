context("functions")

test_that("single year data is summarized successfully", {
  setwd(system.file("extdata", package = "fars"))

  fars_summary_2013 <- tibble::tribble(
    ~MONTH, ~`2013`,
    1,   2230,
    2,   1952,
    3,   2356,
    4,   2300,
    5,   2532,
    6,   2692
  )
  fars_summary_2013 <- purrr::map_df(fars_summary_2013, as.integer)

  expect_equal(dim(fars_summarize_years(2014)), c(12, 2))
  expect_equal(head(fars_summarize_years(2013)), fars_summary_2013)
})

test_that("mulitple years data is summarized successfully", {
  setwd(system.file("extdata", package = "fars"))

  fars_summary_2013_to_15 <- tibble::tribble(
    ~MONTH, ~`2013`, ~`2014`, ~`2015`,
    7,   2660,   2696,   2998,
    8,   2899,   2800,   3016,
    9,   2741,   2618,   2865,
    10,   2768,   2831,   3019,
    11,   2615,   2714,   2724,
    12,   2457,   2604,   2781
  )
  fars_summary_2013_to_15 <- purrr::map_df(fars_summary_2013_to_15, as.integer)

  expect_equal(dim(fars_summarize_years(c(2013, 2014, 2015))), c(12, 4))
  expect_equal(tail(fars_summarize_years(c(2013, 2014, 2015))),
               fars_summary_2013_to_15)
})
