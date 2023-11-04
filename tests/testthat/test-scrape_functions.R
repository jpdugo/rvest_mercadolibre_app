box::use(
  testthat[...],
  app / logic / scrape_functions[...],
  purrr[map_lgl]
)

test_that("search_product returns expected results", {
  result <- search_product("teclado epomaker")
  expect_is(result, "tbl_df")
  expect_equal(names(result), c("title", "href"))
})

test_that("search_product works for special searches", {
  result <- search_product("casa", max_pages = 2)
  expect_is(result, "tbl_df")
  expect_equal(names(result), c("title", "href"))
})

test_that("search_product returns expected results", {
  result <- search_product("teclado epomaker")
  result_extra <- search_product_extra(result)

  expect_is(result_extra, "tbl_df")
  expect_equal(names(result_extra), c("title", "image", "price", "href"))
  expect_true(
    all(
      result_extra$price |>
        map_lgl(~ length(.x) > 0, .progress = TRUE)
    )
  )
})
