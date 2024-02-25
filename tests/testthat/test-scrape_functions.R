box::use(
  testthat[expect_is, expect_equal, test_that, expect_true],
  purrr[map_lgl],
)

box::use(
  app/logic/scrape_functions[search_product, search_product_extra],
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
        map_lgl(~ length(.x) > 0)
    )
  )
})
