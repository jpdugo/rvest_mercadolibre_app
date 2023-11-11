box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app / main[...],
)

test_that("Datatable not visible at start", {
  testServer(server, {
    expect_error(output$results_table, "")
  })
})
