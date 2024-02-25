box::use(
  shiny[testServer],
  testthat[expect_error, test_that],
)
box::use(
  app/main[server],
)

test_that("Datatable not visible at start", {
  testServer(server, {
    expect_error(output$results_table, "")
  })
})
