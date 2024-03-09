
# Use rvest with shiny

This repository is a demonstration of the usage of rvest inside a shiny application to show data from mercadolibre. It is intended for educational purposes.

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)



## Run Locally

Clone the project and open in Rstudio or vscode

Install dependencies

```bash
  renv::restore()
```

Configure

- No db
``` r
  # Inside app/main.R
  db_mode <- "none"
```

- Only Mysql
``` r
  # Inside app/main.R
  db_mode <- "local"
```

```bash
  docker compose up db -d
```

- With docker compose

``` r
  # Inside app/main.R
  db_mode <- "compose"
```

```bash
  docker compose build
  docker compose up
```

Run

```r
  # run
  shiny::runApp()
```
## Running Tests

To run tests, run the following command

```r
  # in app/main.R
  db_mode <- "none"

  # run 
  rhino::test_r()
```
