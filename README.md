
# Use rvest with shiny

This repository is a demonstration of the usage of rvest inside a shiny application to show data from mercadolibre. It is intended for educational purposes.

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)



## Run Locally

**1** - Clone the project and open in Rstudio or vscode

**2** -  Install dependencies

```bash
  renv::restore()
```

**3** - Configure the database connection in app/main.R file, by default it is configured for a local mysql instance.

### Only Mysql
i. Go to app/main.R script and modify:
``` r
  db_mode <- "local"
```
ii. From the terminal run:
```bash
  docker compose up db -d
```

### Using docker compose
i. Go to app/main.R script and modify:
``` r
  db_mode <- "compose"
```
ii. From the terminal run:
```bash
  docker compose build
  docker compose up
```

### No Mysql
i. Go to app/main.R script and modify:
``` r
  db_mode <- "none"
```

**4** Finally to run the app:

```r
  rhino::app()
```

## Testing

To run tests, run the following command

```r
  # in app/main.R
  db_mode <- "none"

  # run 
  rhino::test_r()
```
