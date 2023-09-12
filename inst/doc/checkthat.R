## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(checkthat)

## -----------------------------------------------------------------------------
library(checkthat)

mtcars |>
  check_that(
    all(cyl > 2),
    !any(is.na(mpg))
  )

## ---- echo=FALSE--------------------------------------------------------------
set.seed(123456)

## ---- error=TRUE--------------------------------------------------------------
mtcars |>
  check_that(
    all(cyl > 2),
    any(mpg > 35)
  )

## -----------------------------------------------------------------------------
mtcars |>
  check_that(
    some_of(cyl > 4, at_least = .30, at_most = 25),
    whenever(wt < 3, then_expect = mpg > 19),
    for_case(2, mpg == 21, hp == 110)
  )

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
library(dplyr)

## -----------------------------------------------------------------------------
library(dplyr)

new_mtcars <- mtcars |>
  select(mpg) |>
  mutate(km_per_litre = 0.425 * mpg) |>
  check_that(max(km_per_litre) < 15)

head(new_mtcars)

## ---- error=TRUE--------------------------------------------------------------
mtcars |>
  mutate(type = factor(wt < 3, labels = c("sm", "lg"), ordered = TRUE)) |>
  check_that(max(wt[type == "sm"]) <= min(wt[type == "lg"])) |>
  filter(type == "sm") |>
  summarise(desired_mpg = mean(mpg)) |>
  check_that(desired_mpg > 15)

## -----------------------------------------------------------------------------
mtcars |>
  check_that(
    at_least(mpg < 35, p = .95),
    more_than(hp == 110, n = 2),
    exactly_equal(cyl == 6, n = 7),
    less_than(wt > 3, p = .75),
    at_most(is.na(mpg), n = 3),
  )

## -----------------------------------------------------------------------------
mtcars |>
  check_that(
    some_of(cyl > 4, at_least = .30, at_most = 25),
    whenever(is_observed = wt < 3, then_expect = mpg > 19),
    for_case(2, mpg == 21, hp == 110)
  )

## -----------------------------------------------------------------------------
mtcars |>
  check_that(
    nrow(.d) > 10,
    "mpg" %in% names(.d)
  )

## -----------------------------------------------------------------------------
library(tidyr)

mtcars |>
  check_that(ncol(.d) == 11, nrow(.d) == 32) |> # original dimensions
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "values"
  ) |>
  check_that(ncol(.d) == 2, nrow(.d) == 32 * 11) # check that cols became rows

## -----------------------------------------------------------------------------
cyl_ratings_df <- data.frame(cyl = c(4, 6, 8), group = c("A", "B", "C"))

mtcars |>
  left_join(cyl_ratings_df, by = "cyl") |>
  check_that(
    ncol(.d) == 12, # check that there's one new column
    names(.d)[length(names(.d))] == "group", # check new column is "group"
    nrow(.d) == 32 # check that no new rows
  )

