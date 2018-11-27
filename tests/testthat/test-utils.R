context("test-utils")

test_that("validate_card works", {
  expect_silent(validate_card(shuffle_card()))
  expect_error(validate_card(tags$div()))
})

test_that("validate_cards works", {
  expect_silent(validate_cards(tagList(shuffle_card(), shuffle_card())))
  expect_error(validate_cards(tagList(tags$div(), tags$div())))
})

test_that("removeDependencies works", {
  tag_dep <- attachDependencies(
    tags$div(),
    shuffle_dependencies()
  )
  tag_dep <- removeDependencies(tag_dep)
  expect_null(htmltools::htmlDependencies(tag_dep))
})

test_that("snake_to_camel works", {
  camelCase <- snake_to_camel("var_name")
  expect_identical(camelCase, "varName")
})

test_that("get_eval works", {
  evals <- get_eval(list(noneval = 1, eval = I("a")))
  expect_identical(evals, "eval")
})

