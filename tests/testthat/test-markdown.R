context("test-markdown")

test_that("arrange_button works", {

  btn <- arrange_button(label = "Sort", by = "value")

  expect_is(btn, "shiny.tag")
  expect_identical(btn$attribs$`data-sort-by`, "value")
  expect_identical(btn$attribs$`data-sort-decreasing`, "false")
})

test_that("arrange_button works", {

  expect_error(rmd_group_buttons(arrange_button(label = "Sort", by = "value")))

  btns <- rmd_group_buttons(
    shuffleId = "grid",
    label = "Click!",
    arrange_button(label = "Sort", by = "value"),
    arrange_button(label = "Sort", by = "value")
  )

  expect_is(btns, "shiny.tag")
  expect_identical(btns$attribs$class, "button-group sort-shuffle-btn-grid")
})

