context("test-tags")


# shuffle_cards ----

test_that("'shuffle_cards' works", {
  card <- shuffle_card(tags$div())

  expect_is(card, "shufflecard.tag")
  expect_identical(card$attribs$class, "element-item")
  expect_length(card$children, 1)
})

test_that("'shuffle_cards' use name args for data attributes", {
  card <- shuffle_card(argA = "A", tags$div(), arg1 = 1)

  expect_true("data-argA" %in% names(card$attribs))
  expect_true("data-arg1" %in% names(card$attribs))
  expect_identical(card$attribs$`data-argA`, "A")
  expect_identical(card$attribs$`data-arg1`, 1)
})


test_that("'shuffle_cards' respect id", {
  card <- shuffle_card(id = "ID", tags$div())

  expect_true("id" %in% names(card$attribs))
  expect_identical(card$attribs$id, "ID")
})


# shuffle_options ----

test_that("'shuffle_options' works", {
  opts <- shuffle_options()

  expect_is(opts, "shuffle.options")
  expect_is(opts, "list")
  expect_length(opts, 2)
})


test_that("'shuffle_options' return not-null args", {
  opts <- shuffle_options(is_centered = TRUE, roundTransforms = TRUE)

  expect_length(opts$options, 2)
})

test_that("'shuffle_options' respect AsIs", {
  opts <- shuffle_options(column_width = I("function() {}"), myTest = I("test"))

  expect_length(opts$eval, 2)
  expect_true(all(opts$eval %in% names(opts$options)))
})

test_that("'shuffle_options' return custom args as camelCase", {
  opts <- shuffle_options(my_custom_arg = 4)

  expect_identical(names(opts$options), "myCustomArg")
})



# shuffle_container ----

test_that("'shuffle_container' works", {
  container <- shuffle_container(shuffleId = "grid")
  deps <- htmltools::htmlDependencies(container[[1]])

  expect_is(container, "shiny.tag.list")
  expect_identical(container[[1]]$attribs$class, "shuffle-container")
  expect_identical(container[[1]]$attribs$id, "grid")
  expect_true(length(deps) > 0)
})

test_that("'shuffle_container' accept 'shufflecards'", {
  container <- shuffle_container(
    shuffleId = "grid",
    shuffle_card(tags$div()),
    shuffle_card(tags$div()),
    shuffle_card(tags$div())
  )
  expect_length(container[[1]]$children[[1]][[1]], 3)
})

test_that("'shuffle_container' append nodata tag", {
  container <- shuffle_container(
    shuffleId = "grid",
    no_card = "NOTHING"
  )
  expect_true(!is.null(container[[2]]))
  expect_identical(container[[2]]$attribs$id, "grid-nodata")
  expect_identical(container[[2]]$children[[1]], "NOTHING")
})


