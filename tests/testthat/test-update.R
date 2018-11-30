context("test-update")

session <- as.environment(list(
  ns = identity,
  sendInputMessage = function(inputId, message) {
    session$lastInputMessage = list(id = inputId, message = message)
  }
))

test_that("arrange_cards works", {
  arrange_cards(session, "grid", by = "value")

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "sort")
  expect_identical(session$lastInputMessage$message$sortBy, "value")
  expect_identical(session$lastInputMessage$message$numeric, FALSE)
  expect_identical(session$lastInputMessage$message$decreasing, FALSE)
  expect_identical(session$lastInputMessage$message$random, FALSE)

  arrange_cards(session, "grid", by = "value2", numeric = TRUE, desc = TRUE)

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "sort")
  expect_identical(session$lastInputMessage$message$sortBy, "value2")
  expect_identical(session$lastInputMessage$message$numeric, TRUE)
  expect_identical(session$lastInputMessage$message$decreasing, TRUE)
  expect_identical(session$lastInputMessage$message$random, FALSE)
})



test_that("randomize_cards works", {
  randomize_cards(session, "grid")

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "sort")
  expect_null(session$lastInputMessage$message$sortBy)
  expect_null(session$lastInputMessage$message$numeric)
  expect_null(session$lastInputMessage$message$decreasing)
  expect_identical(session$lastInputMessage$message$random, TRUE)
})



test_that("filter_cards_groups works", {
  filter_cards_groups(session, "grid", groups = "A")

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "filter-groups")
  expect_identical(session$lastInputMessage$message$groups, "A")

  filter_cards_groups(session, "grid", groups = c("A", "B"))
  expect_identical(session$lastInputMessage$message$groups, c("A", "B"))
})



test_that("filter_cards works", {
  filter_cards(session, "grid", by = "id", filters = list(a = TRUE, b = FALSE))

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "filter-custom")
  expect_identical(session$lastInputMessage$message$filterBy, "id")

  # named list
  expect_length(session$lastInputMessage$message$filterList, 2)
  expect_true(session$lastInputMessage$message$filterList$a)
  expect_false(session$lastInputMessage$message$filterList$b)

  # two-columns data.frame
  filter_cards(session, "grid", by = "id", filters = data.frame(var1 = c("a", "b"), var2 = c(TRUE, FALSE)))
  expect_length(session$lastInputMessage$message$filterList, 2)
  expect_true(session$lastInputMessage$message$filterList$a)
  expect_false(session$lastInputMessage$message$filterList$b)

  # one-column data.frame
  filter_cards(session, "grid", by = "id", filters = data.frame(var1 = c("a")))
  expect_length(session$lastInputMessage$message$filterList, 1)
  expect_true(session$lastInputMessage$message$filterList$a)
  expect_null(session$lastInputMessage$message$filterList$b)

  # character vector
  filter_cards(session, "grid", by = "id", filters = c("a"))
  expect_length(session$lastInputMessage$message$filterList, 1)
  expect_true(session$lastInputMessage$message$filterList$a)
  expect_null(session$lastInputMessage$message$filterList$b)

  # Invalid format
  expect_error(filter_cards(session, "grid", by = "id", filters = data.frame(var2 = c(TRUE, FALSE), var1 = c("a", "b"))))
  expect_warning(filter_cards(session, "grid", by = "id", filters = list(a = c(TRUE, TRUE), b = c(FALSE, TRUE))))
})



test_that("update_shuffle works", {
  update_shuffle(session, "grid")

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "update")
})



test_that("update_card works", {
  # update attribute
  update_card(session, "grid", cardId = "A", value = 12)

  expect_identical(session$lastInputMessage$id, "grid")
  expect_identical(session$lastInputMessage$message$type, "update-card")
  expect_identical(session$lastInputMessage$message$id, "A")

  expect_named(session$lastInputMessage$message$args)
  expect_identical(names(session$lastInputMessage$message$args), "data-value")

  expect_null(session$lastInputMessage$message$title)

  # update title
  update_card(session, "grid", cardId = "A", title = "New title")
  expect_null(session$lastInputMessage$message$args)
  expect_identical(session$lastInputMessage$message$title, "New title")
})


