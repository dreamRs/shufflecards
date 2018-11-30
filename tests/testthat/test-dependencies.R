context("test-dependencies")

test_that("shuffle_dependencies works", {
  dep <- htmltools::resolveDependencies(list(
    shuffle_dependencies()
  ), resolvePackageDir = FALSE)
  path <- system.file(dep[[1]]$src$file, package = "shufflecards")
  expect_true(all(file.exists(file.path(path, dep[[1]]$script))))
  expect_true(all(file.exists(file.path(path, dep[[1]]$stylesheet))))
})

test_that("playing_cards_dependencies works", {
  dep <- htmltools::resolveDependencies(list(
    playing_cards_dependencies()
  ), resolvePackageDir = FALSE)
  path <- system.file(dep[[1]]$src$file, package = "shufflecards")
  expect_true(all(file.exists(file.path(path, dep[[1]]$stylesheet))))
})

test_that("polyfill_dependencies works", {
  dep <- htmltools::resolveDependencies(list(
    polyfill_dependencies()
  ), resolvePackageDir = FALSE)
  path <- system.file(dep[[1]]$src$file, package = "shufflecards")
  expect_true(all(file.exists(file.path(path, dep[[1]]$script))))
})
