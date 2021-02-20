test_that("str_extract", {
  expect_error(
    str_extract(character(0), character(0)),
    "^invalid 'pattern' argument$"
  )

  expect_identical(str_extract(character(0), ".*"), character(0))
  expect_identical(str_extract(NA_character_, ".*"), character(0))
  expect_identical(str_extract("a", ".*"), "a")
  expect_identical(str_extract(c("a", NA), ".*"), "a")
  expect_identical(str_extract(c("a", "b"), ".*"), c("a", "b"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w"), c("a", "b"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w\\w"), c("aa", "bb"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\w+"), c("aa", "bbb"))
  expect_identical(str_extract(c("aa a", "bbb"), "\\s\\w+"), " a")
})
