context("internal-utils")

test_that("set_class",{
  x <- list()
  expect_identical(class(x), "list")
  x <- set_class(x, "new_class")
  expect_identical(class(x), "new_class")
})

test_that("is_try_error",{
  expect_false(is_try_error(1))
  expect_false(is_try_error(identity(1)))
  expect_true(is_try_error(try(stop(), silent = TRUE)))
})

test_that("sys_time",{
  expect_is(sys_time(), "POSIXct")
  expect_identical(attr(sys_time(), "tzone"), "UTC")
})

test_that("remove_nulls",{
  # vector
  expect_identical(remove_nulls(integer(0)), integer(0))
  expect_identical(remove_nulls(1), 1)
  expect_identical(remove_nulls(NA), NA)
  expect_identical(remove_nulls(c(2, NA)), c(2, NA))
  
  # list
  expect_identical(remove_nulls(list()), list())
  expect_identical(remove_nulls(list(1)), list(1))
  expect_identical(remove_nulls(list(NA)), list(NA))
  expect_identical(remove_nulls(list(2, NA)), list(2, NA))
  expect_identical(remove_nulls(list(2, NA, NULL)), list(2, NA))
  expect_identical(remove_nulls(list(NULL)), list())
  expect_identical(remove_nulls(list(x = 1, y = NULL)), list(x = 1))
})

test_that("str_extract",{
  expect_error(str_extract(character(0), character(0)), 
               "^invalid 'pattern' argument$")
  
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


test_that("tibble",{
  expect_identical(tibble(data.frame()), 
                   structure(list(), .Names = character(0), class = c("tbl_df", 
                                                                      "tbl", "data.frame"), row.names = integer(0)))

  expect_identical(tibble(data.frame(x = 1)), 
                   structure(list(x = 1), class = c("tbl_df", "tbl", "data.frame"
), row.names = c(NA, -1L))) 
  
  expect_identical(tibble(x = 1), 
                   structure(list(x = 1), class = c("tbl_df", "tbl", "data.frame"
), row.names = c(NA, -1L))) 
  
  expect_identical(tibble(x = "1"), 
                   structure(list(x = "1"), class = c("tbl_df", "tbl", "data.frame"
), row.names = c(NA, -1L))) 
})

test_that("correct_modulo_sprintf",{
  code <- "z <- a %*% 2 \n v <- 5 %*% 3"
  expect_identical(correct_modulo_sprintf(code), "z <- a %%*%% 2 \n v <- 5 %%*%% 3")
})

