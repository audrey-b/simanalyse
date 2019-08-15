rinteger <- function(n = 1) as.integer(runif(n, -.max_integer, .max_integer))

set_class <- function(x, class) {
  class(x) <- class
  x
}

set_names <- function(x, names) {
  names(x) <- names
  x
}

sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC" 
  time
}

tibble <- function(...) {
  data <- data.frame(..., stringsAsFactors = FALSE)
  class(data) <- c("tbl_df", "tbl", "data.frame")
  data
}

is_try_error <- function(x) inherits(x, "try-error")

rinteger <- function(n = 1) as.integer(runif(n, -.max_integer, .max_integer))

# from https://recology.info/2018/10/limiting-dependencies/
remove_nulls <- function(x) Filter(Negate(is.null), x)

# from https://recology.info/2018/10/limiting-dependencies/
str_extract <- function(x, y) regmatches(x, regexpr(y, x))

# from https://recology.info/2018/10/limiting-dependencies/
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

cp.low = function(x, alpha) quantile(x, alpha/2)

cp.high = function(x, alpha) quantile(x, 1-alpha/2)
