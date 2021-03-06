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

cp.lower = function(x, alpha) quantile(x, alpha/2)

cp.upper = function(x, alpha) quantile(x, 1-alpha/2)

name_of_function = function(fun) deparse(substitute(fun))

sprintf_custom = function(code, code.value){
  spl <- strsplit(code, "\\?")[[1]]
  chk_equal(length(code.value), length(spl)-1)
  return(paste0(spl, c(code.value,""), collapse = ""))
}

logit <- function(x){
  return(log(x/(1-x)))
}
