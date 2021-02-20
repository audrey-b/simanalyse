rinteger <- function(n = 1) as.integer(runif(n, -.max_integer, .max_integer))

# from https://recology.info/2018/10/limiting-dependencies/
str_extract <- function(x, y) regmatches(x, regexpr(y, x))

cp.lower <- function(x, alpha) quantile(x, alpha / 2)

cp.upper <- function(x, alpha) quantile(x, 1 - alpha / 2)

sprintf_custom <- function(code, code.value) {
  spl <- strsplit(code, "\\?")[[1]]
  chk_equal(length(code.value), length(spl) - 1)
  return(paste0(spl, c(code.value, ""), collapse = ""))
}
