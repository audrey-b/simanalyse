#' Function Template
#'
#' A function template.
#' 
#' @param x A flag specifying whether to return TRUE or FALSE.
#' @param chk A flag specifying whether to check inputs.
#' @return A flag.
#' @export
#'
#' @examples
#' function_template()
#' function_template(FALSE)
function_template <- function(x = TRUE, chk = TRUE) {
  if(isTRUE(chk)) {
    chk_flag(x)
  }
  x
}
