#' Read files
#'
#' Read output files
#' 
#' @param fun The function from which to read the ouput file. Options are sma_analyse_files, sma_derive_files and sma_evaluate_files.
#' @param path A string. The object is read using this path.
#' @param folder A string for the name of the folder within the path that contains the analysis.
#' @param id An integer for the index of the object to extract. 
#' 
#' @return The object read from files.
#' @export

#' @examples
#' params <- nlist(mu=0)
#' sims::sims_simulate("a ~ dnorm(mu, 1)", 
#'                     parameters = params, 
#'                     nsims = 5,
#'                     save = TRUE,
#'                     path=tempdir(),
#'                     exist = NA)
#' sma_analyse_files(code = "a ~ dnorm(mu, 1)
#'                           mu ~ dunif(-3,3)",
#'                   mode = sma_set_mode("quick"),
#'                   monitor = "mu",
#'                   path = tempdir())
#' sma_evaluate_files(parameters = params, 
#'                    path = tempdir())
#' sma_read_files(sma_evaluate_files, path = tempdir())

sma_read_files <- function(fun,
                           path=".",
                           folder="analysis0000001",
                           id=1){
  
  chk_function(fun)
  chkor(chk_identical(fun, sma_analyse_files),
        chk_identical(fun, sma_derive_files),
        chk_identical(fun, sma_evaluate_files))
  
  extract_file <- function(name){
    path_long <- file.path(path, folder, name)
    files <- list.files(path_long)
    files <- stringr::str_sort(files)
    file <- file.path(path_long, files[id])
    return(readRDS(file))
  }
  
  if(vld_identical(fun, sma_analyse_files)){
    return(extract_file("results"))
  }
  if(vld_identical(fun, sma_derive_files)){
    return(extract_file("derived"))
  }
  if(vld_identical(fun, sma_evaluate_files)){
    return(extract_file("performance"))
  }
  
}