.prmzs <- list()

add_parameterizations <- function(prm_type, input, output){
  .prmzs[[prm_type]][["input"]] <<- input
  .prmzs[[prm_type]][["output"]] <<- output
  invisible()
}

