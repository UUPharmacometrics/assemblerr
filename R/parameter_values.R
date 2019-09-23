



#' Get parameter values
#'
#' helpful: (https://sites.google.com/site/probonto/download)
#'
#' @param m Model
#' @param prm Parameter
#' @param prmz Parameterization
#'
#' @return Parameter values as a named vector in requested parameterization or a vector of NA values if
#' something went wrong.
get_pv <- function(m, prm, prmz){
  # collect all parameter value information for specific parameter
  pvs <- get_all_pvs(m, prm$name)
  # convert provided prm values to std parameterization

  std_pvs <- get_standard_parameterization(pvs, .prmzs[[prm$type]][["input"]], prm$name)

  # convert from std prmz to requested one

  convert_to_parameterization(std_pvs, .prmzs[[prm$type]][["output"]], prmz)
}

get_all_pvs <- function(model, prm_name){
  pvs <- get_all(m, "parameter_values", parameter == prm_name) %>%
    purrr::map("values") %>%
    purrr::flatten_dbl()
}

get_standard_parameterization <- function(parameter_values, input_prmz, prm_name){
  # safely evaluate each input paramterization with the list of pvs
  conversion_results <- purrr::map(input_prmz, purrr::safely) %>%
    purrr::map(~rlang::exec(.x, !!!parameter_values))

  # retrieve successful conversions
  successful_conversion <- conversion_results %>%
    purrr::map("result") %>%
    purrr::compact()

  if(rlang::is_empty(successful_conversion)) {
    ui_warning("Did not find matching parameter values.")
    return(c(NA,NA))
  }
  if(length(successful_conversion)>1) {
    ui_warning("Multiple parameter values.")
  }
  std_pvs <- successful_conversion[[1]]
  # warn about unused values
  if(!rlang::is_empty(std_pvs$not_used))
    ui_warning("Parameter values ", paste(names(std_pvs$not_used), collapse = ","),
               " provided for parameter '", prm_name,"' not required and ignored.")
  std_pvs$not_used <- NULL
  std_pvs
}

convert_to_parameterization <- function(std_pvs, output_prmzs, prmz){
  if(!exists(prmz, output_prmzs)) {
    ui_warning("Requested output parameterization not available.")
    return(c(NA,NA))
  }
  rlang::exec(output_prmzs[[prmz]], !!!std_pvs)
}
