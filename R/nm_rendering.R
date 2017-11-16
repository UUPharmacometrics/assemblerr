#' @export
render.model_nm <- function(model){
  # generate $INPUT code
  input_code <- model$data_items %>%
      dplyr::arrange(index) %>%
      purrr::pluck("name") %>%
      paste(collapse = " ")


  # generate parameter code
  param_code <- model$parameter_equations%>%
    purrr::transpose() %>%
    purrr::map("equation") %>%
    purrr::map(render) %>%
    stringr::str_c(collapse = "\n")

  # generate $MODEL code
  model_code <- model$odes %>%
    dplyr::mutate(
      nm_name = toupper(.$name),
      code = paste0("COMPARTMENT=(",nm_name,")")) %>%
    .$code %>%
    {paste0("$MODEL NCOMPARTMENTS=", length(.), " ", paste(., collapse = " "))}

  # generate ODE code
  ode_code <- model$odes %>%
    dplyr::arrange(index) %>%
    purrr::transpose() %>%
    purrr::map("equation") %>%
    purrr::map(render) %>%
    stringr::str_c(collapse = "\n")

  # generate observation code
  observation_code <- model$observation_equations %>%
    purrr::transpose() %>%
    purrr::map(~list(.x$ipred_equation, .x$ruv_equation)) %>%
    purrr::flatten() %>%
    purrr::map(render) %>%
    stringr::str_c(collapse = "\n")

  # generate $THETA code
  theta_code <- model$thetas %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$THETA (%s, %s, %s) \t;%s", lbound, initial, ubound, toupper(name)) %>% toupper()) %>%
    {paste(.$init_code, collapse = "\n")}

  # generate $OMEGA code
  omega_code <- model$omegas %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$OMEGA %s \t;IIV-%s", initial, toupper(name))) %>%
    {paste(.$init_code, collapse = "\n")}

  # generate $OMEGA code
  sigma_code <- model$sigmas %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$SIGMA %s \t;%s", initial, toupper(name))) %>%
    {paste(.$init_code, collapse = "\n")}


  stringr::str_interp(
    "
$PROBLEM assemblerr model
$INPUT ${input_code}
$SUBROUTINES ADVAN6 TOL=9
${model_code}
$PK
${param_code}
$DES
${ode_code}
$ERROR
${observation_code}
${theta_code}
${omega_code}
${sigma_code}
")
}

