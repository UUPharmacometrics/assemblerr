#' @export
render.model_nm <- function(model){
  # generate $PROBLEM title
  problem_title <- get_by_name(model, "meta_tags", "title")$value
  if(is.null(problem_title)) problem_title <- "assemblerr model"

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
    render_str()

  # generate pk variable code
  pk_variable_code <- model$algebraic_equations %>%
    purrr::transpose() %>%
    purrr::map("equation") %>%
    purrr::map(render) %>%
    render_str()

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
  dvid_item <- get_first(model, "data_items", type == "dvid")
  use_dvid <- nrow(model$observation_equations)>1
  if(!rlang::is_empty(dvid_item)) {
    dvid_name <- dvid_item$name
  }else{
    dvid_name <- "DVID"
    if(use_dvid) rlang::warn("More than one observation model used but no data item with type dvid found. Resorting to 'DVID'.")
  }
  observation_code <- model$observation_equations %>%
    purrr::transpose() %>%
    {purrr::set_names(., purrr::map(., "name"))} %>%
    purrr::map(~list(.x$ipred_equation, .x$ruv_equation)) %>%
    purrr::modify_depth(2, render) %>%
    purrr::imap(function(obs_code_list,dv_name) {
        obs_code <- render_str(obs_code_list)
        if(dv_name!="" && use_dvid) {
          stringr::str_interp("
IF(${dvid_name}.EQ.${dv_name}) THEN ;${dv_name}
${obs_code}
ENDIF")
        }else{
          obs_code
        }
      }) %>%
    render_str()

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
$PROBLEM ${problem_title}
$INPUT ${input_code}
$SUBROUTINES ADVAN6 TOL=9
${model_code}
$PK
${param_code}
${pk_variable_code}
$DES
${ode_code}
$ERROR
${observation_code}
${theta_code}
${omega_code}
${sigma_code}
")
}

render_str <- function(str){
  if(rlang::is_empty(str)) return("")
  return(paste(str, collapse = "\n"))
}

