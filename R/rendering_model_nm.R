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

  pk_declarations <- model$parameter_equations%>%
    purrr::transpose() %>%
    purrr::map("equation")

  pred_model <- FALSE
  if(nrow(model$odes)==0){
    pred_model <- TRUE
  }

  if(!pred_model){
    ode_declarations <- model$odes %>%
      dplyr::arrange(index) %>%
      purrr::transpose() %>%
      purrr::map("equation")
  }else{
    ode_declarations <- list()
  }


  algebraic_declarations <- model$algebraic_equations %>%
    purrr::transpose() %>%
    purrr::map("equation") %>%
    classify_declarations(ode_declarations)

  error_declarations <- list()

  append_order_render <- function(declarations, block){
    append(declarations,
           dplyr::filter(algebraic_declarations, block == !!(block)) %>%
             purrr::pluck("equation")) %>%
             {.[topologic_order(.)]} %>%
      purrr::map(render) %>%
      render_str()
  }

  pk_code <- append_order_render(pk_declarations, "PK")
  error_code <- append_order_render(error_declarations, "ERROR")

  if(!pred_model){
    ode_code <- append_order_render(ode_declarations, "DES")

    # generate $MODEL code
    model_code <- model$odes %>%
      dplyr::mutate(
        nm_name = toupper(.$name),
        code = paste0("COMPARTMENT=(",nm_name,")")) %>%
      .$code %>%
      {paste0("$MODEL NCOMPARTMENTS=", length(.), " ", paste(., collapse = " "))}
  }


  # generate observation code
  dvid_item <- get_first(model, "data_items", type == "dvid")
  use_dvid <- nrow(model$observation_declarations)>1
  if(!rlang::is_empty(dvid_item)) {
    dvid_name <- dvid_item$name
    mapping <- dvid_item$properties$mapping
  }else{
    dvid_name <- "DVID"
    if(use_dvid) rlang::warn("More than one observation model used but no data item with type dvid found. Resorting to 'DVID'.")
  }

  observation_code <- model$observation_declarations %>%
    purrr::transpose() %>%
    {purrr::set_names(., purrr::map(., "name"))} %>%
    purrr::map("declarations") %>%
    purrr::modify_depth(2, render) %>%
    purrr::imap(function(obs_code_list,dv_name) {
        obs_code <- render_str(obs_code_list)
        if(!is.na(dv_name) && use_dvid) {
          dvid <- mapping[[dv_name]]
          stringr::str_interp(
"IF(${dvid_name}.EQ.${dvid}) THEN ;${dv_name}
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

  if(pred_model){
      stringr::str_interp(
    "
$PROBLEM ${problem_title}
$INPUT ${input_code}
$PRED
${pk_code}
${error_code}
${observation_code}
${theta_code}
${omega_code}
${sigma_code}
")
  }else{

      stringr::str_interp(
    "
$PROBLEM ${problem_title}
$INPUT ${input_code}
$SUBROUTINES ADVAN6 TOL=9
${model_code}
$PK
${pk_code}
$DES
${ode_code}
$ERROR
${error_code}
${observation_code}
${theta_code}
${omega_code}
${sigma_code}
")
  }
}

render_str <- function(str){
  if(rlang::is_empty(str)) return("")
  return(paste(str, collapse = "\n"))
}

# function to determine whether an algebraic declaration needs to go to PK, DES or ERROR
classify_declarations <- function(dl, odes){
  ode_identifiers <- purrr::map_chr(odes, ~dec_get_id(.) %>% deparse())
  dl %>%
    purrr::map_chr(function(d){
      var <- dec_get_id(d) %>% deparse()
      if(!depends_on(var, "A", dl)){
        return("PK")
      }else{
        if(any(purrr::map_lgl(ode_identifiers, ~depends_on(.x, var, odes)))){
          return("DES")
        }else{
          return("ERROR")
        }
      }
    }) %>%
  { tibble::tibble(equation = dl, block = .) }
}
