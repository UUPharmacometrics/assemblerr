#' @export
render.nm_model <- function(object, opts){

  problem_title <- get_by_name(object, "problem", "name")$value
  if(is.null(problem_title)) problem_title <- "assemblerr model"

  input_code <- object$input %>%
    dplyr::arrange(.data$index) %>%
    purrr::pluck("name") %>%
    paste(collapse = " ")

  pk_code <- object$pk %>%
    purrr::transpose() %>%
    purrr::map("statement") %>%
    purrr::map(render, opts = render_opts_nm()) %>%
    paste(collapse="\n")

  des_code <- object$des %>%
    purrr::transpose() %>%
    purrr::map("statement") %>%
    purrr::map(render, opts = render_opts_nm()) %>%
    paste(collapse="\n")


  error_code <- object$error %>%
    purrr::transpose() %>%
    purrr::map("statement") %>%
    purrr::map(render, opts = render_opts_nm()) %>%
    paste(collapse="\n")

  # generate $THETA code
  theta_code <- object$theta %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$THETA (%s, %s, %s) \t;%s", .data$lbound, .data$initial, .data$ubound, toupper(.data$name)) %>% toupper()) %>%
    {paste(.$init_code, collapse = "\n")}

  # generate $OMEGA code
  omega_code <- object$omega %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$OMEGA %s \t;IIV-%s", .data$initial, toupper(.data$name))) %>%
    {paste(.$init_code, collapse = "\n")}

  # generate $OMEGA code
  sigma_code <- object$sigma %>%
    dplyr::mutate_if(is.numeric, format) %>%
    dplyr::mutate(init_code = sprintf("$SIGMA %s \t;%s", .data$initial, toupper(.data$name))) %>%
    {paste(.$init_code, collapse = "\n")}

  header <- stringr::str_interp("
$PROBLEM ${problem_title}
$INPUT ${input_code}")
  footer <- stringr::str_interp("${theta_code}
${omega_code}
${sigma_code}")

  if(nrow(object$des)>0) {
    body <- stringr::str_interp("$PK
${pk_code}
$DES
${des_code}
$ERROR
${error_code}")
  }else{
    body <- stringr::str_interp("$PRED
${pk_code}
${error_code}")
  }
  paste(header, body, footer, sep = "\n")
# ${error_code}
# ${observation_code}
# ${theta_code}
# ${omega_code}
# ${sigma_code}
# ")
#   }else{
#
#     stringr::str_interp(
#       "
# $PROBLEM ${problem_title}
# $INPUT ${input_code}
# $SUBROUTINES ADVAN6 TOL=9
# ${model_code}
# $PK
# ${pk_code}
# $DES
# ${ode_code}
# $ERROR
# ${error_code}
# ${observation_code}
# ${theta_code}
# ${omega_code}
# ${sigma_code}
# ")
#   }
}
