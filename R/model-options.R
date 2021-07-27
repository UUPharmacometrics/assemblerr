
#' Options
#'
#' This function creates a list of options for the use with the render function.
#'
#' The function helps to create properly formatted list that can serve as input to the `options=` argument
#' of the `render()` function.
#'
#' @param prm.use_mu_referencing Use mu-referencing?
#' @param ode.use_special_advans Use analytic solution ADVANs?
#' @param ode.use_general_linear_advans Use ADVANs for linear ODEs?
#' @param ode.general_nonlinear_advan ADVAN to be used for non-linear ODEs
#' @param ode.general_linear_advan ADVAN to be used for linear ODEs
#' @param ode.preferred_trans_routines Order of TRANS routines to be tried
#' @param issues.missing_variables How to handle missing variables
#'
#' @return A list of options
#'
#' @md
#' @export
assemblerr_options <- function(prm.use_mu_referencing = FALSE,
                       ode.use_special_advans = TRUE,
                       ode.use_general_linear_advans = TRUE,
                       ode.general_nonlinear_advan = "advan13",
                       ode.general_linear_advan = "advan5",
                       ode.preferred_trans_routines = c("trans2", "trans4"),
                       issues.missing_variables = c("fix-warn", "fix", "ignore", "fail")) {
  # TODO: add 'warn' option for issues.missing_variables
  return(
    list(
      prm.use_mu_referencing = prm.use_mu_referencing,
      ode.use_special_advans = ode.use_special_advans,
      ode.use_general_linear_advans = ode.use_general_linear_advans,
      ode.general_nonlinear_advan = ode.general_nonlinear_advan,
      ode.general_linear_advan = ode.general_linear_advan,
      ode.preferred_trans_routines = ode.preferred_trans_routines,
      issues.missing_variables = match.arg(issues.missing_variables)
    )
  )
}
