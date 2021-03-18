#' @export
assemblerr_options <- function(prm.use_mu_referencing = FALSE,
                       ode.use_special_advans = TRUE,
                       ode.use_general_linear_advans = TRUE,
                       ode.general_nonlinear_advan = "advan13",
                       ode.general_linear_advan = "advan5",
                       ode.preferred_trans_routines = c("trans2", "trans4"),
                       default_record.covariance_step = nm_covariance(),
                       default_record.estimation_step = nm_estimation(),
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
      default_record.covariance_step = default_record.covariance_step,
      default_record.estimation_step = default_record.estimation_step,
      issues.missing_variables = match.arg(issues.missing_variables)
    )
  )
}
