#' @export
assemblerr_options <- function(prm.use_mu_referencing = TRUE,
                       ode.use_special_advans = TRUE,
                       ode.use_general_linear_advans = TRUE,
                       ode.general_nonlinear_advan = "advan13",
                       ode.general_linear_advan = "advan5",
                       ode.preferred_trans_routines = c("trans2", "trans4")) {
  return(
    list(
      prm.use_mu_referencing = prm.use_mu_referencing,
      ode.use_special_advans = ode.use_special_advans,
      ode.use_general_linear_advans = ode.use_general_linear_advans,
      ode.general_nonlinear_advan = ode.general_nonlinear_advan,
      ode.general_linear_advan = ode.general_linear_advan,
      ode.preferred_trans_routines = ode.preferred_trans_routines
    )
  )
}
