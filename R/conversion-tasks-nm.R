#' @include tasks.R


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "GenericModel", component = "ModelingTasks"),
  definition = function(target, source, component, options) {
    purrr::reduce(
      .init = target,
      .x = component,
      .f = ~convert(.x, source, .y, options)
    )
  }
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "GenericModel", component = "EstimationTask"),
  definition = function(target, source, component, options) {
    method <- switch(
      component@algorithm,
      foce = "cond",
      `foce-inter` = "cond",
      `foce-no-inter` = "cond",
      `fo` = "0",
      `imp` = "imp",
      `saem` = "saem"
    )
    if (component@algorithm == "foce") {
      interaction <- source@facets$ObservationFacet@entries[[1]]@proportional_term
    } else {
      interaction <- component@algorithm == "foce-inter"
    }
    if (method %in% c("cond", "0")) {
      maxeval <- 999999L
    } else {
      maxeval <- NA_integer_
    }
    if (method %in% c("imp", "saem")) {
      auto <- TRUE
    } else {
      auto <- NA
    }
    cov_step <- NULL
    if (component@standard_errors && length(target@facets$NmCovarianceStepFacet@entries) == 0) {
      cov_step <- nm_covariance()
    }
    target +
      nm_estimation(
        method = method,
        interaction = interaction,
        maxeval = maxeval,
        auto = auto,
        target_options = component@target_options
      ) +
      cov_step
  }
)


setMethod(
  f = "convert",
  signature = c(target = "NmModel", source = "GenericModel", component = "OutputTask"),
  definition = function(target, source, component, options) {
    available_variables <- c(
      defined_variables(source),
      defined_variables(target)
    )
    selected_variables <- select_variables(available_variables, !!component@selector)
    target +
      nm_table(
        filename = component@filename,
        items = names(selected_variables)
      )
  }
)
