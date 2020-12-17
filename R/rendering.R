#' @include model.R
#' @include pk_model.R


setGeneric(name = "render_component",
           def = function(x, ...) standardGeneric("render_component"),
           valueClass = "character")

#' @export
render <- function(model,
                   target_tool = "nonmem",
                   options = assemblerr_options()) {
  if (target_tool == "nonmem") {
    convert(nm_model(), model, options = options) %>%
      render_component()
  } else {
    cli::cli_alert_danger("Tool '{target_tool}' is currently not supported.")
  }
}


# setMethod(
#   f = "render",
#   signature = c(x = "Model"),
#   definition = function(x) {
#     convert(nm_model(), x) %>%
#       render()
#   }
# )
#
#
# setMethod(
#   f = "render",
#   signature = c(x = "PkModel"),
#   definition = function(x) {
#     convert(model(), x) %>%
#     convert(nm_model(), .) %>%
#       render()
#   }
# )
