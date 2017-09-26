#' #' @importFrom R6 R6Class
#' AssembledModel <- R6Class(
#'   "AssembledModel",
#'   public = list(
#'     theta_facet = NMThetaFacet$new()
#'   )
#' )
#'
#' #' @importFrom R6 R6Class
#' AssembledODEModel <- R6Class(
#'   "AssembledODEModel",
#'   inherit = AssembledModel,
#'   public = list(
#'     ode_system = NULL,
#'     initialize = function(){
#'       self$ode_system <- ODESystem$new()
#'     }
#'   )
#' )

