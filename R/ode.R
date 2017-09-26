
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
ODESystem <- R6Class(
  "ODESystem",
  inherit = Artefact,
  public = list(
    compartments = list(),
    observation_compartment = NULL,
    dose_compartment = NULL,
    initialize = function(){
      self$compartments$null  <-  Compartment$new("null", 0)
    },

    add_compartment = function(name, volume) {
      if (name %in% names(self$compartments))
        stop("Compartment ", name, " already exists.")
      self$compartments[[name]] <- Compartment$new(
        name = name,
        index = length(self$compartments),
        volume = volume
      )
      return(self$compartments[[name]])
    },
    request_compartment = function(name, volume) {
      if (name %in% names(self$compartments)) {
        return(self$compartments[[name]])
      } else{
        return(self$add_compartment(name, volume))
      }
    },
    add_flow = function(from = self$compartments[[1]],
                        to  = self$compartments[[1]],
                        equation) {
      from$add_outflow(to, equation)
      to$add_inflow(from, equation)
    },
    get_des_code = function() {
      self$compartments %>%
        purrr::map(~ .x$get_des_code())
    }

  )
)


#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
Compartment <- R6Class(
  "Compartment",
  public = list(
    name = NULL,
    index = NULL,
    volume = NULL,
    flows = list(),
    initialize = function(name, index, volume = quote(1)) {
      self$name <- name
      self$index <- index
      self$volume <- volume
    },
    add_outflow = function(to, equation) {
      self$flows <-
        c(self$flows, Outflow$new(from = self, to = to, equation))
    },
    add_inflow = function(from, equation) {
      self$flows  <-
        c(self$flows, Inflow$new(from = from, to = self,  equation))
    },
    get_des_code = function() {
      equation <- self$flows %>%
        purrr::map( ~ .x$get_des_code()) %>%
        purrr::reduce(function(a, b)
          substitute(a + b, list(a = a, b = b)))

      substitute(dA_dt[i]  <-
                   equation, list(i = self$index, equation = equation))
    }
  )
)

#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
Flow <- R6Class(
  "Flow",
  public <- list(
    from = NULL,
    to = NULL,
    equation = NULL,
    initialize = function(from, to, equation) {
      if (!inherits(from, "Compartment"))
        stop("argument 'from' needs to be a compartment")

      if (!inherits(to, "Compartment"))
        stop("argument 'to' needs to be a compartment")

      if (!is.language(equation))
        stop("argument 'equation' need to be of type language")
      self$from <- from
      self$to <- to
      self$equation <- equation
    },
    get_des_code = function() {
      amount_expression <- substitute(A[i], list(i = self$from$index))
      concentration_expression <-
        substitute(A[i] / V, list(i = self$from$index,
                                  V = self$from$volume))
      des_code <- pryr::substitute_q(self$equation,
                                     list(A = amount_expression,
                                          C = concentration_expression))
      return(des_code)
    }
  )
)


#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
Inflow <- R6Class("Inflow",
                  inherit = Flow)

#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
Outflow <- R6Class("Outflow",
                   inherit = Flow,
                   public = list(
                     initialize = function(from, to, equation){
                       super$initialize(from, to, equation)
                       self$equation <- substitute(-x, list(x = self$equation))
                     }

                   )
                   )
