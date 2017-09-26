
NMVariablesFacet <- R6Class("NMVariablesFacet",
                        inherit = Facet,
                        public = list(
                          add = function(item, name){
                            assertthat::assert_that(!missing(name), !is.null(name))
                            super$add(item, name = name)
                            last_index <- self$count()
                            private$items[[last_index]]$index <- last_index
                            return(private$items[[last_index]])
                          }
                        )
)


NMThetaFacet <- R6Class("NMThetaFacet",
                      inherit = NMVariablesFacet,
                      private = list(
                        type = "NMTheta"
                      )
)


NMTheta <- R6Class("NMTheta",
                   inherit = NMSyntaxElement,
                   public = list(
                     index = NULL,
                     name = NULL,
                     lbound = -Inf,
                     ubound = Inf,
                     initial_value = NA,
                     initialize = function(name, lbound = -Inf, ubound = Inf){
                       self$name <-  name
                       self$lbound <-  lbound
                       self$ubound <-  ubound
                     },
                     get_code = function() private$render_template("THETA(${index})"),
                     get_iv_code = function() private$render_template("$THETA (${lbound},${initial_value},${ubound}) ; ${name}")
                     )
)


NMEtaFacet <- R6Class("NMEtaFacet",
                        inherit = NMVariablesFacet,
                        private = list(
                          type = "NMEta"
                        )
)


NMEta <- R6Class("NMEta",
                 inherit = NMSyntaxElement,
                   public = list(
                     index = NULL,
                     name = NULL,
                     initial_value = NA,
                     initialize = function(name){
                       self$name <-  name
                     },
                     get_code = function() private$render_template("ETA(${index})"),
                     get_iv_code = function() private$render_template("$OMEGA ${initial_value} ; ${name}")
                   )
)

