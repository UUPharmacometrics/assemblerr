NMCodeFacet <- R6Class("NMCodeFacet",
                            inherit = Facet,
                            public = list(
                            ),
                       private = list(
                         type = "NMExpression"
                       )
)


NMExpression <- R6Class("NMExpression",
                        inherit = NMSyntaxElement,
                        public = list(
                          expression = list(),
                          initialize = function(...){
                            self$expression = list(...)
                          },
                          get_code = function(){
                            purrr::map_if(self$expression, is_nm_syntax_element, ~.x$get_code()) %>%
                              paste(collapse = "")
                          }
                        ))

NMParameterFacet <- R6Class("NMParameterFacet",
                            inherit = NMCodeFacet,
                            public = list(
                              add_new = function(name, ...){
                                super$add_new(name, "=", ...)
                              }
                            ))
