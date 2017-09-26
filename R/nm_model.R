NMModel <- R6::R6Class("NMModel",
                       public = list(
                         initialize = function(){
                           private$parameter_facet <- NMParameterFacet$new()
                           private$theta_facet <- NMThetaFacet$new()
                           private$eta_facet <- NMEtaFacet$new()
                         },
                         add_parameter =function(name, ...) private$parameter_facet$add_new(name = name, ...),
                         add_theta = function(name, ...) private$theta_facet$add_new(name = name, ...),
                         add_eta = function(name, ...) private$eta_facet$add_new(name = name, ...),
                         print = function(){
                           template_vars <- list(
                             parameter_definition = private$parameter_facet$map(~.x$get_code()) %>% paste(collapse = "\n"),
                             theta_iv = private$theta_facet$map(~.x$get_iv_code()) %>% paste(collapse = "\n"),
                             omega_iv = private$eta_facet$map(~.x$get_iv_code()) %>% paste(collapse = "\n")
                           )
                           nmtran_code <- stringr::str_interp(
"$PRED
\t${parameter_definition}
${theta_iv}
${omega_iv}
", template_vars)
                           cat(nmtran_code)
                         }
                       ),
                       private = list(
                         parameter_facet = NULL,
                         theta_facet = NULL,
                         eta_facet = NULL
                       ))
