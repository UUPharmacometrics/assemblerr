AssembledModel <- function(){
  self <- structure(class = "AssembledModel", environment())
  header_code <- list()
  theta_set <- list()
  eta_set <- list()
  eps_set <- list()
  code_scopes <- list()


}

AssembledODEModel <- function(){
  parent <- AssembledModel()
  self <- structure(class = c("AssembledODEModel", class(self)),
            environment())
  self$ode_system <- list()

}
