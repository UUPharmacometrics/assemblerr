ODESystem <- function(){
  self <- structure(class = "ODESystem", environment())

  compartments <- list(
    null = Compartment("null", 0)
  )
  observation_compartment <- NULL
  dose_compartment <- NULL

  add_compartment <- function(name, volume){
    if(name %in% names(compartments)) stop("Compartment ", name, " already exists.")
    compartments[[name]] <<- Compartment(name = name,
                                      index = length(compartments),
                                      volume = volume)
    return(compartments[[name]])
  }

  request_compartment <- function(name, volume){
    if(name %in% names(compartments)){
      return(compartments[[name]])
    }else{
      return(add_compartment(name, volume))
    }
  }

  add_flow <- function(from = compartments[[1]],
                       to  = compartments[[1]],
                       equation){
    from$add_outflow(to, equation)
    to$add_inflow(from, equation)
  }

  get_des_code <- function(){
    compartments %>%
      purrr::map(~.x$get_des_code())
  }
  self
}


Compartment <- function(name, index, volume){
  self <- structure(class = "Compartment", environment())
  name <- name
  index <- index
  if(missing(volume)) volume <- quote(1)
  volume <- volume

  flows <- list()

  add_outflow <- function(to, equation){
    flows <<-c(flows, Outflow(from = self, to = to, equation))
  }

  add_inflow <- function(from, equation){
    flows <<- c(flows, Inflow(from = from, to = self,  equation))
  }

  get_des_code <- function(){
    equation <- flows %>%
      purrr::map(~ .x$get_des_code()) %>%
      purrr::reduce(function(a,b) substitute(a + b, list(a = a, b = b)))

      substitute(dA_dt[i]  <- equation, list(i = index, equation = equation))
  }
  self
}

Flow <- function(from, to, equation){
  if(!inherits(from, "Compartment")) stop("argument 'from' needs to be a compartment")

  if(!inherits(to, "Compartment")) stop("argument 'to' needs to be a compartment")

  if(!is.language(equation)) stop("argument 'equation' need to be of type language")

  from <- from
  to <- to
  equation <- equation

  get_des_code <- function(){
    amount_expression <- substitute(A[i], list(i = from$index))
    concentration_expression <- substitute(A[i]/V, list(i = from$index,
                                                 V = from$volume))
    des_code <- pryr::substitute_q(equation,
                                   list(A = amount_expression,
                                        C = concentration_expression))
    return(des_code)
  }

  structure(class = "Flow", environment())
}

Inflow <- function(from, to, equation){
  self <- Flow(from, to, equation)
  structure(class = c("Inflow", class(self)), self)
}

Outflow <- function(from, to, equation){
  self <- Flow(from, to, equation)

  self$equation <- substitute(-x, list(x = self$equation))

  structure(class = c("Outflow", class(self)), self)
}

ode <- ODESystem()
central <- ode$request_compartment("central", 1)
peri <- ode$request_compartment("peri", 2)
ode$add_flow(central, equation = quote(CL*C))
ode$add_flow(central, peri, quote(Q*C))
# central$add_outflow(to = peri, equation = quote(CL*A))
# central$add_inflow(from = peri, equation = quote(Q*A))
ode$get_des_code()
