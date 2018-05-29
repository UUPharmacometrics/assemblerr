#' @export
render <- function(object, ...) UseMethod("render")

#' @export
render_opts_nm <- function(){
  list(
    allowed_functions = c(
      '+', '-', '*', '/', '^', # basic arithmetics
      '[', # vector access
      '(', # grouping
      'log', 'log10', 'exp', 'sqrt', 'sin', 'cos', 'abs', 'tan', 'asin', 'acos', 'atan', 'int', 'dnorm', 'min', 'max', '%%', 'lgamma' # built-in functions
      ),
    function_subtitutions = c(
      '^' = "%**%", # power operator
      '%%' = 'mod', # modulo operator
      'lgamma' = 'gamln', # logarithm of gamma function
      'dnorm' = 'phi' # cummulative distribution function of the normal distribution
    ),
    capitalize = TRUE,
    round_vec_brackets = TRUE,
    equal_assign_op = TRUE
  )
}


render_opts_viz <- function(){
  list(
    capitalize = FALSE,
    round_vec_brackets = FALSE,
    equal_assign_op = TRUE
  )
}
