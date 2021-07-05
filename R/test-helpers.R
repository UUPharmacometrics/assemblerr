
#' @keywords internal
local_create_nonmem_test_directory <- function(path = tempdir(), debug = FALSE, env = parent.frame()) {
  dirname <- tempfile("dir", tmpdir = path)
  dir.create(dirname, showWarnings = TRUE, recursive = FALSE)
  if (!debug) withr::defer(unlink(dirname, recursive = TRUE), envir = env)

  wd <- getwd()
  setwd(dirname)
  withr::defer(setwd(wd), envir = env)
  return(dirname)
}


simple_model <- function(prm = prm_log_normal("k"), obs = obs_additive(~conc), vars = input_variable("time") + input_variable("c0") + input_variable("id")) {
  model() +
    prm +
    algebraic(conc~c0*exp(-k*time)) +
    obs +
    vars
}

simple_ode_model <- function(prms = prm_log_normal("cl") + prm_log_normal("v"), obs = obs_additive(~C["central"])) {
  model() +
    prms +
    obs +
    compartment("central", volume = "v") +
    flow(~cl*C, from = "central")
}

simple_pk_model <- function() {
  pk_model() +
    pk_absorption_fo() +
    pk_distribution_1cmp() +
    pk_elimination_linear() +
    obs_additive(~C["central"])
}


#' @keywords internal
#' @importFrom utils write.csv
create_dummy_data <- function(model, path = NULL){
  nm <- convert(nm_model(), source = model, options = assemblerr_options())
  variables <- names(nm@facets$NmInputEntryFacet)
  values <- list()
  if ("id" %in% variables) values[["id"]] <- 1:10
  if ("time" %in% variables) values[["time"]] <- c(0, 1, 2, 4, 8, 16)
  values[variables[!variables %in% names(values)]] <- 0
  df <- purrr::cross(values) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    new_data_frame() %>%
    vec_sort()
  if (is.null(path)) {
    return(df)
  }
  write.csv(df, path, quote = FALSE, row.names = FALSE)
}

expect_contains <- function(object, str) {
  return(testthat::expect_match(object, str, fixed = TRUE, all = FALSE))
}

expect_does_not_contain <- function(object, regexp) {
  act <- testthat::quasi_label(rlang::enquo(object), label = NULL, arg = "object")
  stopifnot(is.character(regexp), length(regexp) == 1)
  stopifnot(is.character(act$val))
  if (length(object) == 0) {
    testthat::fail(sprintf("%s is empty.", act$lab))
  }
  matches <- !grepl(regexp, act$val, fixed = TRUE)
  if (length(act$val) == 1) {
    values <- paste0("Actual value: \"", encodeString(act$val),
                     "\"")
  }
  else {
    values <- paste0("Actual values:\n", paste0("* ", encodeString(act$val),
                                                collapse = "\n"))
  }
  testthat::expect(all(matches), sprintf("%s does contain %s.\n%s", act$lab,
                               encodeString(regexp, quote = "\""), values))
  invisible(act$val)
}

expect_matching_issue <- function(object, regexp) {
  act <- testthat::quasi_label(rlang::enquo(object), label = NULL, arg = "object")
  matches <- grepl(pattern = regexp, x = as.character(object))
  testthat::expect(
    any(matches),
    paste(act$lab, "did not contain issues matching", encodeString(regexp, quote = "\""))
  )
  return(invisible(object))
}

expect_no_matching_issue <- function(object, regexp) {
  act <- testthat::quasi_label(rlang::enquo(object), label = NULL, arg = "issues")
  matches <- grepl(pattern = regexp, x = as.character(object))
  testthat::expect(
    !any(matches),
    paste(act$lab, "contained issue matching", encodeString(regexp, quote = "\""))
  )
  return(invisible(object))
}
