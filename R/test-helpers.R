#' @export
#' @keywords internal
local_create_nonmem_test_directory <- function(path = tempdir(), env = parent.frame()) {
  dirname <- tempfile("dir", tmpdir = path)
  dir.create(dirname, showWarnings = TRUE, recursive = FALSE)
  withr::defer(unlink(dirname, recursive = TRUE), envir = env)

  wd <- getwd()
  setwd(dirname)
  withr::defer(setwd(wd), envir = env)
  return(dirname)
}

#' @export
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

expect_does_not_contain <- function(object, regexp) {
  act <- testthat::quasi_label(rlang::enquo(object), label = NULL, arg = "object")
  stopifnot(is.character(regexp), length(regexp) == 1)
  stopifnot(is.character(act$val))
  if (length(object) == 0) {
    fail(sprintf("%s is empty.", act$lab))
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
  expect(all(matches), sprintf("%s does contain %s.\n%s", act$lab,
                               encodeString(regexp, quote = "\""), values))
  invisible(act$val)
}
