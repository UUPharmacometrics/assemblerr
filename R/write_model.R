#' @export
write_model <- function(model, filename, directory = getwd()) UseMethod("write_model")

#' @export
write_model.nm_model <- function(model, filename, directory = getwd()){
  if(!dir.exists(directory)) stop("Directory ", directory, " does not exist.")
  path <- file.path(directory, filename)
  model %>%
    render() %>%
    writeLines(con = path)
}
