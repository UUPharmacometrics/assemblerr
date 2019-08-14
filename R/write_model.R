#' Write model code to file
#'
#' @param model The model
#' @param filename The filename for the model file
#' @param directory The directory where to save the file
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
