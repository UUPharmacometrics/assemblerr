ui_warning <- function(...){
  rlang::warn(paste(..., sep = ""))
}

sym <- function(x) rlang::sym(x)
