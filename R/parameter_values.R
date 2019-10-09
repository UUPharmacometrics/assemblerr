
check_pvs <- function(pvs, pv_names, lower = NULL, upper = NULL){
  if(rlang::is_named(pvs)) {
    if(!setequal(names(pvs), pv_names)) stop("Not all required parameters were provided.", call. = FALSE)
  }else{
    if(length(pvs)!=length(pv_names)) stop("Not enough parameters provided.", call. = FALSE)
    names(pvs) <- pv_names
  }
  below_lower <- purrr::imap_lgl(lower, ~pvs[.y]<.x)
  above_upper <- purrr::imap_lgl(upper, ~pvs[.y]>.x)
  if(any(below_lower)) stop("Parameter values ", names(which(below_lower))," below lower bound.", call. = FALSE)
  if(any(above_upper)) stop("Parameter values ", names(which(above_upper))," above upper bound.", call. = FALSE)
  return(pvs)
}

to_prmz <- function(prmz, prm){
  rlang::exec(prmz, !!!unlist(prm$values))
}
