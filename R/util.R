ui_warning <- function(...){
  rlang::warn(paste(..., sep = ""))
}

sym <- function(x) rlang::sym(x)


name_index_map <- function(x){
  names(x) %>%
  {purrr::set_names(seq_along(.),.)}
}

generate_permutations <- function(indicies){
  if (vec_size(indicies) == 1) return(indicies)
  permutations <- matrix(nrow = 0, ncol = length(indicies))
  for (i in seq_along(indicies)) {
    permutations <- rbind(permutations,
                        cbind(indicies[i], generate_permutations(indicies[-i])))
  }
  return(permutations)
}

permutation_matrix <- function(index){
  purrr::map(index, ~as.numeric(seq_along(index)==.x)) %>%
    do.call(cbind, args = .)
}


