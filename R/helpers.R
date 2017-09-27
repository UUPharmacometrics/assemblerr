as_equation <- function(x){
    if(is(x, "formula")){
      return(x[[2]])
    }else if(is.numeric(x)){
      return(x)
    }else if(is.expression(x)){
      return(x)
    }
}
