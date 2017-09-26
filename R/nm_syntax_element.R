
NMSyntaxElement <- R6Class("NMSyntaxElement",
                 public = list(
                   get_code = function(){
                     return("")
                   }
                 ),
                 private = list(
                   render_template = function(template, envir = self){
                     return(stringr::str_interp(template, envir))
                   }
                 )
)

is_nm_syntax_element <- function(x) return(inherits(x, "NMSyntaxElement"))
