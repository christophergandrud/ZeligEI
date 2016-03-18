#' Multinomial Dirichlet model for Ecological Inference in RxC tables
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zeligei-eirxc.html}
#' @import methods
#' @export Zelig-eirxc
#' @exportClass Zelig-eirxc
#'
#' @include model-ei.R

zeirxc <- setRefClass("Zelig-eirxc",
                          contains = "Zelig-ei")

zeirxc$methods(
  initialize = function() {
    callSuper()
    .self$name <- "eirxc"
    .self$description <- "Multinomial Dirichlet model for Ecological Inference in RxC tables"
    .self$fn <- quote(eiPack::ei.MD.bayes)
    .self$packageauthors <- "Michael Kellerman, Olivia Lau"
    .self$wrapper <- "eirxc"
    .self$vignette.url <- "http://docs.zeligproject.org/en/latest/zeligei-eirxc.html"
  }
)

zeirxc$methods(
  zelig = function(formula, data, N = NULL, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    if(is.null(N)){
        stop("The argument N needs to be set to the name of the variable giving the total for each unit, or a vector of counts.")
        
        # Put in automated fix if data is integer.
    }
    
    .self$zelig.call <- match.call(expand.dots = TRUE)
    
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$N <- NULL
    if(is.numeric(N)){
        .self$model.call$total <- "ZeligN"
    }else{
        .self$model.call$total <- N
    }
    callSuper(formula = formula, data = data, N=NULL, ..., weights = weights, by = by, bootstrap = bootstrap)
  }
)
