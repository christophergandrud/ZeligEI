#' Ecological Inference Model by Maximum Likelihood
#'
#' Vignette: \url{http://docs.zeligproject.org/en/latest/zeligei-eiml.html}
#' @import methods
#' @export Zelig-eiml
#' @exportClass Zelig-eiml
#'
#' @include model-ei.R

zeiml <- setRefClass("Zelig-eiml",
                          contains = "Zelig-ei")

zeiml$methods(
  initialize = function() {
    callSuper()
    .self$name <- "eiml"
    .self$description <- "Ecological Inference Model by Maximum Likelihood"
    .self$fn <- quote(ei::ei)
    .self$packageauthors <- "Gary King, Molly Roberts"
    .self$wrapper <- "eiml"
    .self$vignette.url <- "http://docs.zeligproject.org/en/latest/zeligei-eiml.html"
  }
)

zeiml$methods(
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
    callSuper(formula = formula, data = data, N=N, ..., weights = weights, by = by, bootstrap = bootstrap)
  }
)

