#' Ecological Inference Model for Something Spectacular
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
    .self$description <- "Ecological Inference Model for Something Spectacular"
    .self$fn <- quote(ei::ei)
    .self$packageauthors <- "Someone"
    .self$wrapper <- "eirxc"
    .self$vignette.url <- "http://docs.zeligproject.org/en/latest/zeligei-eirxc.html"
  }
)

zeirxc$methods(
  zelig = function(formula, data, N=NULL, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, bootstrap = bootstrap)
  }
)

