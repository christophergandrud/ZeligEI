#' Ecological Inference object for inheritance across models in ZeligEI
#'
#' @import methods
#' @export Zelig-ei
#' @exportClass Zelig-ei

zei <- setRefClass("Zelig-ei",
                          contains = "Zelig")

zei$methods(
  initialize = function() {
    callSuper()
    .self$authors <- "James Honaker"
    .self$year <- 2016
    .self$category <- "ei"
    .self$acceptweights <- FALSE
  }
)

zei$methods(
  zelig = function(formula, data, N, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
   # .self$zelig.call <- match.call(expand.dots = TRUE)
   # .self$model.call <- match.call(expand.dots = TRUE)
    callSuper(formula = formula, data = data, N=N, ..., weights = weights, by = by, bootstrap = bootstrap)
  }
)

zei$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      return(mvrnorm(.self$num, coef(z.out), vcov(z.out))) 
    } else if(identical(method,"point")){
      return(t(as.matrix(coef(z.out))))
    } else {
      stop("param called with method argument of undefined type.")
    }
  }
)
