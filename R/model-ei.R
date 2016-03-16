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

#' Conversion utility to allow different possible formula notations for EI models
#' @keywords internal

convertEIformula = function(formula, N, data){
  formula <- as.formula(formula)
  
  if(!is.null(N)){
    if(is.character(N)){
      Nvalid <- N %in% names(data)
      if(Nvalid){
        Nvalues <- data[[ as.character(N) ]]
      }
    }else if(is.numeric(N)){
      Nvalid <- length(N) == nrow(data)
      if(Nvalid){
        Nvalues <- N
      }
    }
  }else{
    Nvalid <- FALSE
  }


  check <- formula[[1]]=="~"
  if(length(formula[[2]]) == 1){
    check <- check & (length(formula[[3]]) == 1) & Nvalid   # Need same length covariate list, and must have useable N argument
    if(check){
      r0 <- data[[ as.character(formula[[2]]) ]]
      r1 <- Nvalues - r0
      c0 <- data[[ as.character(formula[[3]]) ]]
      c1 <- Nvalues - c0
    }

  } else if (length(formula[[2]]) == 3){
    check <- check & (length(formula[[3]]) == 3)            # Need same length covariate list
    check <- check & formula[[2]][1] == "cbind()"
    if(check){
      r0 <- data[[ as.character(formula[[2]][2]) ]]
      r1 <- data[[ as.character(formula[[2]][3]) ]]
      c0 <- data[[ as.character(formula[[3]][2]) ]]
      c1 <- data[[ as.character(formula[[3]][3]) ]]
    }
    if( !identical(floor(r0),r0) ){
      check <- check & Nvalid                               # If variables expressed as proportions, must have useable N argument

      if(check){
        r0 <- round(r0 * Nvalues)
        r1 <- round(r1 * Nvalues)
        c0 <- round(c0 * Nvalues)
        c1 <- round(c1 * Nvalues)
      }
    }
  } else {
    check <- FALSE
  }

  if(!check){
    stop("Formula and/or N argument provided for EI model does not appear to match any of the accepted templates.")
  }

  return(list(r0=r0,r1=r1,c0=c0,c1=c1,N=Nvalues))
}
