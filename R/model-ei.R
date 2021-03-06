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

   # Check for zeros


   # Check for NA's

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

#' Checking function for argument to na.action
#' @keywords internal

checkZeligEIna.action = function(na.action){
  # Allow the user to accidentally pass the na.omit or na.fail functions, instead of a text string
  if(identical(na.action,stats::na.omit)){
    na.action<-"na.omit"
  }
  if(identical(na.action,na.fail)){
    na.action<-"na.fail"
  }

  if(!(na.action %in% c("na.omit","na.fail"))){
    stop("Error: Zelig's na.action argument should be a text string of 'na.omit' or 'na.fail' ")
  }
  return(na.action)
}

#' Conversion utility to allow different possible formula notations, and deal with zeroes and missing values, for EI models in eiml, eirxc
#' @keywords internal

convertEIformula2 = function(formula, data, N, na.action){

  if(!is.null(N)){
    if(is.character(N)){
      Nvalid <- N %in% names(data)
      if(Nvalid){
        Nvalues <- data[[ as.character(N) ]]
        .self$model.call$total <- N
      }
    }else if(is.numeric(N)){
      Nvalid <- length(N) == nrow(data)
      if(Nvalid){
        data$ZeligN <- N
        .self$model.call$total <- "ZeligN"
      } else {
        stop("The argument N needs to match in length the number of observations in the dataset.")
      }
    }
  }else{
    rterms <- length(formula[[2]])
    cterms <- length(formula[[3]])
    if((rterms==1) | (cterms==1)){
      stop("The argument 'N' has not been specified, however the formula does not define all terms.  Either set the 'N' argument, or redefine formula using 'cbind' notation, or both.")
    }

    rdata <- data[ as.character(formula[[2]][2:rterms] ) ]
    rtotal <- apply(rdata,1,sum)
    cdata <- data[ as.character(formula[[3]][2:cterms] ) ]
    ctotal <- apply(cdata,1,sum)

    Nvalid <- all(rtotal == ctotal)
    if(Nvalid){
      data$ZeligN <- rtotal
      .self$model.call$total <- "ZeligN"
    } else {
      stop("Some of the row observations do not sum to the same total as the column observations.  Please correct the data or the formula, or set the N argument.")
    }
  }

  flag.zero <- Nvalues<1

  if(any(flag.zero)){
    warnings("There are observations in the EI model with zero as the total count for the observation.  Check data.  These observations have been removed.")
    data <- data[!flag.zero]
  }



  check <- formula[[1]]=="~"

  return(list(formula=newformula, data=newdata, N=newN))
}

#' Conversion utility to allow different possible formula notations, and deal with zeroes and missing values, for EI models in eiheir, eidynamic
#' @keywords internal

convertEIformula = function(formula, N, data, na.action){
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
      } else {
        stop("The argument 'N' needs to match in length the number of observations in the dataset.")
      }
    }
  }else{
    rterms <- length(formula[[2]])
    cterms <- length(formula[[3]])
    if((rterms==1) | (cterms==1)){
      stop("The argument 'N' has not been specified, however the formula does not define all terms.  Either set the 'N' argument, or redefine formula using 'cbind' notation, or both.")
    }

    rdata <- data[ as.character(formula[[2]][2:rterms] ) ]
    rtotal <- apply(rdata,1,sum)
    cdata <- data[ as.character(formula[[3]][2:cterms] ) ]
    ctotal <- apply(cdata,1,sum)

    Nvalid <- all(rtotal == ctotal)
    if(Nvalid){
      data$ZeligN <- rtotal
      .self$model.call$total <- "ZeligN"
    } else {
      stop("Some of the row observations do not sum to the same total as the column observations.  Please correct the data or the formula, or set the 'N' argument.")
    }
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
    if( !identical(floor(r0),r0) ){   # Make a better check here.  Deal with case of fraction and 0-100.
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

  flag.zero <- Nvalues<1
  if(any(flag.zero)){
    warnings("There are observations in the EI model with zero as the total count for the observation.  Check data.  These observations have been removed.")
    r0<-r0[!flag.zero]
    r1<-r1[!flag.zero]
    c0<-c0[!flag.zero]
    c1<-c1[!flag.zero]
    Nvalues<-Nvalues[!flag.zero]
  }

  flag.missing <- is.na(r0) | is.na(r1) | is.na(c0) | is.na(c1) | is.na(Nvalues)
  if(any(flag.missing)){
    if (na.action=="na.omit"){ 
      warnings("There are observations in the EI model with missing values.  These observations have been removed.")
      r0<-r0[!flag.missing]
      r1<-r1[!flag.missing]
      c0<-c0[!flag.missing]
      c1<-c1[!flag.missing]
      Nvalues<-Nvalues[!flag.missing]
    } else {
      stop("Error: There are observations in the EI model with zero as the total count for the observation. \nRemove these observations from data, or change Zelig's 'na.action' argument.")   
    }
  }

  return(list(r0=r0,r1=r1,c0=c0,c1=c1,N=Nvalues))
}

# This works for eiheir, eidynamic, eiml and needs overwriting for eirxc
zei$methods(
  getcoef = function() {
    "Get estimated model coefficients"
    return(.self$zelig.out$z.out)
  } 
)

# This works for eihier, eidynamic and eirxc (with overwritten $getcoef method).  Model eiml is not MCMC.
zei$methods(
  geweke.diag = function() {
    mycoef<-.self$getcoef()
    if(is.list(mycoef[[1]])){  # eirxc is list of lists
      diag <- lapply(mycoef, function(x)lapply(x, coda::geweke.diag) )
    }else{                     # eidynamic and eihier pack all parameters into one list
      diag <- lapply(mycoef, coda::geweke.diag)
    }
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }

    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="InCollection",
            title = "Evaluating the accuracy of sampling-based approaches to calculating posterior moments.",
            booktitle = "Bayesian Statistics 4",
            author = person("John", "Geweke"),
            year = 1992,
            publisher = "Clarendon Press",
            address = "Oxford, UK",
            editor = c(person("JM", "Bernado"), person("JO", "Berger"), person("AP", "Dawid"), person("AFM", "Smith")) 
            )
    .self$refs<-c(.self$refs,ref1)
    return(diag)
  } 
)

zei$methods(
  heidel.diag = function() {
    mycoef<-.self$getcoef()
    if(is.list(mycoef[[1]])){  # eirxc is list of lists
      diag <- lapply(mycoef, function(x)lapply(x, coda::heidel.diag) )
    }else{                     # eidynamic and eihier pack all parameters into one list
      diag <- lapply(mycoef, coda::heidel.diag)
    }
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }

    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="Article",
            title = "Simulation run length control in the presence of an initial transient.",
            author = c(person("P", "Heidelberger"), person("PD", "Welch")),
            journal = "Operations Research",
            volume = 31,
            year = 1983,
            pages = "1109--44")
    .self$refs<-c(.self$refs,ref1)
    return(diag)
  } 
)

zei$methods(
  raftery.diag = function() {
    mycoef<-.self$getcoef()
    if(is.list(mycoef[[1]])){  # eirxc is list of lists
      diag <- lapply(mycoef, function(x)lapply(x, coda::raftery.diag) )
    }else{                     # eidynamic and eihier pack all parameters into one list
      diag <- lapply(mycoef, coda::raftery.diag)
    }
    # Collapse if only one list element for prettier printing
    if(length(diag)==1){
        diag<-diag[[1]]
    }



    if(!citation("coda") %in% .self$refs){
      .self$refs<-c(.self$refs,citation("coda"))
    }
    ref1<-bibentry(
            bibtype="Article",
            title = "One long run with diagnostics: Implementation strategies for Markov chain Monte Carlo.",
            author = c(person("Adrian E", "Raftery"), person("Steven M", "Lewis")),
            journal = "Statistical Science",
            volume = 31,
            year = 1992,
            pages = "1109--44")
    ref2<-bibentry(
            bibtype="InCollection",
            title = "The number of iterations, convergence diagnostics and generic Metropolis algorithms.",
            booktitle = "Practical Markov Chain Monte Carlo",
            author = c(person("Adrian E", "Raftery"), person("Steven M", "Lewis")),
            year = 1995,
            publisher = "Chapman and Hall",
            address = "London, UK",
            editor = c(person("WR", "Gilks"), person("DJ", "Spiegelhalter"), person("S", "Richardson")) 
            )
    .self$refs<-c(.self$refs,ref1,ref2)
    return(diag)
  } 
)

