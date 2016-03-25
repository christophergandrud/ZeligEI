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
    ref1<-bibentry(
            bibtype="Book",
            title = "A Solution to the Ecological Inference Problem: Reconstructing Individual Behavior from Aggregate Data",
            author = person("Gary", "King"),
            year = 1997,
            publisher = "Princeton University Press",
            organization = "Princeton University Press",
            address = "Princeton"
            )
    .self$refs<-c(.self$refs, ref1)
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

zeiml$methods(
  param = function(z.out, method=NULL) {  # method arguments allows bootstrap.  However, the same object should be returned regardless.
    sim <- ei::ei.sim(z.out)
    return(sim)
  }
)

zeiml$methods(
  qi = function(simparam, mm) {

    if(.self$bootstrap){
      ev <- ei::eiread(simparam, "maggs")
    }else{
      ev <- ei::eiread(simparam, "aggs")
    }

    return(list(ev = ev))
  }
)



# Overwrite diagnostic test that are inherited from model-ei
zeiml$methods(
  geweke.diag = function() {
    stop("The eiml model is not estimated by Markov chain Monte Carlo, so this test for MCMC convergence is not needed.")
  } 
)

zeiml$methods(
  heidel.diag = function() {
    stop("The eiml model is not estimated by Markov chain Monte Carlo, so this test for MCMC convergence is not needed.")
  } 
)

zeiml$methods(
  raftery.diag = function() {
    stop("The eiml model is not estimated by Markov chain Monte Carlo, so this test for MCMC convergence is not needed.")
  } 
)

