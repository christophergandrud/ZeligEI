#' @include model-ei.R
#' @include model-hier-ei.R
#' @include model-ml-ei.R
#' @include model-dynamic-ei.R
#' @include model-rxc-ei.R

#library(jsonlite)


createJSONzeligei <- function(){

  z5eirxc <- zeirxc$new()
  z5eirxc$toJSON()

  z5eidynamic <- zeidynamic$new()
  z5eidynamic$toJSON()

  z5eihier <- zeihier$new()
  z5eihier$toJSON()

  z5eiml <- zeiml$new()
  z5eiml$toJSON()

  zeligeimodels <- list(zelig5eimodels = list("eirxc" = z5eirxc$ljson, 
                                                      "eidynamic" = z5eidynamic$ljson, 
                                                      "eihier" = z5eihier$ljson, 
                                                      "eiml" = z5eiml$ljson))

  # cat(jsonlite::toJSON(zeligchoicemodels, pretty = TRUE),
  #     file = file.path("inst/JSON", "zelig5choicemodels.json"))

  cat(toJSON(zeligeimodels, pretty = TRUE), file = file.path("zelig5eimodels.json"))
  file.rename(from = file.path("zelig5eimodels.json"),
            to = file.path("inst", "JSON", "zelig5eimodels.json"))
  file.remove(file.path("zelig5eimodels.json"))

  return(TRUE)
}