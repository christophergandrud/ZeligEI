#' @include model-eirxc.R

#library(jsonlite)


createJSONzeligei <- function(){

  z5eirxc <- zeirxc$new()
  z5eirxc$toJSON()

  zeligchoicemodels <- list(zelig5choicemodels = list("eirxc" = z5eirxc$ljson))

  # cat(jsonlite::toJSON(zeligchoicemodels, pretty = TRUE),
  #     file = file.path("inst/JSON", "zelig5choicemodels.json"))

  cat(toJSON(zeligeimodels, pretty = TRUE), file = file.path("zelig5eimodels.json"))
  file.rename(from = file.path("zelig5eimodels.json"),
            to = file.path("inst", "JSON", "zelig5eimodels.json"))
  file.remove(file.path("zelig5eimodels.json"))

  return(TRUE)
}