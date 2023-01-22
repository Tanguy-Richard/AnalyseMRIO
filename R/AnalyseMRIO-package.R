#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib AnalyseMRIO, .registration = TRUE
## usethis namespace: end
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("AnalyseMRIO", libpath)
}



