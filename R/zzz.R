.db <- new.env()
.db$fonts <- NULL

.allWeights <- NULL

.onLoad <- function(libname, pkgname){
  if (is.null(.db$fonts)) {
    .db$fonts <<- as.environment(.fontList)
  }
}
