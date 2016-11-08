#' @importFrom methods as getSlots new setClass setGeneric setMethod setAs
#' @import methods
#' @importFrom utils packageName
NULL

#' S4 class for font objects
#'
#' @slot fontname length-one character vector
#' @rdname Font
Font <- setClass(
  "Font",
  slots = list(fontname = "character",
               fontdir = "character",
               fontfile = "character",
               fontext = "character",
               weight = "character",
               shape = "character"))

#' @importFrom purrr discard
#' @importFrom dplyr %>%
setAs(from = "list", to = "Font",
      def = function(from){
        args <- from[names(getSlots("Font"))] %>%
          discard(is.null)
        if (is.null(args$fontext))
          args$fontext <- ".otf"
        do.call("Font", args)
      })

setAs(from = "NULL", to = "Font",
      def = function(from){
        return(NULL)
      })

#' Get font directory
#'
#' Returns the full (normalized) directory containing font file.
#'
#' @export
#' @rdname fontdir
setGeneric(
  "fontdir",
  function(x, ...) standardGeneric("fontdir"))

setMethod(
  "fontdir",
  c("Font"),
  function(x, ..., ext = TRUE){
    path <- system.file("fonts", x@fontdir, package = packageName())
    path
  })

#' Get full path for font file
#'
#' @export
setGeneric(
  "fontpath",
  function(x, ...) standardGeneric("fontpath")
)

setMethod(
  "fontpath",
  c("Font"),
  function(x, ...){
    system.file("fonts", x@fontdir, paste0(x@fontfile, x@fontext),
                package = packageName())
  })

#' Get font filename
#'
#' @export
setGeneric("fontfile",
           function(x) standardGeneric("fontfile"))

setMethod(
  "fontfile",
  c("Font"),
  function(x){
    x@fontfile
  }
)
