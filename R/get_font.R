
#' Get font object
#'
#' @param fontname Name of font as string.
#' @param weight Weight of font to retrieve.
#' @param shape Either \code{\"Upright\"} or \code{\"Italic\"}
#' @export
get_font <- function(fontname = c("FiraSans", "FiraSansCondensed", "FiraMono", "CooperHewitt"),
                     weight = "Regular",
                     shape = "Upright"){
  fontname <- match.arg(fontname)
  fontDesc <- .db$fonts[[fontname]][[weight]][[shape]]
  if (is.null(fontDesc) && weight == "Regular")
    fontDesc <- .db$fonts[[fontname]][["Book"]][[shape]]
  as(fontDesc, "Font")
}
