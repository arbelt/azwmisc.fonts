#' @include font.R
NULL

#' S4 Class for font family
#'
setClass("FontFamily",
         slots = c(UprightFont = "Font",
                   ItalicFont = "Font",
                   BoldFont = "Font",
                   BoldItalicFont = "Font"))


#' Create font family
#'
#' Create font family.
#'
#' @importFrom purrr cross_n invoke set_names invoke_map reduce map discard
#' @importFrom dplyr if_else
#' @param fontname Name of font
#' @param reg_wt Weight to use for regular
#' @param bold_wt Weight to use for bold
#' @aliases createFontFamily
#' @export
font_family <- function(fontname, reg_wt = "Regular", bold_wt = "Bold"){
  args <- list(fontname = fontname, weight = c(reg_wt, bold_wt), shape = c("Upright", "Italic"))
  fonts <- args %>% cross_n %>% invoke_map(get_font, .) %>%
    set_names(c("UprightFont", "BoldFont", "ItalicFont", "BoldItalicFont")) %>%
    discard(is.null)
  fontdirs <- fonts %>% map(fontdir) %>%
    reduce(~ if_else(.x == .y, .x, NA_character_))
  # Require all fonts to share a directory
  stopifnot(!is.na(fontdirs))
  invoke(new, c(list("FontFamily"), fonts))
}

#' @export
createFontFamily <- font_family


setMethod(
  "fontdir",
  c("FontFamily"),
  function(x){
    fontdir(x@UprightFont)
  }
)


#' Add font family to showtext
#'
#' Adds font family to showtext database.
#'
#' @importFrom rlang quo eval_tidy
#' @importFrom purrr discard map
#'
#' @export
setGeneric(
    "showtext_add",
    function(x, ...) standardGeneric("showtext_add")
)

setMethod(
    "showtext_add",
    c("FontFamily"),
    function(x, ..., name = NULL) {
        if (!.pkg_loaded("showtext")) {
            stop("Showtext must be available")
        }
        if (is.null(name)) {
            name <- x@UprightFont@fontname
        }
        call_args_ <- list(regular = x@UprightFont,
                           bold = x@BoldFont,
                           italic = x@ItalicFont,
                           bolditalic = x@BoldItalicFont) %>%
            map(fontpath) %>%
            discard(~ nchar(.x) == 0)
        call_ <- quo(font.add(family = !!name, !!!call_args_))
        call_
    }
)

.pkg_loaded <- function(x) {
    x %in% .packages()
}
