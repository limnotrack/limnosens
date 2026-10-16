#' Generate Code to Recreate a Shelves Data Frame
#'
#' Prints R code that reconstructs a shelves data.frame in centre/width
#' format. Accepts either \code{centre}/\code{width} or legacy
#' \code{shelf_temp}/\code{band_lo}/\code{band_hi} format.
#'
#' @param shelves Data.frame of shelves.
#' @param name Variable name to use in the generated code. Default
#'   \code{"shelves"}.
#'
#' @return The code string (invisibly). Also printed to the console via
#'   \code{cat()}.
#'
#' @export
shelves_to_code <- function(shelves, name = "shelves") {

  if (nrow(shelves) == 0L) {
    code <- sprintf("%s <- data.frame(centre = numeric(), width = numeric())", name)
    cat(code, "\n")
    return(invisible(code))
  }

  # Convert legacy format if needed
  if ("shelf_temp" %in% names(shelves) && !"centre" %in% names(shelves)) {
    shelves$centre <- shelves$shelf_temp
    shelves$width <- shelves$band_hi - shelves$band_lo
  }

  code <- paste0(
    name, " <- data.frame(\n",
    "  centre = c(", paste(sprintf("%.4f", shelves$centre), collapse = ", "), "),\n",
    "  width  = c(", paste(sprintf("%.4f", shelves$width), collapse = ", "), ")\n",
    ")"
  )

  cat(code, "\n")
  invisible(code)
}
