
#' LRH Brand Colors
#'
#' Translate human friendly LRH brand color names like "lt_blue" into
#' accurate hex values for use in plotting. This function can also be used to
#' show a named vector of all available LRH brand colors and values. Use
#' `show_lrh_colors()` to quickly visualize selected colors in the plot window.
#'
#' @param ... Supply quoted LRH color names to return. If no colors are specified, returns all available colors.
#' @param pattern Optional regular expression. If provided, will return only brand colors that match the regular expression
#' @param labels Logical. Label colors with names and hex values?
#' @param borders Border color for each tile. Default uses `par("fg")`. Use `border = NA` to omit borders.
#' @param cex_label Size of printed labels, as multiplier of default size.
#' @param ncol Number of columns. If not supplied, tries to be as square as possible.
#'
#' @export
#' @returns
#' * `lrh_colors()` returns a character vector of color codes
#' * `show_lrh_colors()` returns nothing
#'
#' @md
#' @examples
#'
#' library(ggplot2)
#'
#' # Use lrh_colors() to retrieve a single color...
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(color = lrh_colors('green'))
#'
#' #... multiple colors ...
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_manual(values = lrh_colors('green', 'blue', 'red'))
#'
#' #... or a list of all possible TNTP brand colors
#' lrh_colors()
#'
#' # Use show_lrh_colors() to quickly see brand colors in the plotting window
#' show_lrh_colors('dk_red', 'red', 'lt_red')
#'
#' # You can also use a pattern to return similar colors
#' show_lrh_colors(pattern = 'green')
#'
#' # You can see all colors (and names) by running it with no arguments
#' show_lrh_colors()

lrh_colors <- function(...) {

  lrh_color_list <- c(

    dk_green  = "#1F5B42",
    green     = "#4c8c72",
    lt_green  = "#DFEFE8",

    dk_blue   = "#125687",
    blue      = "#4380ab",
    lt_blue   = "#98BDD7", # NOT OFFICIAL

    dk_yellow = "#DDB122", # NOT OFFICIAL
    yellow    = "#f9e497",
    lt_yellow = "#FDFCE6",

    dk_gray   = "#57585A",
    gray      = "#8A8B8F", # NOT OFFICIAL
    lt_gray   = "#BEBEC1", # NOT OFFICIAL

    dk_red    = "#7C1F1D", # NOT OFFICIAL
    red       = "#964340", # NOT OFFICIAL
    lt_red    = "#D49D9B",

    black     = "black",
    white     = "white"

  )

  supplied_colors <- c(...)
  supplied_names <- names(supplied_colors)

  # Return full color list with names if run with no arguments
  if(is.null(supplied_colors)) {

    # Consolidate names for colors that appear in the list multiple times
    consolidated_colors <- unique(lrh_color_list)
    consolidated_names <- character(length(consolidated_colors))

    for(i in seq_along(consolidated_colors)) {
      consolidated_names[i] <- paste0(names(lrh_color_list)[lrh_color_list == consolidated_colors[i]], collapse = "|")
    }

    names(consolidated_colors) <- consolidated_names

    return(consolidated_colors)
  }

  # Return an error if arguments include unmatched colors
  unmatched_colors <- supplied_colors[!supplied_colors %in% names(lrh_color_list)]
  if(length(unmatched_colors) > 0) {
    cli::cli_abort(c("!" = "No match for the following color name(s)",
                     "x" = paste0("{.val ", unmatched_colors, "}", collapse = ", "),
                     "i" = "Run {.run LRHTools::show_lrh_colors()} to see available colors"))
  }

  # Return supplied colors, with names if provided (for use in scale_*_manual)
  colors <- lrh_color_list[supplied_colors]
  names(colors) <- supplied_names
  colors
}

#' Validate color inputs
#'
#' @param x a color
#'
#' @returns TRUE if x can be interpreted as a color
is_color <- function(x) {
  res <- try(grDevices::col2rgb(x), silent = TRUE)
  return(!"try-error" %in% class(res))
}

#' @export
#' @rdname lrh_colors
show_lrh_colors <- function(..., pattern = NULL, labels = TRUE, borders = NULL,
                            cex_label = 1, ncol = NULL) {


  # Validate parameters
  if(!is.logical(labels)) {
    cli::cli_warn(c("!" = "Invalid {.var labels} value of {.val {labels}}",
                    "i" = "{.var labels} accepts values of {.val TRUE} or {.val FALSE}",
                    "i" = "Defaulting to {.val TRUE}"))
    labels <- TRUE
  }


  if(!is_color(borders)) {
    cli::cli_warn(c("!" = "Invalid {.var borders} value of {.val {borders}}",
                    "i" = "{.var borders} accepts any color value or {.val NA}",
                    "i" = "Defaulting to {.val {par(\"fg\")}}"))
    borders <- graphics::par("fg")
  }

  # Code adapted from scales::show_col but with text labels in addition to hex codes
  colours <- lrh_colors(...)

  # If the full list was pulled
  if(is.null(c(...))) {
    # Format consolidated names for printing
    names(colours) <- gsub("|", "\n", names(colours), fixed = TRUE)
  } else {
    # Otherwise add back in selected names
    names(colours) <- c(...)
  }

  # Filter using the pattern, if provided
  if(!is.null(pattern)) {
    colours <- colours[grepl(pattern, names(colours))]
    if(length(colours) == 0) {
      cli::cli_abort(c("!" = "No colors match the pattern {.val {pattern}}",
                       "i" = "Run {.run LRHTools::show_lrh_colors()} to see available colors"))
    }
  }

  n <- length(colours)
  ncol <- ifelse(is.null(ncol), ceiling(sqrt(length(colours))), ncol)
  nrow <- ceiling(n/ncol)
  colours <- c(colours, rep(NA, nrow * ncol - length(colours)))

  # Create label: name + hex value
  color_labels <- paste0(names(colours), "\n", colours)
  color_labels[color_labels == "\nNA"] <- ""

  colours <- matrix(colours, ncol = ncol, byrow = TRUE)
  color_labels <- matrix(color_labels, ncol = ncol, byrow = TRUE)
  old <- graphics::par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(graphics::par(old))
  size <- max(dim(colours))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  graphics::rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
                 col = colours, border = borders)
  if (labels) {
    label_col <- tntpr::choose_text_color(colours)
    graphics::text(x = col(colours) - 0.5,
                   y = -row(colours) + 0.5,
                   labels = color_labels,
                   cex = cex_label,
                   col = label_col)
  }
}
