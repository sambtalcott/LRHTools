#' LRH ggplot2 theme
#'
#' Cobbled together with inspiration from [tntpr::tntp_style()] and the default
#' `ggplot2` themes.
#'
#' @param grid Which grid lines should be shown? Use `TRUE` or `FALSE` to toggle
#' all grid lines, or a string combination of `X`, `x`, `Y`, `y` for major and minor x and y grid lines.
#' @param legend_style Either "top" or "right"
#' @param border Include a panel border?
#' @param md Should text elements use [ggtext::element_markdown()]?
#' @param ... Additional arguments passed on to [ggplot2::theme()]
#' @param base_font Basic font
#' @param title_font Title font
#' @param subtitle_font Subtitle font
#'
#' @returns a ggplot2 theme statement
#' @export
#' @md
theme_lrh <- function(grid = FALSE, legend_style = "top", border = FALSE, md = FALSE,
                      base_font = "Open Sans",
                      title_font = "Open Sans SemiCondensed ExtraBold",
                      subtitle_font = "Open Sans SemiCondensed SemiBold", ...) {

  # Load fonts if not already loaded.
  # See tntpr:::get_usable_familiy for a more resilient version of this
  if (!title_font %in% names(grDevices::windowsFonts())) {
    extrafont::loadfonts(quiet = TRUE)
  }

  # Validate inputs
  if (!is.logical(grid) && !grepl("^[xXyY]+$", grid)) {
    cli::cli_abort(c(
      "Invalid value {.val {grid}} for {.var grid}",
      i = "To show all grid-lines, set to {.val TRUE}",
      i = "To show some grid-lines set to a string combination of {.val {c('X', 'x', 'Y', 'y')}}, where capital letters represent major grid-lines and lowercase letters represent minor grid-lines."
    ))
  }
  if (!isTRUE(legend_style %in% c("top", "right"))) {
    cli::cli_abort(c(
      "Invalid value {.val {legend_style}} for {.var legend_style}",
      i = "Accepted values are {.val top} or {.val right}"
    ))
  }

  # Should text elements use markdown formatting?
  txt <- if (md && rlang::is_installed("ggtext")) ggtext::element_markdown else ggplot2::element_text

  # Setup base theme
  out <- ggplot2::theme_minimal(
    base_family = base_font,
    base_size = 14
  ) +
    ggplot2::theme(
      # text = txt(size = 14),
      # Title/subtitle
      title = txt(family = subtitle_font),
      plot.title = txt(size = 25, family = title_font),
      plot.subtitle = txt(family = subtitle_font, size = 18, face = "bold"),
      plot.title.position = "plot",
      # Axis Title
      axis.title = txt(size = 16, face = "bold"),
      # Caption
      plot.caption = txt(face = "italic", color = "gray", size = 10),
      # Faceting
      strip.text = txt(family = subtitle_font, size = 16, face = "bold"),
      # Grid
      panel.grid.minor = ggplot2::element_line(color = "#CBCBCB", linewidth = 0.5),
      panel.grid.major = ggplot2::element_line(color = "#CBCBCB", linewidth = 1)
    )

  # Adjust grid
  if (grid == FALSE) {
    out <- out + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                panel.grid.minor = ggplot2::element_blank())
  } else if (is.character(grid)) {
    if (!grepl("X", grid))
      out <- out + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    if (!grepl("x", grid))
      out <- out + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if (!grepl("Y", grid))
      out <- out + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    if (!grepl("y", grid))
      out <- out + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  # Adjust legend
  if (legend_style == "top") {
    out <- out + ggplot2::theme(
      legend.position = "top",
      legend.justification = "left",
      legend.location = "plot",
    )
  }

  # Adjust border
  if (border) {
    out <- out + ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = "black")
    )
  }

  # Add additional elements
  out <- out + ggplot2::theme(...)

  # Return theme
  out
}
