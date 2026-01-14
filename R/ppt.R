

#' Create new Powerpoint
#'
#' Extends [officer] to create a new LRH Data PPT Slide with a title and subtitle
#'
#' @param title Title Text (optional)
#' @param subtitle Subtitle Text (optional)
#' @param template_loc Location of PPT Template
#'
#' @returns a rpptx object from [officer]
#' @md
#' @export
ppt_lrh <- function(title = NULL, subtitle = NULL,
                    template_loc = "~/Custom Office Templates/LRH Data PPT Template.potx") {

  p <- officer::read_pptx(template_loc)
  if (!is.null(title)) {
    p <- p |>
      officer::ph_remove(type = "ctrTitle") |>
      officer::ph_with(title, officer::ph_location_type("ctrTitle"))
  }
  if (!is.null(subtitle)) {
    p <- p |>
      officer::ph_remove(type = "subTitle") |>
      officer::ph_with(subtitle, officer::ph_location_type("subTitle"))
  }
  p
}

#' Add a single data slide
#'
#' @param p rpptx object (from [ppt_lrh()])
#' @param content content to add to center
#'
#' @returns rpptx object
#' @md
#' @export
ppt_d1 <- function(p, content) {
  p |>
    officer::add_slide("Data 1 Image") |>
    officer::ph_with(content, officer::ph_location_id(3))
}

#' Add a double data slide
#'
#' @param p rpptx object (from [ppt_lrh()])
#' @param left content to add to left pane
#' @param right content to add to right pane
#'
#' @returns rpptx object
#' @md
#' @export
ppt_d2 <- function(p, left, right) {
  p |>
    officer::add_slide("Data 2 Images") |>
    officer::ph_with(left, officer::ph_location_id(3)) |>
    officer::ph_with(right, officer::ph_location_id(4))
}
