

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
#' @param notes optional speaker notes for the slide
#'
#' @returns rpptx object
#' @md
#' @export
ppt_d1 <- function(p, content, notes = NULL) {
  p <- p |>
    officer::add_slide("Data 1 Image") |>
    officer::ph_with(content, officer::ph_location_id(3))
  if (!is.null(notes)) {
    p <- officer::set_notes(p, notes, officer::notes_location_type("body"))
  }
  p
}

#' Add a double data slide
#'
#' @param p rpptx object (from [ppt_lrh()])
#' @param left content to add to left pane
#' @param right content to add to right pane
#' @param notes optional speaker notes for the slide
#'
#' @returns rpptx object
#' @md
#' @export
ppt_d2 <- function(p, left, right, notes = NULL) {
  p <- p |>
    officer::add_slide("Data 2 Images") |>
    officer::ph_with(left, officer::ph_location_id(3)) |>
    officer::ph_with(right, officer::ph_location_id(4))
  if (!is.null(notes)) {
    p <- officer::set_notes(p, notes, officer::notes_location_type("body"))
  }
  p
}


#' For adding gt tables to slides
#'
#' Uses chrome to print the table to an image, then adds it using
#' [officer::ph_with.external_img()]. By default, makes the table as large as
#' possible for the location without changing its scale and centers it.
#'
#' @param x an rpptx object
#' @param value gt() object
#' @param location location. See [officer::ph_with()]
#' @param ... Additional arguments passed on to external image ph_with()
#'
#' @method ph_with gt_tbl
#' @importFrom officer ph_with
#'
#' @returns x
#' @export
ph_with.gt_tbl <- function(x, value, location, ...) {

  loc_dim <- officer::fortify_location(location, x)
  loc_hw <- loc_dim$height/loc_dim$width

  tf <- tempfile(fileext = ".png")
  gt::gtsave(value, tf) |> suppressMessages()

  gt_dim <- magick::image_read(tf) |>
    magick::image_data() |>
    attr("dim") |>
    _[-1] |>
    as.list() |>
    rlang::set_names(c("width", "height"))
  gt_hw <- gt_dim$height/gt_dim$width

  # Determine final dimensions
  if (gt_hw > loc_hw) {
    final_h <- loc_dim$height
    final_w <- final_h / gt_hw
    final_t <- loc_dim$top
    final_l <- loc_dim$left + (loc_dim$width - final_w)/2
  } else {
    final_w <- loc_dim$width
    final_h <- final_w * gt_hw
    final_l <- loc_dim$left
    final_t <- loc_dim$top + (loc_dim$height - final_h)/2
  }
  new_loc <- officer::ph_location(
    left = final_l, top = final_t,
    height = final_h, width = final_w
  )

  officer::ph_with(x, officer::external_img(tf), new_loc)
}
