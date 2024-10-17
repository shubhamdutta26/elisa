#' Plots microplate ELISA data. Use the create_plate() function to create an
#' empty file and then populate the fields.
#'
#' @param file A character string containing the path to a csv or excel
#' plate file.
#' @param type A character string containing the type of elisa. The types
#' are "regular" or "cbt" for checkerboard elisa.
#' @param method A character string containing the method to be used in
#' ggplot. "L4" will use drc::L.4 with geom_smooth (default), "line" will use
#' geom_line() and "na" will not draw any lines.
#' @param point_size A numeric value for size in ggplot2.
#' @param linewidth A numeric value for linewidth in ggplot2.
#' @param xlog A logical value that transforms x-axis to log10 (default is FALSE).
#' @param errorbars A logical value that adds errorbars (with sd) (default is FALSE).
#' @param errorbar_width A numeric value for errorbar width in ggplot2.
#' @param primary An character string of the main plate name with
#' "blanks". If using default names (using create_plate()) this argument may
#' remain NULL.
#' @param od An character string of the plate name containing the od data.
#' By default it will use "od450."  If using default names (using create_plate())
#' this argument may remain NULL.
#' @param group_by An character vector of the plate name(s) for
#' grouping the data. If using default names (using create_plate()) this argument may
#' remain NULL.
#' @param x An character string for the x axis variable for ggplot2::aes.  If
#' using default names (using create_plate()) this argument may remain NULL.
#' @param color An character string for color in ggplot2::aes. If using default
#' names (using create_plate()) this argument may remain NULL.
#' @param filter_col character string to select plate name on which
#' the dplyr::filter will be used.
#' @param filter_var  An character vector with the elements that needs to be
#' filtered from `filter_col` using the dplyr::filter function.
#' @param ... Any other arguments from tidyplate::tidy_plate function
#'
#' @return A ggplot2 object
#' @export
#' @importFrom stats sd
#' @importFrom rlang :=
#'
#' @examples
#' file <- system.file("extdata", "elisa_example.xlsx", package = "elisa")
#' plot_elisa(file, xlog = TRUE, errorbars = TRUE)
#'
#' file_2 <- system.file("extdata", "elisa_example_2.xlsx", package = "elisa")
#' plot_elisa(file_2, xlog = TRUE, errorbars = TRUE)
#'
#' empty_plates <- system.file("extdata", "empty_plates.xlsx", package = "elisa")
#' plot_elisa(empty_plates, xlog = TRUE, errorbars = TRUE)
#'
#' file_cbt <- system.file("extdata", "cbt_example.xlsx", package = "elisa")
#' plot_elisa(file_cbt, type = "cbt", xlog = TRUE)
plot_elisa <- function(file,
                       type = c("regular", "cbt"),
                       method = c("L4", "line", "na"),
                       point_size = 3,
                       linewidth = 0.5,
                       xlog = FALSE,
                       errorbars = FALSE,
                       errorbar_width = 0.2,
                       primary = NULL,
                       od = NULL,
                       group_by = NULL,
                       x = NULL,
                       color = NULL,
                       filter_col = NULL,
                       filter_var = NULL,
                       ...) {

  # Ensure 'type' and 'method' are single values
  type <- match.arg(type)
  method <- match.arg(method)

  # Load and preprocess the data
  raw_data <- tidyplate::tidy_plate(file, ...)

  # Remove columns where all cells are empty
  raw_data <- raw_data[, !apply(raw_data, 2, function(x) all(is.na(x) | x == ""))]

  if (is.null(od)) {
    od <- "od450"
  }

  # Set default group_by based on 'type' and available columns if not
  # provided by the user
  if (is.null(group_by)) {
    if (type == "regular") {
      # Check if specific columns are present in raw_data
      if (all(c("primary_mab_conc", "primary_mab_name") %in% colnames(raw_data))) {
        primary <- "primary_mab_name"
        x <- "primary_mab_conc"
        group_by <- c("primary_mab_conc", "primary_mab_name")
        color <- "primary_mab_name"
      } else {
        primary <- "coat_protein_name"
        x <- "coat_protein_ug"
        group_by <- c("coat_protein_ug", "coat_protein_name")
        color <- "coat_protein_ug"
      }
    } else if (type == "cbt") {
      x <- "primary_mab_conc"
      primary <- "primary_mab_name"
      group_by <- c("primary_mab_conc", "coat_protein_ug")
      color <- "coat_protein_ug"
    }
  }

  # Convert character inputs to symbols
  primary_sym <- rlang::sym(primary)
  od_sym <- rlang::sym(od)
  group_by_syms <- rlang::syms(group_by)
  x_sym <- rlang::sym(x)
  color_sym <- rlang::sym(color)

  # Apply filtering if required
  if (!is.null(filter_var)) {
    filter_col_sym <- rlang::sym(filter_col)
    raw_data <- raw_data |>
      dplyr::filter(!(!!filter_col_sym %in% filter_var))
  }

  # Filter once for the "blank" and calculate the mean blank
  blank_data <- raw_data |>
    dplyr::filter(!!primary_sym == "blank")
  mean_blank <- mean(blank_data[[od]], na.rm = TRUE)

  # Apply blank subtraction and summarise in one step
  summary <- raw_data |>
    dplyr::filter(!!primary_sym != "blank") |>
    dplyr::mutate(blanked_od = .data[[od]] - mean_blank) |>
    dplyr::group_by(!!!group_by_syms) |>
    dplyr::summarise(
      mean_od = mean(blanked_od, na.rm = TRUE),
      mean_sd = sd(blanked_od, na.rm = TRUE),
      .groups = 'drop'
    )

  # Special handling for "cbt" type
  if (type == "cbt") {
    summary <- summary |>
      dplyr::filter(.data[[color]] != 0) |>
      dplyr::mutate(!!color_sym := forcats::fct_rev(forcats::as_factor(round(.data[[color]], 5))))
  }

  # Create the base plot with minimal layers for efficiency
  plot <- ggplot2::ggplot(summary,
                          ggplot2::aes(x = .data[[x]],
                                       y = mean_od,
                                       group = .data[[color]],
                                       color = .data[[color]])) +
    ggplot2::geom_point(size = point_size)

  # Add line or smooth layer based on the method
  if (method == "line") {
    plot <- plot + ggplot2::geom_line(linewidth = linewidth)
  } else if (method == "L4") {
    plot <- plot + ggplot2::geom_smooth(method = drc::drm,
                                        method.args = list(fct = drc::L.4()),
                                        se = FALSE, linewidth = linewidth)
  }

  # Conditionally add error bars
  if (errorbars) {
    plot <- plot +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_od - mean_sd,
                                          ymax = mean_od + mean_sd),
                             width = errorbar_width)
  }

  # Conditionally apply log scale for the x-axis
  if (xlog) {
    plot <- plot + ggplot2::scale_x_log10()
  }

  return(plot)
}
