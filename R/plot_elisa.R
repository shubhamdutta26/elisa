#' Plots microplate ELISA data
#'
#' @param file A character string containing the path to a csv or excel
#' plate file.
#' @param type A character string containing the type of elisa. The types
#' are "regular" or "cbt" for checkerboard elisa.
#' @param primary An character string of the main plate name with
#' "blanks".
#' @param od An character string of the plate name containing the od
#' data. By default it will use "od450."
#' @param group_by An character vector of the plate name(s) for
#' grouping the data.
#' @param x An character string for the x axis variable for ggplot2::aes.
#' @param color An character string for color in ggplot2::aes.
#' @param method A character string containing the method to be used in
#' ggplot. "L4" will use drc::L.4 with geom_smooth (default), "line" will use
#' geom_line() and "na" will not draw any lines.
#' @param point_size A numeric value for size in ggplot2.
#' @param linewidth A numeric value for linewidth in ggplot2.
#' @param xlog A logical value that transforms x-axis to log10 (default is FALSE).
#' @param errorbars A logical value that adds errorbars (with sd) (default is FALSE).
#' @param errorbar_width A numeric value for errorbar width in ggplot2.
#' @param filter_col character string to select plate name on which
#' the dplyr::filter will be used.
#' @param filter_data  An character vector that needs to be filtered out
#' from `filter_col` using the dplyr::filter function.
#' @param ... Any other arguments from tidyplate::tidy_plate function
#'
#' @return A ggplot2 object
#' @export
#' @importFrom stats sd
#' @importFrom rlang :=
#'
#' @examples
#' file <- system.file("extdata", "elisa_example.xlsx", package = "elisa")
#' plot_elisa(file, primary = "primary_mab_name",
#'            group_by = c("primary_mab_conc", "primary_mab_name"),
#'            x = "primary_mab_conc", color = "primary_mab_name",
#'            xlog = TRUE, errorbars = TRUE)
#' plot_elisa(file, primary = "primary_mab_name", od = "od450",
#'            x = "primary_mab_conc", color = "primary_mab_name",
#'            xlog = TRUE, errorbars = TRUE)
#'
#' file_2 <- system.file("extdata", "elisa_example_2.xlsx", package = "elisa")
#' plot_elisa(file_2, primary = "coat_protein_name",
#'            group_by = c("coat_protein_conc", "coat_protein_name"),
#'            x = "coat_protein_conc", color = "coat_protein_name",
#'            xlog = TRUE, errorbars = TRUE)
#' plot_elisa(file_2, primary = "coat_protein_name", od = "od450",
#'            x = "coat_protein_conc", color = "coat_protein_name",
#'            xlog = TRUE, errorbars = TRUE)
#'
#' file_cbt <- system.file("extdata", "cbt_example.xlsx", package = "elisa")
#' plot_elisa(file_cbt, type = "cbt", primary = "primary_mab_name",
#'            group_by = c("primary_mab_conc", "coat_protein_ug"),
#'            x = "primary_mab_conc", color = "coat_protein_ug",
#'            xlog = TRUE)
#' plot_elisa(file_cbt, type = "cbt", primary = "primary_mab_name",
#'            x = "primary_mab_conc", color = "coat_protein_ug", xlog = TRUE)
plot_elisa <- function(file,
                       type = c("regular", "cbt"),
                       primary,
                       od = "od450",
                       group_by = NULL,
                       x,
                       color,
                       method = c("L4", "line", "na"),
                       point_size = 3,
                       linewidth = 0.5,
                       xlog = FALSE,
                       errorbars = FALSE,
                       errorbar_width = 0.2,
                       filter_col = NULL,
                       filter_data = NULL,
                       ...) {

  # Ensure 'type' and 'method' are single values
  type <- match.arg(type)
  method <- match.arg(method)

  # Load and preprocess the data
  raw_data <- tidyplate::tidy_plate(file, ...)

  # Set default group_by based on 'type' and available columns if not
  # provided by the user
  if (is.null(group_by)) {
    if (type == "regular") {
      # Check if specific columns are present in raw_data
      if (all(c("primary_mab_conc", "primary_mab_name") %in% colnames(raw_data))) {
        group_by <- c("primary_mab_conc", "primary_mab_name")
      } else {
        group_by <- c("coat_protein_conc", "coat_protein_name")
      }
    } else if (type == "cbt") {
      group_by <- c("primary_mab_conc", "coat_protein_ug")
    }
  }

  # Convert character inputs to symbols
  primary_sym <- rlang::sym(primary)
  od_sym <- rlang::sym(od)
  group_by_syms <- rlang::syms(group_by)
  x_sym <- rlang::sym(x)
  color_sym <- rlang::sym(color)

  # Apply filtering if required
  if (!is.null(filter_data)) {
    filter_col_sym <- rlang::sym(filter_col)
    raw_data <- raw_data |>
      dplyr::filter(!(!!filter_col_sym %in% filter_data))
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
    plot <- plot + ggplot2::geom_line()
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
