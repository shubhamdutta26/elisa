#' Plots microplate ELISA data
#'
#' @param file A quoted character string containing the path to a csv or excel
#' plate file.
#' @param type A quoted character string containing the type of elisa. The types
#' are "regular" or "cbt" for checkerboard elisa.
#' @param primary An unquoted character string of the main plate name with
#' "blanks".
#' @param od An unquoted character string of the plate name containing the od
#' data. By default it will use od450.
#' @param group_by An unquoted character vector of the plate name(s) for
#' grouping the data.
#' @param x An unquoted character string for the x axis variable for ggplot2::aes.
#' @param color An unquoted character string for color in ggplot2::aes.
#' @param method A quoted character string containing the method to be used in
#' ggplot. "L4" will use drc::L.4 with geom_smooth (default), "line" will use
#' geom_line() and "na" will not draw any lines.
#' @param point_size A numeric value for size in ggplot2.
#' @param linewidth A numeric value for linewidth in ggplot2.
#' @param xlog A logical value that transforms x-axis to log10 (default is FALSE).
#' @param errorbars A logical value that adds errorbars (with sd) (default is FALSE).
#' @param errorbar_width A numeric value for errorbar width in ggplot2.
#' @param filter_col An unquoted character string to select plate name on which
#' the dplyr::filter will be used.
#' @param filter_data  An quoted character vector that needs to be filtered out
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
#' plot_elisa(file, primary = primary_mab_name, od = od450,
#'            group_by = c(primary_mab_conc, primary_mab_name),
#'            x = primary_mab_conc, color = primary_mab_name,
#'            xlog = TRUE, errorbars = TRUE)
#'
#' file_2 <- system.file("extdata", "elisa_example_2.xlsx", package = "elisa")
#' plot_elisa(file_2, primary = coat_protein_name, od = od450,
#'            group_by = c(coat_protein_conc, coat_protein_name),
#'            x = coat_protein_conc, color = coat_protein_name,
#'            xlog = TRUE, errorbars = TRUE)
#'
#' file_cbt <- system.file("extdata", "cbt_example.xlsx", package = "elisa")
#' plot_elisa(file_cbt, type = "cbt", primary = primary_mab_name, od = od450,
#'            group_by = c(primary_mab_conc, coat_protein_ug),
#'            x = primary_mab_conc, color = coat_protein_ug,
#'            xlog = TRUE)
plot_elisa <- function(file,
                       type = c("regular", "cbt"),
                       primary,
                       od = od450,
                       group_by,
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

  raw_data <- tidyplate::tidy_plate(file, ...)

  type <- match.arg(type)
  method <- match.arg(method)

  # The primary plate must have the blanks marked
  if(!is.null(filter_data)) {
    raw_data_filtered <- raw_data |>
      dplyr::filter(!({{ filter_col }} %in% filter_data))
  } else {
    raw_data_filtered <- raw_data
  }


  # Calculate mean for the "blank" group
  mean_blank <- raw_data_filtered |>
    dplyr::filter({{ primary }} == "blank") |>
    dplyr::summarise(mean_blank = mean({{ od }}, na.rm = TRUE)) |>
    dplyr::pull(mean_blank)

  # Create a summary by subtracting the mean_blank and calculating mean and sd
  summary <- raw_data_filtered |>
    dplyr::filter({{ primary }} != "blank") |>
    dplyr::mutate(blanked_od = {{ od }} - mean_blank) |>
    dplyr::group_by(dplyr::across({{ group_by }})) |>
    dplyr::summarise(
      mean_od = mean(blanked_od, na.rm = TRUE),
      mean_sd = sd(blanked_od, na.rm = TRUE),
      .groups = 'drop'  # Add this to avoid grouping issues
    )

  if (type == "cbt") {
    summary <- summary |>
      dplyr::filter({{ color }} != 0) |>
      dplyr::mutate({{ color }} := round({{ color }}, 5)) |>
      dplyr::mutate({{ color }} := forcats::as_factor({{ color }})) |>
      dplyr::mutate({{ color }} := forcats::fct_rev({{ color }}))
  }

  # Plotting
  plot <- ggplot2::ggplot(summary,
                          ggplot2::aes(x = {{ x }},
                                       y = mean_od,
                                       group = {{ color }},
                                       color = {{ color }})) +
    ggplot2::geom_point(size = point_size)

  if(method == "line") {
    plot <- plot +
      ggplot2::geom_line()
  } else if(method == "L4") {
    plot <- plot +
      ggplot2::geom_smooth(method = drc::drm,
                           method.args = list(fct = drc::L.4()),
                           se = FALSE, linewidth = linewidth)
  }

  # Conditionally add error bars if errorbars = TRUE
  if (errorbars) {
    plot <- plot +
      ggplot2::geom_errorbar(ggplot2::aes(ymax = mean_od + mean_sd,
                                          ymin = mean_od - mean_sd),
                                          width = errorbar_width)
  }

  # Conditionally use log scale for x-axis if xlog = TRUE
  if (xlog) {
    plot <- plot +
      ggplot2::scale_x_log10()
  }

  return(plot)
}
