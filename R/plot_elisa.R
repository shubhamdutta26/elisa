#' Plots microplate ELISA data
#'
#' @param file A character quoted string containing the path to a csv or excel
#' plate file.
#' @param primary An unquoted character string of the main plate name with
#' "blanks".
#' @param od An unquoted character string of the plate name containing the od
#' data.
#' @param group_by An unquoted character vector of the plate name(s) for
#' grouping the data
#' @param x An unquoted character string for the x axis variable for ggplot2::aes.
#' @param color An unquoted character string for color in ggplot2::aes.
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
#'
#' @examples
#' file <- system.file("extdata", "elisa_example.xlsx", package = "elisa")
#' plot_elisa(file, primary = primary_mab, od = od450,
#'            group_by = c(primary_mab, primary_mab_conc),
#'            x = primary_mab_conc, color = primary_mab,
#'            xlog = TRUE, errorbars = TRUE)
plot_elisa <- function(file,
                       primary,
                       od,
                       group_by,
                       x,
                       color,
                       point_size = 3,
                       linewidth = 0.5,
                       xlog = FALSE,
                       errorbars = FALSE,
                       errorbar_width = 0.2,
                       filter_col = NULL,
                       filter_data = NULL,
                       ...) {

  raw_data <- tidyplate::tidy_plate(file, ...)

  # If needed, filter out specific primary conditions
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

  # Plotting
  plot <- ggplot2::ggplot(summary,
                          ggplot2::aes(x = {{ x }},
                                       y = mean_od,
                                       group = {{ color }},
                                       color = {{ color }})) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::geom_smooth(method = drc::drm,
                         method.args = list(fct = drc::L.4()),
                         se = FALSE, linewidth = linewidth)

  # Conditionally add error bars if errorbars = TRUE
  if (errorbars) {
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymax = mean_od + mean_sd,
                                                       ymin = mean_od - mean_sd),
                                          width = errorbar_width)
  }

  # Conditionally use log scale for x-axis if xlog = TRUE
  if (xlog) {
    plot <- plot + ggplot2::scale_x_log10()
  }

  return(plot)
}
