#' Title
#'
#' @param file
#' @param group
#' @param x
#' @param plot_group
#' @param xlog
#' @param errorbars
#' @param ...
#'
#' @return
#' @export
#' @importFrom stats sd
#'
#' @examples
#'

# Declare global variables to avoid "no visible binding for global variable" note
utils::globalVariables(c("od450", "mean_blank", "blanked_od", "primary_mab", "mean_od", "mean_sd"))

plot_elisa <- function(file,
                       group,
                       x,
                       plot_group,
                       xlog = FALSE,
                       errorbars = FALSE,
                       ...) {

  raw_data <- tidyplate::tidy_plate(file,...)

  col_names <- c("primary_mab", "od450")

  if (col_names %in% names(raw_data)) {
    stop("Input data must have 'primary_mab' and 'od450'.")
  }

  mean_blank <- raw_data |>
    dplyr::filter(primary_mab == "blank") |>
    dplyr::summarise(mean_blank = mean(od450, na.rm = TRUE)) |>
    dplyr::pull(mean_blank)

  summary <- raw_data |>
    dplyr::filter(primary_mab != "blank") |>
    dplyr::mutate(blanked_od = od450 - mean_blank, ) |>
    dplyr::group_by(dplyr::across(c({{ group }}))) |>
    dplyr::summarise(
      mean_od = mean(blanked_od, na.rm = TRUE),
      mean_sd = sd(blanked_od, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  plot <- ggplot2::ggplot(summary,
                          ggplot2::aes({{ x }}, y = mean_od,
                                       group = {{ plot_group }},
                                       color = {{ plot_group }})) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_smooth(method = drc::drm,
                         method.args = list(fct = drc::L.4()),
                         se = F, linewidth = 0.5)
  # Conditionally add error bars if errorbars = TRUE
  if (errorbars) {
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymax = mean_od + mean_sd,
                                                       ymin = mean_od - mean_sd))
  }

  # Conditionally use log scale for x-axis if xlog = TRUE
  if (xlog) {
    plot <- plot + ggplot2::scale_x_log10()
  }

  return(plot)
}
