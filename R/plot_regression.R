#' Plot of linear and dose response regression.
#'
#' @param file An excel or csv file path containing data to be fitted in a tidy format.
#' @param data A dataframe or tibble of data to be fitted.
#' @param group The column containing the different categories in the data or file.
#' @param dose The column in the data or file that will be used in plotting
#' the X axis. If it is log10 transformed `xLog` argument should be FALSE.
#' @param response The column in the data or file that will be used in plotting
#' the Y axis.
#' @param regression_model The regression model; either "linear" or "dose_response".
#' The default is "linear".
#' @param dose_response_type If `regression_model` is "dose_response" then specify
#' whether the response is stimulatory ("stimulation") or inhibitory ("inhibition").
#' The default is "stimulation".
#' @param xLog A logical value that determines whether the X axis will be log
#' transformed or not. The default is FALSE.
#' @param facet A logical value of whether to facet or not. The default is FALSE.
#' @param ... Other arguments to be passed to ggplot2.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
plot_regression <- function(
    file = NULL,
    data = NULL,
    group,
    dose,
    response,
    regression_model = c("linear", "dose_response"),
    dose_response_type = c("stimulation", "inhibition"),
    xLog = FALSE,
    facet = FALSE,
    ...
) {
  # Read data
  if (!is.null(file) && is.character(file) && length(file) == 1) {
    if (tools::file_ext(file) == "csv") {
      raw_data <- suppressMessages(readr::read_csv(file))
    } else if (tools::file_ext(file) %in% c("xls", "xlsx")) {
      raw_data <- suppressMessages(readxl::read_excel(file))
    } else {
      rlang::abort("File type not supported. Use csv, xls, or xlsx files.", call = NULL)
    }
  } else if (is.data.frame(data)) {
    raw_data <- data
  } else {
    rlang::abort("Input data must be a file path or a dataframe/tibble.", call = NULL)
  }

  regression_model <- rlang::arg_match(regression_model)
  dose_response_type <- rlang::arg_match(dose_response_type)

  # Generate base plot
  p <- ggplot2::ggplot(raw_data, ggplot2::aes_string(x = dose, y = response, color = group)) +
    ggplot2::labs(x = dose, y = response, color = group)

  if (regression_model == "linear") {
    p <- p + ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE)
  } else if (regression_model == "dose_response") {
    # Define the dose-response model function
    model_func <- drc::LL.3()

    # Apply dose-response model for each level of group
    fit_list <- split(raw_data, raw_data[[group]]) %>%
      lapply(function(subdata) {
        drc::drm(as.formula(paste(response, "~", dose)), data = subdata, fct = model_func)
      })

    for (level in names(fit_list)) {
      # Generate x values based on xLog flag
      data_fit <- if (xLog) {
        data.frame(
          x = exp(seq(log(min(raw_data[[dose]], na.rm = TRUE)), log(max(raw_data[[dose]], na.rm = TRUE)), length.out = 100))
        )
      } else {
        data.frame(
          x = seq(min(raw_data[[dose]], na.rm = TRUE), max(raw_data[[dose]], na.rm = TRUE), length.out = 100)
        )
      }

      data_fit$y <- predict(fit_list[[level]], newdata = data_fit)
      data_fit[[group]] <- level

      p <- p + ggplot2::geom_line(data = data_fit,
                                  ggplot2::aes(x = x, y = y, color = !!rlang::sym(group)),
                                  inherit.aes = FALSE)
    }

    p <- p + ggplot2::geom_point()
  }

  # Add log scale to x-axis if specified
  if (xLog) {
    p <- p + ggplot2::scale_x_log10()
  }

  # Add faceting if specified
  if (facet) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", group)))
  }

  # Add additional layers specified in ...
  additional_layers <- list(...)
  for (layer in additional_layers) {
    # Check if each layer is a ggplot2 layer or compatible object before adding
    if (inherits(layer, "ggproto") || inherits(layer, "gg")) {
      p <- p + layer
    } else {
      warning("An element in `...` is not a ggplot2 layer and was ignored.")
    }
  }

  return(p)
}
