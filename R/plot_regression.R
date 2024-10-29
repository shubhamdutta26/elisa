plot_regression <- function(
    data = NULL,
    file_path = NULL,
    treatment,
    dose,
    response,
    curve_type = c("linear", "dose_response"),
    dose_response_model = c("3PL", "4PL"),
    log_dose = TRUE,
    facet = TRUE,
    ...
) {
  # Load data from file if not directly provided
  if (is.null(data)) {
    if (is.null(file_path)) {
      rlang::abort("Either `data` or `file_path` must be provided.")
    }

    # Detect file type and read accordingly
    file_ext <- tools::file_ext(file_path)
    if (file_ext == "csv") {
      data <- readr::read_csv(file_path)
    } else if (file_ext %in% c("xls", "xlsx")) {
      data <- readxl::read_excel(file_path)
    } else {
      rlang::abort("Unsupported file format. Please provide a csv or excel file.")
    }
  }

  # Validate curve type input
  curve_type <- rlang::arg_match(curve_type)

  # Validate drc model input
  dose_response_model <- rlang::arg_match(dose_response_model)

  # Generate base plot
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = dose, y = response, color = treatment)) +
    ggplot2::labs(x = dose, y = response, color = treatment)

  if (curve_type == "linear") {
    p <- p + ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE)
  } else if (curve_type == "dose_response") {
    # Define the dose-response model function
    model_func <- switch(
      dose_response_model,
      "3PL" = drc::LL.3(),
      "4PL" = drc::LL.4(),
      rlang::abort("Unsupported dose-response model.")
    )

    # Apply dose-response model for each level of treatment
    fit_list <- split(data, data[[treatment]]) %>%
      lapply(function(subdata) {
        drc::drm(as.formula(paste(response, "~", dose)), data = subdata, fct = model_func)
      })

    for (level in names(fit_list)) {
      # Generate x values spaced logarithmically if log_dose is TRUE
      if (log_dose) {
        data_fit <- data.frame(
          x = exp(seq(log(min(data[[dose]], na.rm = TRUE)), log(max(data[[dose]], na.rm = TRUE)), length.out = 100))
        )
      } else {
        data_fit <- data.frame(
          x = seq(min(data[[dose]], na.rm = TRUE), max(data[[dose]], na.rm = TRUE), length.out = 100)
        )
      }

      data_fit$y <- predict(fit_list[[level]], newdata = data_fit)
      data_fit[[treatment]] <- level

      p <- p + ggplot2::geom_line(data = data_fit,
                                  ggplot2::aes(x = x, y = y, color = !!rlang::sym(treatment)),
                                  inherit.aes = FALSE)
    }

    p <- p + ggplot2::geom_point()
  }

  # Add log scale to x-axis if specified
  if (log_dose) {
    p <- p + ggplot2::scale_x_log10()
  }

  # Add faceting if specified
  if (facet) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", treatment)))
  }

  # Add additional layers specified in ...
  p <- p + rlang::exprs(...)

  return(p)
}
