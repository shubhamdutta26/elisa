stat_regression <- function(
    file = NULL,
    data = NULL,
    group,
    dose,
    response,
    regression_model = c("linear", "dose_response"),
    dose_response_type = c("stimulation", "inhibition"),
    doseLog = TRUE
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

  # Set labels based on dose-response type
  ec50_label <- if (dose_response_type == "inhibition") "IC50" else "EC50"
  logec50_label <- if (dose_response_type == "inhibition") "LogIC50" else "LogEC50"

  unique_groups <- unique(raw_data[[group]])

  final_list <- list(
      BestFitValues = tibble::tibble(),
      Coefficients = tibble::tibble(),
      GoodnessOfFit = tibble::tibble()
    )

  # Iterate over each unique group for analysis
  for (each_group in unique_groups) {
    each_data <- dplyr::filter(raw_data, .data[[group]] == each_group)

    # Wrap model fitting in tryCatch to handle errors
    result <- tryCatch({
      if (regression_model == "dose_response") {
        # Prepare start values for nls fitting
        start_vals <- list(
          bottom = min(each_data[[response]], na.rm = TRUE),
          top = max(each_data[[response]], na.rm = TRUE),
          logEC50 = log10(median(each_data[[dose]], na.rm = TRUE)),
          hill_slope = ifelse(dose_response_type == "stimulation", 1, -1)
        )

        formula <- as.formula(
          paste(response, "~ bottom + (top - bottom) / (1 + 10^((logEC50 - log10(", dose, ")) * hill_slope))")
        )

        # Fit the model
        fit <- suppressWarnings(nls(formula, data = each_data, start = start_vals))

        coef_fit <- coef(fit)

        # Best fit values for dose-response
        ec50 <- unname(10^coef_fit["logEC50"])
        confint_vals <- confint(fit, level = 0.95)
        ec50_conf <- paste0(round(10^confint_vals["logEC50", ], 5), collapse = " to ")
        logec50_conf <- paste0(round(confint_vals["logEC50", ], 5), collapse = " to ")

        final_list$BestFitValues <- dplyr::bind_rows(
          final_list$BestFitValues,
          tibble::tibble(
            Name = each_group,
            !!rlang::sym(ec50_label) := round(ec50, 5),
            !!rlang::sym(paste0(ec50_label, " - 95% CI")) := ec50_conf,
            !!rlang::sym(logec50_label) := unname(round(coef_fit["logEC50"], 5)),
            !!rlang::sym(paste0(logec50_label, " - 95% CI")) := logec50_conf
          )
        )

        # Coefficients for dose-response
        final_list$Coefficients <- dplyr::bind_rows(
          final_list$Coefficients,
          tibble::tibble(
            Name = each_group,
            Top = unname(round(coef_fit["top"], 5)),
            Bottom = unname(round(coef_fit["bottom"], 5)),
            `Hill slope` = unname(round(coef_fit["hill_slope"], 5))
          )
        )

        # Goodness of fit for dose-response
        residuals <- residuals(fit)
        ss_res <- sum(residuals^2)
        ss_tot <- sum((each_data[[response]] - mean(each_data[[response]], na.rm = TRUE))^2)
        r_squared <- 1 - (ss_res / ss_tot)

        final_list$GoodnessOfFit <- dplyr::bind_rows(
          final_list$GoodnessOfFit,
          tibble::tibble(
            Name = each_group,
            `Degrees of Freedom` = df.residual(fit),
            `Residual Standard Error` = round(sigma(fit), 5),
            `Sum of Squares` = round(ss_res, 5),
            `R-Squared` = round(r_squared, 5)
          )
        )
      } else if (regression_model == "linear") {
        # Linear regression analysis
        lm_formula <- as.formula(paste(response, "~", dose))
        lm_fit <- tryCatch(
          suppressWarnings(lm(lm_formula, data = each_data)),
          error = function(e) NULL
        )

        if (!is.null(lm_fit)) {
          lm_summary <- summary(lm_fit)
          coef_lm <- lm_summary$coefficients

          # Extract coefficients
          slope <- coef_lm[2, 1]
          intercept <- coef_lm[1, 1]
          slope_se <- coef_lm[2, 2]
          intercept_se <- coef_lm[1, 2]
          r_squared_lm <- lm_summary$r.squared
          sy_x <- sqrt(sum(lm_fit$residuals^2) / lm_fit$df.residual)

          confint_lm <- confint(lm_fit)

          # 95% CI for slope and intercept
          slope_ci <- confint_lm[2, ]
          intercept_ci <- confint_lm[1, ]

          # Store linear regression results
          final_list$BestFitValues <- dplyr::bind_rows(
            final_list$BestFitValues,
            tibble::tibble(
              Name = each_group,
              Equation = paste("Y =", round(slope, 5), "X +", round(intercept, 5)),
              Slope = round(slope, 5),
              Slope_SE = round(slope_se, 5),
              Slope_95_CI = paste0(round(slope_ci[1], 5), " to ", round(slope_ci[2], 5)),
              Y_intercept = round(intercept, 5),
              Y_intercept_SE = round(intercept_se, 5),
              Y_intercept_95_CI = paste0(round(intercept_ci[1], 5), " to ", round(intercept_ci[2], 5)),
              X_intercept = round(-intercept / slope, 5)
            )
          )

          # Goodness of fit for linear regression
          final_list$GoodnessOfFit <- dplyr::bind_rows(
            final_list$GoodnessOfFit,
            tibble::tibble(
              Name = each_group,
              R_Squared_LM = round(r_squared_lm, 5),
              Sy_x = round(sy_x, 5)
            )
          )

          # Statistical significance for linear regression
          anova_lm <- anova(lm_fit)
          f_stat <- anova_lm$`F value`[1]
          dfn <- anova_lm$Df[1]
          dfd <- anova_lm$Df[2]
          p_val <- anova_lm$`Pr(>F)`[1]

          # Adding significance stars based on p-value
          p_stars <- ifelse(p_val < 0.05, "*", "ns")

          # Store statistical significance
          final_list$StatisticalSignificance <- dplyr::bind_rows(
            final_list$StatisticalSignificance,
            tibble::tibble(
              Name = each_group,
              F_Statistic = round(f_stat, 4),
              Dfn = dfn,
              Dfd = dfd,
              p_value = round(p_val, 4),
              p_value_stars = p_stars
            )
          )

          # Pairwise interaction analysis
          drug_combinations <- combn(unique_groups, 2, simplify = FALSE)
          for (drug_pair in drug_combinations) {
            pair_data <- raw_data %>% dplyr::filter(.data[[group]] %in% drug_pair)

            # Convert group to a factor to enable interaction term
            pair_data[[group]] <- factor(pair_data[[group]], levels = drug_pair)

            # Interaction model with group and dose
            interaction_formula <- as.formula(paste(response, "~", group, "*", dose))
            interaction_fit <- tryCatch(
              lm(interaction_formula, data = pair_data),
              error = function(e) NULL
            )

            if (!is.null(interaction_fit)) {
              anova_interaction <- anova(interaction_fit)

              # Get interaction term significance
              interaction_f_stat <- anova_interaction$`F value`[3]  # Interaction term
              interaction_dfn <- anova_interaction$Df[3]
              interaction_dfd <- anova_interaction$Df[4]
              interaction_p_val <- anova_interaction$`Pr(>F)`[3]

              # Adding significance stars based on p-value
              interaction_p_stars <- ifelse(interaction_p_val < 0.05, "*", "ns")

              # Store interaction significance results
              final_list$InteractionSignificance <- dplyr::bind_rows(
                final_list$InteractionSignificance,
                tibble::tibble(
                  Drug_Pair = paste(drug_pair, collapse = " & "),
                  Interaction_F_Statistic = round(interaction_f_stat, 4),
                  Dfn = interaction_dfn,
                  Dfd = interaction_dfd,
                  Interaction_p_value = round(interaction_p_val, 4),
                  p_value_stars = interaction_p_stars
                )
              )
            }
          }
        }
      }
    }, warning = function(e) {
      rlang::warn(paste(each_group, "could not be processed due to model fitting issues."),
                  call = NULL)})
  }

  return(final_list)
}
