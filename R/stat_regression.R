analyse_regression <- function(
    file = NULL,
    data = NULL,
    treatment,
    dose,
    response,
    regression_model = c("linear", "dose response"),
    dose_response_type = c("stimulation", "inhibition"),
    xlog = TRUE
) {
  # Read data
  if (!is.null(file)) {
    if (tools::file_ext(file) == "csv") {
      raw_data <- suppressMessages(readr::read_csv(file))
    } else if (tools::file_ext(file) %in% c("xls", "xlsx")) {
      raw_data <- suppressMessages(readxl::read_excel(file))
    } else {
      stop("File type not supported. Use csv, xls, or xlsx.")
    }
  } else {
    raw_data <- data
  }

  regression_model <- rlang::arg_match(regression_model)
  dose_response_type <- rlang::arg_match(dose_response_type)

  unique_treatments <- unique(raw_data[[treatment]])

  # Initialize lists for results
  final_list <- list(
    BestFitValues = tibble::tibble(),
    Coefficients = tibble::tibble(),
    GoodnessOfFit = tibble::tibble(),
    StatisticalSignificance = tibble::tibble(),
    InteractionSignificance = tibble::tibble()
  )

  # Iterate over each unique treatment for analysis
  for (each_treatment in unique_treatments) {
    each_data <- dplyr::filter(raw_data, .data[[treatment]] == each_treatment)

    if (regression_model == "dose response") {
      # Prepare start values for nls fitting
      start_vals <- list(
        bottom = min(each_data[[response]], na.rm = TRUE),
        top = max(each_data[[response]], na.rm = TRUE),
        logEC50 = log10(median(each_data[[dose]], na.rm = TRUE)),
        hill_slope = 1
      )

      # Select the formula based on the dose response type
      if (dose_response_type == "stimulation") {
        formula <- as.formula(
          paste(response, "~ bottom + (top - bottom) / (1 + 10^((logEC50 - log10(", dose, ")) * hill_slope))")
        )
      } else if (dose_response_type == "inhibition") {
        formula <- as.formula(
          paste(response, "~ top - (top - bottom) / (1 + 10^((logEC50 - log10(", dose, ")) * hill_slope))")
        )
      }

      # Fit the model
      fit <- tryCatch(
        nls(formula, data = each_data, start = start_vals),
        error = function(e) NULL
      )

      # Skip if the model fit failed
      if (is.null(fit)) next

      # Best fit values for dose-response
      ec50 <- 10^coef(fit)["logEC50"]
      confint_vals <- confint(fit, level = 0.95)
      ec50_conf <- paste0(round(10^confint_vals["logEC50", ], 5), collapse = " to ")
      logec50_conf <- paste0(round(confint_vals["logEC50", ], 5), collapse = " to ")

      final_list$BestFitValues <- dplyr::bind_rows(
        final_list$BestFitValues,
        tibble::tibble(
          Name = each_treatment,
          EC50 = round(ec50, 5),
          EC50_CI = ec50_conf,
          LogEC50 = round(coef(fit)["logEC50"], 5),
          LogEC50_CI = logec50_conf
        )
      )

      # Coefficients for dose-response
      final_list$Coefficients <- dplyr::bind_rows(
        final_list$Coefficients,
        tibble::tibble(
          Name = each_treatment,
          Top = round(coef(fit)["top"], 5),
          Bottom = round(coef(fit)["bottom"], 5),
          HillSlope = round(coef(fit)["hill_slope"], 5)
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
          Name = each_treatment,
          DF = df.residual(fit),
          RSE = round(sigma(fit), 5),
          Sum_of_Squares = round(ss_res, 5),
          R_Squared = round(r_squared, 5)
        )
      )

    } else if (regression_model == "linear") {
      # Linear regression analysis
      lm_formula <- as.formula(paste(response, "~", dose))
      lm_fit <- tryCatch(
        lm(lm_formula, data = each_data),
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

        # 95% CI for slope and intercept
        slope_ci <- confint(lm_fit)[2, ]
        intercept_ci <- confint(lm_fit)[1, ]

        # Store linear regression results
        final_list$BestFitValues <- dplyr::bind_rows(
          final_list$BestFitValues,
          tibble::tibble(
            Name = each_treatment,
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
            Name = each_treatment,
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
            Name = each_treatment,
            F_Statistic = round(f_stat, 4),
            Dfn = dfn,
            Dfd = dfd,
            p_value = round(p_val, 4),
            p_value_stars = p_stars
          )
        )
      }
    }
  }

  # Pairwise interaction analysis
  drug_combinations <- combn(unique_treatments, 2, simplify = FALSE)
  for (drug_pair in drug_combinations) {
    pair_data <- raw_data %>% dplyr::filter(.data[[treatment]] %in% drug_pair)

    # Convert treatment to a factor to enable interaction term
    pair_data[[treatment]] <- factor(pair_data[[treatment]], levels = drug_pair)

    # Interaction model with treatment and dose
    interaction_formula <- as.formula(paste(response, "~", treatment, "*", dose))
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

  return(final_list)
}
