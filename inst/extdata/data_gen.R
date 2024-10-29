set.seed(123)  # For reproducibility

# Define dose levels
doses <- c(0.1, 0.5, 1, 5, 10, 50, 100)

# Function to generate stimulation data for multiple drugs
generate_stimulation_data <- function(doses, drug_names) {
  drug_data <- lapply(drug_names, function(drug_name) {
    response <- 10 + (90 - 10) / (1 + 10^((1 - log10(doses)) * 1.2)) # Using a Hill slope of 1.2
    response_triplicates <- rep(response, each = 3) + rnorm(length(response) * 3, sd = 2)

    tibble::tibble(
      Dose = rep(doses, each = 3),
      Drug = drug_name,
      Response = response_triplicates
    )
  })
  dplyr::bind_rows(drug_data)
}

# Function to generate inhibition data for multiple drugs
generate_inhibition_data <- function(doses, drug_names) {
  drug_data <- lapply(drug_names, function(drug_name) {
    response <- 90 - (90 - 10) / (1 + 10^((1 - log10(doses)) * 1.2)) # Using a Hill slope of 1.2
    response_triplicates <- rep(response, each = 3) + rnorm(length(response) * 3, sd = 2)

    tibble::tibble(
      Dose = rep(doses, each = 3),
      Drug = drug_name,
      Response = response_triplicates
    )
  })
  dplyr::bind_rows(drug_data)
}

# Define drug names
drugs <- c("Drug_A", "Drug_B", "Drug_C")

# Generate data for stimulation and inhibition drugs
stimulation_data <- generate_stimulation_data(doses, drugs)
inhibition_data <- generate_inhibition_data(doses, drugs)

write.csv(stimulation_data, "inst/extdata/stimulation_data.csv", row.names = FALSE)
write.csv(inhibition_data, "inst/extdata/inhibition_data.csv", row.names = FALSE)


linear_data <- data.frame(
  Dose = rep(c(0.1, 0.5, 1.0), times = 9),
  Drug = rep(c("Drug A", "Drug B", "Drug C"), each = 9),
  Response = c(
    1.2, 1.1, 1.3,
    1.8, 1.7, 1.9,
    2.5, 2.4, 2.6,
    1.0, 1.2, 0.9,
    1.6, 1.7, 1.5,
    2.2, 2.3, 2.1,
    0.8, 0.9, 0.7,
    1.3, 1.4, 1.2,
    1.9, 2.0, 1.8
  )
)

write.csv(linear_data, "inst/extdata/linear_data.csv", row.names = FALSE)






# Extract best fit values and calculate EC50 in both log and linear scale
best_fit_values[[each_id]] <- tibble::tibble(
  Name = each_id,
  EC50 = formatC(10 ^ coef(fit)["logEC50"], format = "f", digits = 5),
  `EC50 - 95% CI` = paste0(formatC(10 ^ conf_intervals["logEC50", 1], format = "f", digits = 5), " to ",
                           formatC(10 ^ conf_intervals["logEC50", 2], format = "f", digits = 5)),
  `Log EC50` = formatC(coef(fit)["logEC50"], format = "f", digits = 5),
  `Log EC50 - 95% CI` = paste0(formatC(conf_intervals["logEC50", 1], format = "f", digits = 5), " to ",
                               formatC(conf_intervals["logEC50", 2], format = "f", digits = 5))
)

# Extract coefficients for top, bottom, and HillSlope
coefficients[[each_id]] <- tibble::tibble(
  Name = each_id,
  Top = formatC(coef(fit)["top"], format = "f", digits = 5),
  Bottom = formatC(coef(fit)["bottom"], format = "f", digits = 5),
  `Hill slope` = formatC(coef(fit)["HillSlope"], format = "f", digits = 5)
)

# Calculate goodness-of-fit metrics
residuals <- resid(fit)
rss <- sum(residuals^2)  # Residual Sum of Squares
df <- df.residual(fit)   # Degrees of freedom
rse <- sqrt(rss / df)    # Residual Standard Error
tss <- sum((each_data[[response]] - mean(each_data[[response]]))^2) # Total Sum of Squares
r_squared <- 1 - (rss / tss) # R-squared

goodness_of_fit[[each_id]] <- tibble::tibble(
  Name = each_id,
  `Degrees of freedom` = df,
  `Residual standard error` = formatC(rse, format = "f", digits = 5),
  `Sum of squares` = formatC(rss, format = "f", digits = 5),
  `R-squared` = formatC(r_squared, format = "f", digits = 5)
)
}
}
