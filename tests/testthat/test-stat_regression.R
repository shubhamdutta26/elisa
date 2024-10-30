# Dose response regression tests

test_that("stat_regression displays error for wrong file type", {
  expect_error(stat_regression("test_data/wrong_file.cpp"),
               "File type not supported. Use csv, xls, or xlsx files.")
})

test_that("stat_regression displays error for wrong data type", {
  wrong_type <- LETTERS[1:8]
  wrong_type_2 <- list(x = LETTERS[1:8], y = 1:4, z = c(TRUE, FALSE))
  expect_error(stat_regression(wrong_type),
               "Input data must be a file path or a dataframe/tibble.")
})

test_that("stat_regression displays correct statistical information", {

  # stimulation data biorender values
  stimulation_biorender <- list(
    BestFitValues = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      EC50 = c(0.13443, 0.13709),
      `EC50 - 95% CI` = c("0.09725 to 0.18241", "0.10681 to 0.17415"),
      LogEC50 = c(-0.8715, -0.86298),
      `LogEC50 - 95% CI` = c("-1.01213 to -0.73896", "-0.97139 to -0.75908")
    ),
    Coefficients = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      Top = c(1.33321, 1.30986),
      Bottom = c(0.17931, 0.16828),
      `Hill slope` = c(1.05846, 1.11686)
    ),
    GoodnessOfFit = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      `Degrees of Freedom` = as.integer(c(17, 17)),
      `Residual Standard Error` = c(0.06741, 0.05543),
      `Sum of Squares` = c(0.07724, 0.05224),
      `R-Squared` = c(0.98232, 0.98809)
    )
  )

  stimulation <- stat_regression("test_data/stimulation_data.csv",
                              data = NULL,
                              group = "Drug",
                              dose = "Dose",
                              response ="Response",
                              regression_model = "dose_response",
                              dose_response_type = "stimulation",
                              doseLog = FALSE)

  expect_type(stimulation, "list")
  expect_identical(stimulation, stimulation_biorender)

  # inhibition data biorender values
  inhibition_biorender <- list(
    BestFitValues = tibble::tibble(
      Name = c("Drug_A", "Drug_B", "Drug_C"),
      IC50 = c(9.68022, 11.07297, 9.23308),
      `IC50 - 95% CI` = c("8.52064 to 11.48447", "9.79396 to 12.98923", "8.49869 to 10.21291"),
      LogIC50 = c(0.98589, 1.04426, 0.96535),
      `LogIC50 - 95% CI` = c("0.93047 to 1.06011", "0.99096 to 1.11358", "0.92935 to 1.00915")
    ),
    Coefficients = tibble::tibble(
      Name = c("Drug_A", "Drug_B", "Drug_C"),
      Top = c(89.48905, 91.12258, 88.28172),
      Bottom = c(11.21693, 6.87087, 12.96007),
      `Hill slope` = c(-1.24957, -1.13270, -1.42501)
    ),
    GoodnessOfFit = tibble::tibble(
      Name = c("Drug_A", "Drug_B", "Drug_C"),
      `Degrees of Freedom` = as.integer(c(17, 17, 17)),
      `Residual Standard Error` = c(2.0539, 1.61963, 1.56561),
      `Sum of Squares` = c(71.71452, 44.5945, 41.66933),
      `R-Squared` = c(0.99602, 0.99767, 0.99768)
    )
  )

  inhibition <- stat_regression("test_data/inhibition_data.csv",
                                 data = NULL,
                                 group = "Drug",
                                 dose = "Dose",
                                 response ="Response",
                                 regression_model = "dose_response",
                                 dose_response_type = "inhibition",
                                 doseLog = FALSE)

  expect_type(inhibition, "list")
  expect_identical(inhibition, inhibition_biorender)

  # one faulty group biorender values
  one_faulty_biorender <- list(
    BestFitValues = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      EC50 = c(0.13443, 0.13709),
      `EC50 - 95% CI` = c("0.09725 to 0.18241", "0.10681 to 0.17415"),
      LogEC50 = c(-0.87150, -0.86298),
      `LogEC50 - 95% CI` = c("-1.01213 to -0.73896", "-0.97139 to -0.75908")
    ),
    Coefficients = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      Top = c(1.33321, 1.30986),
      Bottom = c(0.17931, 0.16828),
      `Hill slope` = c(1.05846, 1.11686)
    ),
    GoodnessOfFit = tibble::tibble(
      Name = c("aBACE", "aSARS"),
      `Degrees of Freedom` = as.integer(c(17, 17)),
      `Residual Standard Error` = c(0.06741, 0.05543),
      `Sum of Squares` = c(0.07724, 0.05224),
      `R-Squared` = c(0.98232, 0.98809)
    )
  )

  one_faulty <- stat_regression("test_data/bad_drc_data.csv",
                                data = NULL,
                                group = "Drug",
                                dose = "Dose",
                                response ="Response",
                                regression_model = "dose_response",
                                dose_response_type = "stimulation",
                                doseLog = FALSE)

  expect_type(one_faulty, "list")
  expect_identical(one_faulty, one_faulty_biorender)

  # all faulty data biorender values
  all_faulty_expected <- list(
    BestFitValues = tibble::tibble(),
    Coefficients = tibble::tibble(),
    GoodnessOfFit = tibble::tibble()
  )

  all_faulty <- stat_regression("test_data/all_bad_drc_data.csv",
                                data = NULL,
                                group = "Drug",
                                dose = "Dose",
                                response ="Response",
                                regression_model = "dose_response",
                                dose_response_type = "stimulation",
                                doseLog = FALSE)

  expect_type(all_faulty, "list")
  expect_identical(all_faulty, all_faulty_expected)
})

test_that("stat_regression aborts when data cannot be fit", {

  expect_error(stat_regression(
    file = "test_data/stimulation_data.csv",
    data = NULL,
    group = "Drug",
    dose = "Dose",
    response ="Response",
    regression_model = "dose_response",
    dose_response_type = "stimulation",
    doseLog = TRUE),
    regexp = "The data could not be processed due to model fitting issues")

  expect_error(stat_regression(
    file = "test_data/inhibition_data.csv",
    data = NULL,
    group = "Drug",
    dose = "Dose",
    response ="Response",
    regression_model = "dose_response",
    dose_response_type = "inhibition",
    doseLog = TRUE),
    regexp = "The data could not be processed due to model fitting issues")
})

# Linear regression tests
