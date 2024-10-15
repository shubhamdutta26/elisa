#' Creates a 96 well ELISA plate template
#'
#' @param plate_type An integer that defines the type of microwell plate (default
#' is 96).
#' @param n_plates An integer that defines the total number of plates (default
#' is 19).
#' @param plate_names A character vector that defines name of each plate.
#' @param file_type A character string that defines type of file (default is
#' xlsx)
#' @param file_name A character string that defines name of file
#'
#' @return A xlsx or csv file (default is xlsx)
#' @importFrom utils write.table
#' @export
#'
#' @examples
#' \dontrun{
#' create_plate()
#' }
create_plate <- function(plate_type = 96,
                         n_plates = 19,
                         plate_names = NULL,
                         file_type = c("xlsx", "csv"),
                         file_name = NULL) {
  # Validate inputs
  file_type <- match.arg(file_type)

  if (!(plate_type %in% c(6, 12, 24, 48, 96, 384, 1536))) {
    stop("Invalid `plate_type` provided. `plate_type` must be an integer and one of 6, 12, 24, 48, 96, 384, or 1536.", call. = FALSE)
  }

  # Validate n_plates (check if it's a positive integer)
  if (!is.numeric(n_plates) || n_plates != as.integer(n_plates) || n_plates < 1) {
    stop("Invalid `n_plates` value provided. `n_plates` must be a positive integer.", call. = FALSE)
  }

  if (is.null(plate_names)) {
    plate_names <- c("coat_protein_name", "coat_protein_ug", "coat_protein_vendor",
                     "coat_protein_cat", "coat_protein_lot", "coat_protein_source",
                     "primary_mab_name", "primary_mab_conc", "primary_mab_vendor",
                     "primary_mab_cat", "primary_mab_lot", "primary_mab_source",
                     "detection_mab_name", "detection_mab_dil", "detection_mab_vendor",
                     "detection_mab_cat", "detection_mab_lot", "detection_mab_source",
                     "od450")
  } else {
    plate_names <- plate_names
  }

  # Validate file (if provided)
  if (!is.null(file_name) && !is.character(file_name)) {
    stop("`file` must be a character string.", call. = FALSE)
  }

  dims <- plate_dims[[as.character(plate_type)]]
  n_rows <- dims[1]
  n_cols <- dims[2]

  # Generate row labels (extend beyond A-Z for larger plates)
  row_labels <- c(LETTERS, paste0("A", LETTERS))[1:n_rows]

  # Initialize plate list
  plate_list <- vector("list", n_plates * 2 - 1)  # For plates and spacing

  for (i in seq_len(n_plates)) {
    # Create the plate with the correct number of rows and columns
    plate <- data.frame(
      X1 = c(plate_names[i], row_labels),  # Plate name + row labels
      X2 = c(NA, rep(NA, n_rows))          # Initialize other columns as NA
    )

    for (col in seq_len(n_cols)) {
      plate[, paste0("X", col + 1)] <- c(col, rep(NA, n_rows))  # Fill columns with well numbers
    }

    plate_list[[2 * i - 1]] <- plate  # Add plate

    if (i < n_plates) {
      # Add a spacer row if more plates remain
      plate_list[[2 * i]] <- data.frame(matrix(NA, nrow = 1, ncol = n_cols + 1))
    }
  }

  # Combine plates
  final_plate <- do.call(rbind, plate_list)

  # Define default file path and name if not provided
  if (is.null(file_name)) {
    file_name <- paste0(Sys.Date(), "_exptType_", "descExp_SD.", file_type)
  } else {
    file_name <- file_name
  }

  # Export as file
  if (file_type == "xlsx") {
    openxlsx::write.xlsx(final_plate, file = file_name, colNames = FALSE)
  } else if (file_type == "csv") {
    write.table(final_plate, file = file_name, sep = ",",
                row.names = FALSE, col.names = FALSE, na = "")
  }
}
