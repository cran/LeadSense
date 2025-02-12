# Declare global variables to avoid CRAN NOTE
utils::globalVariables(c("Hemisphere", "Type", "ResultValue",
                         "SensingElectrodes", "LFPFrequency",
                         "LFPMagnitude", "BandPower"))

#' Extract and summarize Impedance data if available
#'
#' This function extracts impedance data from a JSON-like dataset and computes summary statistics.
#'
#' @param dataset A JSON object/list loaded into the work environment. If NULL, attempts to load the default dataset from the LeadSense package.
#'
#' @return A list containing:
#' \itemize{
#'   \item `combined_impedance_df` - The full impedance dataset (if available).
#'   \item `impedance_summary` - Summary of mean impedance values by Hemisphere and Type.
#' }
#' If no valid impedance data is found, a message is printed instead.
#'
#' @importFrom dplyr group_by summarize %>%
#' @importFrom utils data
#' @importFrom grDevices rainbow
#' @export
#'
#' @examples
#' impedance_results <- impedance_summary(dataset)
#' print(impedance_results$impedance_summary)
#' print(impedance_results$combined_impedance_df)

impedance_summary <- function(dataset = NULL) {

  # Load default dataset if none is provided
  if (is.null(dataset)) {
    data("dataset", package = "LeadSense", envir = environment())
    dataset <- get("dataset", envir = environment())
  }

  if (!exists("dataset") || is.null(dataset)) {
    stop("No dataset provided, and default dataset could not be loaded.")
  }

  # Check if Impedance dataset exists
  if (!is.null(dataset$Impedance) && is.list(dataset$Impedance$Hemisphere)) {

    impedance_dataset <- dataset$Impedance$Hemisphere  # Full list

    # Function to extract and clean impedance dataset
    extract_impedance <- function(hemisphere_dataset, label) {
      if (is.null(hemisphere_dataset$SessionImpedance)) return(NULL)  # Skip if missing

      # Extract Monopolar if it exists
      monopolar <- if (!is.null(hemisphere_dataset$SessionImpedance$Monopolar)) {
        df <- do.call(rbind, hemisphere_dataset$SessionImpedance$Monopolar)
        df$Type <- "Monopolar"
        df$Hemisphere <- label
        df
      } else NULL

      # Extract Bipolar if it exists
      bipolar <- if (!is.null(hemisphere_dataset$SessionImpedance$Bipolar)) {
        df <- do.call(rbind, hemisphere_dataset$SessionImpedance$Bipolar)
        df$Type <- "Bipolar"
        df$Hemisphere <- label
        df
      } else NULL

      # Combine Monopolar and Bipolar dataset
      return(rbind(monopolar, bipolar))
    }

    # Extract Left and Right Hemisphere dataset
    left_dataset <- Filter(function(x) x$Hemisphere == "HemisphereLocationDef.Left", impedance_dataset)
    right_dataset <- Filter(function(x) x$Hemisphere == "HemisphereLocationDef.Right", impedance_dataset)

    # Apply function (only if dataset exists)
    left_impedance <- if (length(left_dataset) > 0) extract_impedance(left_dataset[[1]], "Left") else NULL
    right_impedance <- if (length(right_dataset) > 0) extract_impedance(right_dataset[[1]], "Right") else NULL

    # Combine left and right hemisphere dataset
    combined_impedance_df <- do.call(rbind, Filter(Negate(is.null), list(left_impedance, right_impedance)))

    # If we have a combined impedance dataset
    if (!is.null(combined_impedance_df) && nrow(combined_impedance_df) > 0) {

      # Summarize Impedance by Hemisphere and Type
      impedance_summary <- combined_impedance_df %>%
        dplyr::group_by(Hemisphere, Type) %>%
        dplyr::summarize(MeanImpedance = mean(ResultValue, na.rm = TRUE), .groups = "drop")

      # Return both the combined dataset and summary
      return(list(
        combined_impedance_df = combined_impedance_df,
        impedance_summary = impedance_summary
      ))

    } else {
      message("No valid impedance dataset available for this patient.")
    }

  } else {
    message("Impedance dataset is missing for this patient.")
  }
}
