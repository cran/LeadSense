
# Declare global variables to avoid CRAN NOTE
utils::globalVariables(c("Band", "Power"))

#' Extract and summarize LFP data
#'
#' This function extracts and summarizes LFP (Local Field Potential) data from a JSON-like dataset.
#'
#' @param dataset A JSON object/list loaded into the work environment. If NULL, attempts to load the default dataset from the LeadSense package.
#'
#' @return A structured LFP dataset including:
#' \itemize{
#'   \item Power in each frequency band
#'   \item LFP Frequency vs Magnitude for each electrode
#'   \item Time-domain signals for all sequences in the LFP montage
#' }
#'
#' @importFrom dplyr filter group_by summarize mutate select across rowwise %>%
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line labs facet_wrap scale_colour_manual scale_fill_manual theme_minimal
#' @importFrom ggpubr theme_pubr
#' @export
#'
#' @examples
#' lfp_dataset <- lfp_data(dataset)
#' print(lfp_dataset$band_power_results)
#' print(lfp_dataset$structured_lfp_dataset)

lfp_data <- function(dataset = NULL) {

  # Load default dataset if none is provided
  if (is.null(dataset)) {
    data("dataset", package = "LeadSense", envir = environment())
  }

  if (!exists("dataset")) {
    stop("No dataset provided, and default dataset could not be loaded.")
  }

  # Initialize an empty dataframe
  structured_lfp_dataset <- data.frame(
    Hemisphere = character(0),
    SensingElectrodes = character(0),
    ArtifactStatus = character(0),
    PeakFrequencyInHertz = numeric(0),
    PeakMagnitudeInMicroVolt = numeric(0),
    LFPFrequency = numeric(0),
    LFPMagnitude = numeric(0)
  )

  # Extract LFP dataset if available
  if (!is.null(dataset$LFPMontage)) {
    for (i in seq_along(dataset$LFPMontage$LFPFrequency)) {

      # Extract relevant data
      temp_dataset <- data.frame(
        Hemisphere = rep(dataset$LFPMontage$Hemisphere[i], length(dataset$LFPMontage$LFPFrequency[[i]])),
        SensingElectrodes = rep(dataset$LFPMontage$SensingElectrodes[i], length(dataset$LFPMontage$LFPFrequency[[i]])),
        ArtifactStatus = rep(dataset$LFPMontage$ArtifactStatus[i], length(dataset$LFPMontage$LFPFrequency[[i]])),
        PeakFrequencyInHertz = rep(dataset$LFPMontage$PeakFrequencyInHertz[i], length(dataset$LFPMontage$LFPFrequency[[i]])),
        PeakMagnitudeInMicroVolt = rep(dataset$LFPMontage$PeakMagnitudeInMicroVolt[i], length(dataset$LFPMontage$LFPFrequency[[i]])),
        LFPFrequency = dataset$LFPMontage$LFPFrequency[[i]],
        LFPMagnitude = dataset$LFPMontage$LFPMagnitude[[i]]
      )

      # Append to the structured dataset
      structured_lfp_dataset <- rbind(structured_lfp_dataset, temp_dataset)
    }
  } else {
    message("No LFP montage data available.")
    return(NULL)
  }

  # Define frequency bands
  frequency_bands <- list(
    Delta = c(0.5, 4),
    Theta = c(4, 8),
    Alpha = c(8, 12),
    Beta = c(12, 30),
    Gamma = c(30, 100)
  )

  # Function to compute power in each frequency band
  calculate_band_power <- function(frequencies, magnitudes, bands) {
    sapply(bands, function(band) {
      sum(magnitudes[frequencies >= band[1] & frequencies <= band[2]], na.rm = TRUE)
    })
  }

  # Compute band power for each electrode
  band_power_results <- data.frame()

  unique_electrodes <- unique(structured_lfp_dataset$SensingElectrodes)

  for (electrode in unique_electrodes) {
    electrode_dataset <- structured_lfp_dataset %>% filter(SensingElectrodes == electrode)

    # Ensure the dataset exists for processing
    if (nrow(electrode_dataset) > 0) {
      band_powers <- calculate_band_power(electrode_dataset$LFPFrequency, electrode_dataset$LFPMagnitude, frequency_bands)

      band_power_results <- rbind(band_power_results, data.frame(
        SensingElectrodes = electrode,
        Band = names(band_powers),
        Power = band_powers
      ))
    }
  }

  # Check the results to make sure it's correct
  message("Returning band_power_results dataset.")

  # Visualize band power distribution
  p1 <- ggplot2::ggplot(band_power_results, ggplot2::aes(x = Band, y = Power, fill = Band)) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::facet_wrap(~SensingElectrodes) +
    ggplot2::labs(title = "Frequency Band Power Distribution per Electrode",
                  x = "\n Frequency Band",
                  y = "Power \n") +
    ggplot2::scale_colour_manual(values = c("deepskyblue4", "deeppink4", "bisque3", "darkslategray", "cadetblue4")) +
    ggplot2::scale_fill_manual(values = c("deepskyblue4", "deeppink4", "bisque3", "darkslategray", "cadetblue4")) +
    ggpubr::theme_pubr()

  # Print the plot
  print(p1)


  # Loop through each unique electrode type and plot the LFP Frequency vs Magnitude
  unique_electrodes <- unique(structured_lfp_dataset$SensingElectrodes)

  for (electrode in unique_electrodes) {
    electrode_dataset <- dplyr::filter(structured_lfp_dataset, SensingElectrodes == electrode)

    p2 <- ggplot2::ggplot(electrode_dataset, ggplot2::aes(LFPFrequency, LFPMagnitude, colour = Hemisphere, fill = Hemisphere)) +
      ggplot2::geom_point(size = 2, shape = 1, stroke = 1.5, alpha = 0.9) +
      ggplot2::scale_colour_manual(values = c("firebrick", "midnightblue")) +
      ggplot2::geom_line(linewidth = 2, alpha = 0.7) +
      ggplot2::labs(title = paste("LFP Frequency vs Magnitude for Electrodes:", electrode),
                    x = "\n LFP Frequency (Hz)",
                    y = "LFP Magnitude (uV) \n") +
      ggplot2::theme_minimal()

    print(p2)
  }

  # Time-domain signals plot
  if (!is.null(dataset$LfpMontageTimeDomain$TimeDomainData)) {
    for (i in seq_along(dataset$LfpMontageTimeDomain$TimeDomainData)) {
      plot(dataset$LfpMontageTimeDomain$TimeDomainData[[i]], type = "l",
           main = paste("Time-Domain Signal - Channel", dataset$LfpMontageTimeDomain$Channel[[i]]),
           xlab = "Time (seconds)", ylab = "Amplitude",
           col = rainbow(30)[i], lwd = 2)
    }
  }

  # Time-domain signals plot with actual channel labels and time in seconds
  if (!is.null(dataset$LfpMontageTimeDomain$TimeDomainData)) {
    for (i in seq_along(dataset$LfpMontageTimeDomain$TimeDomainData)) {

      signal <- dataset$LfpMontageTimeDomain$TimeDomainData[[i]]
      sample_rate <- dataset$LfpMontageTimeDomain$SampleRateInHz[[i]]
      time <- seq(0, length(signal) - 1) / sample_rate  # Time in seconds

      plot(time, signal, type = "l",
           main = paste("Time-Domain Signal Quality Check \n Channel", dataset$LfpMontageTimeDomain$Channel[[i]]),
           xlab = "Time (seconds)", ylab = "Amplitude (uV)",
           col = "black", lwd = 2)
    }
  }



  message("Returning structured LFP dataset.")

  return(list(
  band_power_results = band_power_results,
  structured_lfp_dataset = structured_lfp_dataset
  ))

}
