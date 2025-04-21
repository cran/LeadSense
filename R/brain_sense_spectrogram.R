#' Plot BrainSense Spectrograms and Return Data (with Optional Band Filtering)
#'
#' This function generates spectrograms for Medtronic BrainSense time-domain signals across one or more data passes.
#' Optionally, the user can select specific passes to plot, filter by frequency band, save the plots, and extract the underlying spectrogram data.
#'
#' @param dataset A JSON-like object (e.g., parsed with `jsonlite::fromJSON()`) containing Medtronic BrainSense data. If NULL, attempts to load the default dataset from the LeadSense package.
#' @param wl Integer. Window length for FFT. Default is 512.
#' @param ovlp Numeric. Overlap percentage between successive windows. Default is 75.
#' @param collevels Numeric. A sequence of color levels for the spectrogram image (in dB). Default is `seq(-80, 0, by = 0.2)`.
#' @param save_as Character. File format to save plots ("png", "pdf", or "jpeg"). If NULL (default), plots are not saved.
#' @param output_dir Character. Path to the directory where plots will be saved. Default is current working directory.
#' @param passes Integer vector. Indices of passes to plot (e.g., `c(1,3)`). Default is NULL, which plots all available passes.
#' @param band Character. One of "Delta", "Theta", "Alpha", "Beta", "Gamma". If provided, filters signal to this frequency band before generating the spectrogram.
#'
#' @return A list of data frames (invisible). Each data frame corresponds to one spectrogram and contains:
#' \describe{
#'   \item{time}{Time in seconds}
#'   \item{frequency}{Frequency in Hz}
#'   \item{magnitude}{Spectral power in dB}
#'   \item{channel}{Channel label}
#'   \item{pass}{Pass index (i)}
#' }
#'
#' @details
#' \strong{WARNING:} This function may be computationally intensive and take significant time to execute. Please wait until all plots are rendered.
#'
#' @importFrom seewave spectro
#' @importFrom reshape2 melt
#' @importFrom signal butter filtfilt
#' @export
#'
#' @examples
#' brain_sense_spectrogram(dataset, passes = c(2), band = "Beta")

brain_sense_spectrogram <- function(dataset = NULL,
                                    wl = 512,
                                    ovlp = 75,
                                    collevels = seq(-80, 0, by = 0.2),
                                    save_as = NULL,
                                    output_dir = getwd(),
                                    passes = NULL,
                                    band = NULL) {

  message("Warning: This function may take a long time to run as it generates spectrograms for each pass. Please be patient until all plots are rendered.\n")

  # Frequency bands in Hz
  bands <- list(
    Delta = c(0.5, 4),
    Theta = c(4, 8),
    Alpha = c(8, 13),
    Beta  = c(13, 35),
    Gamma = c(35, 100)
  )

  # Load default dataset if none provided
  if (is.null(dataset)) {
    data("dataset", package = "LeadSense", envir = environment())
    dataset <- get("dataset", envir = environment())
  }

  if (!exists("dataset") || is.null(dataset$BrainSenseTimeDomain$TimeDomainData) ||
      is.null(dataset$BrainSenseTimeDomain$Channel)) {
    message("No BrainSense time-domain data found.")
    return(NULL)
  }

  # Check required packages
  if (!requireNamespace("seewave", quietly = TRUE)) stop("Package 'seewave' is required. Please install it.")
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("Package 'reshape2' is required. Please install it.")
  if (!requireNamespace("signal", quietly = TRUE)) stop("Package 'signal' is required for filtering. Please install it.")

  # Validate band input
  if (!is.null(band)) {
    band <- tools::toTitleCase(tolower(band))  # case-insensitive match
    if (!(band %in% names(bands))) {
      stop("Invalid band specified. Choose one of: Delta, Theta, Alpha, Beta, Gamma.")
    }
  }

  output_list <- list()
  total_passes <- seq_along(dataset$BrainSenseTimeDomain$TimeDomainData)

  if (is.null(passes)) passes <- total_passes

  for (i in total_passes) {
    if (!(i %in% passes)) next

    signal_raw   <- dataset$BrainSenseTimeDomain$TimeDomainData[[i]]
    channel      <- dataset$BrainSenseTimeDomain$Channel[[i]]
    sampling_rate <- dataset$BrainSenseTimeDomain$SampleRateInHz[[i]]

    # Apply bandpass filter if requested
    if (!is.null(band)) {
      band_range <- bands[[band]]
      nyq <- sampling_rate / 2
      W <- band_range / nyq
      butter_filter <- signal::butter(n = 4, W = W, type = "pass")
      signal_filt <- signal::filtfilt(butter_filter, signal_raw)
    } else {
      signal_filt <- signal_raw
    }

    # Generate spectrogram
    spectro_output <- seewave::spectro(signal_filt,
                                       f = sampling_rate,
                                       wl = wl,
                                       ovlp = ovlp,
                                       dB = "max0",
                                       plot = TRUE,
                                       fastdisp = TRUE,
                                       main = paste("\n Spectrogram - Channel", channel, "(Pass", i, if (!is.null(band)) paste0(", ", band), ")\n"),
                                       collevels = collevels)

    # Convert to data.frame
    S_db <- spectro_output$amp
    time <- spectro_output$time
    freq <- spectro_output$freq

    df <- reshape2::melt(S_db)
    df$time <- time[df$Var2]
    df$frequency <- freq[df$Var1]
    df$magnitude <- df$value
    df$channel <- channel
    df$pass <- i

    df <- df[, c("time", "frequency", "magnitude", "channel", "pass")]
    output_list[[length(output_list) + 1]] <- df

    # Save plot if requested
    if (!is.null(save_as)) {
      file_name <- file.path(output_dir, paste0("spectrogram_pass_", i, ".", save_as))
      grDevices::dev.copy(match.fun(save_as), file_name)
      grDevices::dev.off()
    }
  }

  if (length(output_list) == 0) {
    message("No spectrogram data was generated. Check if the specified passes exist in the dataset.")
    return(NULL)
  }

  invisible(output_list)
}
