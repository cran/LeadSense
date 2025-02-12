
#' Extract basic session summary information in long format
#'
#' @param dataset A JSON object/list loaded into the work environment
#'
#' @return Long format table with summary session information
#' @export
#'
#' @examples summary_long()


summary_long <- function(dataset = NULL ) {

  # Define function to extract basic summary information in long format

  if (is.null(dataset)) {
    data("dataset", package = "LeadSense", envir = environment())
  }

  if (!exists("dataset")) {
    stop("No dataset provided, and default dataset could not be loaded.")
  }


    # Initialize an empty dataset frame with defined column names
  summary_df <- data.frame(Variable = character(0), Value = character(0), stringsAsFactors = FALSE)


  # Function to safely add dataset to the summary datasetframe
  add_to_summary <- function(variable_name, value) {
    # Ensure the 'Value' is coerced to a character type to avoid type mismatch
    value <- as.character(value)

    # Append the new dataset to the summary_df
    summary_df <<- rbind(summary_df, data.frame(Variable = variable_name, Value = value, stringsAsFactors = FALSE))
  }

  # Extract fields if they exist and handle missing fields with NA or other defaults
  add_to_summary("AbnormalEnd", ifelse(!is.null(dataset$AbnormalEnd), dataset$AbnormalEnd, NA))
  add_to_summary("FullyReadForSession", ifelse(!is.null(dataset$FullyReadForSession), dataset$FullyReadForSession, NA))
  add_to_summary("FeatureInformationCode", ifelse(!is.null(dataset$FeatureInformationCode), dataset$FeatureInformationCode, NA))
  add_to_summary("SessionDate", ifelse(!is.null(dataset$SessionDate), dataset$SessionDate, NA))
  add_to_summary("SessionEndDate", ifelse(!is.null(dataset$SessionEndDate), dataset$SessionEndDate, NA))
  add_to_summary("ProgrammerTimezone", ifelse(!is.null(dataset$ProgrammerTimezone), dataset$ProgrammerTimezone, NA))
  add_to_summary("ProgrammerUtcOffset", ifelse(!is.null(dataset$ProgrammerUtcOffset), dataset$ProgrammerUtcOffset, NA))
  add_to_summary("ProgrammerLocale", ifelse(!is.null(dataset$ProgrammerLocale), dataset$ProgrammerLocale, NA))
  add_to_summary("ProgrammerVersion", ifelse(!is.null(dataset$ProgrammerVersion), dataset$ProgrammerVersion, NA))

  # Patient Information Initial
  add_to_summary("PatientFirstNameInitial", ifelse(!is.null(dataset$PatientInformation$Initial$PatientFirstName),
                                                   dataset$PatientInformation$Initial$PatientFirstName, NA))
  add_to_summary("PatientLastNameInitial", ifelse(!is.null(dataset$PatientInformation$Initial$PatientLastName),
                                                  dataset$PatientInformation$Initial$PatientLastName, NA))
  add_to_summary("PatientGenderInitial", ifelse(!is.null(dataset$PatientInformation$Initial$PatientGender),
                                                dataset$PatientInformation$Initial$PatientGender, NA))
  add_to_summary("PatientDateOfBirthInitial", ifelse(!is.null(dataset$PatientInformation$Initial$PatientDateOfBirth),
                                                     dataset$PatientInformation$Initial$PatientDateOfBirth, NA))
  add_to_summary("PatientIdInitial", ifelse(!is.null(dataset$PatientInformation$Initial$PatientId),
                                             dataset$PatientInformation$Initial$PatientId, NA))
  add_to_summary("ClinicianNotesInitial", ifelse(!is.null(dataset$PatientInformation$Initial$ClinicianNotes),
                                                 dataset$PatientInformation$Initial$ClinicianNotes, NA))
  add_to_summary("DiagnosisInitial", ifelse(!is.null(dataset$PatientInformation$Initial$Diagnosis),
                                            dataset$PatientInformation$Initial$Diagnosis, NA))

  # Patient Information Final
  add_to_summary("PatientFirstNameFinal", ifelse(!is.null(dataset$PatientInformation$Final$PatientFirstName),
                                                 dataset$PatientInformation$Final$PatientFirstName, NA))
  add_to_summary("PatientLastNameFinal", ifelse(!is.null(dataset$PatientInformation$Final$PatientLastName),
                                                dataset$PatientInformation$Final$PatientLastName, NA))
  add_to_summary("PatientGenderFinal", ifelse(!is.null(dataset$PatientInformation$Final$PatientGender),
                                              dataset$PatientInformation$Final$PatientGender, NA))
  add_to_summary("PatientDateOfBirthFinal", ifelse(!is.null(dataset$PatientInformation$Final$PatientDateOfBirth),
                                                  dataset$PatientInformation$Final$PatientDateOfBirth, NA))
  add_to_summary("PatientIdFinal", ifelse(!is.null(dataset$PatientInformation$Final$PatientId),
                                          dataset$PatientInformation$Final$PatientId, NA))
  add_to_summary("ClinicianNotesFinal", ifelse(!is.null(dataset$PatientInformation$Final$ClinicianNotes),
                                               dataset$PatientInformation$Final$ClinicianNotes, NA))

  # Device Information Initial
  add_to_summary("NeurostimulatorInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$Neurostimulator),
                                                   dataset$DeviceInformation$Initial$Neurostimulator, NA))
  add_to_summary("NeurostimulatorModelInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$NeurostimulatorModel),
                                                      dataset$DeviceInformation$Initial$NeurostimulatorModel, NA))
  add_to_summary("NeurostimulatorSerialNumberInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$NeurostimulatorSerialNumber),
                                                             dataset$DeviceInformation$Initial$NeurostimulatorSerialNumber, NA))
  add_to_summary("NeurostimulatorLocationInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$NeurostimulatorLocation),
                                                          dataset$DeviceInformation$Initial$NeurostimulatorLocation, NA))
  add_to_summary("ImplantDateInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$ImplantDate),
                                              dataset$DeviceInformation$Initial$ImplantDate, NA))
  add_to_summary("DeviceDateTimeInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$DeviceDateTime),
                                                 dataset$DeviceInformation$Initial$DeviceDateTime, NA))
  add_to_summary("OverdischargeCountInitial", ifelse(!is.null(dataset$DeviceInformation$Initial$OverdischargeCount),
                                                      dataset$DeviceInformation$Initial$OverdischargeCount, NA))

  # Device Information Final
  add_to_summary("NeurostimulatorFinal", ifelse(!is.null(dataset$DeviceInformation$Final$Neurostimulator),
                                                dataset$DeviceInformation$Final$Neurostimulator, NA))
  add_to_summary("NeurostimulatorModelFinal", ifelse(!is.null(dataset$DeviceInformation$Final$NeurostimulatorModel),
                                                     dataset$DeviceInformation$Final$NeurostimulatorModel, NA))

  # Battery Information
  add_to_summary("BatteryPercentage", ifelse(!is.null(dataset$BatteryInformation$BatteryPercentage),
                                             dataset$BatteryInformation$BatteryPercentage, NA))
  add_to_summary("EstimatedBatteryLifeMonths", ifelse(!is.null(dataset$BatteryInformation$EstimatedBatteryLifeMonths),
                                                      dataset$BatteryInformation$EstimatedBatteryLifeMonths, NA))
  add_to_summary("TTEdataset", ifelse(!is.null(dataset$BatteryInformation$TTEdataset),
                                   dataset$BatteryInformation$TTEdataset, NA))
  add_to_summary("BatteryStatus", ifelse(!is.null(dataset$BatteryInformation$BatteryStatus),
                                         dataset$BatteryInformation$BatteryStatus, NA))

  # Stimulation Information
  add_to_summary("InitialStimStatus", ifelse(!is.null(dataset$Stimulation$InitialStimStatus),
                                             dataset$Stimulation$InitialStimStatus, NA))
  add_to_summary("FinalStimStatus", ifelse(!is.null(dataset$Stimulation$FinalStimStatus),
                                           dataset$Stimulation$FinalStimStatus, NA))

  # Lead Configuration Information
  add_to_summary("LeadConfigurationInitial", ifelse(!is.null(dataset$LeadConfiguration$Initial),
                                                    dataset$LeadConfiguration$Initial, NA))
  add_to_summary("LeadConfigurationFinal", ifelse(!is.null(dataset$LeadConfiguration$Final),
                                                  dataset$LeadConfiguration$Final, NA))

  # Battery Reminder Information
  add_to_summary("BatteryReminderEnabled", ifelse(!is.null(dataset$BatteryReminder$Enabled),
                                                  dataset$BatteryReminder$Enabled, NA))

  # Return the summary datasetframe in long format
  return(summary_df)
}



# Example of calling the function
# summary_long_table <- summary_long(dataset)

# print(summary_long_table)

