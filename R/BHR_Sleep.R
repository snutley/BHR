#' @title \strong{BHR Sleep Module}
#'
#' @description This function cleanses data from the Brain Health Registry Sleep Module. The user is asked to input a raw dataframe from the BHR Sleep Module and to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point, last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and scores the Pittsburgh Sleep Quality Index (PSQI) using standard scoring algorithms .
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Sleep Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a completed PSQI assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a completed PSQI assessment.
#' (3) "ALL_PSQI": For each participant, return all time points with a completed PSQI assessment.
#' (4) "ALL": For each participant, return all time points with hoarding data, regardless of whether the PSQI was completed at the given time point.
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "PSQI": Output dataframe includes variables PSQI component and summary scores only.
#' (2) "ALL": Output dataframe includes all sleep variables, including PSQI component and summary scores.
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Sleep:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Sleep:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID142:} {See BHR Codebook}
#'  \item \code{QID143:} {See BHR Codebook}
#'  \item \code{QID94:} {See BHR Codebook}
#'  \item \code{QID95.1:} {See BHR Codebook}
#'  \item \code{QID95.2:} {See BHR Codebook}
#'  \item \code{QID95.3:} {See BHR Codebook}
#'  \item \code{QID95.4:} {See BHR Codebook}
#'  \item \code{QID95.5:} {See BHR Codebook}
#'  \item \code{QID95.6:} {See BHR Codebook}
#'  \item \code{QID95.7:} {See BHR Codebook}
#'  \item \code{QID95.8:} {See BHR Codebook}
#'  \item \code{QID95.9:} {See BHR Codebook}
#'  \item \code{QID95.10:} {See BHR Codebook}
#'  \item \code{QID95.10.TEXT:} {See BHR Codebook}
#'  \item \code{QID96:} {See BHR Codebook}
#'  \item \code{QID97:} {See BHR Codebook}
#'  \item \code{QID98:} {See BHR Codebook}
#'  \item \code{QID145:} {See BHR Codebook}
#'  \item \code{QID146:} {See BHR Codebook}
#'  \item \code{QID101.1:} {See BHR Codebook}
#'  \item \code{QID101.2:} {See BHR Codebook}
#'  \item \code{QID101.3:} {See BHR Codebook}
#'  \item \code{QID101.4:} {See BHR Codebook}
#'  \item \code{QID101.5:} {See BHR Codebook}
#'  \item \code{QID101.5.TEXT:} {See BHR Codebook}
#'  \item \code{QID196:} {See BHR Codebook}
#'  \item \code{QID197:} {See BHR Codebook}
#'  \item \code{Sleep_Quality:} {PSQI Component 1. Calculated using BHR item QID96.}
#'  \item \code{Sleep_Latency:} {PSQI Component 2. Calculated using BHR items QID142 and QID95.1.}
#'  \item \code{Sleep_Duration:} {PSQI Component 3. Calculated using BHR item QID94.}
#'  \item \code{Sleep_Efficiency:} {PSQI Component 4. Calculated using BHR items QID94, QID141, and QID143.}
#'  \item \code{Sleep_Disturbance:} {PSQI Component 5. Calculated using BHR items QID95.2, QID95.3, QID95.4, QID95.5, QID95.6, QID95.7, QID95.8, QID95.9, and QID95.10.}
#'  \item \code{Sleep_Medication:} {PSQI Component 6. Calculated using BHR item QID97.}
#'  \item \code{Daytime_Fatigue:} {PSQI Component 7. Calculated using BHR items QID98 and QID145.}
#'  \item \code{PSQI_Total:} {Total score on the Pittsburgh Sleep Quality Index. Calculated by summing the seven components of the PSQI. Scores range from 0 to 21}
#'  }
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr between
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr drop_na
#'
#' @export

BHR_Sleep <- function(dataset, TimePoint = "ALL_PSQI", Return = "PSQI"){

  #Check 1: BHR Sleep data has been entered into the function
  if(("QID96" %in% colnames(dataset) &
      "QID142" %in% colnames(dataset) &
      "QID94" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Sleep module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_PSQI", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
             'FIRST' = First time point with PSQI data,
             'LAST' = Last time point with PSQI data,
             'ALL_PSQI' = All time points with PSQI data,
             'ALL' = All time points with and without complete PSQI data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("PSQI", "ALL")) {
    stop("Please specify the output variables:
             'PSQI' = Pittsburgh Sleep Quality Index component and summary scores only,
             'ALL' = All data columns")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Recode PSQI
  dataset$Sleep_Quality <- dataset$QID96

  dataset$Sleep_Latency <- NA
        dataset$QID142_new <- NA
        dataset$QID142_new[dataset$QID142 <= 4] <- 0
        dataset$QID142_new[between(dataset$QID142, 5, 6)] <- 1
        dataset$QID142_new[between(dataset$QID142, 7, 8)] <- 2
        dataset$QID142_new[dataset$QID142 == 9] <- 3
    dataset$Sleep_Latency[rowSums(dataset[c("QID142_new", "QID95.1")]) == 0] <- 0
    dataset$Sleep_Latency[between(rowSums(dataset[c("QID142_new", "QID95.1")]), 1, 2)] <- 1
    dataset$Sleep_Latency[between(rowSums(dataset[c("QID142_new", "QID95.1")]), 3, 4)] <- 2
    dataset$Sleep_Latency[between(rowSums(dataset[c("QID142_new", "QID95.1")]), 5, 6)] <- 3

  dataset$Sleep_Duration <- NA
    dataset$Sleep_Duration[dataset$QID94 < 4] <- 3
    dataset$Sleep_Duration[between(dataset$QID94, 4, 5)] <- 2
    dataset$Sleep_Duration[between(dataset$QID94, 6, 7)] <- 1
    dataset$Sleep_Duration[dataset$QID94 >= 8] <- 0

  dataset$Sleep_Efficiency <- NA
  dataset$HoursSlept <- NA
  dataset$HoursSlept[dataset$QID94 < 3] <- 4
  dataset$HoursSlept[dataset$QID94 == 3] <- 4.5
  dataset$HoursSlept[dataset$QID94 == 4] <- 5
  dataset$HoursSlept[dataset$QID94 == 5] <- 5.5
  dataset$HoursSlept[dataset$QID94 == 6] <- 6
  dataset$HoursSlept[dataset$QID94 == 7] <- 6.5
  dataset$HoursSlept[dataset$QID94 == 8] <- 7
  dataset$HoursSlept[dataset$QID94 == 9] <- 7.5
  dataset$HoursSlept[dataset$QID94 == 10] <- 8
  dataset$HoursSlept[dataset$QID94 == 11] <- 8.5
  dataset$HoursSlept[dataset$QID94 == 12] <- 9
  dataset$HoursSlept[dataset$QID94 == 13] <- 9.5
  dataset$HoursSlept[dataset$QID94 == 14] <- 10
  dataset$HoursSlept[dataset$QID94 == 15] <- 10.5
  dataset$HoursSlept[dataset$QID94 == 16] <- 11
  dataset$HoursSlept[dataset$QID94 == 17] <- 11.5
  dataset$HoursSlept[dataset$QID94 > 17] <- 12
  dataset$BedTime <- NA
  dataset$BedTime[dataset$QID141 < 3] <- 6
  dataset$BedTime[dataset$QID141 == 3] <- 5.5
  dataset$BedTime[dataset$QID141 == 4] <- 5
  dataset$BedTime[dataset$QID141 == 5] <- 4.5
  dataset$BedTime[dataset$QID141 == 6] <- 4
  dataset$BedTime[dataset$QID141 == 7] <- 3.5
  dataset$BedTime[dataset$QID141 == 8] <- 3
  dataset$BedTime[dataset$QID141 == 9] <- 2.5
  dataset$BedTime[dataset$QID141 == 10] <- 2
  dataset$BedTime[dataset$QID141 == 11] <- 1.5
  dataset$BedTime[dataset$QID141 == 12] <- 1
  dataset$BedTime[dataset$QID141 == 13] <- 0.5
  dataset$BedTime[dataset$QID141 == 14] <- 0
  dataset$BedTime[dataset$QID141 == 15] <- -0.5
  dataset$BedTime[dataset$QID141 > 15] <- -1
  dataset$WakeTime <- NA
  dataset$WakeTime[dataset$QID143 < 3] <- 5
  dataset$WakeTime[dataset$QID143 == 3] <- 5.5
  dataset$WakeTime[dataset$QID143 == 4] <- 6
  dataset$WakeTime[dataset$QID143 == 5] <- 6.5
  dataset$WakeTime[dataset$QID143 == 6] <- 7
  dataset$WakeTime[dataset$QID143 == 7] <- 7.5
  dataset$WakeTime[dataset$QID143 == 8] <- 8
  dataset$WakeTime[dataset$QID143 == 9] <- 8.5
  dataset$WakeTime[dataset$QID143 == 10] <- 9
  dataset$WakeTime[dataset$QID143 == 11] <- 9.5
  dataset$WakeTime[dataset$QID143 == 12] <- 10
  dataset$WakeTime[dataset$QID143 == 13] <- 10.5
  dataset$WakeTime[dataset$QID143 > 13] <- 11
  dataset$Sleep_Efficiency_Num <- round((dataset$HoursSlept / (dataset$BedTime + dataset$WakeTime)) * 100, 2)
  dataset$Sleep_Efficiency[dataset$Sleep_Efficiency_Num >= 85] <- 0
  dataset$Sleep_Efficiency[dataset$Sleep_Efficiency_Num >= 75 &
                                 dataset$Sleep_Efficiency_Num <= 84] <- 1
  dataset$Sleep_Efficiency[dataset$Sleep_Efficiency_Num >= 65 &
                                 dataset$Sleep_Efficiency_Num <= 74] <- 2
  dataset$Sleep_Efficiency[dataset$Sleep_Efficiency_Num < 65] <- 3

  dataset$Sleep_Disturbance <- NA
  dataset$Sleep_Disturbance[rowSums(dataset[c("QID95.2", "QID95.3", "QID95.4", "QID95.5", "QID95.6",
                                                      "QID95.7", "QID95.8", "QID95.9", "QID95.10")]) == 0] <- 0
  dataset$Sleep_Disturbance[between(rowSums(dataset[c("QID95.2", "QID95.3", "QID95.4", "QID95.5", "QID95.6",
                                                              "QID95.7", "QID95.8", "QID95.9", "QID95.10")]), 1, 9)] <- 1
  dataset$Sleep_Disturbance[between(rowSums(dataset[c("QID95.2", "QID95.3", "QID95.4", "QID95.5", "QID95.6",
                                                              "QID95.7", "QID95.8", "QID95.9", "QID95.10")]), 10, 18)] <- 2
  dataset$Sleep_Disturbance[between(rowSums(dataset[c("QID95.2", "QID95.3", "QID95.4", "QID95.5", "QID95.6",
                                                              "QID95.7", "QID95.8", "QID95.9", "QID95.10")]), 19, 27)] <- 3

  dataset$Sleep_Medication <- dataset$QID97 - 1

  dataset$Daytime_Fatigue <- (dataset$QID98 - 1) + (dataset$QID145 - 1)

  dataset$PSQI_Total <- rowSums(dataset[c("Sleep_Quality", "Sleep_Latency", "Sleep_Duration",
                                                  "Sleep_Efficiency", "Sleep_Disturbance", "Sleep_Medication",
                                                  "Daytime_Fatigue")])

  #Requests
  #Number of data points
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(PSQI_Total) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(PSQI_Total) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_PSQI"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(PSQI_Total) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {
    dataset <- dataset %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "PSQI") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime,
                    Sleep_Quality, Sleep_Latency, Sleep_Duration, Sleep_Efficiency,
                    Sleep_Disturbance, Sleep_Medication, Daytime_Fatigue, PSQI_Total) %>%
      rename(DaysAfterBaseline.Sleep = DaysAfterBaseline,
             StatusDateTime.Sleep = StatusDateTime)}

  else if (Return == "ALL") {Output <- dataset %>%
    select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID141,
                  QID142, QID143, QID94, QID95.1, QID95.2, QID95.3, QID95.4, QID95.5, QID95.6,
                  QID95.7, QID95.8, QID95.9, QID95.10, QID95.10.TEXT, QID96, QID97, QID98,
                  QID145, QID146, QID101.1, QID101.2, QID101.3, QID101.4, QID101.5, QID101.5.TEXT,
                  QID196, QID197, Sleep_Quality, Sleep_Latency, Sleep_Duration, Sleep_Efficiency,
                  Sleep_Disturbance, Sleep_Medication, Daytime_Fatigue, PSQI_Total) %>%
    rename(DaysAfterBaseline.Sleep = DaysAfterBaseline,
           StatusDateTime.Sleep = StatusDateTime)}

  data.frame(Output)

}

