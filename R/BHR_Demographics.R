#' @title \strong{BHR Demograhics Module}
#'
#' @description This function cleanses data from the Brain Health Registry Demographics Module. The user is asked to input a raw dataframe from the BHR Demographics Module and to specify the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function  performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from  the previous time point) and to calculate participant body mass index (BMI).
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Demographics Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with complete BMI information.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a complete BMI information.
#' (3) "ALL_DEMO": For each participant, return all time points with complete BMI information.
#' (4) "ALL": For each participant, return all time points with demographics data, regardless of whether the demographics assessment is complete (i.e., data may be missing for some variables)
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Demo:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Demo:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID177:} {See BHR Codebook}
#'  \item \code{QID178:} {See BHR Codebook}
#'  \item \code{BMI_6cat:} {Body Mass Index (6 categories). Coded as 1=Underweight, 2=Normal Weight, 3=Overweight, 4=Obeseity Class I, 5=Obesity Class II, 6=Obesity Class III}
#'  \item \code{BMI_3cat:} {Body Mass Index (3 categories). Coded as 1=Underweight/Normal Weight, 3=Overweight, 4=Obeseity}
#'  \item \code{QID180:} {See BHR Codebook}
#'  \item \code{QID189:} {See BHR Codebook}
#'  \item \code{QID182:} {See BHR Codebook}
#'  \item \code{QID183.TEXT:} {See BHR Codebook}
#'  \item \code{QID185:} {See BHR Codebook}
#'  \item \code{QID186:} {Year of retirement, Recoded with Not applicable (i.e., not retired)=99}
#'  \item \code{QID192:} {See BHR Codebook}
#'  \item \code{QID193:} {Please indicate which branch of the Armed Forces, Recoded with Not applicable (i.e., not a veteran of the Armed Forces)=99}
#'  \item \code{QID184:} {See BHR Codebook}
#'  \item \code{QID197:} {See BHR Codebook (Added to Demographics Module in February 2021)}
#'  \item \code{QID199:} {See BHR Codebook (Added to Demographics Module in February 2021)}
#'  \item \code{QID200:} {See BHR Codebook (Added to Demographics Module in February 2021)}
#'  \item \code{QID201:} {See BHR Codebook (Added to Demographics Module in February 2021)}
#' }
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


BHR_Demographics <- function(dataset, TimePoint = "ALL_DEMO"){

  #Check 1: BHR demographics data has been entered into the function
  if(("QID177" %in% colnames(dataset) & "QID178" %in% colnames(dataset) & "QID180" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Demographics module")}

  #Check 2: An appropriate number of timepoints has been specified
  if (!TimePoint %in% c("ALL_DEMO", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
          'FIRST' = First time point with demographics data,
          'LAST' = Last time point with demographics data,
          'ALL_DEMO' = All time points with demographics data,
          'ALL' = All time points with and without demographics data")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Calculate BMI
  dataset$BMI_6cat <- NA
  dataset$BMI_6cat[!is.na(dataset$QID178) & !is.na(dataset$QID177)] <- 99 # Unable to calculate
  #4'8"
  dataset$BMI_6cat[dataset$QID178 == 9 & between(dataset$QID177, 0, 2)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 9 & between(dataset$QID177, 3, 4)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 9 & between(dataset$QID177, 5, 6)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 9 & between(dataset$QID177, 7, 8)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 9 & 8 < dataset$QID177] <- 6
  #4'9"
  dataset$BMI_6cat[dataset$QID178 == 10 & between(dataset$QID177, 0, 2)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 10 & between(dataset$QID177, 3, 4)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 10 & between(dataset$QID177, 5, 7)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 10 & between(dataset$QID177, 8, 9)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 10 & 9 < dataset$QID177] <- 5
  #4'10"
  dataset$BMI_6cat[dataset$QID178 == 11 & between(dataset$QID177, 0, 2)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 11 & between(dataset$QID177, 3, 5)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 11 & between(dataset$QID177, 6, 7)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 11 & between(dataset$QID177, 8, 10)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 11 & 10 < dataset$QID177] <- 6
  #4'11"
  dataset$BMI_6cat[dataset$QID178 == 12 & between(dataset$QID177, 0, 3)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 12 & between(dataset$QID177, 4, 5)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 12 & between(dataset$QID177, 6, 8)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 12 & between(dataset$QID177, 9, 10)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 12 & 10 < dataset$QID177] <- 6
  #5'0"
  dataset$BMI_6cat[dataset$QID178 == 13 & between(dataset$QID177, 0, 3)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 13 & between(dataset$QID177, 4, 6)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 13 & between(dataset$QID177, 7, 8)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 13 & between(dataset$QID177, 9, 10)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 13 & 11 < dataset$QID177] <- 6
  #5'1"
  dataset$BMI_6cat[dataset$QID178 == 14 & between(dataset$QID177, 0, 4)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 14 & between(dataset$QID177, 5, 6)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 14 & between(dataset$QID177, 7, 9)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 14 & between(dataset$QID177, 10, 12)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 14 & 12 < dataset$QID177] <- 6
  #5'2"
  dataset$BMI_6cat[dataset$QID178 == 15 & between(dataset$QID177, 0, 4)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 15 & between(dataset$QID177, 5, 7)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 15 & between(dataset$QID177, 8, 10)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 15 & between(dataset$QID177, 11, 12)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 15 & 12 < dataset$QID177] <- 6
  #5'3"
  dataset$BMI_6cat[dataset$QID178 == 16 & between(dataset$QID177, 0, 5)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 16 & between(dataset$QID177, 6, 7)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 16 & between(dataset$QID177, 8, 10)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 16 & between(dataset$QID177, 11, 13)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 16 & 13 < dataset$QID177] <- 6
  #5'4"
  dataset$BMI_6cat[dataset$QID178 == 17 & between(dataset$QID177, 0, 5)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 17 & between(dataset$QID177, 6, 8)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 17 & between(dataset$QID177, 9, 11)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 17 & between(dataset$QID177, 12, 14)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 17 & 14 < dataset$QID177] <- 6
  #5'5"
  dataset$BMI_6cat[dataset$QID178 == 18 & between(dataset$QID177, 0, 6)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 18 & between(dataset$QID177, 7, 9)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 18 & between(dataset$QID177, 10, 12)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 18 & between(dataset$QID177, 13, 15)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 18 & 15 < dataset$QID177] <- 6
  #5'6"
  dataset$BMI_6cat[dataset$QID178 == 19 & between(dataset$QID177, 0, 6)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 19 & between(dataset$QID177, 7, 9)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 19 & between(dataset$QID177, 10, 12)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 19 & between(dataset$QID177, 13, 15)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 19 & 15 < dataset$QID177] <- 6
  #5'7"
  dataset$BMI_6cat[dataset$QID178 == 20 & between(dataset$QID177, 0, 6)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 20 & between(dataset$QID177, 7, 10)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 20 & between(dataset$QID177, 11, 13)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 20 & between(dataset$QID177, 14, 16)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 20 & 16 < dataset$QID177] <- 6
  #5'8"
  dataset$BMI_6cat[dataset$QID178 == 21 & between(dataset$QID177, 0, 2)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 21 & between(dataset$QID177, 3, 7)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 21 & between(dataset$QID177, 8, 10)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 21 & between(dataset$QID177, 11, 13)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 21 & between(dataset$QID177, 14, 17)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 21 & 17 < dataset$QID177] <- 6
  #5'9"
  dataset$BMI_6cat[dataset$QID178 == 22 & between(dataset$QID177, 0, 2)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 22 & between(dataset$QID177, 2, 7)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 22 & between(dataset$QID177, 8, 11)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 22 & between(dataset$QID177, 12, 14)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 22 & between(dataset$QID177, 15, 18)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 22 & 18 < dataset$QID177] <- 6
  #5'10"
  dataset$BMI_6cat[dataset$QID178 == 23 & between(dataset$QID177, 0, 2)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 23 & between(dataset$QID177, 3, 8)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 23 & between(dataset$QID177, 9, 11)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 23 & between(dataset$QID177, 12, 15)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 23 & between(dataset$QID177, 16, 18)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 23 & 18 < dataset$QID177] <- 6
  #5'11"
  dataset$BMI_6cat[dataset$QID178 == 24 & between(dataset$QID177, 0, 3)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 24 & between(dataset$QID177, 4, 8)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 24 & between(dataset$QID177, 9, 12)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 24 & between(dataset$QID177, 13, 16)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 24 & between(dataset$QID177, 17, 19)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 24 & 19 < dataset$QID177] <- 6
  #6'0"
  dataset$BMI_6cat[dataset$QID178 == 25 & between(dataset$QID177, 0, 3)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 25 & between(dataset$QID177, 4, 9)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 25 & between(dataset$QID177, 10, 13)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 25 & between(dataset$QID177, 14, 16)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 25 & between(dataset$QID177, 17, 20)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 25 & 20 < dataset$QID177] <- 6
  #6'1"
  dataset$BMI_6cat[dataset$QID178 == 26 & between(dataset$QID177, 0, 4)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 26 & between(dataset$QID177, 5, 9)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 26 & between(dataset$QID177, 10, 13)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 26 & between(dataset$QID177, 14, 17)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 26 & between(dataset$QID177, 18, 21)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 26 & 21 < dataset$QID177] <- 6
  #6'2"
  dataset$BMI_6cat[dataset$QID178 == 27 & between(dataset$QID177, 0, 4)] <- 1
  dataset$BMI_6cat[dataset$QID178 == 27 & between(dataset$QID177, 5, 10)] <- 2
  dataset$BMI_6cat[dataset$QID178 == 27 & between(dataset$QID177, 11, 14)] <- 3
  dataset$BMI_6cat[dataset$QID178 == 27 & between(dataset$QID177, 15, 18)] <- 4
  dataset$BMI_6cat[dataset$QID178 == 27 & between(dataset$QID177, 10, 21)] <- 5
  dataset$BMI_6cat[dataset$QID178 == 27 & 21 < dataset$QID177] <- 6

  #Calculate BMI (3 categories)
  dataset$BMI_3cat <- NA
  dataset$BMI_3cat[between(dataset$BMI_6cat, 1, 2)] <- 0
  dataset$BMI_3cat[dataset$BMI_6cat == 3] <- 1
  dataset$BMI_3cat[between(dataset$BMI_6cat, 4, 6)] <- 2
  dataset$BMI_3cat[dataset$BMI_6cat == 99] <- 99

  #Recode conditional variables
  dataset$QID186[dataset$QID185 == 2] <- 99
  dataset$QID193[dataset$QID192 == 2] <- 99

  #Requests
  #Number of data points and Output variables
  if (TimePoint == "FIRST"){
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID177, QID178,
                    BMI_6cat, BMI_3cat, QID180, QID189, QID182, QID183.TEXT, QID185, QID186, QID192,
                    QID193, QID184, QID197, QID199, QID200, QID201) %>%
      group_by(SubjectCode) %>%
      drop_na(QID177, QID178) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE) %>%
      rename(DaysAfterBaseline.Demo = DaysAfterBaseline,
             StatusDateTime.Demo = StatusDateTime)}

  else if (TimePoint == "LAST"){
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID177, QID178,
                    BMI_6cat, BMI_3cat, QID180, QID189, QID182, QID183.TEXT, QID185, QID186, QID192,
                    QID193, QID184, QID197, QID199, QID200, QID201) %>%
      group_by(SubjectCode) %>%
      drop_na(QID177, QID178) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE) %>%
      rename(DaysAfterBaseline.Demo = DaysAfterBaseline,
             StatusDateTime.Demo = StatusDateTime)}

  else if (TimePoint == "ALL_DEMO"){
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID177, QID178,
                    BMI_6cat, BMI_3cat, QID180, QID189, QID182, QID183.TEXT, QID185, QID186, QID192,
                    QID193, QID184, QID197, QID199, QID200, QID201) %>%
      group_by(SubjectCode) %>%
      drop_na(QID177, QID178) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      rename(DaysAfterBaseline.Demo = DaysAfterBaseline,
             StatusDateTime.Demo = StatusDateTime)}

  else {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID177, QID178,
                    BMI_6cat, BMI_3cat, QID180, QID189, QID182, QID183.TEXT, QID185, QID186, QID192,
                    QID193, QID184, QID197, QID199, QID200, QID201) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      rename(DaysAfterBaseline.Demo = DaysAfterBaseline,
             StatusDateTime.Demo = StatusDateTime)}

  data.frame(Output)

}
