#' @title \strong{BHR Mood Module}
#'
#' @description This function cleanses data from the Brain Health Registry Mood Module. The user is asked to input a raw dataframe from the BHR Mood Module and to specify to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and to calculate total scores on the Geriatric Depression Scale (GDS) and 9-item Patient Health Questionnaire (PHQ-9).
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Mood Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with completed PHQ-9 and GDS assessments.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with completed PHQ-9 and GDS assessments
#' (3) "ALL_MOOD": For each participant, return all time points with completed PHQ-9 and GDS assessments
#' (4) "ALL": For each participant, return all time points with mood data, regardless of whether the PHQ-9 and GDS assessments are complete (i.e., data may be missing for some variables)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "PHQ9": Output dataframe includes PHQ-9 single items and summary score only
#' (2) "GDS": Output dataframe includes GDS single items and summary score only
#' (3) "ALL": Output dataframe includes all assessments of mood, including the GDS and PHQ-9 assessments
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Mood:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Mood:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID56:} {See BHR Codebook}
#'  \item \code{QID57:} {See BHR Codebook}
#'  \item \code{QID58:} {See BHR Codebook}
#'  \item \code{QID59:} {See BHR Codebook}
#'  \item \code{QID60:} {See BHR Codebook}
#'  \item \code{QID61:} {See BHR Codebook}
#'  \item \code{QID62:} {See BHR Codebook}
#'  \item \code{QID63:} {See BHR Codebook}
#'  \item \code{QID64:} {See BHR Codebook}
#'  \item \code{QID65:} {See BHR Codebook}
#'  \item \code{QID66:} {See BHR Codebook}
#'  \item \code{QID67:} {See BHR Codebook}
#'  \item \code{QID68:} {See BHR Codebook}
#'  \item \code{QID69:} {See BHR Codebook}
#'  \item \code{QID70:} {See BHR Codebook}
#'  \item \code{GDS:}{Total score on the Geriatric Depression Scale. Scores range from 0 to 15, with high schores indicating more severe depressive symtoms.}
#'  \item \code{QID197.1:} {See BHR Codebook}
#'  \item \code{QID197.2:} {See BHR Codebook}
#'  \item \code{QID197.3:} {See BHR Codebook}
#'  \item \code{QID197.4:} {See BHR Codebook}
#'  \item \code{QID197.5:} {See BHR Codebook}
#'  \item \code{QID197.6:} {See BHR Codebook}
#'  \item \code{QID197.7:} {See BHR Codebook}
#'  \item \code{QID197.8:} {See BHR Codebook}
#'  \item \code{QID197.9:} {See BHR Codebook}
#'  \item \code{PHQ9:} {Total score on the 9-item Patient Health Questionnaire. Scores range from 0 to 27, with high schores indicating more severe depressive symtoms.}
#'  \item \code{QID198:} {See BHR Codebook}
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


BHR_Mood <- function(dataset, TimePoint = "ALL_MOOD", Return = "ALL"){

  #Check 1: BHR mood data has been entered into the function
  if(("QID56" %in% colnames(dataset) & "QID197.1" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Mood module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_MOOD", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
         'FIRST' = First time point with Mood data,
         'LAST' = Last time point with Mood data,
         'ALL_MOOD' = All time points with Mood data,
         'ALL' = All time points with and without Mood data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("PHQ9", "GDS", "ALL")) {
    stop("Please specify the output variables:
         'PHQ9' = 9-item Patient Health Questionnaire (PHQ-9) only,
         'GDS' = Geriatiric Depression Scale Short Form (GDS) only,
         'ALL' = All data columns ")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Summary Scores
  dataset$PHQ9 <- rowSums(dataset[c("QID197.1", "QID197.2", "QID197.3", "QID197.4", "QID197.5",
                                    "QID197.6", "QID197.7", "QID197.8", "QID197.9")])

  dataset$GDS <- rowSums(dataset[c("QID56", "QID57", "QID58", "QID59", "QID60", "QID61", "QID62",
                                   "QID63", "QID64", "QID65", "QID66", 'QID67', 'QID68', 'QID69', 'QID70')])

  #Requests
  #Number of data points and Output variables
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(GDS, PHQ9) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(GDS, PHQ9) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_MOOD"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(GDS, PHQ9) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {dataset <- dataset %>%
    arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "PHQ9") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID197.1,
                    QID197.2, QID197.3, QID197.4, QID197.5, QID197.6, QID197.7, QID197.8,
                    QID197.9, PHQ9, QID198)%>%
      rename(DaysAfterBaseline.Mood = DaysAfterBaseline,
             StatusDateTime.Mood = StatusDateTime)}

  else if (Return == "GDS") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID56,
                    QID57, QID58, QID59, QID60, QID61, QID62, QID63, QID64, QID65, QID66,
                    QID67, QID68, QID69, QID70, GDS)%>%
      rename(DaysAfterBaseline.Mood = DaysAfterBaseline,
             StatusDateTime.Mood = StatusDateTime)}

  else {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID56,
                    QID57, QID58, QID59, QID60, QID61, QID62, QID63, QID64, QID65, QID66, QID67, QID68,
                    QID69, QID70, GDS, QID197.1, QID197.2, QID197.3, QID197.4, QID197.5, QID197.6,
                    QID197.7, QID197.8, QID197.9, PHQ9, QID198) %>%
      rename(DaysAfterBaseline.Mood = DaysAfterBaseline,
             StatusDateTime.Mood = StatusDateTime)}

  data.frame(Output)

}
