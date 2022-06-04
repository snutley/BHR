#' @title \strong{BHR Memtrax Module}
#'
#' @description This function cleanses data from the Brain Health Registry Memtrax Module. The user is asked to input a raw dataframe from the BHR Memtrax Module and to specify to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and assess the validity of each observation/assessment using standard criteria (i.e., no more than 50 total images per assessment, number of responses between 5 and 45, correct response reaction time between 0.3 second and 2.3 seconds).
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Memtrax Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a complete Memtrax assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a complete Memtrax assessment.
#' (3) "ALL_MEM": For each participant, return all time points with a complete Memtrax assessment.
#' (4) "ALL": For each participant, return all time points with Memtrax data, regardless of whether the assessment is complete (i.e., data may be missing for some variables)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "RT_ACC": Output dataframe includes only the percentage of images answered correctly and mean reaction time in seconds, when clicking on repeat images
#' (2) "ALL": Output dataframe includes all Memtrax measures
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Memtrax:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Memtrax:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{Valid_Test:} {Identifies valid tests with no more than 50 total images, the number of responses  between 5 and 45, and a correct response reaction time between 0.3 second and 2.3 seconds. Coded as Invalid=0, Valid=1}
#'  \item \code{OrderSet:} {See BHR Codebook}
#'  \item \code{TotalImages:} {See BHR Codebook}
#'  \item \code{UniqueImages:} {See BHR Codebook}
#'  \item \code{RepeatImages:} {See BHR Codebook}
#'  \item \code{CorrectN:} {See BHR Codebook}
#'  \item \code{CorrectPCT:} {See BHR Codebook}
#'  \item \code{IncorrectN:} {See BHR Codebook}
#'  \item \code{IncorrectPCT:} {See BHR Codebook}
#'  \item \code{CorrectRejectionsN:} {See BHR Codebook}
#'  \item \code{CorrectRejectionsPCT:} {See BHR Codebook}
#'  \item \code{IncorrectRejectionsN:} {See BHR Codebook}
#'  \item \code{IncorrectRejectionsPCT:} {See BHR Codebook}
#'  \item \code{TotalRejectionsN:} {See BHR Codebook}
#'  \item \code{TotalRejectionsPCT:} {See BHR Codebook}
#'  \item \code{CorrectResponsesN:} {See BHR Codebook}
#'  \item \code{CorrectResponsesPCT:} {See BHR Codebook}
#'  \item \code{CorrectResponsesRT:} {See BHR Codebook}
#'  \item \code{IncorrectResponsesN:} {See BHR Codebook}
#'  \item \code{IncorrectResponsesPCT:} {See BHR Codebook}
#'  \item \code{IncorrectResponsesRT:} {See BHR Codebook}
#'  \item \code{TotalResponsesN:} {See BHR Codebook}
#'  \item \code{TotalResponsesPCT:} {See BHR Codebook}
#'  \item \code{WasInterrupted:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{TestingEnvironment:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{SelfAssessment:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{GoodMeasure:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{Experience:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{InstructionsClear:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#'  \item \code{ExplanationHelpful:} {See BHR Codebook (Added to Memtrax Module in November 2016)}
#' }
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#'
#' @export


BHR_Memtrax <- function(dataset, TimePoint = "ALL_MEM", Return = "RT_ACC"){

  #Check 1: BHR Memtrax data has been entered into the function
  if(("TotalImages" %in% colnames(dataset) & "TotalResponsesN" %in% colnames(dataset) &
      "CorrectResponsesRT" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Memtrax module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_MEM", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
         'FIRST' = First time point with Memtrax data,
         'LAST' = Last time point with Memtrax data,
         'ALL_MEM' = All time points with Memtrax data,
         'ALL' = All time points with and without Memtrax data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("RT_ACC", "ALL")) {
    stop("Please specify the output variables:
         'RT_ACC' = Reaction time and accuracy variables only,
         'ALL' = All data columns ")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Make variable for detecting valid data (i.e., more than 50 images, 5 < #Responses < 45, 0.3 < reaction time < 2.3)
  dataset$Valid_Test <- 0

  dataset$Valid_Test[is.na(dataset$TotalImages) | is.na(dataset$TotalResponsesN) |
                       is.na(dataset$CorrectResponsesRT)] <- NA

  dataset$Valid_Test[!is.na(dataset$TotalImages) & dataset$TotalImages <= 50 &
                       dataset$TotalResponsesN >= 5 & dataset$TotalResponsesN <= 45 &
                       dataset$CorrectResponsesRT >= 0.3 & dataset$CorrectResponsesRT <= 2.3] <- 1

  #Requests
  #Number of data points and Output variables
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      filter(Valid_Test == 1) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      filter(Valid_Test == 1) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_MEM"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      filter(Valid_Test == 1) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {Output <- dataset %>%
    arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "RT_ACC") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, Valid_Test,
             CorrectPCT, CorrectResponsesRT) %>%
      rename(DaysAfterBaseline.Memtrax = DaysAfterBaseline,
             StatusDateTime.Memtrax = StatusDateTime)}

  else if (Return == "ALL") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, Valid_Test, OrderSet,
             TotalImages, UniqueImages, RepeatImages, CorrectN, CorrectPCT, IncorrectN, IncorrectPCT,
             CorrectRejectionsN, CorrectRejectionsPCT, IncorrectRejectionsN, IncorrectRejectionsPCT, TotalRejectionsN,
             TotalRejectionsPCT, CorrectResponsesN,  CorrectResponsesPCT, CorrectResponsesRT, IncorrectResponsesN,
             IncorrectResponsesPCT, IncorrectResponsesRT, TotalResponsesN, TotalResponsesPCT, WasInterrupted,
             TestingEnvironment, SelfAssessment, GoodMeasure, Experience, InstructionsClear, ExplanationHelpful) %>%
      rename(DaysAfterBaseline.Memtrax = DaysAfterBaseline,
             StatusDateTime.Memtrax = StatusDateTime)}

  data.frame(Output)

}
