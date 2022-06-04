#' @title \strong{BHR CogState Module}
#'
#' @description This function cleanses data from the Brain Health Registry CogState Module. The user is asked to input four raw dataframes from the BHR CogState Module. Dataframes include (1) CogState DET (i.e., the detection test), (2) CogState IDN (i.e., the identification test), (3) CogState OCL (i.e., the one card learning test), and (4) CogState ONB (i.e., the one back test). Additionally, the user is asked to specify to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to merge the four dataframes, remove duplicate observations (including obsertvations less than 30 days from the previous time point) and assess the validity of each observation/assessment using pre-coded integrity checks.
#'
#' @param DET Raw dataframe extracted from the detection (DET) task in the Brain Health Registry CogState Module
#' @param IDN Raw dataframe extracted from the identification (IDN) task in the Brain Health Registry CogState Module
#' @param OCL Raw dataframe extracted from the one card learning (OCL) task in the Brain Health Registry CogState Module
#' @param ONB Raw dataframe extracted from the one back (ONB) task in the Brain Health Registry CogState Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a complete CogState assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a complete CogState assessment.
#' (3) "ALL_COG": For each participant, return all time points with a complete CogState assessment.
#' (4) "ALL": For each participant, return all time points with CogState data, regardless of whether the assessment is complete (i.e., data may be missing for some variables)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "SPEED_ACC": Output dataframe includes measures of accuracy and speed for all 4 CogState tasks
#' (2) "ALL": Output dataframe includes all CogState measures
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.CogState:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.CogState:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{Valid_Test:} {Identifies valid tests passing the BHR integrity check in each CogState domain. Coded as Invalid=0, Valid=1}
#'  \item \code{dur.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{lmn.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{lsd.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{acc.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{cor.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{err.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{presnt.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{sti.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{WasInterrupted.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in September 2014)}
#'  \item \code{TestingEnvironment.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in September 2014)}
#'  \item \code{SelfAssessment.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{GoodMeasure.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{Experience.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{InstructionsClear.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{ExplanationHelpful.DET:} {See BHR Codebook (Detection Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{IntegrityCheck.DET:} {See BHR Codebook (Detection Task)}
#'  \item \code{dur.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{lmn.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{lsd.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{acc.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{cor.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{err.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{presnt.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{sti.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{WasInterrupted.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in September 2014)}
#'  \item \code{TestingEnvironment.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in September 2014)}
#'  \item \code{SelfAssessment.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{GoodMeasure.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{Experience.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{InstructionsClear.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{ExplanationHelpful.IDN:} {See BHR Codebook (Identification Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{IntegrityCheck.IDN:} {See BHR Codebook (Identification Task)}
#'  \item \code{dur.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{lmn.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{lsd.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{acc.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{cor.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{err.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{presnt.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{sti.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{WasInterrupted.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in September 2014)}
#'  \item \code{TestingEnvironment.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in September 2014)}
#'  \item \code{SelfAssessment.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{GoodMeasure.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{Experience.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{InstructionsClear.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{ExplanationHelpful.OCL:} {See BHR Codebook (One Card Learning Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{IntegrityCheck.OCL:} {See BHR Codebook (One Card Learning Task)}
#'  \item \code{dur.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{lmn.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{lsd.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{acc.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{cor.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{err.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{presnt.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{sti.ONB:} {See BHR Codebook (One Back Task)}
#'  \item \code{WasInterrupted.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in September 2014)}
#'  \item \code{TestingEnvironment.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in September 2014)}
#'  \item \code{SelfAssessment.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{GoodMeasure.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{Experience.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{InstructionsClear.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{ExplanationHelpful.ONB:} {See BHR Codebook (One Back Task; Added to CogState Module in Novemver 2016)}
#'  \item \code{IntegrityCheck.ONB:} {See BHR Codebook (One Back Task)}
#' }
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#'
#' @export

BHR_CogState <- function(DET, IDN, OCL, ONB, TimePoint = "ALL_COG", Return = "SPEED_ACC"){

  #Check 1: BHR CogState data has been entered into the function
  if(("dur" %in% colnames(DET) & "lmn" %in% colnames(DET)) == FALSE){
    stop("Please enter a dataset from the BHR CogState Detection Test module")}

  if(("dur" %in% colnames(IDN) & "lmn" %in% colnames(IDN)) == FALSE){
    stop("Please enter a dataset from the BHR CogState Identification Test module")}

  if(("dur" %in% colnames(OCL) & "lmn" %in% colnames(OCL)) == FALSE){
    stop("Please enter a dataset from the BHR CogState One Card Learning Test module")}

  if(("dur" %in% colnames(ONB) & "lmn" %in% colnames(ONB)) == FALSE){
    stop("Please enter a dataset from the BHR CogState One Back Test module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_COG", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
         'FIRST' = First time point with CogState data,
         'LAST' = Last time point with CogState data,
         'ALL_COG' = All time points with CogState data,
         'ALL' = All time points with and without CogState data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("SPEED_ACC", "ALL")) {
    stop("Please specify the output variables:
         'SPEED_ACC' = Speed and accuracy variables only,
         'ALL' = All data columns ")}

  #General data cleaning (Add indicator for each dataset)
  DET <- DET %>% rename_with(~paste0(., ".DET"), IntegrityCheck:ExplanationHelpful) %>%
    select(-(Latest:Status), -Result.DET)

  IDN <- IDN %>% rename_with(~paste0(., ".IDN"), dur:IntegrityCheck) %>%
    select(-StatusDateTime, -TimepointCode, -(Latest:Result))

  OCL <- OCL %>% rename_with(~paste0(., ".OCL"), IntegrityCheck:ExplanationHelpful) %>%
    select(-StatusDateTime, -TimepointCode, -(Latest:Status), -Result.OCL)

  ONB <- ONB %>% rename_with(~paste0(., ".ONB"), dur:IntegrityCheck) %>%
    select(-StatusDateTime, -TimepointCode, -(Latest:Status))

  #Merge to one cogstate dataset
  dataset <-  left_join(DET, IDN, by=c("SubjectCode", "DaysAfterBaseline")) %>%
    left_join(., OCL, by=c("SubjectCode", "DaysAfterBaseline")) %>%
    left_join(., ONB, by=c("SubjectCode", "DaysAfterBaseline")) %>%

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Make variable for detecting valid data (i.e., more than 50 images, 5 < #Responses < 45, 0.3 < reaction time < 2.3)
  dataset$Valid_Test <- NA
  dataset$Valid_Test[dataset$IntegrityCheck.DET == 0 | dataset$IntegrityCheck.IDN == 0 |
                       dataset$IntegrityCheck.OCL == 0 | dataset$IntegrityCheck.ONB == 0] <- 0
  dataset$Valid_Test[dataset$IntegrityCheck.DET == 1 & dataset$IntegrityCheck.IDN == 1 &
                       dataset$IntegrityCheck.OCL == 1 & dataset$IntegrityCheck.ONB == 1] <- 1

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

  else if (TimePoint == "ALL_COG"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      filter(Valid_Test == 1) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {Output <- dataset %>%
    arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "SPEED_ACC") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, Valid_Test, lmn.DET,
             lsd.DET, acc.DET, lmn.IDN, lsd.IDN, acc.IDN, lmn.OCL, lsd.OCL, acc.OCL, lmn.ONB, lsd.ONB, acc.ONB) %>%
      rename(DaysAfterBaseline.CogState = DaysAfterBaseline,
             StatusDateTime.CogState = StatusDateTime)}

  else if (Return == "ALL") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, Valid_Test, dur.DET,
             lmn.DET, lsd.DET,  acc.DET, cor.DET, err.DET, presnt.DET, sti.DET, WasInterrupted.DET,
             TestingEnvironment.DET, SelfAssessment.DET, GoodMeasure.DET, Experience.DET, InstructionsClear.DET,
             ExplanationHelpful.DET, IntegrityCheck.DET, dur.IDN, lmn.IDN, lsd.IDN, acc.IDN, cor.IDN, err.IDN,
             presnt.IDN, sti.IDN, WasInterrupted.IDN, TestingEnvironment.IDN, SelfAssessment.IDN, GoodMeasure.IDN,
             Experience.IDN, InstructionsClear.IDN, ExplanationHelpful.IDN, IntegrityCheck.IDN, dur.OCL, lmn.OCL,
             lsd.OCL, acc.OCL, cor.OCL, err.OCL, presnt.OCL, sti.OCL, WasInterrupted.OCL, TestingEnvironment.OCL,
             SelfAssessment.OCL, GoodMeasure.OCL, Experience.OCL, InstructionsClear.OCL, ExplanationHelpful.OCL,
             IntegrityCheck.OCL, dur.ONB, lmn.ONB, lsd.ONB, acc.ONB, cor.ONB, err.ONB, presnt.ONB, sti.ONB,
             WasInterrupted.ONB, TestingEnvironment.ONB, SelfAssessment.ONB, GoodMeasure.ONB, Experience.ONB,
             InstructionsClear.ONB,  ExplanationHelpful.ONB, IntegrityCheck.ONB) %>%
      rename(DaysAfterBaseline.CogState = DaysAfterBaseline,
             StatusDateTime.CogState = StatusDateTime)}

  data.frame(Output)

}
