#' @title \strong{BHR Everyday Cognition Module}
#'
#' @description This function cleanses data from the Brain Health Registry Everyday Cognition (ECog) Module. The user is asked to input a raw dataframe from the BHR ECog Module and to specify to specify the desired output time point(s) from which data was collected from each participant  (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and recode single items measures of cognitive performance. Standard scoring algorithms are used to calculate total scores on ECog subscales, as well as a ECog total/global score.
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry ECog Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a completed ECog assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a completed ECog assessment.
#' (3) "ALL_ECOG": For each participant, return all time points with a completed ECog assessment
#' (4) "ALL": For each participant, return all time points with ECog data, regardless of whether the ECog assessment is complete (i.e., data may be missing for some variables)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "SummaryScores": Output dataframe includes six domain-specific ECog scores (e.g., Memory, Language, etc.), as well as the total ECog score (mean)
#' (2) "ALL": Output dataframe includes all single-item and summary ECog measures.
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.ECog:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.ECog:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID48} {See BHR Codebook}
#'  \item \code{QID49.1} {MEMORY: Remembering a few shopping items without a list. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.2} {MEMORY: Remembering things that happened recently. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.3} {MEMORY: Recalling conversations a few days later. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.4} {MEMORY: Remembering where I have placed objects. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.5} {MEMORY: Repeating stories and/or questions. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.6} {MEMORY: Remembering the current date or day of the week. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.7} {MEMORY: Remembering I have already told someone something. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID49.8} {MEMORY: Remembering appointments, meetings, or engagements. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.1} {LANGUAGE: Forgetting the names of objects. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.2} {LANGUAGE: Verbally giving instructions to others. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.3} {LANGUAGE: Finding the right words to use in a conversation. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.4} {LANGUAGE: Communicating thoughts in a conversation. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.5} {LANGUAGE: Following a story in a book or on TV. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.6} {LANGUAGE: Understanding the point of what other people are trying to say. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.7} {LANGUAGE: Remembering the meaning of common words. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.8} {LANGUAGE: Describing a program I have watched on TV. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID50.9} {LANGUAGE: Understanding spoken directions or instructions. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.1} {VISUAL: Following a map to find a new location. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.2} {VISUAL: Reading a map and helping with directions when someone else is driving. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.3} {VISUAL: Finding my car in a parking lot. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.4} {VISUAL: Finding my way back to a meeting spot in the mall or other location. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consist Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.6} {VISUAL:  Finding my way around a familiar store. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID51.7} {VISUAL: Finding my way around a house visited many times. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID52.1} {PLANNING: Planning a sequence of stops on a shopping trip. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID52.2} {PLANNING: The ability to anticipate weather changes and plan accordingly. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID52.3} {PLANNING: Developing a schedule in advance of anticipated events. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID52.4} {PLANNING: Thinking things through before acting. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID52.5} {PLANNING: Thinking ahead. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.1} {ORGANIZATION: Keeping living and work space organized. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.2} {ORGANIZATION: Balancing the checkbook without error. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.3} {ORGANIZATION: Keeping financial records organized. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.4} {ORGANIZATION: Prioritizing tasks by importance. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.5} {ORGANIZATION: Keeping mail and papers organized. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID53.6} {ORGANIZATION: Using an organized strategy to manage a medication schedule. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID54.1} {ATTENTION: The ability to do two things at once. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID54.2} {ATTENTION: Returning to a task after being interrupted. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID54.3} {ATTENTION: The ability to concentrate on a task without being distracted. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'  \item \code{QID54.4} {ATTENTION: Cooking or working and talking at the same time. Recoded as: Better or No Change=1, Questionable/Occasionally Worse=2, Consistently a Little Worse=3, Consistently Much Worse = 4, I don't know=NA}
#'}
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom tidyr drop_na
#'
#' @export

BHR_ECog <- function(dataset, TimePoint = "ALL_ECOG", Return = "SummaryScores"){

  #Check 1: BHR ECog data has been entered into the function
  if(("QID49.1" %in% colnames(dataset) & "QID50.1" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Everyday Cognition module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_ECOG", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
         'FIRST' = First time point with Everyday Cognition data,
         'LAST' = Last time point with Everyday Cognition data,
         'ALL_ECOG' = All time points with Everyday Cognition data,
         'ALL' = All time points with and without Everday Cognition data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("SummaryScores", "ALL")) {
    stop("Please specify the output variables:
         'SummaryScores' = Component and summary scores only,
         'ALL' = All data columns ")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30) %>%
    select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, starts_with("QID"), -QID48)

  #Clean ECog Data
  #Recode ECog values: 5 "Dont Know" is recoded as NA and 8 is recoded as "better or no change" (1)
  dataset[, grepl("QID", names(dataset))] <- lapply(dataset[, grepl("QID", names(dataset))],
                                                    function(x){x = ifelse(x == 8, 1,
                                                                       ifelse(1 <= x & x<= 4, x, NA))})

  #Score E-Cog subsections using mean score method
  dataset$Memory <- rowMeans(dataset[, grepl("QID49", colnames(dataset))], na.rm = TRUE)
    dataset$Memory[is.nan(dataset$Memory)] <- NA
  dataset$Language <- rowMeans(dataset[, grepl("QID50", colnames(dataset))], na.rm = TRUE)
    dataset$Language[is.nan(dataset$Language)] <- NA
  dataset$Visual <- rowMeans(dataset[, grepl("QID51", colnames(dataset))], na.rm = TRUE)
    dataset$Visual[is.nan(dataset$Visual)] <- NA
  dataset$Planning <- rowMeans(dataset[, grepl("QID52", colnames(dataset))], na.rm = TRUE)
    dataset$Planning[is.nan(dataset$Planning)] <- NA
  dataset$Organize <- rowMeans(dataset[, grepl("QID53", colnames(dataset))], na.rm = TRUE)
    dataset$Organize[is.nan(dataset$Organize)] <- NA
  dataset$Attention <- rowMeans(dataset[, grepl("QID54", colnames(dataset))], na.rm = TRUE)
    dataset$Attention[is.nan(dataset$Attention)] <- NA
  dataset$ECog_Total <- rowMeans(dataset[, grepl("QID", colnames(dataset))], na.rm = TRUE)
    dataset$ECog_Total[is.nan(dataset$ECog_Total)] <- NA

  #Requests
  #Number of data points and Output variables
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Memory, Language, Visual, Planning, Organize, Attention) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Memory, Language, Visual, Planning, Organize, Attention) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_ECOG"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Memory, Language, Visual, Planning, Organize, Attention) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {dataset <- dataset %>%
    arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "SummaryScores") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime,
             Memory, Language, Visual, Planning, Organize, Attention, ECog_Total) %>%
      rename(DaysAfterBaseline.ECog = DaysAfterBaseline,
             StatusDateTime.ECog = StatusDateTime)}
  else if (Return == "ALL") {
    Output <- dataset %>%
      rename(DaysAfterBaseline.ECog = DaysAfterBaseline,
             StatusDateTime.ECog = StatusDateTime)}

  data.frame(Output)

}
