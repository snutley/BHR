#' @title \strong{BHR Hoarding Module}
#'
#' @description This function cleanses data from the Brain Health Registry Hoarding Module. The user is asked to input a raw dataframe from the BHR Hoarding Module and to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point, last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and to recode single items measures of hoarding symptoms and disability on the Hoarding Rating Scale, Self-Report (HRS-SR) and the Activities of Daily Living in Hoarding (ADLH). Standard scoring algorithms are used to calculate total scores on the HRS-SR and the World Health Organization Disability Asssessment Schedule (WHODAS).
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Hoarding Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a completed HRS-SR assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a completed HRS-SR assessment.
#' (3) "ALL_HRS": For each participant, return all time points with a completed HRS-SR assessment.
#' (4) "ALL": For each participant, return all time points with hoarding data, regardless of whether the HRS-SR was completed at the given time point.
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "HRS": Output dataframe includes variables HRS-SR single-item and total scores only
#' (2) "ALL": Output dataframe includes all hoarding variables, including HRS-SR and WHODAS total scores
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Hoarding:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Hoarding:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{HRS_Clutter:} {Because of the clutter or number of possessions, how difficult is it for you to use the rooms in your home? Recoded using a scale of 0 to 8, with higher scores indicating greater severity}
#'  \item \code{HRS_Discard:} {To what extent do you have difficulty discarding (or recycling, selling, giving away) ordinary things that other people would get rid of? Recoded using a scale of 0 to 8, with higher scores indicating greater severity}
#'  \item \code{HRS_Acquire:} {To what extent do you currently have a problem with collecting free things or buying more things than you need or can use or can afford? Recoded using a scale of 0 to 8, with higher scores indicating greater severity}
#'  \item \code{HRS_Distress:} {To what extent do you experience emotional distress because of clutter, difficulty discarding or problems with buying or acquiring things? Recoded using a scale of 0 to 8, with higher scores indicating greater severity}
#'  \item \code{HRS_Impairment:} {To what extent do you experience impairment in your life (daily routine, job/school, social activities, family activities, financial difficulties) because of clutter, difficulty discarding, or problems with buying or acquiring things? Recoded using a scale of 0 to 8, with higher scores indicating greater severity}
#'  \item \code{HRS_Total:} {Total score on the Hoarding Rating Scale, Self-Report. Scores range from 0 to 40, with high schores indicating greater hoarding symtomatology. In the BHR, cutoffs of 10 and 14 have been used for identifying subsclinical and clinically relevant hoarding, respectively}
#'  \item \code{QID11.1:} {ADLH item 1: Use stove. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.2:} {ADLH item 2: Use kitchen counters. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.3:} {ADLH item 3: Eat at table. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.4:} {ADLH item 4: Use bath/shower. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.5:} {ADLH item 5: Sit in sofa/chair. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.6:} {ADLH item 6: Sleep in bed. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID11.7:} {ADLH item 7: Find important things. Scores of 6 (NA) are recoded as NA}
#'  \item \code{QID14.1:} {See BHR Codebook}
#'  \item \code{QID14.2:} {See BHR Codebook}
#'  \item \code{QID14.3:} {See BHR Codebook}
#'  \item \code{QID14.4:} {See BHR Codebook}
#'  \item \code{QID14.5:} {See BHR Codebook}
#'  \item \code{QID44.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID44.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID45.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID45.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID46.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID46.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID47.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID47.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID48.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID48.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID49.1:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{QID49.2:} {See BHR Codebook (Added to Hoarding Module in February 2019)}
#'  \item \code{WHODAS_Total:} {Total score on the World Health Organization Disability Asssessment Schedule. Scores range from 0 to 48, with high schores indicating greater disability.}
#'  \item \code{QID49.4:} {See BHR Codebook}
#'  \item \code{QID50:} {See BHR Codebook}
#'  \item \code{QID51:} {See BHR Codebook}
#'  \item \code{QID52:} {See BHR Codebook}
#'  \item \code{QID39:} {See BHR Codebook}
#'  \item \code{QID32:} {See BHR Codebook}
#'  \item \code{QID33.1:} {See BHR Codebook}
#'  \item \code{QID33.2:} {See BHR Codebook}
#'  \item \code{QID33.3:} {See BHR Codebook}
#'  \item \code{QID33.4:} {See BHR Codebook}
#'  \item \code{QID33.5:} {See BHR Codebook}
#'  \item \code{QID33.6:} {See BHR Codebook}
#'  \item \code{QID33.7:} {See BHR Codebook}
#'  \item \code{QID33.7.TEXT:} {See BHR Codebook}
#'  \item \code{QID34:} {Do you breed animals? Recoded as Yes=1, No=2, Not applicable (i.e., no pets)=99 }
#'  \item \code{QID35:} {Has anyone ever told you that you have too many pets? Recoded as Yes=1, No=2, Not applicable (i.e., no pets)=99}
#'  \item \code{QID36:} {Have you ever had difficulty in caring for your animals for financial or other reasons? Recoded as Yes=1, No=2, Not applicable (i.e., no pets)=99}
#'  \item \code{QID37:} {Has anyone ever told you that they were worried about your ability to care for your animals? Recoded as Yes=1, No=2, Not applicable (i.e., no pets)=99}
#'  \item \code{QID15.TEXT:} {See BHR Codebook}
#'  \item \code{QID16.TEXT:} {See BHR Codebook}
#'  \item \code{QID20:} {See BHR Codebook}
#'  \item \code{QID21:} {See BHR Codebook}
#'  \item \code{QID22:} {See BHR Codebook (Removed from Hoarding Module in February 2019)}
#'  \item \code{QID23:} {See BHR Codebook (Removed from Hoarding Module in February 2019)}
#'  \item \code{QID24:} {See BHR Codebook (Removed from Hoarding Module in February 2019)}
#'  \item \code{QID25:} {See BHR Codebook (Removed from Hoarding Module in February 2019)}
#'  \item \code{QID26.1:} {See BHR Codebook}
#'  \item \code{QID26.2:} {See BHR Codebook}
#'  \item \code{QID26.3:} {See BHR Codebook}
#'  \item \code{QID26.4:} {See BHR Codebook}
#'  \item \code{QID26.5:} {See BHR Codebook}
#'  \item \code{QID26.6:} {See BHR Codebook}
#'  \item \code{QID26.7:} {See BHR Codebook}
#'  \item \code{QID26.8:} {See BHR Codebook}
#'  \item \code{QID27:} {See BHR Codebook}
#'  \item \code{QID41:} {See BHR Codebook}
#'  \item \code{QID28:} {See BHR Codebook}
#'  \item \code{QID29:} {See BHR Codebook}
#'  \item \code{QID42:} {See BHR Codebook}
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

BHR_Hoarding <- function(dataset, TimePoint = "ALL_HRS", Return = "HRS"){

  #Check 1: BHR hoarding data has been entered into the function
  if(("QID1.1" %in% colnames(dataset) &
      "QID4.1" %in% colnames(dataset) &
      "QID6.1" %in% colnames(dataset) &
      "QID8.1" %in% colnames(dataset) &
      "QID9.1" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Hoarding module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_HRS", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
             'FIRST' = First time point with HRS-SR data,
             'LAST' = Last time point with HRS-SR data,
             'ALL_HRS' = All time points with HRS-SR data,
             'ALL' = All time points with and without HRS-SR data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("HRS", "ALL")) {
    stop("Please specify the output variables:
             'HRS' = Hoarding Rating Scale, Self-Report only,
             'ALL' = All data columns ")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Recode HRS-SR
  dataset$HRS_Clutter <- NA
      dataset$HRS_Clutter[dataset$QID1.1 == 1] <- 0
      dataset$HRS_Clutter[dataset$QID1.1 == 2] <- 2
      dataset$HRS_Clutter[dataset$QID1.1 == 3] <- 4
      dataset$HRS_Clutter[dataset$QID1.1 == 4] <- 6
      dataset$HRS_Clutter[dataset$QID1.1 == 5] <- 8
      dataset$HRS_Clutter[dataset$QID1.1 == 6] <- 1
      dataset$HRS_Clutter[dataset$QID1.1 == 7] <- 3
      dataset$HRS_Clutter[dataset$QID1.1 == 8] <- 5
      dataset$HRS_Clutter[dataset$QID1.1 == 9] <- 7
  dataset$HRS_Discard <- NA
      dataset$HRS_Discard[dataset$QID4.1 == 1] <- 0
      dataset$HRS_Discard[dataset$QID4.1 == 2] <- 2
      dataset$HRS_Discard[dataset$QID4.1 == 3] <- 4
      dataset$HRS_Discard[dataset$QID4.1 == 4] <- 6
      dataset$HRS_Discard[dataset$QID4.1 == 5] <- 8
      dataset$HRS_Discard[dataset$QID4.1 == 6] <- 1
      dataset$HRS_Discard[dataset$QID4.1 == 7] <- 3
      dataset$HRS_Discard[dataset$QID4.1 == 8] <- 5
      dataset$HRS_Discard[dataset$QID4.1 == 9] <- 7
  dataset$HRS_Acquire <- NA
        dataset$HRS_Acquire[!is.na(dataset$QID6.1) & between(dataset$QID6.1, 1, 9)] <-
        na.omit(dataset$QID6.1[between(dataset$QID6.1, 1, 9)]) - 1
  dataset$HRS_Distress <- NA
      dataset$HRS_Distress[!is.na(dataset$QID8.1) & between(dataset$QID8.1, 1, 9)] <-
        na.omit(dataset$QID8.1[between(dataset$QID8.1, 1, 9)]) - 1
  dataset$HRS_Impairment <- NA
      dataset$HRS_Impairment[!is.na(dataset$QID9.1) & between(dataset$QID9.1, 1, 9)] <-
        na.omit(dataset$QID9.1[between(dataset$QID9.1, 1, 9)]) - 1

  #Recode measures outside of bounds on ADLH-H
  dataset$QID11.1[!between(dataset$QID11.1, 1, 6)] <- NA
  dataset$QID11.2[!between(dataset$QID11.2, 1, 6)] <- NA
  dataset$QID11.3[!between(dataset$QID11.3, 1, 6)] <- NA
  dataset$QID11.4[!between(dataset$QID11.4, 1, 6)] <- NA
  dataset$QID11.5[!between(dataset$QID11.5, 1, 6)] <- NA
  dataset$QID11.6[!between(dataset$QID11.6, 1, 6)] <- NA
  dataset$QID11.7[!between(dataset$QID11.7, 1, 6)] <- NA
  dataset$QID14.1[!between(dataset$QID14.1, 1, 6)] <- NA
  dataset$QID14.2[!between(dataset$QID14.2, 1, 6)] <- NA
  dataset$QID14.3[!between(dataset$QID14.3, 1, 6)] <- NA
  dataset$QID14.4[!between(dataset$QID14.4, 1, 6)] <- NA
  dataset$QID14.5[!between(dataset$QID14.5, 1, 6)] <- NA

  #Recode measures outside of bounds on WHODAS
  dataset$QID44.1[!between(dataset$QID44.1, 1, 5)] <- NA
  dataset$QID44.2[!between(dataset$QID44.2, 1, 5)] <- NA
  dataset$QID45.1[!between(dataset$QID45.1, 1, 5)] <- NA
  dataset$QID45.2[!between(dataset$QID45.2, 1, 5)] <- NA
  dataset$QID46.1[!between(dataset$QID46.1, 1, 5)] <- NA
  dataset$QID46.2[!between(dataset$QID46.2, 1, 5)] <- NA
  dataset$QID47.1[!between(dataset$QID47.1, 1, 5)] <- NA
  dataset$QID47.2[!between(dataset$QID47.2, 1, 5)] <- NA
  dataset$QID48.1[!between(dataset$QID48.1, 1, 5)] <- NA
  dataset$QID48.2[!between(dataset$QID48.2, 1, 5)] <- NA
  dataset$QID49.1[!between(dataset$QID49.1, 1, 5)] <- NA
  dataset$QID49.2[!between(dataset$QID49.2, 1, 5)] <- NA

  # Recode Animal Hoarding variables
  dataset$QID34[dataset$QID32 < 5] <- 99
  dataset$QID35[dataset$QID32 < 5] <- 99
  dataset$QID36[dataset$QID32 < 5] <- 99
  dataset$QID37[dataset$QID32 < 5] <- 99

  #Summary Scores
  dataset$HRS_Total <- rowSums(dataset[c("HRS_Clutter", "HRS_Discard", "HRS_Acquire",
                                         "HRS_Distress", "HRS_Impairment")])

  dataset$WHODAS_Total <- rowMeans(dataset[c("QID44.1", "QID44.2", "QID45.1", "QID45.2",
                                             "QID46.1", "QID46.2", "QID47.1", "QID47.2",
                                             "QID48.1", "QID48.2", "QID49.1", "QID49.2")])

  #Requests
  #Number of data points
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(HRS_Total) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(HRS_Total) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_HRS"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(HRS_Total) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {
    dataset <- dataset %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "HRS") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime,
                    HRS_Clutter, HRS_Discard, HRS_Acquire, HRS_Distress, HRS_Impairment, HRS_Total) %>%
      rename(DaysAfterBaseline.Hoarding = DaysAfterBaseline,
             StatusDateTime.Hoarding = StatusDateTime)}

  else if (Return == "ALL") {Output <- dataset %>%
    select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, HRS_Clutter,
                  HRS_Discard, HRS_Acquire, HRS_Distress,HRS_Impairment, HRS_Total, QID11.1, QID11.2,
                  QID11.3, QID11.4, QID11.5, QID11.6, QID11.7, QID14.1, QID14.2, QID14.3, QID14.4,
                  QID14.5, QID44.1, QID44.2, QID45.1, QID45.2, QID46.1, QID46.2, QID47.1, QID47.2,
                  QID48.1, QID48.2, QID49.1, QID49.2, WHODAS_Total, QID49.4, QID50, QID51, QID52,
                  QID39, QID32,  QID33.1, QID33.2, QID33.3, QID33.4, QID33.5, QID33.6, QID33.7,
                  QID33.7.TEXT, QID34, QID35, QID36, QID37, QID15.TEXT, QID16.TEXT, QID20, QID21, QID22,
                  QID23, QID24, QID25, QID26.1, QID26.2, QID26.3, QID26.4, QID26.5, QID26.6, QID26.7,
                  QID26.8, QID27, QID41, QID28, QID29, QID42) %>%
    rename(DaysAfterBaseline.Hoarding = DaysAfterBaseline,
           StatusDateTime.Hoarding = StatusDateTime)}

  data.frame(Output)

}
