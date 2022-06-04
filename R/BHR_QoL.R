#' @title \strong{BHR Quality of Life Module}
#'
#' @description This function cleanses data from the Brain Health Registry Quality of Life (QoL) Module. The user is asked to input a raw dataframe from the BHR QoL Module and to specify to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and recodes SF-36 (Medical Outcomes Study 36-item Short Form) single item measures using a scale ranging from 0 o 100. Eight domain-specific measures and two summary scores measuring quality of life are calculated using SF-36 recommended scoring criteria. All domain-specific and summary scores are standardized using U.S. general population means and standard deviations.
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Quality of Life Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a complete SF-36 assessment.
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a complete SF-36 assessment.
#' (3) "ALL_QOL": For each participant, return all time points with a complete SF-36 assessment.
#' (4) "ALL": For each participant, return all time points with SF-36 data, regardless of whether the assessment is complete (i.e., data may be missing for some variables)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "SummaryScores": Output dataframe includes eight domain-specific QoL scores (e.g., physical functioning, role limitations due to physical health, etc.), as well as the two summary scores for mental and physical quality of life (i.e., mental and physical component summary scores)
#' (2) "SF36_Scored": Output dataframe includes eight domain-specific QoL scores, two summary scores for mental and physical QoL, and all single items measures of QoL recoded using a scale ranging from 0 to 100.
#' (2) "ALL": Output dataframe includes all scored and unscored SF-36 measures.
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.QoL:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.QoL:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID80:} {See BHR Codebook}
#'  \item \code{QID81:} {See BHR Codebook}
#'  \item \code{QID82.1:} {See BHR Codebook}
#'  \item \code{QID82.2:} {See BHR Codebook}
#'  \item \code{QID82.3:} {See BHR Codebook}
#'  \item \code{QID82.4:} {See BHR Codebook}
#'  \item \code{QID82.5:} {See BHR Codebook}
#'  \item \code{QID82.6:} {See BHR Codebook}
#'  \item \code{QID82.7:} {See BHR Codebook}
#'  \item \code{QID82.8:} {See BHR Codebook}
#'  \item \code{QID82.9:} {See BHR Codebook}
#'  \item \code{QID82.10:} {See BHR Codebook}
#'  \item \code{QID83.1:} {See BHR Codebook}
#'  \item \code{QID83.2:} {See BHR Codebook}
#'  \item \code{QID83.3:} {See BHR Codebook}
#'  \item \code{QID83.4:} {See BHR Codebook}
#'  \item \code{QID84.1:} {See BHR Codebook}
#'  \item \code{QID84.2:} {See BHR Codebook}
#'  \item \code{QID84.3:} {See BHR Codebook}
#'  \item \code{QID85:} {See BHR Codebook}
#'  \item \code{QID86:} {See BHR Codebook}
#'  \item \code{QID87:} {See BHR Codebook}
#'  \item \code{QID140.1:} {See BHR Codebook}
#'  \item \code{QID140.2:} {See BHR Codebook}
#'  \item \code{QID140.3:} {See BHR Codebook}
#'  \item \code{QID140.4:} {See BHR Codebook}
#'  \item \code{QID140.5:} {See BHR Codebook}
#'  \item \code{QID140.6:} {See BHR Codebook}
#'  \item \code{QID140.7:} {See BHR Codebook}
#'  \item \code{QID140.8:} {See BHR Codebook}
#'  \item \code{QID140.9:} {See BHR Codebook}
#'  \item \code{QID88:} {See BHR Codebook}
#'  \item \code{QID89.1:} {See BHR Codebook}
#'  \item \code{QID89.2:} {See BHR Codebook}
#'  \item \code{QID89.3:} {See BHR Codebook}
#'  \item \code{QID89.4:} {See BHR Codebook}
#'  \item \code{Q1: In general, would you say your health is. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q2: Compared to one year ago, how would you rate your health in general now? Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q3: Does your health now limit you in these activities? If so, how much?... Vigorous activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q4: Does your health now limit you in these activities? If so, how much?... Moderate activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q5: Does your health now limit you in these activities? If so, how much?... Lifting or carrying groceries. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q6: Does your health now limit you in these activities? If so, how much?... Climbing several flights of stairs. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q7: Does your health now limit you in these activities? If so, how much?... Climbing one flight of stairs. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q8: Does your health now limit you in these activities? If so, how much?... Bending, kneeling, or stooping. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q9: Does your health now limit you in these activities? If so, how much?... Walking more than a mile. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q10: Does your health now limit you in these activities? If so, how much?... Walking several blocks. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q11: Does your health now limit you in these activities? If so, how much?... Walking one block. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q12: Does your health now limit you in these activities? If so, how much?... Bathing or dressing yourself. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q13: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of your physical health?... Cut down the amount of time you spent on work or other activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q14: PDuring the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of your physical health?... Accomplished less than you would like. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q15: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of your physical health?... Were limited in the kind of work or other activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q16: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of your physical health?... Had difficulty performing the work or other activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q17: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of any emotional problems?... Cut down the amount of time you spent on work or other activities. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q18: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of any emotional problems?... Accomplished less than you would like. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q19: During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of any emotional problems?... Didn't do work or other activities as carefully as usual. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q20: During the past 4 weeks, to what extent has your physical health or emotional problems interfered with your normal social activities with family, friends, neighbors, or groups?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q21: How much bodily pain have you had during the past 4 weeks?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q22: During the past 4 weeks, how much did pain interfere with your normal work?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q23: How much of the time during the past 4 weeks...... Did you feel full of pep?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q24: How much of the time during the past 4 weeks...... Have you been a very nervous person?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q25, How much of the time during the past 4 weeks...... Have you felt so down in the dumps that nothing could cheer you up?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q26, How much of the time during the past 4 weeks...... Have you felt calm and peaceful?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q27, How much of the time during the past 4 weeks...... Did you have a lot of energy?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q28, How much of the time during the past 4 weeks...... Have you felt downhearted and blue?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q29, How much of the time during the past 4 weeks...... Did you feel worn out?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q30, How much of the time during the past 4 weeks...... Have you been a happy person?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q31, How much of the time during the past 4 weeks...... Did you feel tired?. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q32, During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your social activities? Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q33, TRUE or FALSE... I seem to get sick a little easier than other people. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q34, TRUE or FALSE... I am as healthy as anybody I know. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q35, TRUE or FALSE... I expect my health to get worse. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{Q36, TRUE or FALSE...My health is excellent. Recoded using a scale ranging from 0 to 100, where higher scores indicate better QoL}
#'  \item \code{PhysFunc:} {Physical Functioning Scale (mean of all items in the physical functioning domain)}
#'  \item \code{PhysFuncZ:} {Z-score for the Physical Functioning Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{LimitPhys:} {Role Limitations Due to Physical Health Scale (mean of all items in the role limitations domain)}
#'  \item \code{LimitPhysZ:} {Z-score for the Role Limitations Due to Physical Health scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{LimitEmo:} {Role Limitations Due to Emotional Health Scale (mean of all items in the role limitations domain)}
#'  \item \code{LimitEmoZ:} {Z-score for the Role Limitations Due to Emotional Health scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{Energy:} {Energy/Vitality Scale (mean of all items in the energy domain)}
#'  \item \code{EnergyZ:} {Z-score for the Energy Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{Emotional:} {Emotional Health Scale (mean of all items in the emotional health domain)}
#'  \item \code{EmotionalZ:} {Z-score for the Emotional Health  Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{Social:} {Social Functioning Scale (mean of all items in the social functioning domain)}
#'  \item \code{SocialZ:} {Z-score for the Social Functioning  Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{Pain:} {Pain Scale (mean of all items in the pain domain)}
#'  \item \code{PainZ:} {Z-score for the Pain  Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{Health:} {General health Scale (mean of all items in the general health domain)}
#'  \item \code{HealthZ:} {Z-score for the General Health  Scale. Determined by subtracting the U.S. general population scale mean from individuals' scale score and dividing by the U.S. population standard deviation.}
#'  \item \code{MCS_tscore:} {Mental Component Summary Score (aggregates the eight SF-36 subscales/domains using the eight z-scores and factor scoring coefficients determined by Ware et al.) MCS scores range from 0 to 100, with a mean of 50 and a standard deviation of 10 for the U.S. general population. Higher scores indicate better quality of life.}
#'  \item \code{PCS_tscore:} {Physical Component Summary Score (aggregates the eight SF-36 subscales/domains using the eight z-scores and factor scoring coefficients determined by Ware et al.) MCS scores range from 0 to 100, with a mean of 50 and a standard deviation of 10 for the U.S. general population. Higher scores indicate better quality of life.}
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
#' @importFrom tidyr drop_na
#'
#' @export

BHR_QoL <- function(dataset, TimePoint = "ALL_QOL", Return = "SummaryScores"){

  #Check 1: BHR CogState data has been entered into the function
  if(("QID81" %in% colnames(dataset) & "QID81" %in% colnames(dataset) & "QID88" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Quality of Life module")}

  #Check 2: An appropriate number of timepoints has been specified,
  if (!TimePoint %in% c("ALL_QOL", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
         'FIRST' = First time point with QoL data,
         'LAST' = Last time point with QoL data,
         'ALL_QOL' = All time points with QoL data,
         'ALL' = All time points with and without QoL data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("SummaryScores", "SF36_Scored", "ALL")) {
    stop("Please specify the output variables:
         'SummaryScores' = Component and summary scores only,
         'SF36_Scored' = All variables used to calculate the SF-36 + component and summary scores,
         'ALL' = All data columns ")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)

  #Score SF-36

  #Item Numbers 1, 2, 20, 22, 34, 36
  dataset$Q1 <- dataset$QID80
  dataset$Q2 <- dataset$QID81
  dataset$Q20 <- dataset$QID85
  dataset$Q22 <- dataset$QID87
  dataset$Q34 <- dataset$QID89.2
  dataset$Q36 <- dataset$QID89.4
  subset1 <- c("Q1", "Q2", "Q20", "Q22", "Q34", "Q36")
  dataset[subset1][dataset[subset1] == 1] <- 100
  dataset[subset1][dataset[subset1] == 2] <- 75
  dataset[subset1][dataset[subset1] == 3] <- 50
  dataset[subset1][dataset[subset1] == 4] <- 25
  dataset[subset1][dataset[subset1] == 5] <- 0

  #Item Numbers 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
  dataset$Q3 <- dataset$QID82.1
  dataset$Q4 <- dataset$QID82.2
  dataset$Q5 <- dataset$QID82.3
  dataset$Q6 <- dataset$QID82.4
  dataset$Q7 <- dataset$QID82.5
  dataset$Q8 <- dataset$QID82.6
  dataset$Q9 <- dataset$QID82.7
  dataset$Q10 <- dataset$QID82.8
  dataset$Q11 <- dataset$QID82.9
  dataset$Q12 <- dataset$QID82.10
  subset2 <- c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")
  dataset[subset2][dataset[subset2] == 1] <- 0
  dataset[subset2][dataset[subset2] == 2] <- 50
  dataset[subset2][dataset[subset2] == 3] <- 100

  #Item Numbers 13, 14, 15, 16, 17, 18, 19
  dataset$Q13 <- dataset$QID83.1
  dataset$Q14 <- dataset$QID83.2
  dataset$Q15 <- dataset$QID83.3
  dataset$Q16 <- dataset$QID83.4
  dataset$Q17 <- dataset$QID84.1
  dataset$Q18 <- dataset$QID84.2
  dataset$Q19 <- dataset$QID84.3
  subset3 <- c("Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19")
  dataset[subset3][dataset[subset3] == 1] <- 0
  dataset[subset3][dataset[subset3] == 2] <- 100

  #Item Numbers 21, 23, 26, 27, 30
  dataset$Q21 <- dataset$QID86
  dataset$Q23 <- dataset$QID140.1
  dataset$Q26 <- dataset$QID140.4
  dataset$Q27 <- dataset$QID140.5
  dataset$Q30 <- dataset$QID140.8
  subset4 <- c("Q21", "Q23", "Q26", "Q27", "Q30")
  dataset[subset4][dataset[subset4] == 1] <- 100
  dataset[subset4][dataset[subset4] == 2] <- 80
  dataset[subset4][dataset[subset4] == 3] <- 60
  dataset[subset4][dataset[subset4] == 4] <- 40
  dataset[subset4][dataset[subset4] == 5] <- 20
  dataset[subset4][dataset[subset4] == 6] <- 0

  #Item Numbers 24, 25, 28, 29, 31
  dataset$Q24 <- dataset$QID140.2
  dataset$Q25 <- dataset$QID140.3
  dataset$Q28 <- dataset$QID140.6
  dataset$Q29 <- dataset$QID140.7
  dataset$Q31 <- dataset$QID140.9
  subset5 <- c("Q24", "Q25", "Q28", "Q29", "Q31")
  dataset[subset5][dataset[subset5] == 1] <- 0
  dataset[subset5][dataset[subset5] == 2] <- 20
  dataset[subset5][dataset[subset5] == 3] <- 40
  dataset[subset5][dataset[subset5] == 4] <- 50
  dataset[subset5][dataset[subset5] == 5] <- 80
  dataset[subset5][dataset[subset5] == 6] <- 100

  #Item Numbers 32, 33, 35
  dataset$Q32 <- dataset$QID88
  dataset$Q33 <- dataset$QID89.1
  dataset$Q35 <- dataset$QID89.3
  subset6 <- c("Q32", "Q33", "Q35")
  dataset[subset6][dataset[subset6] == 1] <- 0
  dataset[subset6][dataset[subset6] == 2] <- 25
  dataset[subset6][dataset[subset6] == 3] <- 50
  dataset[subset6][dataset[subset6] == 4] <- 75
  dataset[subset6][dataset[subset6] == 5] <- 100

  #Component Scores
  #Physical functioning
  dataset$PhysFunc <- rowMeans(dataset[c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12")])
  dataset$PhysFuncZ <- (dataset$PhysFunc - 84.52404)/22.89490

  #Role limitations due to physical health
  dataset$LimitPhys <- rowMeans(dataset[c("Q13", "Q14", "Q15", "Q16")])
  dataset$LimitPhysZ <- (dataset$LimitPhys - 81.19907)/33.79729

  #Role limitations due to emotional health
  dataset$LimitEmo <- rowMeans(dataset[c("Q17", "Q18", "Q19")])
  dataset$LimitEmoZ <- (dataset$LimitEmo - 81.29467)/33.02717

  #Energy
  dataset$Energy <- rowMeans(dataset[c("Q23", "Q27", "Q29", "Q31")])
  dataset$EnergyZ <- (dataset$Energy - 61.05453)/20.86942

  #Emotional well-being
  dataset$Emotional <- rowMeans(dataset[c("Q24", "Q25", "Q26", "Q28", "Q30")])
  dataset$EmotionalZ <- (dataset$Emotional - 74.84212)/18.01189

  #Social functioning
  dataset$Social <- rowMeans(dataset[c("Q20", "Q32")])
  dataset$SocialZ <- (dataset$Social - 83.59753)/22.37642

  #Pain
  dataset$Pain <- rowMeans(dataset[c("Q21", "Q22")])
  dataset$PainZ <- (dataset$Pain - 75.49196)/23.55879

  #General health
  dataset$Health <- rowMeans(dataset[c("Q1", "Q33", "Q34", "Q35", "Q36")])
  dataset$HealthZ <- (dataset$Health - 72.21316)/20.16964

  #Mental Component Summary
  dataset$MCS <- (dataset$PhysFuncZ*(-0.22999)) + (dataset$LimitPhysZ*(-0.12329)) +
    (dataset$LimitEmoZ*0.43407) + (dataset$EnergyZ*0.23534) + (dataset$EmotionalZ*0.48581) +
    (dataset$SocialZ*0.26876) + (dataset$PainZ*(-0.09731)) + (dataset$HealthZ*(-0.01571))
  dataset$MCS_tscore = (dataset$MCS*10) + 50;

  #Physical Component Summary
  dataset$PCS <- (dataset$PhysFuncZ*(0.42402)) + (dataset$LimitPhysZ*(0.35119)) +
    (dataset$LimitEmoZ*(-0.19206)) +  (dataset$EnergyZ*0.02877) + (dataset$EmotionalZ*(-0.22069)) +
    (dataset$SocialZ*(-0.00753)) + (dataset$PainZ*0.31754) + (dataset$HealthZ*(0.24954))
  dataset$PCS_tscore = (dataset$PCS*10) + 50;

  #Requests
  #Number of data points and Output variables
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(MCS, PCS) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(MCS, PCS) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}

  else if (TimePoint == "ALL_QOL"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(MCS, PCS) %>%
      arrange(SubjectCode, DaysAfterBaseline)}

  else {Output <- dataset %>%
    arrange(SubjectCode, DaysAfterBaseline)}

  #Output variables
  if (Return == "SummaryScores") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, PhysFunc, LimitPhys,
             LimitEmo, Energy, Emotional, Social, Pain, Health, MCS_tscore, PCS_tscore) %>%
      rename(DaysAfterBaseline.QoL = DaysAfterBaseline,
             StatusDateTime.QoL = StatusDateTime)}

  else if (Return == "SF36_Scored") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, Q1, Q2, Q3, Q4, Q5, Q6, Q7,
             Q8, Q9, Q10, Q11, Q12,  Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26,
             Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34, Q35, Q36, PhysFunc, PhysFuncZ, LimitPhys, LimitPhysZ,
             LimitEmo, LimitEmoZ, Energy, EnergyZ, Emotional, EmotionalZ, Social, SocialZ, Pain, PainZ,
             Health, HealthZ, MCS_tscore, PCS_tscore) %>%
      rename(DaysAfterBaseline.QoL = DaysAfterBaseline,
             StatusDateTime.QoL = StatusDateTime)}

  else {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID80, QID81, QID82.1,
             QID82.2, QID82.3, QID82.4, QID82.5, QID82.6, QID82.7, QID82.8, QID82.9, QID82.10, QID83.1, QID83.2,
             QID83.3, QID83.4, QID84.1, QID84.2, QID84.3, QID85, QID86, QID87, QID140.1, QID140.2, QID140.3,
             QID140.4, QID140.5, QID140.6, QID140.7, QID140.8, QID140.9, QID88, QID89.1, QID89.2, QID89.3,
             QID89.4, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19,
             Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30, Q31, Q32, Q33, Q34, Q35, Q36, PhysFunc,
             PhysFuncZ, LimitPhys, LimitPhysZ, LimitEmo, LimitEmoZ, Energy, EnergyZ, Emotional, EmotionalZ,
             Social, SocialZ, Pain, PainZ, Health, HealthZ, MCS_tscore, PCS_tscore) %>%
      rename(DaysAfterBaseline.QoL = DaysAfterBaseline,
             StatusDateTime.QoL = StatusDateTime)}

  data.frame(Output)

}
