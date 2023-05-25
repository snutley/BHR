#' @title \strong{BHR Medical Module}
#'
#' @description This function cleanses data from the Brain Health Registry Medical Module. The user is asked to input a raw dataframe from the BHR Medical Module and to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point, last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and to recode single item measures of medical, nerological, and psychiatric health history. Variables for lifetime psychiatric diagnoses are created using information on current and past diagnosis, and measures of medical, nerological, and psychiatric disease burden are calculated by summing the number of conditions endorsed in each category.
#'
#' @param dataset Raw dataframe extracted from the Brain Health Registry Medical Module
#'
#' @param TimePoint Desired number of timepoints per participant to be returned in the output dataframe. Options include:
#' (1) "FIRST": For each participant, return the first (i.e., baseline) time point with a completed medical assessment (i.e., medical, neuro, and psychiatric health conditions assessed).
#' (2) "LAST": For each participant, return the last time (i.e., most recent) point with a completed medical assessment.
#' (3) "ALL_MED": For each participant, return all time points with a completed medical assessment.
#' (4) "ALL": For each participant, return all time points with medical data, regardless of whether the medical assessment is complete (i.e., data may be missing for some health conditions)
#'
#' @param Return Variables to be included in the output dataframe. Options include:
#' (1) "MED_NEURO": Output dataframe includes medical and nuerological health conditions only
#' (2) "PSYCH": Output dataframe includes psychiatric health conditions (and lifetime psychiatric variables) only
#' (3) "ALL_MED": Output dataframe includes all medical variables, including lifetime psychiatric health history variables and variables measuring disease burden
#' (3) "ALL": All data columns, including medical, psychiatric, and neurological health conditions, as well as information regarding surgeries
#'
#' @return The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
#' #' \itemize{
#'  \item \code{SubjectCode:} {See BHR Codebook}
#'  \item \code{TimepointCode:} {See BHR Codebook}
#'  \item \code{DaysAfterBaseline.Med:} {See BHR Codebook (DaysAfterBaseline)}
#'  \item \code{StatusDateTime.Med:} {See BHR Codebook (StatusDateTime)}
#'  \item \code{QID1.3} {Lifetime history of stroke. Recoded as No=0, Yes=1}
#'  \item \code{QID1.6} {Lifetime history of heart disease. Recoded as No=0, Yes=1}
#'  \item \code{QID1.7} {Lifetime history of high blood pressure. Recoded as No=0, Yes=1}
#'  \item \code{QID1.8} {Lifetime history of high cholesterol. Recoded as No=0, Yes=1}
#'  \item \code{QID1.9} {Lifetime history of diabetes. Recoded as No=0, Yes=1}
#'  \item \code{QID1.10} {Lifetime history of cancer. Recoded as No=0, Yes=1}
#'  \item \code{QID1.15} {Lifetime history of lung disease. Recoded as No=0, Yes=1  (Added to Medical Module in April 2015)}
#'  \item \code{QID1.16} {Lifetime history of asthma. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.20} {Lifetime history of hearing loss. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.17} {Lifetime history of arthritis. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.14} {Lifetime history of traumatic brain injury. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.18} {Lifetime history of concussion. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID2} {Lifetime history of chronic pain. Recoded as No=0, Yes=1, Prefer Not to Say=99}
#'  \item \code{QID3.1} {See BHR Codebook}
#'  \item \code{QID4} {Lifetime history of sleep apnea. Recoded as No=0, Yes=1, Prefer Not to Say=99}
#'  \item \code{QID6} {Lifetime history of allergies. Recoded as No=0, Yes=1, Prefer Not to Say=99}
#'  \item \code{Med_Burden} {Number of medical health conditions, including stroke, heart disease, high blood pressure, high cholesterol, diabetes, cancer, asthma, hearing loss, arthritis, traumatic brin injury, concussion, chronic pain, sleep apnea, and allergies. Values ranges from 0 to 16, with higher values indicating a greater number of medical conditions}
#'  \item \code{QID1.1} {Lifetime history of Parkinson's disease. Recoded as No=0, Yes=1}
#'  \item \code{QID1.2} {Lifetime history of movement disorder. Recoded as No=0, Yes=1}
#'  \item \code{QID1.24} {Lifetime history of essential tremor. Recoded as No=0, Yes=1 (Added to Medical Module in April 2015)}
#'  \item \code{QID1.25} {Lifetime history of Huntington's disease. Recoded as No=0, Yes=1 (Added to Medical Module in April 2015)}
#'  \item \code{QID1.4} {Lifetime history of motor neuron disease. Recoded as No=0, Yes=1}
#'  \item \code{QID1.26} {Lifetime history of amyotrophic lateral sclerosis. Recoded as No=0, Yes=1 (Added to Medical Module in April 2015)}
#'  \item \code{QID1.12} {Lifetime history of Alzheimer's disease. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.13} {Lifetime history of mild cognitive impairment. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID1.5} {Lifetime history of dementia. Recoded as No=0, Yes=1}
#'  \item \code{QID1.22} {Lifetime history of frontotemporal dementia. Recoded as No=0, Yes=1 (Added to Medical Module in August 2014)}
#'  \item \code{QID1.23} {Lifetime history of Lewy body disease. Recoded as No=0, Yes=1 (Added to Medical Module in August 2014)}
#'  \item \code{QID1.21} {Lifetime history of multiple sclerosis. Recoded as No=0, Yes=1 (Added to Medical Module in August 2014)}
#'  \item \code{QID1.19} {Lifetime history of epilepsy or seizures. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{Neuro_Burden} {Number of neurological health conditions, including Parkinson's disease, movement  disorder, essential tremor, motor neuron disease, Alzheimer's disease, mild cognitive impairment, dementia, frontotemporal dementia, lewy body disease, multiple sclerosis, and epilepsy or seizures. Values ranges from 0 to 11, with higher values indicating a greater number of neurological conditions}
#'  \item \code{QID28.1.1} {Current major depressive disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.1} {Past major depressive disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_MDD} {Lifetime history of major depressive disorder. Coded as No=0, Yes=1}
#'  \item \code{MDD_Most} {Major depressive disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.3} {Current specific phobia or social phobia. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.3} {Past specific phobia or social phobia. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_Phobia} {Lifetime history of specific phobia or social phobia. Coded as No=0, Yes=1}
#'  \item \code{Phobia_Most} {Phobia endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.4} {Current obsessive compulsive disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.4} {Past obsessive compulsive disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_OCD} {Lifetime history of obsessive compulsive disorder. Coded as No=0, Yes=1}
#'  \item \code{OCD_Most} {Obsessive compulsive disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.5} {Current hoarding disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.5} {Past hoarding disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_HD} {Lifetime history of hoarding disorder. Coded as No=0, Yes=1}
#'  \item \code{HD_Most} {Hoarding disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.6} {Current attention deficit/hyperactivity disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.6} {Past attention deficit/hyperactivity disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_ADHD} {Lifetime history of attention deficit/hyperactivity disorder. Coded as No=0, Yes=1}
#'  \item \code{ADHD_Most} {Attention deficit/hyperactivity disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.8} {Current post-traumatic stress disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.8} {Past post-traumatic stress disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_PTSD} {Lifetime history of post-traumatic stress disorder. Coded as No=0, Yes=1}
#'  \item \code{PTSD_Most} {Post-traumatic stress disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.9} {Current generalized anxiety disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.9} {Past generalized anxiety disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_GAD} {Lifetime history of generalized anxiety disorder. Coded as No=0, Yes=1}
#'  \item \code{GAD_Most} {Generalized anxiety disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.10} {Current panic disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.10} {Past panic disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_Panic} {Lifetime history of panic disorder. Coded as No=0, Yes=1}
#'  \item \code{Panic_Most} {Panic disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.11} {Current bipolar disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.11} {Past bipolar disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_BPD} {Lifetime history of bipolar disorder. Coded as No=0, Yes=1}
#'  \item \code{BPD_Most} {Bipolar disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.12} {Current autism spectrum disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.12} {Past autism spectrum disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_ASM} {Lifetime history of autism spectrum disorder. Coded as No=0, Yes=1}
#'  \item \code{ASM_Most} {Autism spectrum disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.13} {Current schizophrenia. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.13} {Past schizophrenia. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_SCZ} {Lifetime history of schizophrenia. Coded as No=0, Yes=1}
#'  \item \code{SCZ_Most} {Schizophrenia endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.14} {Current eating disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.14} {Past eating disorder. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_ED} {Lifetime history of eating disorder. Coded as No=0, Yes=1}
#'  \item \code{ED_Most} {Eating disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID28.1.15} {Current psychosis. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{QID28.2.15} {Past psychosis. Recoded as No=0, Yes=1 (Added to Medical Module in July 2014)}
#'  \item \code{LT_Psychosis} {Lifetime history of psychosis. Coded as No=0, Yes=1}
#'  \item \code{Psychosis_Most} {Psychosis endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{QID9} {Lifetime history of alcohol abuse. Recoded as No=0, Yes=1, Prefer Not to Say = 99}
#'  \item \code{QID13.TEXT} {See BHR Codebook}
#'  \item \code{QID15.TEXT} {See BHR Codebook}
#'  \item \code{QID16.TEXT} {See BHR Codebook}
#'  \item \code{QID17} {Lifetime history of drug abuse. Recoded as No=0, Yes=1, Prefer Not to Say = 99}
#'  \item \code{QID18.TEXT} {See BHR Codebook}
#'  \item \code{QID19.TEXT} {See BHR Codebook}
#'  \item \code{ANX} {Lifetime history of any anxiety disorders (i.e., generalized anxiety disorder, specific or social phobia, or panic disorder). Coded as No=0, Yes=1}
#'  \item \code{SMI} {Lifetime history of any severe mental illness (i.e., bipolar disorder, schizophrenia, or psychosis). Coded as No=0, Yes=1}
#'  \item \code{PsychChild} {Lifetime history of any psychiatric disorder of childhood (i.e., attention deficit/hyperactivity disorder or autism spectrum disorder). Coded as No=0, Yes=1}
#'  \item \code{OCRD} {Lifetime history of any obsessive compulsive related disorder (i.e., obsessive compulsive disorder or eating disorder). Coded as No=0, Yes=1}
#'  \item \code{SUD}  {Lifetime history of any substance use disorder (i.e., alcohol abuse or drug abuse). Coded as No=0, Yes=1}
#'  \item \code{SUD_Most} {Substance use disorder endorsed at most timepoints. Coded as No=0, Yes=1. NA if less than 3 timepoints completed}
#'  \item \code{Psych_Burden} {Number of psychiatric health conditions, including major depressive disorder, specific phobia or social phobia, obsessive compulsive disorder, hoarding disorder, attention deficit/hyperactivity disorder, post-traumatic stress disorder, generalized anxiety disorder, panic disorder, bipolar disorder, autism spectrum disorder, schizophrenia, eating disorder, psychosis, substance use (i.e., alcohol or drug abuse). Values range from 0 to 14, with higher values indicating a greater number of psychiatric conditions}
#'  \item \code{QID20} {Lifetime history of smoking tobacco. Recoded as No=0, Yes=1, Prefer Not to Say = 99}
#'  \item \code{QID21.TEXT} {See BHR Codebook}
#'  \item \code{QID22.TEXT} {See BHR Codebook}
#'  \item \code{QID23.TEXT} {See BHR Codebook}
#'  \item \code{QID30.1} {Current: Use of a cardiac pacemaker/defibrillator, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID30.2} {Current: Any surgical metal or any foreign objects in your body, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID30.3} {Current: Any stents, filter, or intravascular coils, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID30.4} {Current: Internal pacing wires, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID30.5} {Current: Sternum wires, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID30.6} {Current: Claustrophobia, Recoded as No/Don't know=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID31} {Work with metal (grinding, welding, etc.), Recoded as No=0, Yes=1, Prefer Not to Say = 99 (Added to Medical Module in April 2015)}
#'  \item \code{QID32} {Previous MRI scan, Recoded as No=0, Yes=1, Prefer Not to Say = 99  (Added to Medical Module in April 2015)}
#'  }
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


BHR_Medical <- function(dataset, TimePoint = "ALL_MED", Return = "ALL_MED"){

  #Check 1: BHR medical data has been entered into the function
  if(("QID1.9" %in% colnames(dataset) & "QID1.13" %in% colnames(dataset) & "QID28.1.1" %in% colnames(dataset)) == FALSE){
    stop("Please enter a dataset from the BHR Medical History module")}

  #Check 2: An appropriate number of timepoints has been specified
  if (!TimePoint %in% c("ALL_MED", "ALL", "FIRST", "LAST")) {
    stop("Please select the number of time points needed for each BHR participant:
             'FIRST' = First time point with medical data,
             'LAST' = Last time point with medical data,
             'ALL_MED' = All time points with medical data (medical, psychiatric, neuro),
             'ALL' = All time points with and without medical data")}

  #Check 3: Requested output has been specified
  if (!Return %in% c("ALL_MED", "MED_NEURO", "PSYCH", "ALL")) {
    stop("Please specify the output variables:
             'ALL_MED' = All medical, psychiatric, and neurological health conditions,
             'MED_NEURO' = Medical and Neurological health conditions only,
             'PSYCH' = Psychiatric health conditions only,
             'ALL' = All data columns (medical, neuorlogical, psychiatric, surgical, and perscription data)")}

  #Timepoint cleaning: Delete double entries and entries <30 days from last timepoint
  dataset <- dataset %>%
    distinct(SubjectCode, DaysAfterBaseline, .keep_all = TRUE) %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, DaysAfterBaseline) %>%
    filter(DaysAfterBaseline >= 0) %>%
    mutate(diff = DaysAfterBaseline - lag(DaysAfterBaseline)) %>%
    filter(is.na(diff) | diff > 30)
  
  #Clean Medical Data
  #Recode "No" and "Don't know" responses for consistency (all coded to 0)
  #Recode "prefer not to respond" and "decline to answer" responses for consistency (all coded to 99)
  #Find all numeric columns for medical history
  num_cols <- lapply(dataset, is.numeric)
  num_cols$DaysAfterBaseline <- FALSE
  num_cols <- unlist(num_cols)
  #Recode all No (2) = 0
  #Recode two variables with Prefer Not to Answer (3) to Prefer Not to Answer = 99
  #Recode all Don't Know (3) = 0
  #Recode all Decline to Answer (4) = 99
  dataset[ , num_cols][dataset[ , num_cols] == 2] <- 0
  dataset$QID4[dataset$QID4 == 3] <- 99
  dataset$QID32[dataset$QID32 == 3] <- 99
  dataset[ , num_cols][dataset[ , num_cols] == 3] <- 0
  dataset[ , num_cols][dataset[ , num_cols] == 4] <- 99
  
  #Create lifetime psychiatric health history variables
  dataset$LT_MDD <- NA
  dataset$LT_MDD[dataset$QID28.1.1 == 1 | dataset$QID28.2.1 == 1] <- 1
  dataset$LT_MDD[dataset$QID28.1.1 == 0 & dataset$QID28.2.1 == 0] <- 0
  
  dataset$LT_Phobia <- NA
  dataset$LT_Phobia[dataset$QID28.1.3 == 1 | dataset$QID28.2.3 == 1] <- 1
  dataset$LT_Phobia[dataset$QID28.1.3 == 0 & dataset$QID28.2.3 == 0] <- 0
  
  dataset$LT_OCD <- NA
  dataset$LT_OCD[dataset$QID28.1.4 == 1 | dataset$QID28.2.4 == 1] <- 1
  dataset$LT_OCD[dataset$QID28.1.4 == 0 & dataset$QID28.2.4 == 0] <- 0
  
  dataset$LT_HD <- NA
  dataset$LT_HD[dataset$QID28.1.5 == 1 | dataset$QID28.2.5 == 1] <- 1
  dataset$LT_HD[dataset$QID28.1.5 == 0 & dataset$QID28.2.5 == 0] <- 0
  
  dataset$LT_ADHD <- NA
  dataset$LT_ADHD[dataset$QID28.1.6 == 1 | dataset$QID28.2.6 == 1] <- 1
  dataset$LT_ADHD[dataset$QID28.1.6 == 0 & dataset$QID28.2.6 == 0] <- 0
  
  dataset$LT_PTSD <- NA
  dataset$LT_PTSD[dataset$QID28.1.8 == 1 | dataset$QID28.2.8 == 1] <- 1
  dataset$LT_PTSD[dataset$QID28.1.8 == 0 & dataset$QID28.2.8 == 0] <- 0
  
  dataset$LT_GAD <- NA
  dataset$LT_GAD[dataset$QID28.1.9 == 1 | dataset$QID28.2.9 == 1] <- 1
  dataset$LT_GAD[dataset$QID28.1.9 == 0 & dataset$QID28.2.9 == 0] <- 0
  
  dataset$LT_Panic <- NA
  dataset$LT_Panic[dataset$QID28.1.10 == 1 | dataset$QID28.2.10 == 1] <- 1
  dataset$LT_Panic[dataset$QID28.1.10 == 0 & dataset$QID28.2.10 == 0] <- 0
  
  dataset$LT_BPD <- NA
  dataset$LT_BPD[dataset$QID28.1.11 == 1 | dataset$QID28.2.11 == 1] <- 1
  dataset$LT_BPD[dataset$QID28.1.11 == 0 & dataset$QID28.2.11 == 0] <- 0
  
  dataset$LT_ASM <- NA
  dataset$LT_ASM[dataset$QID28.1.12 == 1 | dataset$QID28.2.12 == 1] <- 1
  dataset$LT_ASM[dataset$QID28.1.12 == 0 & dataset$QID28.2.12 == 0] <- 0
  
  dataset$LT_SCZ <- NA
  dataset$LT_SCZ[dataset$QID28.1.13 == 1 | dataset$QID28.2.13 == 1] <- 1
  dataset$LT_SCZ[dataset$QID28.1.13 == 0 & dataset$QID28.2.13 == 0] <- 0
  
  dataset$LT_ED <- NA
  dataset$LT_ED[dataset$QID28.1.14 == 1 | dataset$QID28.2.14 == 1] <- 1
  dataset$LT_ED[dataset$QID28.1.14 == 0 & dataset$QID28.2.14 == 0] <- 0
  
  dataset$LT_Psychosis <- NA
  dataset$LT_Psychosis[dataset$QID28.1.15 == 1 | dataset$QID28.2.15 == 1] <- 1
  dataset$LT_Psychosis[dataset$QID28.1.15 == 0 & dataset$QID28.2.15 == 0] <- 0
  
  dataset$ANX <- NA
  dataset$ANX[dataset$LT_GAD == 1 | dataset$LT_Panic | dataset$LT_Phobia == 1] <- 1
  dataset$ANX[dataset$LT_GAD == 0 & dataset$LT_Panic == 0 & dataset$LT_Phobia == 0] <- 0
  
  dataset$SMI <- NA
  dataset$SMI[dataset$LT_BPD == 1 | dataset$LT_SCZ == 1 | dataset$LT_Psychosis == 1] <- 1
  dataset$SMI[dataset$LT_BPD == 0 & dataset$LT_SCZ == 0 & dataset$LT_Psychosis == 0] <- 0
  
  dataset$PsychChild <- NA
  dataset$PsychChild[dataset$LT_ADHD == 1 | dataset$LT_ASM == 1] <- 1
  dataset$PsychChild[dataset$LT_ADHD == 0 & dataset$LT_ASM == 0] <- 0
  
  dataset$OCRD <- NA
  dataset$OCRD[dataset$LT_OCD == 1 | dataset$LT_ED == 1] <- 1
  dataset$OCRD[dataset$LT_OCD == 0 & dataset$LT_ED == 0] <- 0
  
  dataset$SUD <- NA
  dataset$SUD[dataset$QID9 == 1 | dataset$QID17 == 1] <- 1
  dataset$SUD[dataset$QID9 == 0 & dataset$QID17 == 0] <- 0
  
  #Psych classifications using the most method
  dataset <- dataset %>%
    group_by(SubjectCode) %>%
    mutate(
      #MDD
      MDD_Most = ifelse(sum(!is.na(LT_MDD)) < 3, NA, 
                        ifelse(sum(!is.na(LT_MDD)) > 2 &
                                 (sum(na.omit(LT_MDD) == 1) / sum(!is.na(LT_MDD))) > 0.5, 1, 0)),
      #Phobia
      Phobia_Most = ifelse(sum(!is.na(LT_Phobia)) < 3, NA, 
                           ifelse(sum(!is.na(LT_Phobia)) > 2 &
                                    (sum(na.omit(LT_Phobia) == 1) / sum(!is.na(LT_Phobia))) > 0.5, 1, 0)),
      #OCD
      OCD_Most = ifelse(sum(!is.na(LT_OCD)) < 3, NA, 
                        ifelse(sum(!is.na(LT_OCD)) > 2 &
                                 (sum(na.omit(LT_OCD) == 1) / sum(!is.na(LT_OCD))) > 0.5, 1, 0)),
      #HD
      HD_Most = ifelse(sum(!is.na(LT_HD)) < 3, NA, 
                       ifelse(sum(!is.na(LT_HD)) > 2 &
                                (sum(na.omit(LT_HD) == 1) / sum(!is.na(LT_HD))) > 0.5, 1, 0)),
      #ADHD
      ADHD_Most = ifelse(sum(!is.na(LT_ADHD)) < 3, NA, 
                         ifelse(sum(!is.na(LT_ADHD)) > 2 &
                                  (sum(na.omit(LT_ADHD) == 1) / sum(!is.na(LT_ADHD))) > 0.5, 1, 0)),
      #PTSD
      PTSD_Most = ifelse(sum(!is.na(LT_PTSD)) < 3, NA, 
                         ifelse(sum(!is.na(LT_PTSD)) > 2 &
                                  (sum(na.omit(LT_PTSD) == 1) / sum(!is.na(LT_PTSD))) > 0.5, 1, 0)),
      
      #GAD
      GAD_Most = ifelse(sum(!is.na(LT_GAD)) < 3, NA, 
                          ifelse(sum(!is.na(LT_GAD)) > 2 &
                                   (sum(na.omit(LT_GAD) == 1) / sum(!is.na(LT_GAD))) > 0.5, 1, 0)),
      #Panic
      Panic_Most = ifelse(sum(!is.na(LT_Panic)) < 3, NA, 
                          ifelse(sum(!is.na(LT_Panic)) > 2 &
                                   (sum(na.omit(LT_Panic) == 1) / sum(!is.na(LT_Panic))) > 0.5, 1, 0)),
      #BPD
      BPD_Most = ifelse(sum(!is.na(LT_BPD)) < 3, NA, 
                        ifelse(sum(!is.na(LT_BPD)) > 2 &
                                 (sum(na.omit(LT_BPD) == 1) / sum(!is.na(LT_BPD))) > 0.5, 1, 0)),
      #ASM
      ASM_Most = ifelse(sum(!is.na(LT_ASM)) < 3, NA, 
                        ifelse(sum(!is.na(LT_ASM)) > 2 &
                                 (sum(na.omit(LT_ASM) == 1) / sum(!is.na(LT_ASM))) > 0.5, 1, 0)),
      #SCZ
      SCZ_Most = ifelse(sum(!is.na(LT_SCZ)) < 3, NA, 
                        ifelse(sum(!is.na(LT_SCZ)) > 2 &
                                 (sum(na.omit(LT_SCZ) == 1) / sum(!is.na(LT_SCZ))) > 0.5, 1, 0)),
      #ED
      ED_Most = ifelse(sum(!is.na(LT_ED)) < 3, NA, 
                       ifelse(sum(!is.na(LT_ED)) > 2 &
                                (sum(na.omit(LT_ED) == 1) / sum(!is.na(LT_ED))) > 0.5, 1, 0)),
      #Psychosis
      Psychosis_Most = ifelse(sum(!is.na(LT_Psychosis)) < 3, NA, 
                              ifelse(sum(!is.na(LT_Psychosis)) > 2 &
                                       (sum(na.omit(LT_Psychosis) == 1) / sum(!is.na(LT_Psychosis))) > 0.5, 1, 0)),
      #SUD
      SUD_Most = ifelse(sum(!is.na(SUD)) < 3, NA, 
                        ifelse(sum(!is.na(SUD)) > 2 &
                                 (sum(na.omit(SUD) == 1) / sum(!is.na(SUD))) > 0.5, 1, 0)))
  
  #Create Disease burden variables
  dataset$Med_Burden <- rowSums(dataset[c("QID1.3", "QID1.6", "QID1.7", "QID1.8", "QID1.9", "QID1.10",
                                          "QID1.16", "QID1.20", "QID1.17", "QID1.14",
                                          "QID1.18", "QID2", "QID4", "QID6")] , na.rm = TRUE
                               )
  
  dataset$Neuro_Burden <- rowSums(dataset[c("QID1.1", "QID1.2", "QID1.24", "QID1.4", "QID1.12", "QID1.13",
                                            "QID1.5", "QID1.22", "QID1.23", "QID1.21", "QID1.19")] , na.rm = TRUE
                                 )
  
  dataset$Psych_Burden <- rowSums(dataset[c("LT_MDD", "LT_Phobia", "LT_OCD", "LT_HD", "LT_ADHD", "LT_PTSD",
                                            "LT_GAD", "LT_Panic", "LT_BPD", "LT_ASM", "LT_SCZ", "LT_ED",
                                            "LT_Psychosis", "SUD")] , na.rm = TRUE
                                 )
  
  #Requests
  #Number of data points
  if (TimePoint == "FIRST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Med_Burden, Neuro_Burden, Psych_Burden) %>%
      arrange(SubjectCode, DaysAfterBaseline) %>%
      distinct(SubjectCode, .keep_all = TRUE)}
  
  else if (TimePoint == "LAST"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Med_Burden, Neuro_Burden, Psych_Burden) %>%
      arrange(SubjectCode, desc(DaysAfterBaseline)) %>%
      distinct(SubjectCode, .keep_all = TRUE)}
  
  else if (TimePoint == "ALL_MED"){
    dataset <- dataset %>%
      group_by(SubjectCode) %>%
      drop_na(Med_Burden, Neuro_Burden, Psych_Burden) %>%
      arrange(SubjectCode, DaysAfterBaseline)}
  
  else {
    dataset <- dataset %>%
      arrange(SubjectCode, DaysAfterBaseline)}
  
  #Output variables
  if (Return == "ALL_MED") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID1.3, QID1.6,
             QID1.7, QID1.8, QID1.9, QID1.10, QID1.15, QID1.16, QID1.20, QID1.17, QID1.14,
             QID1.18, QID2,  QID3.1, QID4, QID6, Med_Burden, QID1.1, QID1.2, QID1.24, QID1.25, QID1.4,
             QID1.26, QID1.12, QID1.13, QID1.5, QID1.22, QID1.23, QID1.21, QID1.19, Neuro_Burden,
             QID28.1.1, QID28.2.1, LT_MDD, MDD_Most, QID28.1.3, QID28.2.3, LT_Phobia, Phobia_Most, 
             QID28.1.4, QID28.2.4, LT_OCD, OCD_Most, QID28.1.5, QID28.2.5, LT_HD, HD_Most, QID28.1.6, 
             QID28.2.6, LT_ADHD, ADHD_Most, QID28.1.8, QID28.2.8, LT_PTSD, PTSD_Most, QID28.1.9, 
             QID28.2.9, LT_GAD, GAD_Most, QID28.1.10, QID28.2.10, LT_Panic, Panic_Most, QID28.1.11,
             QID28.2.11, LT_BPD, BPD_Most, QID28.1.12, QID28.2.12, LT_ASM, ASM_Most, QID28.1.13, 
             QID28.2.13, LT_SCZ, SCZ_Most, QID28.1.14, QID28.2.14, LT_ED, ED_Most, QID28.1.15, 
             QID28.2.15, LT_Psychosis, Psychosis_Most, QID9, QID13.TEXT, QID15.TEXT, QID16.TEXT, 
             QID17, QID18.TEXT, QID19.TEXT, ANX, SMI, PsychChild, OCRD, SUD, SUD_Most,
             Psych_Burden, QID20, QID21.TEXT, QID22.TEXT, QID23.TEXT) %>%
      rename(DaysAfterBaseline.Med = DaysAfterBaseline,
             StatusDateTime.Med = StatusDateTime)}
  
  else if (Return == "MED_NEURO") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID1.3, QID1.6,
             QID1.7, QID1.8, QID1.9, QID1.10, QID1.15, QID1.16, QID1.20, QID1.17, QID1.14,
             QID1.18, QID2,  QID3.1, QID4, QID6, Med_Burden, QID1.1, QID1.2, QID1.24, QID1.25, QID1.4,
             QID1.26, QID1.12, QID1.13, QID1.5, QID1.22, QID1.23, QID1.21, QID1.19, Neuro_Burden) %>%
      rename(DaysAfterBaseline.Med = DaysAfterBaseline,
             StatusDateTime.Med = StatusDateTime)}
  
  else if (Return == "PSYCH") {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID28.1.1, QID28.2.1, LT_MDD, MDD_Most, QID28.1.3, QID28.2.3, LT_Phobia, Phobia_Most, 
             QID28.1.4, QID28.2.4, LT_OCD, OCD_Most, QID28.1.5, QID28.2.5, LT_HD, HD_Most, QID28.1.6, 
             QID28.2.6, LT_ADHD, ADHD_Most, QID28.1.8, QID28.2.8, LT_PTSD, PTSD_Most, QID28.1.9, 
             QID28.2.9, LT_GAD, GAD_Most, QID28.1.10, QID28.2.10, LT_Panic, Panic_Most, QID28.1.11,
             QID28.2.11, LT_BPD, BPD_Most, QID28.1.12, QID28.2.12, LT_ASM, ASM_Most, QID28.1.13, 
             QID28.2.13, LT_SCZ, SCZ_Most, QID28.1.14, QID28.2.14, LT_ED, ED_Most, QID28.1.15, 
             QID28.2.15, LT_Psychosis, Psychosis_Most, QID9, QID13.TEXT, QID15.TEXT, QID16.TEXT, 
             QID17, QID18.TEXT, QID19.TEXT, ANX, SMI, PsychChild, OCRD, SUD, SUD_Most,
             Psych_Burden, QID20, QID21.TEXT, QID22.TEXT, QID23.TEXT) %>%
      rename(DaysAfterBaseline.Med = DaysAfterBaseline,
             StatusDateTime.Med = StatusDateTime)}
  else {
    Output <- dataset %>%
      select(SubjectCode, TimepointCode, DaysAfterBaseline, StatusDateTime, QID1.3, QID1.6,
             QID1.7, QID1.8, QID1.9, QID1.10, QID1.15, QID1.16, QID1.20, QID1.17, QID1.14,
             QID1.18, QID2,  QID3.1, QID4, QID6, Med_Burden, QID1.1, QID1.2, QID1.24, QID1.25, QID1.4,
             QID1.26, QID1.12, QID1.13, QID1.5, QID1.22, QID1.23, QID1.21, QID1.19, Neuro_Burden,
             QID28.1.1, QID28.2.1, LT_MDD, QID28.1.3, QID28.2.3, LT_Phobia, QID28.1.4, QID28.2.4,
             LT_OCD, QID28.1.5, QID28.2.5, LT_HD, QID28.1.6, QID28.2.6, LT_ADHD, QID28.1.8, QID28.2.8,
             LT_PTSD, QID28.1.9, QID28.2.9, LT_GAD, QID28.1.10, QID28.2.10, LT_Panic, QID28.1.11,
             QID28.2.11, LT_BPD, QID28.1.12, QID28.2.12, LT_ASM, QID28.1.13, QID28.2.13, LT_SCZ,
             QID28.1.14, QID28.2.14, LT_ED, QID28.1.15, QID28.2.15, LT_Psychosis, QID9, QID13.TEXT,
             QID15.TEXT, QID16.TEXT, QID17, QID18.TEXT, QID19.TEXT, ANX, SMI, PsychChild, OCRD, SUD,
             SUD_Most, Psych_Burden, QID20, QID21.TEXT, QID22.TEXT, QID23.TEXT, QID30.1, QID30.2, 
             QID30.3, QID30.4, QID30.5, QID30.6, QID31, QID32) %>%
      rename(DaysAfterBaseline.Med = DaysAfterBaseline,
             StatusDateTime.Med = StatusDateTime)}
  
  data.frame(Output)
  
}


