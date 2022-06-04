#' @title \strong{BHR Participants and Profile Modules}
#'
#' @description This function cleanses data from the Brain Health Registry Participants and Profile Modules. The user is asked to input two raw dataframes from the BHR, one from the Profile Module and one from the Participants Module. The function performs basic data cleaning to merge the two dataframes and create categorical race and education variables.
#'
#' @param Participants Raw dataframe extracted from the Brain Health Registry Profile Module
#' @param Profile Raw dataframe extracted from the Brain Health Registry Participants Module
#'
#' @return The function returns a dataset containing the following variables. Only variables recoded or created within the function are described in detail.
#' \itemize{
#'  \item \code{SubjectCode:} {Unique code identifying BHR participants}
#'  \item \code{IsSubject:} {See BHR Codebook}
#'  \item \code{IsStudyPartner:} {See BHR Codebook}
#'  \item \code{BaselineDate:} {See BHR Codebook}
#'  \item \code{Age_Baseline:} {See BHR Codebook}
#'  \item \code{Gender:} {See BHR Codebook}
#'  \item \code{Race_Categorical:} {Race of participant. Coded as Caucasian/White=0, African American/Black=1, Asian=2, Other/Mixed=3, Declined to State=4}
#'  \item \code{Race_AfricanAmerican:} {See BHR Codebook}
#'  \item \code{Race_Asian:} {See BHR Codebook}
#'  \item \code{Race_Caucasian:} {See BHR Codebook}
#'  \item \code{Race_NativeAmerican:} {See BHR Codebook}
#'  \item \code{Race_PacificIslander:} {See BHR Codebook}
#'  \item \code{Race_Other:} {See BHR Codebook}
#'  \item \code{Race_DeclinedToState:} {See BHR Codebook}
#'  \item \code{Ethnicity:} {See BHR Codebook}
#'  \item \code{Edu_Categorical:} {Educational Attainment. Coded as Less than college (i.e., Grammar School, High School, Some College)=0, Two or Four Year Degree=1, Graduate/Professional Degree (i.e., Masters, Doctoral Degre, or Professional Degree)=2}
#'  \item \code{YearsEducationUS:} {See BHR Codebook}
#'  \item \code{Handedness:} {See BHR Codebook}
#'  \item \code{Deceased:} {See BHR Codebook}
#' }
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#'
#' @export

BHR_Prof_Particip <- function(Profile, Participants){
  #Check 1: BHR Participants and Profile data has been entered into the function
  if(("Gender" %in% colnames(Profile) & "YearsEducationUS" %in% colnames(Profile)) == FALSE){
    stop("Please enter a dataset from the BHR Profile module")}

  if(("IsSubject" %in% colnames(Participants) & "Age_Baseline" %in% colnames(Participants)) == FALSE){
    stop("Please enter a dataset from the BHR Participants module")}

  #Merge data
  dataset <-  left_join(Profile, Participants, by="Code") %>%
    rename(SubjectCode = Code)

  #Create new Race variable
  dataset$Race_Categorical <- NA
  dataset$Race_Categorical[dataset$Race_DeclinedToState == "True"] <- 4 #Declined to State
  dataset$Race_Categorical[dataset$Race_Caucasian == "True" & dataset$Race_AfricanAmerican == "False" &
                             dataset$Race_Asian == "False" & dataset$Race_NativeAmerican == "False" &
                             dataset$Race_PacificIslander == "False" & dataset$Race_Other == "False"] <- 0 #White
  dataset$Race_Categorical[dataset$Race_Caucasian == "False" & dataset$Race_AfricanAmerican == "True" &
                             dataset$Race_Asian == "False" & dataset$Race_NativeAmerican == "False" &
                             dataset$Race_PacificIslander == "False" & dataset$Race_Other == "False"] <- 1 #Black
  dataset$Race_Categorical[dataset$Race_Caucasian == "False" & dataset$Race_AfricanAmerican == "False" &
                             dataset$Race_Asian == "True" & dataset$Race_NativeAmerican == "False" &
                             dataset$Race_PacificIslander == "False" & dataset$Race_Other == "False"] <- 2 #Asian
  dataset$Race_Categorical[dataset$Race_NativeAmerican == "True" | dataset$Race_PacificIslander == "True" |
                             dataset$Race_Other == "True" |
                             (dataset$Race_Caucasian == "True" & dataset$Race_AfricanAmerican == "True") |
                             (dataset$Race_Caucasian == "True" & dataset$Race_Asian == "True") |
                             (dataset$Race_AfricanAmerican == "True" & dataset$Race_Asian == "True")] <- 3 #Other/Mixed

  #Create new Education variable
  dataset$Edu_Categorical <- NA
  dataset$Edu_Categorical[dataset$YearsEducationUS < 3] <- 0 #Less than college
  dataset$Edu_Categorical[dataset$YearsEducationUS == 3 | dataset$YearsEducationUS == 4] <- 1 #2 or 4 year degree
  dataset$Edu_Categorical[dataset$YearsEducationUS > 4] <- 2 #Graduate/Professional

  #Output variables
  Output <- dataset %>%
    arrange(SubjectCode) %>%
    select(SubjectCode, IsSubject, IsStudyPartner, BaselineDate, Age_Baseline, Gender,
           Race_Categorical,  Race_AfricanAmerican, Race_Asian, Race_Caucasian, Race_NativeAmerican,
           Race_PacificIslander, Race_Other, Race_DeclinedToState, Ethnicity, Edu_Categorical,
           YearsEducationUS, Handedness, Deceased)

  Output

}
