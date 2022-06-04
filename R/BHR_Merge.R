#' @title \strong{BHR Merge}
#'
#' @description This function merges data from the Brain Health Registry Hoarding Module. The user is asked to input a list of cleaned BHR dataframes and to specify the desired output time point(s) from which data was collected from each participant (i.e., first time point, last timepoint, etc.).
#'
#' @param BHR_Datasets List of cleaned dataframes from the Brain Health registry
#'
#' @return The function returns a merged BHR dataset.
#'
#' @importFrom dplyr full_join
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom plyr join_all
#'
#' @export

BHR_Merge <- function(BHR_Datasets){

  #Check 1: A list of BHR datasets has been entered
  if(class(BHR_Datasets) != "list"){
    stop("Please enter a list of cleaned BHR datasets")}
  if(any(sapply(BHR_Datasets, function(x) all(names(x) != "SubjectCode"))) == TRUE){
    stop("Please enter a list of cleaned BHR datasets")}

  #Extract datasets without time component
  NoTime_Data <- BHR_Datasets[which(sapply(BHR_Datasets, function(x) all(names(x) != "TimepointCode")))]

  #Extract datasets with time component
  Time_Data <- BHR_Datasets[which(sapply(BHR_Datasets, function(x) any(names(x) == "TimepointCode")))]

  #Merge datasets with and without time component
  if(length(NoTime_Data) == 0){
    MergedData <- plyr::join_all(Time_Data, by = c("SubjectCode", "TimepointCode"),
                                 type = "full", match = "all")}

  if(length(NoTime_Data) == 1){
    No_Time <- as.data.frame(NoTime_Data[[1]])
    MergedData <- plyr::join_all(Time_Data, by = c("SubjectCode", "TimepointCode"),
                                 type = "full", match = "all") %>%
                  full_join(No_Time, ., by="SubjectCode")}

  else if(length(NoTime_Data) > 1){
    No_Time <- plyr::join_all(NoTime_Data, by = c("SubjectCode"), type = "full", match = "all")
    MergedData <- plyr::join_all(Time_Data, by = c("SubjectCode", "TimepointCode"),
                                 type = "full", match = "all") %>%
                  full_join(No_Time, ., by="SubjectCode")}

  #Return Data
  MergedData <- MergedData %>%
    group_by(SubjectCode) %>%
    arrange(SubjectCode, TimepointCode)
  data.frame(MergedData)

}
