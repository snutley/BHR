% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BHR_Mood.R
\name{BHR_Mood}
\alias{BHR_Mood}
\title{\strong{BHR Mood Module}}
\usage{
BHR_Mood(dataset, TimePoint = "ALL_MOOD", Return = "ALL")
}
\arguments{
\item{dataset}{Raw dataframe extracted from the Brain Health Registry Mood Module}

\item{TimePoint}{Desired number of timepoints per participant to be returned in the output dataframe. Options include:
(1) "FIRST": For each participant, return the first (i.e., baseline) time point with completed PHQ-9 and GDS assessments.
(2) "LAST": For each participant, return the last time (i.e., most recent) point with completed PHQ-9 and GDS assessments
(3) "ALL_MOOD": For each participant, return all time points with completed PHQ-9 and GDS assessments
(4) "ALL": For each participant, return all time points with mood data, regardless of whether the PHQ-9 and GDS assessments are complete (i.e., data may be missing for some variables)}

\item{Return}{Variables to be included in the output dataframe. Options include:
(1) "PHQ9": Output dataframe includes PHQ-9 single items and summary score only
(2) "GDS": Output dataframe includes GDS single items and summary score only
(3) "ALL": Output dataframe includes all assessments of mood, including the GDS and PHQ-9 assessments}
}
\value{
The function returns a dataset containing some or all of the following variables (depending on Return parameter). Only variables recoded or created within the function are described in detail.
\itemize{
 \item \code{SubjectCode:} {Unique code identifying BHR participants}
 \item \code{TimepointCode:} {See BHR Codebook}
 \item \code{DaysAfterBaseline.Mood:} {See BHR Codebook (DaysAfterBaseline)}
 \item \code{StatusDateTime.Mood:} {See BHR Codebook (StatusDateTime)}
 \item \code{QID56:} {See BHR Codebook}
 \item \code{QID57:} {See BHR Codebook}
 \item \code{QID58:} {See BHR Codebook}
 \item \code{QID59:} {See BHR Codebook}
 \item \code{QID60:} {See BHR Codebook}
 \item \code{QID61:} {See BHR Codebook}
 \item \code{QID62:} {See BHR Codebook}
 \item \code{QID63:} {See BHR Codebook}
 \item \code{QID64:} {See BHR Codebook}
 \item \code{QID65:} {See BHR Codebook}
 \item \code{QID66:} {See BHR Codebook}
 \item \code{QID67:} {See BHR Codebook}
 \item \code{QID68:} {See BHR Codebook}
 \item \code{QID69:} {See BHR Codebook}
 \item \code{QID70:} {See BHR Codebook}
 \item \code{GDS:}{Total score on the Geriatric Depression Scale. Scores range from 0 to 15, with high schores indicating more severe depressive symtoms.}
 \item \code{QID197.1:} {See BHR Codebook}
 \item \code{QID197.2:} {See BHR Codebook}
 \item \code{QID197.3:} {See BHR Codebook}
 \item \code{QID197.4:} {See BHR Codebook}
 \item \code{QID197.5:} {See BHR Codebook}
 \item \code{QID197.6:} {See BHR Codebook}
 \item \code{QID197.7:} {See BHR Codebook}
 \item \code{QID197.8:} {See BHR Codebook}
 \item \code{QID197.9:} {See BHR Codebook}
 \item \code{PHQ9:} {Total score on the 9-item Patient Health Questionnaire. Scores range from 0 to 27, with high schores indicating more severe depressive symtoms.}
 \item \code{QID198:} {See BHR Codebook}
}
}
\description{
This function cleanses data from the Brain Health Registry Mood Module. The user is asked to input a raw dataframe from the BHR Mood Module and to specify to specify (1) the desired output variables and (2) the desired output time point(s) from which data was collected from each participant (i.e., first time point,  last timepoint, etc.). The function performs basic data cleaning to remove duplicate observations (including obsertvations less than 30 days from the previous time point) and to calculate total scores on the Geriatric Depression Scale (GDS) and 9-item Patient Health Questionnaire (PHQ-9).
}
