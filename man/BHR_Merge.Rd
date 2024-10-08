% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BHR_Merge.R
\name{BHR_Merge}
\alias{BHR_Merge}
\title{\strong{BHR Merge}}
\usage{
BHR_Merge(BHR_Datasets)
}
\arguments{
\item{BHR_Datasets}{List of cleaned dataframes from the Brain Health registry}
}
\value{
The function returns a merged BHR dataset.
}
\description{
This function merges data from the Brain Health Registry Hoarding Module. The user is asked to input a list of cleaned BHR dataframes and to specify the desired output time point(s) from which data was collected from each participant (i.e., first time point, last timepoint, etc.).
}
