% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_ef1_part_A.R
\name{make_ef1_part_A}
\alias{make_ef1_part_A}
\title{Make Fall Enrollment Part A}
\usage{
make_ef1_part_A(df, cips = TRUE)
}
\arguments{
\item{df}{A dataframe of student information}

\item{cips}{A logical indicating if part A  needs to provide breakdowns by particular CIPs}
}
\value{
A dataframe with the required IPEDS structure for this survey part
}
\description{
Breakdown of students level and demographics; also by designated CIPs in required years
}
