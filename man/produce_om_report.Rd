% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/produce_om_report.R
\name{produce_om_report}
\alias{produce_om_report}
\title{Shortcut function with all steps to provide an Outcome Measures report}
\usage{
produce_om_report(df, part = "ALL", format = "uploadable")
}
\arguments{
\item{df}{A dataframe set up according to the readme}

\item{part}{A string with what part of the report you want to produce: 'all',
'A', etc.}

\item{format}{A string (\code{"uploadable"} will produce a properly formatted
upload file. \code{"readable"} will produce a csv of the upload file (only
works for one part at a time). \code{"both"} will provide both options, but
only works with one part at a time.}
}
\value{
A txt or csv file at the path of your choice
}
\description{
Shortcut function with all steps to provide an Outcome Measures report
}
\examples{
\dontshow{
#set temp directory for this example (not necessary for users)
.old_wd <- setwd(tempdir())
}

#entire report
produce_om_report(om_students)

#one part with csv output instead of key-value
produce_om_report(om_students, part = 'A', format = 'readable')

\dontshow{
#reset directory for this example (not necessary for users)
setwd(.old_wd)
}
}
