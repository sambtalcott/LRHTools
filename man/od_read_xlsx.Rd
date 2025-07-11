% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/od_.R
\name{od_read_xlsx}
\alias{od_read_xlsx}
\title{Internal function for reading excel files from OneDrive}
\usage{
od_read_xlsx(path, od, ...)
}
\arguments{
\item{path}{path}

\item{od}{OneDrive object or folder object}

\item{...}{additional arguments from od_read()}
}
\value{
data read by readxl::read_excel()
}
\description{
Internal function for reading excel files from OneDrive
}
