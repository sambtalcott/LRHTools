% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/od_.R
\name{od_write}
\alias{od_write}
\title{Save a file to onedrive}
\usage{
od_write(x, path, od = NULL, ...)
}
\arguments{
\item{x}{file to save}

\item{path}{path to save to}

\item{od}{OneDrive (if null, will use the stored OneDrive)}

\item{...}{Additional parameters to pass on to the reading/writing function.
See details below.}
}
\value{
x, invisibly
}
\description{
Based off of \code{tntpr::sp_write()}.
}
\section{Details}{
For more information on methods (shown as \verb{$__()} below) see documentation
on \link[Microsoft365R:ms_drive]{Microsoft365R::ms_drive}.
\subsection{Reading Functions}{
\itemize{
\item ".csv", ".csv2", ".tsv" are read using the \verb{$load_dataframe()} method,
which uses \code{\link[readr:read_delim]{readr::read_delim()}}.
\item ".rds" is read using the \verb{$load_rds()} method which accepts no additional
arguments.
\item ".xls" and ".xlsx" are read using \code{\link[readxl:read_excel]{readxl::read_excel()}} (if installed).
The function will download the excel file temporarily, then import it and
delete the temporary copy
}
}

\subsection{Writing Functions}{
\itemize{
\item ".csv", ".csv2", ".tsv" are written using the \verb{$save_dataframe()} method
and uses \code{\link[readr:write_delim]{readr::write_delim()}}. Delimiter will be assumed by the extension
unless provided in a \code{delim} argument
\item ".rds" is written using the \verb{$save_rds()} method, which accepts no
additional arguments
\item ".xlsx" is written using \code{\link[writexl:write_xlsx]{writexl::write_xlsx()}} (if
installed) and then uploaded using the \verb{$upload_file()} method.
}
}
}

