% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mthd_archive.R
\name{archive}
\alias{archive}
\title{Archive}
\usage{
archive(x, what = NULL, filePathName = NULL, forceSave = F, ...)
}
\arguments{
\item{x}{a study (environment) or one of its components}

\item{what}{which component of a study to archive (not implemented at the moment)}

\item{forceSave}{each object in a study is time stamped and saving is inhibited if this object has not been modified. forceSave set to TRUE revert this behaviour}
}
\value{
the same object after it has been written to disk or a character string representing the file path
}
\description{
Archive is used to save to disk an entire study, an Analysis or a part of it
}
\details{
archive has specific methods for a study and for its elements
}
\examples{
\dontrun{
archive(x = MyStudy)
}
}
