% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidatePrimKey.R
\name{ValidatePrimKey}
\alias{ValidatePrimKey}
\title{Validate if a data frame column confirms to primary key/ID constraints}
\usage{
ValidatePrimKey(x, prim.key)
}
\arguments{
\item{x}{A data frame.}

\item{prim.key}{A character vector indicating the name of the data frame 
column to be validated for primary key/ID constraints (see 
\strong{Details}).}
}
\value{
A list with containing the following components: \tabular{ll}{ 
  \code{message1} \tab Indicates whether duplicated values were encountered 
  in \code{prim.key} field(column) of data frame \code{x} or not. \cr 
  \code{Duplicates} \tab A data frame of the records with duplicated prim.key
  values if they were encountered. \cr \code{message2} \tab Indicates whether
  NULL values were encountered in \code{prim.key} field(column) of data frame
  \code{x} or not. \cr \code{NullRecords} \tab A data frame of the records 
  with NULL prim.key values if they were encountered. \cr }
}
\description{
\code{ValidatePrimKey} checks if a column in a data frame confirms to the 
primary key/ID constraints of absence of duplicates and NULL values. Aberrant
records if encountered are returned in the output list.
}
\details{
The function checks whether a field(column) in a data frame of PGR passport 
database confirms to the primary key/ID constraints of absence of duplicates 
and NULL values. If records with nonconforming values in the column are 
encountered, they are returned in the output list for rectification.

If multiple fields(columns) are given as a character vector in 
\code{prim.key} field, only the first element will be considered as the 
primary key/ID field(column).

Cleaning of the data in the input field(column) using the 
\code{\link[PGRdup]{DataClean}} function with appropriate arguments is 
suggested before running this function.

It is recommended to run this function and rectify aberrant records in a PGR 
passport database before creating a KWIC index using the 
\code{\link[PGRdup]{KWIC}} function.
}
\examples{
GN <- GN1000
ValidatePrimKey(x=GN, prim.key="NationalID")
\dontrun{
# Show error in case of duplicates and NULL values 
# in the primary key/ID field "NationalID"
GN[1001:1005,] <- GN[1:5,]
GN[1001,3] <- ""
ValidatePrimKey(x=GN, prim.key="NationalID")}
}
\seealso{
\code{\link[PGRdup]{DataClean}}, \code{\link[PGRdup]{KWIC}}
}
