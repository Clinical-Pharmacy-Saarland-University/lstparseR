% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_lst.R
\name{read_lst_file}
\alias{read_lst_file}
\title{Read .lst File Contents}
\usage{
read_lst_file(lst_file_path)
}
\arguments{
\item{lst_file_path}{The file path to the .lst file that needs to be read. The function checks that this file exists
and that it has a .lst extension before attempting to read it.}
}
\value{
Returns a character vector, where each element represents a line from the .lst file.
The returned object is assigned the class "lst" for identification and further use in processing.
}
\description{
Reads the contents of a .lst file, ensuring the file exists and has the correct extension before proceeding.
The function reads the file line by line and assigns a specific class to the resulting object for further processing.
}
\examples{
# Assuming you have a .mod file at the specified path:
lst_file_path <- "path/to/your/file.lst"
lst_contents <- read_lst_file(lst_file_path)
print(lst_contents)

}
\keyword{file-io}
