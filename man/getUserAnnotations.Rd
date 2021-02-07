\name{getUserAnnotations}
\alias{getUserAnnotations}
\title{List installed custom user-defined sitadela annotations}
\usage{
    getUserAnnotations(obj = NULL)
}
\arguments{
    \item{obj}{\code{NULL} or the path to a sitadela
    SQLite annotation database. If \code{NULL}, the
    function will try to guess the location of the 
    SQLite database.}
}
\value{
    The function returns a \code{data.frame} object with
    the installed, custom, user-defined local annotations
    only.
}
\description{
    This function returns a data frame with information
    on locally installed, custom user-defined annotations
    only. For a list of all annotations, see
    \code{\link{getInstalledAnnotations}}.
}
\examples{
db <- file.path(system.file(package="sitadela"),
    "annotation.sqlite")
if (file.exists(db))
    u <- getUserAnnotations(obj=db)
}
\author{
    Panagiotis Moulos
}
