\name{getsetDbPath}
\alias{setDbPath}
\alias{getDbPath}
\title{Get and set sitadela default database path}
\usage{
    setDbPath(db = NULL)
    getDbPath()
}
\arguments{
    \item{db}{path to a valid SQLite database file.}
}
\value{
    This function does not have a return value.
}
\description{
    The \code{setDbPath} and \code{getDbPath} functions are 
    used to set and get the path to a sitadela annotation
    database. If not explicitly provided, it defaults to
    \code{file.path(system.file(package="sitadela"),"annotation.sqlite")}.
    Essentially, the setter function adds an option to the R
    environment pointing to the desired path.
}
\examples{
myPath <- "/home/me/test.sqlite"
setDbPath(myPath)
getDbPath()
}
\author{
    Panagiotis Moulos
}
