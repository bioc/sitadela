\name{removeAnnotation}
\alias{removeAnnotation}
\title{Remove an annotation from a sitadela database}
\usage{
    removeAnnotation(org, refdb, ver = NULL, db = NULL)
}
\arguments{
    \item{org}{an existing organism to remove from the
    database. See also \code{\link{addAnnotation}} and
    \code{\link{addCustomAnnotation}} for details.}

    \item{refdb}{an existing annotation source to remove 
    from the database. See also \code{\link{addAnnotation}} 
    and \code{\link{addCustomAnnotation}} for details.}
    
    \item{ver}{an existing annotation version to remove 
    from the database. See also \code{\link{addAnnotation}} 
    and \code{\link{addCustomAnnotation}} for details. If
    \code{NULL} (default), all versions corresponding to
    \code{org} and \code{refdb} will be removed.}
    
    \item{db}{the database to remove from, defaults to 
    \code{getDbPath()}.}
}
\value{
    The function return the number of rows removed from
    the database contents table.
}
\description{
    This function removes a specific annotation from a 
    sitadela database. It does not support multiple
    organism, resource and version removal for now.
}
\examples{
# Dummy database as example
customDir <- file.path(tempdir(),"test_custom")
dir.create(customDir)

myDb <- file.path(customDir,"testann.sqlite")
chromInfo <- data.frame(length=c(1000L,2000L,1500L),
    row.names=c("A","B","C"))

# Build with the metadata list filled (you can also provide a version)
if (.Platform$OS.type == "unix") {
    addCustomAnnotation(
        gtfFile=file.path(system.file(package="sitadela"),
            "dummy.gtf"),
        metadata=list(
            organism="dummy",
            source="dummy_db",
            version=1,
            chromInfo=chromInfo
        ),
        db=myDb
    )

    # Try to retrieve some data
    myGenes <- loadAnnotation(genome="dummy",refdb="dummy_db",
        type="gene",db=myDb)
    myGenes
}

# Now remove
n <- removeAnnotation("dummy","dummy_db",1,myDb)
}
\author{
    Panagiotis Moulos
}
