\name{testFuns}
\alias{testEnsembl}
\alias{testEnsemblSimple}
\alias{testUcsc}
\alias{testUcscAll}
\alias{testUcscUtr}
\alias{testUcscUtrAll}
\alias{testCustomGtf}
\alias{testKnownBuild}
\alias{testCustomBuild}
\title{Query and database build testing functions}
\usage{
    testEnsembl(level = c("normal", "long", "short"),
        versioned = FALSE)
    testEnsemblSimple(orgs, types, versioned = FALSE)

    testUcsc(orgs, refdbs, types, versioned = FALSE)
    testUcscAll()
    
    testUcscUtr(orgs, refdbs, versioned = FALSE)
    testUcscUtrAll()
    
    testCustomGtf(gtf)
    
    testKnownBuild(org, refdb, ver = NULL, tv = FALSE)
    testCustomBuild(gtf, metadata)
    
}
\arguments{
    \item{level}{how many Ensembl versions from the 
    supported organisms should be checked. It can be 
    \code{"normal"} (default), \code{"long"} or
    \code{"short"}. See also Details.}
    
    \item{orgs}{a vector of sitadela supported organisms.
    See also \code{\link{addAnnotation}}.}
    
    \item{refdbs}{a vector of sitadela supported annotation.
    sources. See also \code{\link{addAnnotation}}.}
    
    \item{versioned}{use versioned genes/transcripts where
    available.}
    
    \item{types}{a vector of sitadela annotation types.
    See also \code{\link{loadAnnotation}}.}
    
    \item{org}{as \code{orgs} above but only one organism.}
    
    \item{refdb}{as \code{refdbs} above but only one source.}
    
    \item{ver}{specific annotation version, see also 
    \code{\link{addAnnotation}}.}
    
    \item{tv}{retrieve versioned genes and transcripts when
    possible, see also \code{\link{addAnnotation}}.}
    
    \item{gtf}{a valid GTF or GFF file.}
    
    \item{metadata}{additional information on the contents
    of GTF/GFF file. See also 
    \code{\link{addCustomAnnotation}}.}
}
\value{
    This group of functions return either a vector of
    logical values showing success or failure of conducted
    tests, or a list of test failure reasons or \code{NULL}
    if all tests are successful. Specifically, 
    \code{testKnownBuild} and \code{testCustomBuild}) return
    logicals while all the rest return \code{NULL} if tests
    are successful or a list of failure reasons (and the
    respective test) otherwise.
}
\description{
    This group of testing fuctions can be used to test the
    entirety of sitadela annotation building capabilities
    from known resources or custom GTF/GFF files. They are
    useful for testing the particular annotation the user
    wishes to build prior to building the final database, 
    in order to avoid failures during the longer build. In
    all cases, useful messages are also displayed.
}
\details{
    Regarding \code{testEnsembl} and its arguments, when
    \code{level="normal"}, only the last one or two 
    (depending on availability with Biomart) supported 
    Ensembl versions are checked for fetching availability.
    If \code{level="long"}, all available versions are
    checked for fetching availability (use with care, it
    can run for some time!). If \code{level="short"}, only
    the last version of each supported organism is checked.
    Simpler tests with Ensembl (single organisms, types)
    can be performed with \code{testEnsemblSimple}. It will
    use only the latest version for the asked organism(s).
    
    Regarding \code{testUcsc}, it can be used to test the
    queries used with the UCSC databases for a given 
    organism and database. \code{testUcscAll} will test
    queries for all supported organisms and databases and
    may take a while to finish.
    
    Similarly, \code{testUcscUtr} and \code{testUcscUtrAll}
    will test the queries and building of 3' UTR regions
    form UCSC databases. 3' UTR constructing is not part
    of the other UCSC testing functions as the process is
    different and may be tested only in Unix/Linux machines.
    
    The function \code{testCustomGtf} will simply test
    whether the provided GTF/GFF file can be parsed and 
    used to extract the sitadela annotation types. If this
    is not possible (rarely), this test will fail. If you
    wish to test complete database building with a custom
    GTF/GFF file, use \code{testCustomBuild}.
    
    Finally, \code{testKnownBuild} will test database
    building and querying (add/remove annotation) for a
    single organism.
}
\examples{
    # Test a dummy GTF file
    gtf <- file.path(system.file(package="sitadela"),
        "dummy.gtf.gz")
    chromInfo <- data.frame(length=c(1000L,2000L,1500L),
        row.names=c("A","B","C"))
    metadata=list(
        organism="dummy",
        source="dummy_db",
        version=1,
        chromInfo=chromInfo
    )
    
    testResult <- testCustomBuild(gtf,metadata)
    # For this case, just testResult <- testCustomBuild()
    # would also work
    
    # More real tests
    if (require(RMySQL))
        f <- testUcsc("hg19","refseq","gene",TRUE)
    
    # Test a complete build for Ensembl mm9
    # testResult <- testKnownBuild()
    
    # Test a complete build for UCSC dm6
    # testResult <- testKnownBuild("dm6","ucsc")
}
\author{
    Panagiotis Moulos
}
