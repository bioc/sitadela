\name{getSeqInfo}
\alias{getSeqInfo}
\title{Retrieve sequence length and other information}
\usage{
    getSeqInfo(org, asSeqInfo = FALSE)
}
\arguments{
    \item{org}{a supported organism to retrieve sequence
    (aka chromosome) information for. See also 
    \code{\link{addAnnotation}} about supported organisms.}

    \item{asSeqInfo}{return a \code{Seqinfo} object or a
    \code{data.frame}.}
}
\value{
    The function returns a \code{Seqinfo} or a
    \code{data.frame} with the a subset of a 
    \code{Seqinfo} information. See also 
    \code{\link{Seqinfo}}.
}
\description{
    This function retrieves sequence (chromosome) length
    and other information for a set of reference sequences
    for a sitadela supported organism. If the organism is
    supported by the \code{\link{getChromInfoFromUCSC}} of
    the GenomeInfoDb package, then this function is used,
    otherwise, a directo download from the UCSC golden
    path takes place to retrieve the required data.
}
\examples{
require(GenomeInfoDb)
s <- getSeqInfo("mm10")
}
\author{
    Panagiotis Moulos
}
