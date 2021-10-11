\name{addAnnotation}
\alias{addAnnotation}
\title{Build a local genomic regions annotation database}
\usage{
    addAnnotation(organisms, sources, db = getDbPath(),
        versioned = FALSE, forceDownload = TRUE, retries = 5,
        rc = NULL, stopIfNotBS = FALSE)
}
\arguments{
    \item{organisms}{a list of organisms and versions
    for which to download and build annotations. See 
    also Details.}

    \item{sources}{a character vector of public sources
    from which to download and build annotations. It can be
    one or more of \code{"ensembl"}, \code{"ucsc"}, 
    \code{"refseq"} or \code{"ncbi"}. See also Details.}
    
    \item{db}{a valid path (accessible at least by the
    current user) where the annotation database will be 
    set up. It defaults to 
    \code{system.file(package = "sitadela"),}
    \code{"annotation.sqlite")} that is, the installation
    path of sitadela package.}
    
    \item{versioned}{create an annotation database with
    versioned genes and transcripts, when possible.}
    
    \item{forceDownload}{by default, 
    \code{addAnnotation} will not download an
    existing annotation again (\code{FALSE}). Set to 
    \code{TRUE} if you wish to update the annotation 
    database for a particular version.}
    
    \item{retries}{how many times should the annotation
    worker try to re-connect to internet resources in case
    of a connection problem or failure.}

    \item{rc}{fraction (0-1) of cores to use in a multicore 
    system. It defaults to \code{NULL} (no parallelization).
    Sometimes used for building certain annotation types.}
    
    \item{stopIfNotBS}{stop or warn (default) if certain 
    \code{BSgenome} packages are not present. See also
    Details.}
}
\value{
    The function does not return anything. Only the SQLite 
    database is created or updated.
}
\description{
    This function is the main annotation database creator 
    of sitadela. It creates a local SQLite database for
    various organisms and categories of genomic regions.
    Annotations are retrieved in simple, tab-delimited or
    GRanges formats.
}
\details{
    Regarding the \code{organisms} argument, it is a list
    with specific format which instructs 
    \code{addAnnotation} on which organisms and
    versions to download from the respective sources. Such
    a list may have the format: 
    \code{organisms=list(hg19=75, mm9=67, mm10=96:97)}
    This is explained as follows:
    \itemize{
        \item A database comprising the human genome versions
        \code{hg19} and the mouse genome versions 
        \code{mm9, mm10} will be constructed.
        \item If \code{"ensembl"} is in \code{sources}, 
        version 75 is downloaded for \code{hg19} and versions 
        \code{67, 96, 97} for \code{mm9, mm10}. 
        \item If \code{"ucsc"} or \code{"refseq"} are in 
        \code{sources}, the latest versions are downloaded
        and marked by the download date. As UCSC and RefSeq
        versions are not accessible in the same way as
        Ensembl, this procedure cannot always be replicated.
    }
    \code{organisms} can also be a character vector with organism
    names/versions (e.g. \code{organisms = c("mm10","hg19")}),
    then the latest versions are downloaded in the case of 
    Ensembl.
    
    The supported supported organsisms are, for human genomes 
    \code{"hg18"}, \code{"hg19"} or \code{"hg38"}, for mouse 
    genomes \code{"mm9"}, \code{"mm10"}, for rat genomes 
    \code{"rn5"} or \code{"rn6"}, for drosophila genome
    \code{"dm3"} or \code{"dm6"}, for zebrafish genome 
    \code{"danrer7"}, \code{"danrer10"} or \code{"danrer11"}, 
    for chimpanzee genome \code{"pantro4"}, \code{"pantro5"}, 
    for pig genome \code{"susscr3"}, \code{"susscr11"}, for 
    Arabidopsis thaliana genome \code{"tair10"} and for 
    Equus caballus genome \code{"equcab2"} and \code{"equcab3"}. 
    Finally, it can be \code{"USER_NAMED_ORG"} with a custom 
    organism which has been imported to the annotation database 
    by the user using a GTF/GFF file. For example 
    \code{org="mm10_p1"}.
    
    Regarding \code{sources}, \code{"ucsc"} corresponds to 
    UCSC Genome Browser annotated transcripts, \code{"refseq"}
    corresponds to UCSC RefSeq maintained transcripts while 
    \code{"ncbi"} corresponds to NCBI RefSeq annotated and 
    maintained transcripts. UCSC, RefSeq and NCBI annotations 
    are constructed by querying the UCSC Genome Browser 
    database.
    
    Regarding \code{stopIfNotBS}, when \code{sources} 
    includes \code{"ucsc"}, \code{"refseq"} or \code{"ncbi"},
    the GC content of a gene is not available as a database
    attribute as with Ensembl and has to be calculated if to
    be included in the respective annotation. For this reason,
    sitadela uses `BSgenome` packages. If 
    \code{stopIfNotBS=FALSE} (default), then the annotation
    building continues and GC content is \code{NA} for the
    missing `BSgenome` packages.If \code{stopIfNotBS=FALSE},
    then building stops until all the required packages for
    the selected organisms become available (installed by
    the user).
}
\examples{
# Build a test database with one genome
myDb <- file.path(tempdir(),"testann.sqlite")

organisms <- list(mm10=100)
sources <- "ensembl"

# If the example is not running in a multicore system, rc is ignored
#addAnnotation(organisms,sources,db=myDb,rc=0.5)

# A more complete case, don't run as example
# Since we are using Ensembl, we can also ask for a version
#organisms <- list(
#    mm9=67,
#    mm10=96:97,
#    hg19=75,
#    hg38=96:97
#)
#sources <- c("ensembl", "refseq")

## Build on the default location (depending on package location, it may
## require root/sudo)
#addAnnotation(organisms,sources)

## Build on an alternative location
#myDb <- file.path(path.expand("~"),"my_ann.sqlite")
#addAnnotation(organisms,sources,db=myDb)
}
\author{
    Panagiotis Moulos
}
