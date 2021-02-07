\name{importCustomAnnotation}
\alias{importCustomAnnotation}
\title{Import a metaseqR2 custom annotation element}
\usage{
    importCustomAnnotation(gtfFile, metadata,
        type = c("gene", "transcript", "utr",
            "transexon", "transutr", "exon"))
}
\arguments{
    \item{gtfFile}{a GTF file containing the gene structure
    of the organism to be imported.}

    \item{metadata}{a list with additional information about
    the annotation to be imported. The same as in the
    \code{\link{addCustomAnnotation}} man page.}
    
    \item{type}{same as the \code{type} in 
    \code{\link{loadAnnotation}}.}
}
\value{
    The function returns a \code{GenomicRanges} object with
    the requested annotation.
}
\description{
    This function imports a custom GTF/GFF file in a manner
    helpful for the addition of custom annotations to sitadela.
}
\examples{
# Dummy GTF as example
chromInfo <- data.frame(length=c(1000L,2000L,1500L),
    row.names=c("A","B","C"))

# Build with the metadata list filled (you can also provide a version)
myGenes <- importCustomAnnotation(
    gtfFile=file.path(system.file(package="sitadela"),"dummy.gtf.gz"),
    metadata=list(
        organism="dummy",
        source="dummy_db",
        version=1,
        chromInfo=chromInfo
    ),
    type="gene"
)

## Real data!
## Gene annotation dump from Ensembl
#download.file(paste0("ftp://ftp.ensembl.org/pub/release-98/gtf/",
#  "dasypus_novemcinctus/Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz"),
#  file.path(tempdir(),"Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz"))

## Build with the metadata list filled (you can also provide a version)
#dasGenes <- importCustomAnnotation(
#  gtfFile=file.path(tempdir(),"Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz"),
#  metadata=list(
#    organism="dasNov3_test",
#    source="ensembl_test"
#  ),
#  type="gene"
#)
}
\author{
    Panagiotis Moulos
}
