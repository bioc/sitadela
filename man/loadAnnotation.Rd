\name{loadAnnotation}
\alias{loadAnnotation}
\title{Load a sitadela simple annotation element}
\usage{
    loadAnnotation(genome, refdb, 
        type = c("gene", "transcript", "utr",
            "transutr", "transexon", "exon"), 
        version="auto", wtv = FALSE,
        db = file.path(system.file(package = "sitadela"),
            "annotation.sqlite"), 
        summarized = FALSE, asdf = FALSE, rc = NULL)
}
\arguments{
    \item{genome}{a sitadela supported organism or a custom 
    organism name imported by the user.}

    \item{refdb}{a sitadela supported annotation source or a 
    custom name imported by the user.}
    
    \item{type}{the transcriptional unit annotation level 
    to load. It can be one of \code{"gene"} (default),
    \code{"transcript"}, \code{"utr"}, \code{"transexon"},
    \code{"transutr"}, \code{"exon"}. See Details for 
    further explanation of each option.}
    
    \item{version}{the version of the annotation to use. 
    See Details.}
    
    \item{wtv}{load annotations with versioned genes and
    transcripts when/where available.}
    
    \item{db}{same as the \code{db} in 
    \code{\link{buildAnnotationDatabase}}.}
    
    \item{summarized}{if \code{TRUE}, retrieve summarized,
    non-overlaping elements where appropriate (e.g. exons).}
    
    \item{asdf}{return the result as a \code{\link{data.frame}}
    (default \code{FALSE}).}
    
    \item{rc}{same as the \code{rc} in 
    \code{\link{buildAnnotationDatabase}}.}
}
\value{
    The function returns a \code{GenomicRanges} object or a
    \code{\link{data.frame}} with the requested annotation.
}
\description{
    This function loads an annotation element from a local 
    sitadela annotation database. If the annotation is not
    found and the organism is supported, the annotation is 
    fetched and created on the fly but not imported in the 
    local database. Use \code{buildAnnotationDatabase} for 
    this purpose (build/update/add annotations).
}
\details{
    Regarding \code{org}, it can be, for human genomes 
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
    
    Regarding \code{type}, it defines the level of 
    transcriptional unit (gene, transcript, 3' UTR, exon)
    coordinates to be loaded or fetched if not present. The
    following types are supported:
    
    \itemize{
        \item \code{"gene"}: canonical gene coordinates are
        retrieved from the chosen database.
        \item \code{"transcript"}: all transcript 
        coordinates are retrieved from the chosen database.
        \item \code{"utr"}: all 3' UTR coordinates are
        retrieved from the chosen database, grouped per 
        gene.
        \item \code{"transutr"}: all 3' UTR coordinates are
        retrieved from the chosen database, grouped per \
        transcript.
        \item \code{"transexon"}: all exon coordinates are
        retrieved from the chosen database, grouped per 
        transcript.
        \item \code{"exon"}: all exon coordinates are
        retrieved from the chosen database.
    }
    
    Regarding \code{version}, this is an integer denoting the
    version of the annotation to use from the local annotation
    database or fetch on the fly. For Ensembl, it corresponds
    to Ensembl releases, while for UCSC/RefSeq, it is the
    date of creation (locally).
}
\examples{
db <- file.path(system.file(package="sitadela"),
    "annotation.sqlite")
if (file.exists(db))
    gr <- loadAnnotation(genome="hg19",refdb="ensembl",
        type="gene",db=db)
}
\author{
    Panagiotis Moulos
}
