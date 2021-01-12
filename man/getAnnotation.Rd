\name{getAnnotation}
\alias{getAnnotation}
\title{Annotation downloader}
\usage{
    getAnnotation(org, type, refdb = "ensembl", ver = NULL,
        tv = FALSE, rc = NULL)
}
\arguments{
    \item{org}{the organism for which to download
    annotation (one of the supported ones).}

    \item{type}{\code{"gene"}, \code{"exon"} or 
    \code{"utr"}. Same as the \code{countType} in 
    \code{\link{metaseqr2}}.}

    \item{refdb}{the online source to use to fetch 
    annotation. It can be \code{"ensembl"} (default), 
    \code{"ucsc"} or \code{"refseq"}. In the later two
    cases, an SQL connection is opened with the UCSC 
    public databases.}
    
    \item{ver}{the version of the annotation to use.}
    
    \item{tv}{attach or not gene/transcript version to 
    gene/transcript name. Defaults to \code{FALSE}.}

    \item{rc}{Fraction of cores to use. Same as the 
    \code{rc} in \code{\link{buildAnnotationDatabase}}.}
}
\value{
    A data frame with the canonical (not isoforms!) genes or
    exons of the requested organism. When
    \code{type="genes"}, the data frame has the following
    columns: chromosome, start, end, gene_id, gc_content,
    strand, gene_name, biotype. When \code{type="exon"} the
    data frame has the following columns: chromosome, start,
    end, exon_id, gene_id, strand, gene_name, biotype. When 
    \code{type="utr"} the data frame has the following columns: 
    chromosome, start, end, transcript_id, gene_id, strand, 
    gene_name, biotype. The gene_id and exon_id correspond to 
    Ensembl, UCSC or RefSeq gene, transcript and exon accessions 
    respectively. The gene_name corresponds to HUGO nomenclature 
    gene names.
}
\description{
    For Ensembl based annotations, this function connects to the 
    EBI's Biomart service using the package biomaRt and downloads 
    annotation elements (gene co-ordinates, exon co-ordinates, 
    gene identifications, biotypes etc.) for each of the supported
    organisms. For UCSC/RefSeq annotations, it connects to the 
    respective SQL databases if the package \code{RMySQL} is 
    present, otherwise it downloads flat files and build a 
    temporary SQLite database to make the necessary build 
    queries. Gene and transcript versions can be attached (when
    available) using the \code{tv} argument. This is very useful
    when transcript versioning is required, such as several
    precision medicine applications.
}
\note{
    The data frame that is returned contains only "canonical"
    chromosomes for each organism. It does not contain
    haplotypes or random locations and does not contain
    chromosome M.
}
\examples{
mm9Genes <- getAnnotation("mm9","gene")
}
\author{
    Panagiotis Moulos
}
