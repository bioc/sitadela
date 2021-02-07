\name{getAnnotation}
\alias{getAnnotation}
\title{Annotation downloader}
\usage{
    getAnnotation(org, type, refdb = "ensembl", ver = NULL,
        tv = FALSE, rc = NULL)
}
\arguments{
    \item{org}{the organism for which to download
    annotation (one of the supported ones, see Details).}

    \item{type}{the transcriptional unit annotation level 
    to load. It can be one of \code{"gene"} (default),
    \code{"transcript"}, \code{"utr"}, \code{"transexon"},
    \code{"transutr"}, \code{"exon"}. See Details for 
    further explanation of each option.}

    \item{refdb}{the online source to use to fetch 
    annotation. It can be \code{"ensembl"} (default), 
    \code{"ucsc"}, \code{"refseq"} or \code{"ncbi"}. In 
    the later three cases, an SQL connection is opened 
    with the UCSC public databases.}
    
    \item{ver}{the version of the annotation to use.}
    
    \item{tv}{attach or not gene/transcript version to 
    gene/transcript name. Defaults to \code{FALSE}.}

    \item{rc}{Fraction of cores to use. Same as the 
    \code{rc} in \code{\link{addAnnotation}}.}
}
\value{
    A data frame with the canonical genes, transcripts, 
    exons or 3' UTRs of the requested organism. When
    \code{type="genes"}, the data frame has the following
    columns: chromosome, start, end, gene_id, gc_content,
    strand, gene_name, biotype. When \code{type="exon"} and
    \code{type="transexon"} the data frame has the following 
    columns: chromosome, start, end, exon_id, gene_id, strand, 
    gene_name, biotype. When \code{type="utr"} or  
    \code{type="transutr"}, the data frame has the following 
    columns: chromosome, start, end, transcript_id, gene_id, 
    strand, gene_name, biotype. The latter applies to when 
    \code{type="transcript"}. The gene_id and exon_id 
    correspond to \code{type="transcript"} Ensembl, UCSC 
    or RefSeq gene, transcript and exon accessions 
    respectively. The gene_name corresponds to HUGO 
    nomenclature gene names.
}
\description{
    For Ensembl based annotations, this function connects to the 
    EBI's Biomart service using the package biomaRt and downloads 
    annotation elements (gene co-ordinates, exon co-ordinates, 
    gene identifications, biotypes etc.) for each of the supported
    organisms. For UCSC/RefSeq annotations, it connects to the 
    respective UCSC SQL databases if the package \code{RMySQL} is 
    present, otherwise it downloads flat files and build a 
    temporary SQLite database to make the necessary build 
    queries. Gene and transcript versions can be attached (when
    available) using the \code{tv} argument. This is very useful
    when transcript versioning is required, such as several
    precision medicine applications.
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
}

\note{
    The data frame that is returned contains only "canonical"
    chromosomes for each organism. It does not contain
    haplotypes or non-anchored sequences and does not contain
    mitochondrial chromosomes.
}
\examples{
mm9Genes <- getAnnotation("mm9","gene")
}
\author{
    Panagiotis Moulos
}
