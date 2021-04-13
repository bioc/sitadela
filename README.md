SiTaDelA - Simple, flexible and reusable tab-delimited genome annotations
================================================================================

Next Generation Sequencing has introduced a massive need for working with 
integer interval data which correspond to actual chromosomal regions, depicted
in linear representations. As a result, previously under-developed algorithms
for working with such data have tremendously evolved. Maybe the most common
application where genomic intervals are used is overlapping a set of query 
intervals with a set of reference intervals. One typical example is counting
the reads produced e.g. from an RNA-Seq experiment and assigning them to genes
of interest through overlapping their mapped coordinates with those of the genes
over a reference genome. As a result, collections of such reference genomic 
regions for several reference organisms are essential for the quick 
interrogation of the latter.

The generation of genomic coordinate systems are nowadays mainstream. Typical
ways of reference genomic region representations are:

* [BED](https://genome.ucsc.edu/FAQ/FAQformat.html#format1) files, which are
simple tab-delimited files with at least 3 columns including the main reference
sequence name (e.g. a chromosome), its start and its end.
* More complex structured files such as 
[GTF](https://genome.ucsc.edu/FAQ/FAQformat.html#format4) and
[GFF](https://genome.ucsc.edu/FAQ/FAQformat.html#format3) which also contain
structures such as exons, different transcripts anf untranslated regions.

Bioconductor offers great infrastructures for fast genomic interval calculations
which are now very mature, high-level and cover most needs. It also offers
many comprehensive and centrally maintained genomic interval annotation packages
as well as tools to quickly create custom annotation packages, such as
[AnnotationForge](https://bioconductor.org/packages/release/bioc/html/AnnotationForge.html).
These packages, are primarily designed to capture genomic structures (genes,
transcripts, exons etc.) accurately and place them in a genomic interval content
suitable for fast calculations. While this is more than sufficient for many 
users and work out-of-the-box, especially for less experienced R users, they may
miss certain characteristics which may be also useful for many users. Such
additional elements are often required by tools that report e.g. transcript
biotypes (such as those in Ensembl) and do not gather mappings between elements
of the same annotation (e.g. gene, transcript, exon ids) in one place in a more
straightforward manner. More specifically, some elements which are not 
directly achievable with standard Bioconductor annotation packages include:

* Simple tab-delimited (or in GRanges objects) genomic interval annotations
capturing several characteristics of these annotations (biotype, GC content).
* Centralization of simple tab-delimited annotations for many organisms and
several genomic interval types in one package.
* Versioning of these annotations under the same database instead of many,
dispersed packages which may be difficult to track and upgrade, especially when
transitioning between Bioconductor versions.
* Gene and transcript versioning (when available, e.g. in NCBI annotations)
which is essential for applications related to precision medicine and diagnostic
procedures.
* A unified interface to several genomic interval annotation sources.

SiTaDelA (**Si**mple **Ta**b **Del**imited **A**nnotations), through efficient
and extensive usage of Bioconductor facilites offers these additional 
functionalities along with certain levels of automation. More specifically, the
`sitadela` package offers:

* Simple tab-delimited (easily output also as GRanges objects) genomic interval
annotations for several transcription unit types with additional characteristics
(gene GC content, biotypes).
* A centralized annotation building and retrieval system, supporting several
organisms, versions and annotation resources as well as custom user annotations
coming in GTF/GFF format.
* Versioning of the annotation builds to improve reproducibility and tracking.
* A unified interface to several genomic interval annotation sources which
automates database build but also fetches annotations on-the-fly if not already
present in the build.
* Centralized gene and transcript versioning where available (e.g. NCBI),
especially useful for genomics precision medicine appplications and the
respective diagnostic processes.
* Additional portability from Bioconductor to other applications through the
simple database schema adopted.
* Additional attributes such as corrected feature lengths (i.e. corrected
gene lengths based on sum of lengths of coding regions, to be used e.g. for
RNA abundance estimation and normalization).

The `sitadela` annotation database building is extremely simple. The user 
defines a list of desired annotations (organisms, sources, versions) and 
supplies them to the `addAnnotation` function which in turn creates a new or
updates a current database. A custom, non-directly supported organism annotation
can be imported through the `addCustomAnnotation` function and annotations not
needed anymore can be removed with the `removeAnnotation` function. Finally, as
the built can require some time, especially if many organisms and sources are
required for a local database, we maintain pre-built databases which are built
periodically (e.g. upon a new Ensembl release).

# Supported organisms

The following organisms (essentially genome versions) are supported for 
automatic database builds:

* Human (*Homo sapiens*) genome version **hg38** (or **GRCh38**)
* Human (*Homo sapiens*) genome version **hg19** (or **GRCh37**)
* Human (*Homo sapiens*) genome version **hg18**
* Mouse (*Mus musculus*) genome version **mm10** (or **GRCm37**)
* Mouse (*Mus musculus*) genome version **mm9**
* Rat (*Rattus norvegicus*) genome version **rn6**
* Rat (*Rattus norvegicus*) genome version **rn5**
* Fruitfly (*Drosophila melanogaster*) genome version **dm6**
* Fruitfly (*Drosophila melanogaster*) genome version **dm3**
* Zebrafish (*Danio rerio*) genome version **danRer7**
* Zebrafish (*Danio rerio*) genome version **danRer10**
* Zebrafish (*Danio rerio*) genome version **danRer11**
* Chimpanzee (*Pan troglodytes*) genome version **panTro4**
* Chimpanzee (*Pan troglodytes*) genome version **panTro5**
* Pig (*Sus scrofa*) genome version **susScr3**
* Pig (*Sus scrofa*) genome version **susScr11**
* Horse (*Equus cabalus*) genome version **equCab2**
* Arabidopsis (*Arabidobsis thaliana*) genome version **TAIR10**

Please note that if genomic annotations from UCSC, RefSeq or NCBI are required,
the following `BSgenome` packages are required (depending on the organisms to
be installed) in order to calculate GC content for gene annotations. Also note 
that there is no `BSgenome` package for some of the `sitadela` supported
organisms and therefore GC contents will not be available anyway.

* BSgenome.Hsapiens.UCSC.hg18
* BSgenome.Hsapiens.UCSC.hg19
* BSgenome.Hsapiens.UCSC.hg38
* BSgenome.Mmusculus.UCSC.mm9
* BSgenome.Mmusculus.UCSC.mm10
* BSgenome.Rnorvegicus.UCSC.rn5
* BSgenome.Rnorvegicus.UCSC.rn6
* BSgenome.Dmelanogaster.UCSC.dm3
* BSgenome.Dmelanogaster.UCSC.dm6
* BSgenome.Drerio.UCSC.danRer7
* BSgenome.Drerio.UCSC.danRer10

Is is therefore advised to install these `BSgenome` packages in advance.

# Building and using a SiTaDelA local database

## Installation of sitadela

To install the sitadela package, one should start R and enter:

```
if(!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("sitadela")
```

To install the latest (perhaps non-stable) version:

```
library(remotes)
remotes::install_github("pmoulos/sitadela")
```

## Setup the database

By default, the database file will be written in the
`tools::R_user_dir("sitadela","data")` directory and the file is called 
`"annotation.sqlite"`. By default the build function will ask for the path where
the database will be installed. You can specify another prefered destination for 
it using the `db` argument in the function call, but if you do that, you will 
have to supply an argument pointing to the SQLite database file you created to 
every sitadela package function call you perform, or any other function that 
uses sitadela annotations, otherwise, the annotation will be downloaded and
formatted on-the-fly instead of using the local database. Upon loading 
`sitadela`, an option is added to the R environment pointing to the default
`sitadela` annotation database. If you wish to change that location and do not
wish to supply the database to other function calls, you can change the default
location of the annotation to your preferred location with the `setDbPath`
function in the beginning of your script/function that uses the annotation 
database.

In this example, we will build a minimal database comprising only the mouse
*mm9* genome version from Ensembl. The database will be built in a temporary
directory inside the current R session's `tempdir()`.

**Important note**: As the annotation build function makes use of 
[Kent](http://hgdownload.soe.ucsc.edu/admin/exe/) utilities for creating 3'UTR
annotations from RefSeq and UCSC, the latter cannot be built in Windows. 
Therefore it is advised to either build the annotation database in a Linux 
system or use our pre-built databases.

```
library(sitadela)

buildDir <- file.path(tempdir(),"test_anndb")
dir.create(buildDir)

# The location of the custom database
myDb <- file.path(buildDir,"testann.sqlite")

# Since we are using Ensembl, we can also ask for a version
organisms <- list(mm9=67)
sources <- ifelse(.Platform$OS.type=="unix",c("ensembl","refseq"),"ensembl")

# If the example is not running in a multicore system, rc is ignored
addAnnotation(organisms,sources,forceDownload=FALSE,db=myDb,rc=0.5)

## Alternatively
# setDbPath(myDb)
# addAnnotation(organisms,sources,forceDownload=FALSE,rc=0.5)
```

## Use the database

Now, that a small database is in place, let's retrieve some data. Remember that
since the built database is not in the default location, we need to pass the
database file in each data retrieval function. The annotation is retrieved as
a `GRanges` object by default.

```
# Load standard annotation based on gene body coordinates
genes <- loadAnnotation(genome="mm9",refdb="ensembl",type="gene",db=myDb)
genes

# Load standard annotation based on 3' UTR coordinates
utrs <- loadAnnotation(genome="mm9",refdb="ensembl",type="utr",db=myDb)
utrs

# Load summarized exon annotation based used with RNA-Seq analysis
sumEx <- loadAnnotation(genome="mm9",refdb="ensembl",type="exon",
    summarized=TRUE,db=myDb)
sumEx

# Load standard annotation based on gene body coordinates from RefSeq
if (.Platform$OS.type=="unix") {
    refGenes <- loadAnnotation(genome="mm9",refdb="refseq",type="gene",
        db=myDb)
    refGenes
}
```

Or as a data frame if you prefer using `asdf=TRUE`. The data frame however does 
not contain metadata like `Seqinfo` to be used for any susequent validations:

```
# Load standard annotation based on gene body coordinates
genes <- loadAnnotation(genome="mm9",refdb="ensembl",type="gene",db=myDb,
    asdf=TRUE)
head(genes)
```

## Add a custom annotation

Apart from the supported organisms and databases, you can add a custom 
annotation. Such an annotation can be: 

* A non-supported organism (e.g. an insect or another mammal e.g. dog)
* A modification or further curation you have done to existing/supported
annotations
* A supported organism but from a different source
* Any other case where the provided annotations are not adequate

This can be achieved through the usage of
[GTF/GFF](https://www.ensembl.org/info/website/upload/gff.html) files, along 
with some simple metadata that you have to provide for proper import to the
annotation database. This can be achieved through the usage of the
`addCustomAnnotation` function. Details on required metadata can be found
in the function's help page.

**Important note:** Please note that importing a custom genome annotation 
directly from UCSC (UCSC SQL database dumps) is not supported in Windows as the
process involves using the `genePredToGtf` program which is not available for
Windows.

Let's try a couple of examples. The first one uses example GTF files shipped
with the package. These are sample chromosomes from:

* Atlantic cod (*Gadus morhua*), sequence HE567025
* Armadillo (*Dasypus novemcinctus*), sequence JH569334
* European bass (*Dicentrarchus labrax*), chromosome LG3

Below, we test custom building with reference sequence HE567025 of Atlantic cod:

```
gtf <- system.file(package="sitadela","extdata",
    "gadMor1_HE567025.gtf.gz")
chrom <- system.file(package="sitadela","extdata",
    "gadMor1_HE567025.txt.gz")
chromInfo <- read.delim(chrom,header=FALSE,row.names=1)
names(chromInfo) <- "length"
metadata <- list(
    organism="gadMor1_HE567025",
    source="sitadela_package",
    chromInfo=chromInfo
)
tmpdb <- tempfile()

addCustomAnnotation(gtfFile=gtf,metadata=metadata,db=tmpdb)

# Try to retrieve some data
g <- loadAnnotation(genome="gadMor1_HE567025",refdb="sitadela_package",
    type="gene",db=tmpdb)
g

# Delete the temporary database
unlink(tmpdb)
```

The next one is part of a custom annotation for the Ebola virus from UCSC:

```{r example-5, eval=TRUE, echo=TRUE, tidy=FALSE, message=TRUE, warning=FALSE}
gtf <- system.file(package="sitadela","extdata",
    "eboVir3_KM034562v1.gtf.gz")
chrom <- system.file(package="sitadela","extdata",
    "eboVir3_KM034562v1.txt.gz")
chromInfo <- read.delim(chrom,header=FALSE,row.names=1)
names(chromInfo) <- "length"
metadata <- list(
    organism="gadMor1_HE567025",
    source="sitadela_package",
    chromInfo=chromInfo
)
tmpdb <- tempfile()

addCustomAnnotation(gtfFile=gtf,metadata=metadata,db=tmpdb)

# Try to retrieve some data
g <- loadAnnotation(genome="gadMor1_HE567025",refdb="sitadela_package",
    type="gene",db=tmpdb)
g

# Delete the temporary database
unlink(tmpdb)
```

Again, please note that complete annotations from UCSC require the 
`genePredToGtf` tool from the UCSC tools base and runs only on Linux. The tool 
is required only for building 3' UTR annotations from UCSC, RefSeq and NCBI, all
of which are being retrieved from the UCSC databases. The next example (full 
EBOLA virus annotation) demonstrates how this is done in a Unix based machine:

```
# Setup a temporary directory to download files etc.
customDir <- file.path(tempdir(),"test_custom")
dir.create(customDir)

# Convert from GenePred to GTF - Unix/Linux only!
if (.Platform$OS.type == "unix" && !grepl("^darwin",R.version$os)) {
    # Download data from UCSC
    goldenPath="http://hgdownload.cse.ucsc.edu/goldenPath/"
    # Gene annotation dump
    download.file(paste0(goldenPath,"eboVir3/database/ncbiGene.txt.gz"),
        file.path(customDir,"eboVir3_ncbiGene.txt.gz"))
    # Chromosome information
    download.file(paste0(goldenPath,"eboVir3/database/chromInfo.txt.gz"),
        file.path(customDir,"eboVir3_chromInfo.txt.gz"))

    # Prepare the build
    chromInfo <- read.delim(file.path(customDir,"eboVir3_chromInfo.txt.gz"),
        header=FALSE)
    chromInfo <- chromInfo[,1:2]
    rownames(chromInfo) <- as.character(chromInfo[,1])
    chromInfo <- chromInfo[,2,drop=FALSE]
    
    # Coversion from genePred to GTF
    genePredToGtf <- file.path(customDir,"genePredToGtf")
    if (!file.exists(genePredToGtf)) {
        download.file(
        "http://hgdownload.soe.ucsc.edu/admin/exe/linux.x86_64/genePredToGtf",
            genePredToGtf
        )
        system(paste("chmod 775",genePredToGtf))
    }
    gtfFile <- file.path(customDir,"eboVir3.gtf")
    tmpName <- file.path(customDir,paste(format(Sys.time(),"%Y%m%d%H%M%S"),
        "tgtf",sep="."))
    command <- paste0(
        "zcat ",file.path(customDir,"eboVir3_ncbiGene.txt.gz"),
        " | ","cut -f2- | ",genePredToGtf," file stdin ",tmpName,
        " -source=eboVir3"," -utr && grep -vP '\t\\.\t\\.\t' ",tmpName," > ",
        gtfFile
    )
    system(command)

    # Build with the metadata list filled (you can also provide a version)
    addCustomAnnotation(
        gtfFile=gtfFile,
        metadata=list(
            organism="eboVir3_test",
            source="ucsc_test",
            chromInfo=chromInfo
        ),
        db=myDb
    )

    # Try to retrieve some data
    eboGenes <- loadAnnotation(genome="eboVir3_test",refdb="ucsc_test",
        type="gene",db=myDb)
    eboGenes
}
```

Another example, the full Atlantic cod genome annotation from UCSC. The same 
things apply for the operating system.

```
if (.Platform$OS.type == "unix") {
    # Gene annotation dump
    download.file(paste0(goldenPath,"gadMor1/database/augustusGene.txt.gz"),
        file.path(customDir,"gadMori1_augustusGene.txt.gz"))
    # Chromosome information
    download.file(paste(goldenPath,"gadMor1/database/chromInfo.txt.gz",sep=""),
        file.path(customDir,"gadMori1_chromInfo.txt.gz"))

    # Prepare the build
    chromInfo <- read.delim(file.path(customDir,"gadMori1_chromInfo.txt.gz"),
        header=FALSE)
    chromInfo <- chromInfo[,1:2]
    rownames(chromInfo) <- as.character(chromInfo[,1])
    chromInfo <- chromInfo[,2,drop=FALSE]
    
    # Coversion from genePred to GTF
    genePredToGtf <- file.path(customDir,"genePredToGtf")
    if (!file.exists(genePredToGtf)) {
        download.file(
        "http://hgdownload.soe.ucsc.edu/admin/exe/linux.x86_64/genePredToGtf",
            genePredToGtf
        )
        system(paste("chmod 775",genePredToGtf))
    }
    gtfFile <- file.path(customDir,"gadMori1.gtf")
    tmpName <- file.path(customDir,paste(format(Sys.time(),"%Y%m%d%H%M%S"),
        "tgtf",sep="."))
    command <- paste0(
        "zcat ",file.path(customDir,"gadMori1_augustusGene.txt.gz"),
        " | ","cut -f2- | ",genePredToGtf," file stdin ",tmpName,
        " -source=gadMori1"," -utr && grep -vP '\t\\.\t\\.\t' ",tmpName," > ",
        gtfFile
    )
    system(command)

    # Build with the metadata list filled (you can also provide a version)
    addCustomAnnotation(
        gtfFile=gtfFile,
        metadata=list(
            organism="gadMor1_test",
            source="ucsc_test",
            chromInfo=chromInfo
        ),
        db=myDb
    )

    # Try to retrieve some data
    gadGenes <- loadAnnotation(genome="gadMor1_test",refdb="ucsc_test",
        type="gene",db=myDb)
    gadGenes
}
```

Another example, Armadillo from Ensembl. This should work irrespectively of 
operating system. We are downloading chromosomal information from UCSC. Again,
a small dataset included in the package is included in this vignette. See the
commented code below for the full annotation case.

```
## Gene annotation dump from Ensembl
#download.file(paste0("ftp://ftp.ensembl.org/pub/release-98/gtf/",
#    "dasypus_novemcinctus/Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz"),
#    file.path(customDir,"Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz"))
#
# gtfFile <- file.path(customDir,"Dasypus_novemcinctus.Dasnov3.0.98.gtf.gz")
#
## Chromosome information will be provided from the following BAM file
## available from Ensembl. We have noticed that when using Windows as the OS,
## a remote BAM file cannot be opened by scanBamParam, so for this example,
## chromosome length information will not be available when running in Windows.
#chromInfo <- NULL
#if (.Platform$OS.type == "unix")
#    chromInfo <- paste0("ftp://ftp.ensembl.org/pub/release-98/bamcov/",
#        "dasypus_novemcinctus/genebuild/Dasnov3.broad.Ascending_Colon_5.1.bam")

gtfFile <- system.file(package="sitadela","extdata",
    "dasNov3_JH569334.gtf.gz")
chromInfo <- read.delim(system.file(package="sitadela",
    "extdata","dasNov3_JH569334.txt.gz"),header=FALSE)

# Build with the metadata list filled (you can also provide a version)
addCustomAnnotation(
    gtfFile=gtfFile,
    metadata=list(
        organism="dasNov3_test",
        source="ensembl_test",
        chromInfo=chromInfo
    ),
    db=myDb
)

# Try to retrieve some data
dasGenes <- loadAnnotation(genome="dasNov3_test",refdb="ensembl_test",
    type="gene",db=myDb)
dasGenes
```

## A complete build

A quite complete build (with latest versions of Ensembl annotations) would look
like (supposing the default annotation database location):

```
organisms <- list(
    hg18=67,
    hg19=75,
    hg38=101:102,
    mm9=67,
    mm10=101:102,
    rn5=77,
    rn6=101:102,
    dm3=77,
    dm6=101:102,
    danrer7=77,
    danrer10=91,
    danrer11=101:102,
    pantro4=90,
    pantro5=101:102,
    susscr3=89,
    susscr11=101:102,
    equcab2=94,
    equcab3=101:102
)

sources <- c("ensembl","ucsc","refseq","ncbi")

addAnnotation(organisms,sources,forceDownload=FALSE,rc=0.5)
```

The aforementioned complete built can be found
[here](https://drive.google.com/drive/folders/14vIQBL2iNlVtHkhhbjSMwt04-ZuDAuuR?usp=sharing)
Complete builts will become available from time to time (e.g. with every new
Ensembl release) for users who do not wish to create annotation databases on
their own. Root access may be required (depending on the sitadela library
location) to place it in the default location where it can be found 
automatically.

# Annotations on-the-fly

If for some reason you do not want to build and use an annotation database but
you wish to benefit from the sitadela simple formats nonetheless, or even to
work with an organism that does not yet exist in the database, the 
`loadAnnotation` function will perform all required actions (download and create
a `GRanges` object) on-the-fly as long as there is an internet connection. 
However, the aforementioned function does not handle custom annotations in GTF 
files. In that case, you should use the `importCustomAnnotation` function with
a list describing the GTF file, that is:

```{r pseudo-1, eval=TRUE, echo=TRUE, message=TRUE, warning=FALSE}
metadata <- list(
    organism="ORGANISM_NAME",
    source="SOURCE_NAME",
    chromInfo="CHROM_INFO"
)
```

The above argument can be passed to the `importCustomAnnotation` call in the
respective position.

For further details about custom annotations on the fly, please check
`addCustomAnnotation` and `importCustomAnnotation` functions.
