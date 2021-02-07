test_sitadela_known <- function() {
    testResult <- testKnownBuild("dm6","ensembl",102)
    checkTrue(all(testResult))
}

test_sitadela_gtf <- function() {
    gtf <- file.path(system.file(package="sitadela"),"dummy.gtf.gz")
    chromInfo <- data.frame(length=c(1000L,2000L,1500L),
        row.names=c("A","B","C"))
    metadata=list(
        organism="dummy",
        source="dummy_db",
        version=1,
        chromInfo=chromInfo
    )
    testResult <- testCustomBuild(gtf,metadata)
    checkTrue(all(testResult))
}

test_sitadela_ensembl <- function() {
    f <- testEnsemblSimple("mm10","gene")
    checkTrue(is.null(f))
}

test_sitadela_ucsc <- function() {
    if (require(RMySQL))
        f <- testUcsc("hg19","refseq","transcript",FALSE)
    else
        f <- NULL
    checkTrue(is.null(f))
}
