test_sitadela_known <- function() {
    testResult <- testKnownBuild("dm6","ensembl",102)
    checkTrue(all(testResult))
}

test_sitadela_gtf <- function() {
    testResult <- testCustomBuild()
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
