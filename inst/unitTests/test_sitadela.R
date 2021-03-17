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

test_sitadela_ensembl_often <- function() {
    f1 <- testEnsemblSimple("mm10","gene")
    checkTrue(is.null(f1))
    
    f2 <- testEnsemblSimple("mm10","transcript")
    checkTrue(is.null(f2))
}

test_sitadela_ensembl_rare <- function() {
    f1 <- testEnsemblSimple("danrer11","gene")
    checkTrue(is.null(f1))
    
    f2 <- testEnsemblSimple("danrer11","transcript")
    checkTrue(is.null(f2))
}

test_sitadela_ucsc_often <- function() {
    if (require(RMySQL)) {
        f1 <- testUcsc("hg19","refseq","transcript",FALSE)
        f2 <- testUcsc("mm10","refseq","gene",FALSE)
        f3 <- testUcsc("hg19","ucsc","transcript",FALSE)
    }
    else
        f1 <- f2 <- f3 <- NULL
    checkTrue(is.null(f1))
    checkTrue(is.null(f2))
    checkTrue(is.null(f3))
}

test_sitadela_ucsc_rare <- function() {
    if (require(RMySQL)) {
        f1 <- testUcsc("dm3","ucsc","gene",FALSE)
        f2 <- testUcsc("equcab3","refseq","gene",FALSE)
        f3 <- testUcsc("danrer11","refseq","transcript",FALSE)
    }
    else
        f1 <- f2 <- f3 <- NULL
    checkTrue(is.null(f1))
    checkTrue(is.null(f2))
    checkTrue(is.null(f3))
}

test_sitadela_ucsc_versioned <- function() {
    if (require(RMySQL)) {
        f1 <- testUcsc("hg19","refseq","gene",TRUE)
        f2 <- testUcsc("mm10","refseq","gene",TRUE)
        f3 <- testUcsc("danrer11","refseq","transcript",TRUE)
    }
    else
        f1 <- f2 <- f3 <- NULL
    checkTrue(is.null(f1))
    checkTrue(is.null(f2))
    checkTrue(is.null(f3))
}

test_sitadela_argcheck <- function() {
    f1 <- tryCatch({
        sitadela:::.checkTextArgs("test","test","test",FALSE)
        sitadela:::.checkTextArgs("test","test1",c("test1","test2"),TRUE)
        TRUE
    },error=function(e) {
        return(FALSE)
    },finally="")
    
    f2 <- tryCatch({
        # Should crash before TRUE
        sitadela:::.checkTextArgs("test","tes","test",FALSE)
        TRUE
    },error=function(e) {
        return(FALSE)
    },finally="")
    
    f3 <- tryCatch({
        sitadela:::.checkNumArgs("test",4L,"integer",3L,"gt")
        sitadela:::.checkNumArgs("test",4,"numeric",5,"lt")
        sitadela:::.checkNumArgs("test",4,"numeric",c(3,4 ),"both")
        TRUE
    },error=function(e) {
        return(FALSE)
    },finally="")
    
    f4 <- tryCatch({
        sitadela:::.checkNumArgs("test",3L,"integer",3L,"gt")
        TRUE
    },error=function(e) {
        return(FALSE)
    },finally="")
    
    checkTrue(f1)
    checkTrue(!f2)
    checkTrue(f3)
    checkTrue(!f4)
}
