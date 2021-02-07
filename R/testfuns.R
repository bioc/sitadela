testKnownBuild <- function(org,refdb,ver=NULL) {
    if (missing(org)) {
        org <- "mm9"
        if (missing(refdb)) {
            refdb <- "ensembl"
            ver <- 67
        }
    }
    
    if (refdb=="ensembl") {
        if (!is.null(ver)) {
            organisms <- list(ver)
            names(organisms) <- org
        }
        else {
            organisms <- org
            names(organisms) <- org
        }
        sources <- refdb
    }
    else {
        organisms <- org
        names(organisms) <- org
        sources <- refdb
    }
    
    tmpdb <- tempfile()
    testResult <- logical(2)
    
    message("Scheduling 2 tests")
    message("==================================================\n")
    message("Running test 1 of 2 scheduled")
    tryCatch({
        addAnnotation(organisms,sources,db=tmpdb,versioned=FALSE,
            forceDownload=TRUE,rc=NULL)
        genes <- loadAnnotation(genome=names(organisms)[1],refdb=sources[1],
            type="gene",db=tmpdb)
        if (is(genes,"GRanges")) {
            message("Test 1 successful!")
            testResult[1] <- TRUE
        }
    },error=function(e) {
        message("Test 1 failed with error:")
        message(e$message)
    },finally="")
    
    message("\nRunning test 2 of 2 scheduled")
    tryCatch({
        org <- names(organisms)[1]
        refdb <- sources[1]
        n <- removeAnnotation(org,refdb,ver,db=tmpdb)
        if (n > 0) {
            message("Test 2 successful!")
            testResult[2] <- TRUE
        }
    },error=function(e) {
        message("Test 2 failed with error:")
        message(e$message)
    },finally="")
    
    message("Deleting temporary database ",tmpdb)
    unlink(tmpdb)
    
    return(testResult)
}

testCustomBuild <- function(gtf,metadata) {
    if (missing(gtf) || missing(metadata)) {
        gtf <- file.path(system.file(package="sitadela"),"dummy.gtf.gz")
        chromInfo <- data.frame(length=c(1000L,2000L,1500L),
            row.names=c("A","B","C"))
        metadata=list(
            organism="dummy",
            source="dummy_db",
            version=1,
            chromInfo=chromInfo
        )
    }
    
    tmpdb <- tempfile()
    testResult <- logical(2)
    
    message("Scheduling 2 tests")
    message("==================================================\n")
    message("Running test 1 of 2 scheduled")
    tryCatch({        
        addCustomAnnotation(gtfFile=gtf,metadata=metadata,db=tmpdb)
        genes <- loadAnnotation(genome=metadata$organism,refdb=metadata$source,
            type="gene",db=tmpdb)
        if (is(genes,"GRanges")) {
            message("Test 1 successful!")
            testResult[1] <- TRUE
        }
    },error=function(e) {
        message("Test 1 failed with error:")
        message(e$message)
    },finally="")
    
    message("\nRunning test 2 of 2 scheduled")
    tryCatch({
        n <- removeAnnotation(metadata$organism,metadata$source,db=tmpdb)
        if (n > 0) {
            message("Test 2 successful!")
            testResult[2] <- TRUE
        }
    },error=function(e) {
        message("Test 2 failed with error:")
        message(e$message)
    },finally="")
    
    message("Deleting temporary database ",tmpdb)
    unlink(tmpdb)
    
    return(testResult)
}

testUcscAll  <- function() {
    orgs <- getSupportedOrganisms()
    refdbs <- getSupportedUcscDbs()
    types <- c("gene","transcript","exon")
    
    # Remove tair10
    orgs <- orgs[-which(orgs=="tair10")]
    
    failNoVer <- testUcsc(orgs,refdbs,types)
    failVer <- testUcsc(orgs,refdbs,types,versioned=TRUE)
    return(list(
        failNoVersion=failNoVer,
        failVersion=failVer
    ))
}

testUcscUtrAll <- function() {
    orgs <- getSupportedOrganisms()
    refdbs <- getSupportedUcscDbs()
    
    # Remove tair10
    orgs <- orgs[-which(orgs=="tair10")]
    
    failNoVer <- testUcscUtr(orgs,refdbs)
    failVer <- testUcscUtr(orgs,refdbs,versioned=TRUE)
    return(list(
        failNoVersion=failNoVer,
        failVersion=failVer
    ))
}

testEnsembl <- function(level=c("normal","long","short"),versioned=FALSE) {
    level <- level[1]
    
    type <- c("gene","transcript","utr","exon","transexon")
    if (level == "normal") {
        org <- list(
            hg18=67,
            hg19=75,
            hg38=101:102,
            mm9=67,
            mm10=101:102,
            rn5=78:79,
            rn6=101:102,
            dm3=77:78,
            dm6=101:102,
            danrer7=79,
            danrer10=90:91,
            danrer11=101:102,
            pantro4=89:90,
            pantro5=101:102,
            susscr3=88:89,
            susscr11=101:102,
            equcab2=93:94,
            equcab3=101:102
        )
    }
    else if (level == "long") {
        org <- .ucscToEnsembl()
    }
    else if (level == "short") {
        org <- as.list(getSupportedOrganisms())
        names(org) <- unlist(org)
        org[seq_along(org)] <- "auto"
    }
    
    nTests <- length(type)*sum(lengths(org))
    message("Scheduling ",nTests," tests")
    message("==================================================\n")
    
    succ <- fail <- 0
    failReasons <- rep(NA,nTests)
    
    currTest <- 0
    for (o in names(org)) {
        for (v in org[[o]]) {
            if (v == "auto") v <- NULL
            for (z in type) {
                currTest <- currTest + 1
                message("\nRunning test ",currTest," of ",nTests," scheduled")
                message("Testing level ",z," for version ",v," from ",o)
                tryCatch({
                    ann <- getEnsemblAnnotation(o,z,v,versioned)
                    if (is(ann,"data.frame")) {
                        message("Test ",currTest," successful!")
                        message("Created ",nrow(ann)," features")
                        message("Sample data:")
                        print(head(ann))
                        succ <- succ + 1
                    }
                },error=function(e) {
                    message("Test ",currTest," failed with error:")
                    message(e$message)
                    fail <- fail + 1
                    failReasons[currTest] <- paste("Test ",currTest,":",
                        message(e$message),sep="")
                },finally="")
            }
        }
    }
    
    message("Testing finished!\n")
    message("Summary")
    message("==================================================\n")
    message("Succesful tests: ",succ," out of ",nTests)
    message("Failed tests: ",fail," out of ",nTests)
    message(" ")
    
    d <- which(is.na(failReasons))
    if (length(d) == nTests) # All successful
        return(NULL)
    else {
        message("Check the output for failure details")
        if (length(d) > 0) # If not, all failed!
            failReasons <- failReasons[-d]
        return(failReasons)
    }
    message("Bye!\n")
}

testEnsemblSimple <- function(orgs,types,versioned=FALSE) {
    nTests <- length(orgs)*length(types)
    message("Scheduling ",nTests," tests")
    message("==================================================\n")
    
    succ <- fail <- 0
    failReasons <- rep(NA,nTests)
    v <- NULL
    
    currTest <- 0
    for (o in orgs) {
        for (z in types) {
            currTest <- currTest + 1
            message("\nRunning test ",currTest," of ",nTests," scheduled")
            message("Testing level ",z," from ",o," from latest version")
            tryCatch({
                ann <- getEnsemblAnnotation(o,z,v,versioned)
                if (is(ann,"data.frame")) {
                    message("Test ",currTest," successful!")
                    message("Created ",nrow(ann)," features")
                    message("Sample data:")
                    print(head(ann))
                    succ <- succ + 1
                }
            },error=function(e) {
                message("Test ",currTest," failed with error:")
                message(e$message)
                fail <- fail + 1
                failReasons[currTest] <- paste("Test ",currTest,":",
                    message(e$message),sep="")
            },finally="")
        }
    }
    
    message("Testing finished!\n")
    message("Summary")
    message("==================================================\n")
    message("Succesful tests: ",succ," out of ",nTests)
    message("Failed tests: ",fail," out of ",nTests)
    message(" ")
    
    d <- which(is.na(failReasons))
    if (length(d) == nTests) # All successful
        return(NULL)
    else {
        message("Check the output for failure details")
        if (length(d) > 0) # If not, all failed!
            failReasons <- failReasons[-d]
        return(failReasons)
    }
    message("Bye!\n")
}

testUcsc <- function(orgs,refdbs,types,versioned=FALSE) {
    if (.Platform$OS.type != "unix") {
        message("Cannot run 3' UTR tests from UCSC on non Unix/Linux machines!")
        return(NULL)
    }
    
    nTests <- length(orgs)*length(refdbs)*length(types)
    message("Scheduling ",nTests," tests")
    message("==================================================\n")
    
    succ <- fail <- 0
    failReasons <- rep(NA,nTests)
    
    if ("tair10" %in% orgs)
        orgs <- orgs[-which(orgs=="tair10")]
    
    dbCreds <- .getUcscCredentials()
    drv <- dbDriver("MySQL")
    currTest <- 0
    
    for (o in orgs) {
        message("==========> Now testing ",getUcscOrganism(o))
        message("Connecting to UCSC database ",getUcscOrganism(o),"...\n")
        con <- dbConnect(drv,user=dbCreds[2],password=NULL,
            dbname=getUcscOrganism(o),host=dbCreds[1])
        for (r in refdbs) {
            for (t in types) {
                currTest <- currTest + 1
                message("---------------------------------------------------")
                message("Running test ",currTest," of ",nTests," scheduled")
                message("Source: ", r)
                message("Type  : ", t)
                query <- .getUcscQuery(org=o,type=t,refdb=r,versioned=versioned)
                message("Query is: ",query)
                tryCatch({
                    dat <- dbGetQuery(con,query)
                    if (is.data.frame(dat)) {
                        message("Test ",currTest," successful!")
                        message("Fetched ",nrow(dat)," features")
                        message("Sample data:")
                        print(head(dat))
                        succ <- succ + 1
                    }                    
                },error=function(e) {
                    message("Test ",currTest," failed with error:")
                    message(e$message)
                    fail <- fail + 1
                    failReasons[currTest] <- paste("Test ",currTest,":",
                        message(e$message),sep="")
                },finally="")
                message("---------------------------------------------------\n")
            }
        }
        message("Disconnecting from UCSC database...")
        dbDisconnect(con)
    }
    
    message("Testing finished!\n")
    message("Summary")
    message("==================================================\n")
    message("Succesful tests: ",succ," out of ",nTests)
    message("Failed tests: ",fail," out of ",nTests)
    message(" ")
    
    d <- which(is.na(failReasons))
    if (length(d) == nTests) # All successful
        return(NULL)
    else {
        message("Check the output for failure details")
        if (length(d) > 0) # If not, all failed!
            failReasons <- failReasons[-d]
        return(failReasons)
    }
    message("Bye!\n")
}

testUcscUtr <- function(orgs,refdbs,versioned=FALSE) {
    if (.Platform$OS.type != "unix") {
        message("Cannot run 3' UTR tests from UCSC on non Unix/Linux machines!")
        return(NULL)
    }
    
    nTests <- length(orgs)*length(refdbs)
    message("Scheduling ",nTests," tests")
    message("==================================================\n")
    
    succ <- fail <- 0
    failReasons <- rep(NA,nTests)
    
    if ("tair10" %in% orgs)
        orgs <- orgs[-which(orgs=="tair10")]
    
    currTest <- 0
    for (o in orgs) {
        message("==========> Now testing ",getUcscOrganism(o))
        for (r in refdbs) {
            currTest <- currTest + 1
            message("---------------------------------------------------")
            message("Running test ",currTest," of ",nTests," scheduled")
            message("Source: ", r)
            message("Type  : utr")
            # For logging only
            if (r=="refseq" && versioned) {
                query <- .getUcscRefseqVersionedUtrQuery()
                message("Query is: ",query)
            }
            tryCatch({
                dat <- getUcscUtr(o,r,versioned,.rmysql=TRUE)
                if (is(dat,"GRanges")) {
                    message("Test ",currTest," successful!")
                    message("Fetched ",length(dat)," features")
                    message("Sample data:")
                    print(head(dat))
                    succ <- succ + 1
                }                    
            },error=function(e) {
                message("Test ",currTest," failed with error:")
                message(e$message)
                fail <- fail + 1
                failReasons[currTest] <- paste("Test ",currTest,":",
                    message(e$message),sep="")
            },finally="")
            message("---------------------------------------------------\n")
        }
    }
    
    message("Testing finished!\n")
    message("Summary")
    message("==================================================\n")
    message("Succesful tests: ",succ," out of ",nTests)
    message("Failed tests: ",fail," out of ",nTests)
    message(" ")
    
    d <- which(is.na(failReasons))
    if (length(d) == nTests) # All successful
        return(NULL)
    else {
        message("Check the output for failure details")
        if (length(d) > 0) # If not, all failed!
            failReasons <- failReasons[-d]
        return(failReasons)
    }
    message("Bye!\n")
}


testCustomGtf <- function(gtf) {
    message("Custom GTF file is ",gtf)
    message("==================================================")
    
    parsed <- parseCustomGtf(gtf)
    txdb <- parsed$txdb
    map <- parsed$map
    
    nTests <- 12
    
    message("\nScheduling ",nTests," tests")
    message("==================================================")
    
    succ <- fail <- 0
    failReasons <- rep(NA,nTests)
    
    currTest <- 0
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing gene gene from GTF")
    tryCatch({
        ann <- .makeGeneGeneFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing gene exon from GTF")
    tryCatch({
        ann <- .makeGeneExonFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing summarized gene exon from GTF")
    tryCatch({
        ann <- .makeSumGeneExonFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing gene utr from GTF")
    tryCatch({
        ann <- .makeGeneUtrFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing summarized gene utr from GTF")
    tryCatch({
        ann <- .makeSumGeneUtrFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing transcript gene from GTF")
    tryCatch({
        ann <- .makeTranscriptGeneFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing summarized transcript gene from GTF")
    tryCatch({
        ann <- .makeSumTranscriptGeneFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing transcript exon from GTF")
    tryCatch({
        ann <- .makeTranscriptExonFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing summarized transcript exon from GTF")
    tryCatch({
        ann <- .makeSumTranscriptExonFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing transcript utr from GTF")
    tryCatch({
        ann <- .makeTranscriptUtrFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing summarized transcript utr from GTF")
    tryCatch({
        ann <- .makeSumTranscriptUtrFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    currTest <- currTest + 1
    message("\nRunning test ",currTest," of ",nTests," scheduled")
    message("Testing exon exon from GTF")
    tryCatch({
        ann <- .makeExonExonFromTxDb(txdb,map,FALSE)
        if (is(ann,"GRanges")) {
            message("Test ",currTest," successful!")
            message("Created ",length(ann)," features")
            message("Sample data:")
            print(head(ann))
            succ <- succ + 1
        }
    },error=function(e) {
        message("Test ",currTest," failed with error:")
        message(e$message)
        fail <- fail + 1
        failReasons[currTest] <- paste("Test ",currTest,":",
            message(e$message),sep="")
    },finally="")
    
    message("Testing finished!\n")
    message("Summary")
    message("==================================================\n")
    message("Succesful tests: ",succ," out of ",nTests)
    message("Failed tests: ",fail," out of ",nTests)
    message(" ")
    
    d <- which(is.na(failReasons))
    if (length(d) == nTests) # All successful
        return(NULL)
    else {
        message("Check the output for failure details")
        if (length(d) > 0) # If not, all failed!
            failReasons <- failReasons[-d]
        return(failReasons)
    }
    message("Bye!\n")
}

#testQuery <- function(query,org) {
#    dbCreds <- .getUcscCredentials()
#    drv <- dbDriver("MySQL")
#    con <- dbConnect(drv,user=dbCreds[2],password=NULL,dbname=org,
#        host=dbCreds[1])
#    rawAnn <- tryCatch(dbGetQuery(con,query),
#        error=function(e) print(e),
#        finally=dbDisconnect(con))
#    return(rawAnn)
#}
