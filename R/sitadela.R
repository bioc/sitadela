addAnnotation <- function(organisms,sources,db=getDbPath(),versioned=FALSE,
    forceDownload=TRUE,retries=5,rc=NULL) {
    
    complete <- FALSE
    times <- 1
    pv <- tryCatch(packageVersion("sitadela"),error=function(e) return(""),
        finally="")
    
    message("\n********************************************************")
    message("This is sitadela ",pv," genomic region annotation builder")
    message("********************************************************")
    while(!complete && times<=retries) {
        message("\n========================================================")
        message(format(Sys.time(),"%Y-%m-%d %H:%M:%S")," - Try ",times)
        message("========================================================\n")

        buildResult <- .annotationWorker(organisms=organisms,sources=sources,
            db=db,versioned=versioned,forceDownload=forceDownload,rc=rc)

        complete <- buildResult$complete

        if (!complete) {
            message("-------------------------------------------------------")
            message("Failed annotation builds:")
            for (i in seq_along(buildResult$failed))
                message("  -> Organism: ",buildResult$failed[[i]]$org," - ",
                    "Source: ",buildResult$failed[[i]]$refdb," - ",
                    "Version: ",buildResult$failed[[i]]$ver," - ",
                    "Versioned IDs: ",buildResult$failed[[i]]$tv)
            message("Will retry ",retries-times," times...")
            
            newRun <- .collapseFailures(buildResult$failed)
            .annotationWorker(organisms=newRun$organisms,
                sources=newRun$sources,db=db,versioned=versioned,
                forceDownload=forceDownload,rc=rc)
            message("-------------------------------------------------------\n")
        }
        else {
            message("\n-------------------------------------------------------")
            message("Building process complete!")
            message("-------------------------------------------------------\n")
        }

        times <- times + 1
    }

    #.annotationWorker(organisms=organisms,sources=sources,db=db,
    #    versioned=versioned,forceDownload=forceDownload,rc=rc)
}

.annotationWorker <- function(organisms,sources,db=getDbPath(),versioned=FALSE,
    forceDownload=TRUE,rc=NULL) {
    if (missing(organisms))
        organisms <- getSupportedOrganisms()
    if (missing(sources))
        sources <- getSupportedRefDbs()
    
    orgIsList <- FALSE
    if (!is.list(organisms) && is.character(organisms)
        && "ensembl" %in% sources)
        warning("When ensembl is in the annotation sources to download, it is ",
            "advised to provide organisms as\na named list with names the ",
            "requested organisms and list members the Ensembl versions.\n",
            "Otherwise, the latest Ensembl version for each organism will be ",
            "used.",immediate.=TRUE)
    if (is.list(organisms)) {
        if (is.null(names(organisms)))
            stop("When organisms is a list, it must be named!")
        orgList <- organisms
        organisms <- names(organisms)
        orgIsList <- TRUE
    }
    
    .checkTextArgs("organisms",organisms,getSupportedOrganisms(),multiarg=TRUE)
    .checkTextArgs("sources",sources,getSupportedRefDbs(),multiarg=TRUE)
    
    if (!is.logical(versioned))
        stop("The versioned argument must be TRUE or FALSE")
    if (!is.logical(forceDownload))
        stop("The forceDownload argument must be TRUE or FALSE")
    
    if (!requireNamespace("GenomeInfoDb"))
        stop("R package GenomeInfoDb is required to construct annotation ",
            "stores!")
    
    # Check database path
    if (!dir.exists(dirname(db)))
        dir.create(dirname(db),recursive=TRUE,mode="0755")
    
    # Initialize or open the annotation SQLite datatabase
    message("Opening sitadela SQLite database ",db)
    con <- initDatabase(db)
    
    # Main fault (connection or other) marker
    completed <- TRUE
    failures <- 0
    failed <- list()
    
    for (s in sources) {
        for (o in organisms) {
            # Retrieving genome info. We will be inserting the seqinfo to the
            # sqlite multiple times for the sake of simplicity regarding later
            # deletion by foreign key cascade.
            message("Retrieving genome information for ",o," from ",s)
            sf <- getSeqInfo(o)
            
            # Now, we must derive versioning. For Ensembl it's obvious as it has
            # official versions. For UCSC we need to keep a date track. Then 
            # inside metaseqR, if date not provided, it will automatically
            # detect the latest one (as in Ensembl with versions, if not
            # provided).
            if (s == "ensembl") {
                if (orgIsList)
                    vs <- orgList[[o]]
                else {
                    vss <- .getUcscToEnsembl(o)
                    vs <- vss[length(vss)]
                }
                vs <- .validateEnsemblVersions(o,vs)
                if (is.null(vs)) { # Something went wrong... Get latest again
                    vss <- .getUcscToEnsembl(o)
                    vs <- vss[length(vss)]
                }
            }
            else if (s %in% getSupportedUcscDbs())
                vs <- format(Sys.Date(),"%Y%m%d")
                        
            for (v in vs) {
                # Retrieve gene annotations
                if (.annotationExists(con,o,s,v,"gene",versioned) 
                    && !forceDownload)
                    message("Gene annotation for ",o," from ",s," version ",v,
                        " has already been created and will be skipped.\nIf ",
                        "you wish to recreate it choose forceDownload = TRUE.")
                else {
                    message("Retrieving gene annotation for ",o," from ",s,
                        " version ",v)
                    tryCatch({
                        ann <- getAnnotation(o,"gene",refdb=s,ver=v,
                            tv=versioned,rc=rc)
                    
                        # First drop if previously exists
                        nr <- .dropAnnotation(con,o,s,v,"gene",versioned)
                        # Then insert to the contents table so as to get the 
                        # content id to attach in the annotation table
                        nr <- .insertContent(con,o,s,v,"gene",versioned)
                        nid <- .annotationExists(con,o,s,v,"gene",versioned,
                            out="id")
                        # If something happens, the whole procedure will break
                        # anyway
                        # Add content_id
                        ann$content_id <- rep(nid,nrow(ann))
                        sfGene <- sf
                        sfGene$content_id <- rep(nid,nrow(sfGene))
                        # Write genes and seqinfo
                        dbWriteTable(con,"gene",ann,row.names=FALSE,append=TRUE)
                        dbWriteTable(con,"seqinfo",sfGene,row.names=FALSE,
                            append=TRUE)
                    },error=function(e) {
                        message("Possible connection failure! Marking...")
                        message("Caught error: ",e$message)
                        completed <- FALSE
                        failures <- failures + 1
                        failed[[failures]] <- 
                            list(org=o,refdb=s,ver=v,tv=versioned)
                    },finally="")
                }
                
                # Retrieve transcript annotations
                if (.annotationExists(con,o,s,v,"transcript",versioned) 
                    && !forceDownload)
                    message("Transcript annotation for ",o," from ",s,
                        " version ",v," has already been created and will be ",
                        "skipped.\nIf you wish to recreate it choose ",
                        "forceDownload = TRUE.")
                else {
                    message("Retrieving transcript annotation for ",o," from ",
                        s," version ",v)
                    tryCatch({
                        ann <- getAnnotation(o,"transcript",refdb=s,ver=v,
                            tv=versioned,rc=rc)
                        nr <- .dropAnnotation(con,o,s,v,"transcript",versioned)
                        nr <- .insertContent(con,o,s,v,"transcript",versioned)
                        nid <- .annotationExists(con,o,s,v,"transcript",
                            versioned,out="id")
                        ann$content_id <- rep(nid,nrow(ann))
                        sfTranscript <- sf
                        sfTranscript$content_id <- rep(nid,nrow(sfTranscript))
                        dbWriteTable(con,"transcript",ann,row.names=FALSE,
                            append=TRUE)
                        dbWriteTable(con,"seqinfo",sfTranscript,row.names=FALSE,
                            append=TRUE)
                    },error=function(e) {
                        message("Possible connection failure! Marking...")
                        message("Caught error: ",e$message)
                        completed <- FALSE
                        failures <- failures + 1
                        failed[[failures]] <- 
                            list(org=o,refdb=s,ver=v,tv=versioned)
                    },finally="")
                }
                
                # Then summarize the transcripts and write again with type 
                # sum_transcript
                if (.annotationExists(con,o,s,v,"summarized_transcript",
                    versioned) && !forceDownload)
                    message("Summarized transcript annotation for ",o," from ",
                        s," version ",v," has already been created and will ",
                        "be skipped.\nIf you wish to recreate it choose ",
                        "forceDownload = TRUE.")
                else {
                    if (!.annotationExists(con,o,s,v,"transcript",versioned)) 
                        stop("Transcript annotation for ",o," from ",s,
                            " version ",v," is required in order to build ",
                            "predefined merged transcript regions for read ",
                            "counting.\nPlease rerun the addAnnotation ",
                            "function with appropriate parameters.")
                    annGr <- .loadPrebuiltAnnotation(con,o,s,v,"transcript",
                        versioned)
                    message("Merging transcripts for ",o," from ",s,
                        " version ",v)
                    annList <- reduceTranscripts(annGr)
                    ann <- as.data.frame(annList$model)
                    ann <- ann[,c(1,2,3,6,7,5,8,9)]
                    names(ann)[1] <- "chromosome"
                    ann$chromosome <- as.character(ann$chromosome)
                    ann <- ann[order(ann$chromosome,ann$start),]
                    
                    nr <- .dropAnnotation(con,o,s,v,"summarized_transcript",
                        versioned)
                    nr <- .insertContent(con,o,s,v,"summarized_transcript",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,"summarized_transcript",
                        versioned,out="id")
                    ann$content_id <- rep(nid,nrow(ann))
                    sfSumTranscript <- sf
                    sfSumTranscript$content_id <- rep(nid,nrow(sfSumTranscript))
                    dbWriteTable(con,"summarized_transcript",ann,
                        row.names=FALSE,append=TRUE)
                    dbWriteTable(con,"seqinfo",sfSumTranscript,row.names=FALSE,
                        append=TRUE)
                }
                
                # Retrieve 3' UTR annotations
                if (.annotationExists(con,o,s,v,"utr",versioned) 
                    && !forceDownload)
                    message("3' UTR annotation for ",o," from ",s," version ",
                        v," has already been created and will be skipped.\nIf ",
                        "you wish to recreate it choose forceDownload = TRUE.")
                else {
                    message("Retrieving 3' UTR annotation for ",o," from ",s,
                        " version ",v)
                    tryCatch({
                        ann <- getAnnotation(o,"utr",refdb=s,ver=v,tv=versioned,
                            rc=rc)
                        nr <- .dropAnnotation(con,o,s,v,"utr",versioned)
                        nr <- .insertContent(con,o,s,v,"utr",versioned)
                        nid <- .annotationExists(con,o,s,v,"utr",versioned,
                            out="id")
                        ann$content_id <- rep(nid,nrow(ann))
                        sfUtr <- sf
                        sfUtr$content_id <- rep(nid,nrow(sfUtr))
                        dbWriteTable(con,"utr",ann,row.names=FALSE,
                            append=TRUE)
                        dbWriteTable(con,"seqinfo",sfUtr,row.names=FALSE,
                            append=TRUE)
                    },error=function(e) {
                        message("Possible connection failure! Marking...")
                        message("Caught error: ",e$message)
                        completed <- FALSE
                        failures <- failures + 1
                        failed[[failures]] <- 
                            list(org=o,refdb=s,ver=v,tv=versioned)
                    },finally="")
                }
                
                # Then summarize the 3'utrs per gene and write again with type 
                # sum_utr
                if (.annotationExists(con,o,s,v,"summarized_3utr",versioned)
                    && !forceDownload)
                    message("Summarized 3' UTR annotation for ",o," from ",
                        s," version ",v," has already been created and will ",
                        "be skipped.\nIf you wish to recreate it choose ",
                        "forceDownload = TRUE.")
                else {
                    if (!.annotationExists(con,o,s,v,"utr",versioned)) 
                        stop("3' UTR annotation for ",o," from ",s," version ",
                            vs," is required in order to build predefined ",
                            "merged 3' UTR regions for read counting.\nPlease ",
                            "rerun the addAnnotation function with ",
                            "appropriate parameters.")
                    annGr <- .loadPrebuiltAnnotation(con,o,s,v,"utr",versioned)
                    message("Merging gene 3' UTRs for ",o," from ",s,
                        " version ",v)
                    annList <- reduceTranscripts(annGr)
                    ann <- as.data.frame(annList$model)
                    ann <- ann[,c(1,2,3,6,7,5,8,9)]
                    names(ann)[1] <- "chromosome"
                    ann$chromosome <- as.character(ann$chromosome)
                    ann <- ann[order(ann$chromosome,ann$start),]
                    nr <- .dropAnnotation(con,o,s,v,"summarized_3utr",versioned)
                    nr <- .insertContent(con,o,s,v,"summarized_3utr",versioned)
                    nid <- .annotationExists(con,o,s,v,"summarized_3utr",
                        versioned,out="id")
                    ann$content_id <- rep(nid,nrow(ann))
                    sfSumUtr <- sf
                    sfSumUtr$content_id <- rep(nid,nrow(sfSumUtr))
                    dbWriteTable(con,"summarized_3utr",ann,row.names=FALSE,
                        append=TRUE)
                    dbWriteTable(con,"seqinfo",sfSumUtr,row.names=FALSE,
                        append=TRUE)
                    
                    activeLength <- annList$length
                    nr <- .dropAnnotation(con,o,s,v,"active_utr_length",
                        versioned)
                    nr <- .insertContent(con,o,s,v,"active_utr_length",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,"active_utr_length",
                        versioned,out="id")
                    active <- data.frame(
                        name=names(activeLength),
                        length=activeLength,
                        content_id=rep(nid,length(activeLength))
                    )
                    dbWriteTable(con,"active_utr_length",active,row.names=FALSE,
                        append=TRUE)
                }
                
                # Then summarize the 3'utrs per transcript and write again with 
                # type sum_utr
                if (.annotationExists(con,o,s,v,"summarized_3utr_transcript",
                    versioned) && !forceDownload)
                    message("Summarized 3' UTR annotation per transcript for ",
                        o," from ",s," version ",v," has already been created ",
                        "and will be skipped.\nIf you wish to recreate it ",
                        "choose forceDownload = TRUE.")
                else {
                    if (!.annotationExists(con,o,s,v,"utr",versioned))
                        stop("3' UTR annotation per transcript for ",o," from ",
                            s," version ",v," is required in order to build ",
                            "predefined merged 3'UTR regions for read ",
                            "counting.\nPlease rerun the ",
                            "addAnnotation function with ",
                            "appropriate parameters.")
                    annGr <- 
                        .loadPrebuiltAnnotation(con,o,s,v,"utr",versioned)
                    message("Merging transcript 3' UTRs for ",o," from ",s,
                        " version ",v)
                    annList <- reduceTranscriptsUtr(annGr)
                    ann <- as.data.frame(annList$model)
                    ann <- ann[,c(1,2,3,6,7,5,8,9)]
                    names(ann)[1] <- "chromosome"
                    ann$chromosome <- as.character(ann$chromosome)
                    ann <- ann[order(ann$chromosome,ann$start),]
                    nr <- .dropAnnotation(con,o,s,v,
                        "summarized_3utr_transcript",versioned)
                    nr <- .insertContent(con,o,s,v,"summarized_3utr_transcript",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,
                        "summarized_3utr_transcript",versioned,out="id")
                    ann$content_id <- rep(nid,nrow(ann))
                    sfSumUtrTranscript <- sf
                    sfSumUtrTranscript$content_id <- 
                        rep(nid,nrow(sfSumUtrTranscript))
                    dbWriteTable(con,"summarized_3utr_transcript",ann,
                        row.names=FALSE,append=TRUE)
                    dbWriteTable(con,"seqinfo",sfSumUtrTranscript,
                        row.names=FALSE,append=TRUE)
                    
                    activeLength <- annList$length
                    nr <- .dropAnnotation(con,o,s,v,"active_trans_utr_length",
                        versioned)
                    nr <- .insertContent(con,o,s,v,"active_trans_utr_length",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,
                        "active_trans_utr_length",versioned,out="id")
                    active <- data.frame(
                        name=names(activeLength),
                        length=activeLength,
                        content_id=rep(nid,length(activeLength))
                    )
                    dbWriteTable(con,"active_trans_utr_length",active,
                        row.names=FALSE,append=TRUE)
                }
                
                # Retrieve exon annotations
                if (.annotationExists(con,o,s,v,"exon",versioned) 
                    && !forceDownload)
                    message("Exon annotation for ",o," from ",s," version ",v,
                        " has already been created and will be skipped.\nIf ",
                        "you wish to recreate it choose forceDownload = TRUE.")
                else {
                    message("Retrieving exon annotation for ",o," from ",s,
                        " version ",v)
                    tryCatch({
                        ann <- getAnnotation(o,"exon",refdb=s,ver=v,
                            tv=versioned,rc=rc)
                        nr <- .dropAnnotation(con,o,s,v,"exon",versioned)
                        nr <- .insertContent(con,o,s,v,"exon",versioned)
                        nid <- .annotationExists(con,o,s,v,"exon",versioned,
                            out="id")
                        ann$content_id <- rep(nid,nrow(ann))
                        sfExon <- sf
                        sfExon$content_id <- rep(nid,nrow(sfExon))
                        dbWriteTable(con,"exon",ann,row.names=FALSE,
                            append=TRUE)
                        dbWriteTable(con,"seqinfo",sfExon,row.names=FALSE,
                            append=TRUE)
                    },error=function(e) {
                        message("Possible connection failure! Marking...")
                        message("Caught error: ",e$message)
                        completed <- FALSE
                        failures <- failures + 1
                        failed[[failures]] <- 
                            list(org=o,refdb=s,ver=v,tv=versioned)
                    },finally="")
                }
                
                # Retrieve extended annotations
                if (.annotationExists(con,o,s,v,"transexon",versioned) 
                    && !forceDownload)
                    message("Extended exon annotation for ",o," from ",s,
                        " version ",v," has already been created and will be ",
                        "skipped.\nIf you wish to recreate it choose ",
                        "forceDownload = TRUE.")
                else {
                    message("Retrieving extended exon annotation for ",o,
                        " from ",s," version ",v)
                    tryCatch({
                        ann <- getAnnotation(o,"transexon",refdb=s,ver=v,
                            tv=versioned,rc=rc)
                        nr <- .dropAnnotation(con,o,s,v,"transexon",versioned)
                        nr <- .insertContent(con,o,s,v,"transexon",versioned)
                        nid <- .annotationExists(con,o,s,v,"transexon",
                            versioned,out="id")
                        ann$content_id <- rep(nid,nrow(ann))
                        sfTrExon <- sf
                        sfTrExon$content_id <- rep(nid,nrow(sfExon))
                        dbWriteTable(con,"transexon",ann,row.names=FALSE,
                            append=TRUE)
                        dbWriteTable(con,"seqinfo",sfTrExon,row.names=FALSE,
                            append=TRUE)
                    },error=function(e) {
                        message("Possible connection failure! Marking...")
                        message("Caught error: ",e$message)
                        completed <- FALSE
                        failures <- failures + 1
                        failed[[failures]] <- 
                            list(org=o,refdb=s,ver=v,tv=versioned)
                    },finally="")
                }
                
                # Then summarize the exons per gene and write again with type 
                # summarized_exon
                if (.annotationExists(con,o,s,v,"summarized_exon",versioned)
                    && !forceDownload)
                    message("Summarized exon annotation for ",o," from ",s,
                        " version ",v," has already been created and will be ",
                        "skipped.\nIf you wish to recreate it choose ",
                        "forceDownload = TRUE.")
                else {
                    if (!.annotationExists(con,o,s,v,"exon",versioned)) 
                        stop("Exon annotation for ",o," from ",s," version ",v,
                            " is required in order to build predefined merged ",
                            "exon regions for RNA-Seq (exon) coverage ",
                            "calculations.\nPlease rerun the ",
                            "addAnnotation function with ",
                            "appropriate parameters.")
                            
                    annGr <- .loadPrebuiltAnnotation(con,o,s,v,"exon",versioned)
                    message("Merging exons for ",o," from ",s," version ",v)
                    annList <- reduceExons(annGr)
                    ann <- as.data.frame(annList$model)
                    ann <- ann[,c(1,2,3,6,7,5,8,9)]
                    names(ann)[1] <- "chromosome"
                    ann$chromosome <- as.character(ann$chromosome)
                    ann <- ann[order(ann$chromosome,ann$start),]
                    nr <- .dropAnnotation(con,o,s,v,"summarized_exon",versioned)
                    nr <- .insertContent(con,o,s,v,"summarized_exon",versioned)
                    nid <- .annotationExists(con,o,s,v,"summarized_exon",
                        versioned,out="id")
                    ann$content_id <- rep(nid,nrow(ann))
                    sfSumExon <- sf
                    sfSumExon$content_id <- rep(nid,nrow(sfSumExon))
                    dbWriteTable(con,"summarized_exon",ann,row.names=FALSE,
                        append=TRUE)
                    dbWriteTable(con,"seqinfo",sfSumExon,row.names=FALSE,
                        append=TRUE)        
                            
                    activeLength <- annList$length
                    nr <- .dropAnnotation(con,o,s,v,"active_length",versioned)
                    nr <- .insertContent(con,o,s,v,"active_length",versioned)
                    nid <- .annotationExists(con,o,s,v,"active_length",
                        versioned,out="id")
                    active <- data.frame(
                        name=names(activeLength),
                        length=activeLength,
                        content_id=rep(nid,length(activeLength))
                    )
                    dbWriteTable(con,"active_length",active,row.names=FALSE,
                        append=TRUE)
                }
                
                # Then summarize the exons per transcript and write again with 
                # type summarized_transcript_exon
                if (.annotationExists(con,o,s,v,"summarized_transcript_exon",
                    versioned) && !forceDownload)
                    message("Summarized exon annotation per transcript for ",o,
                        " from ",s," version ",v," has already been created ",
                        "and will be skipped.\nIf you wish to recreate it ",
                        "choose forceDownload = TRUE.")
                else {
                    if (!.annotationExists(con,o,s,v,"transexon",versioned)) 
                        stop("Extended exon annotation for ",o," from ",s,
                            " version ",v," is required in order to build ",
                            "summarized exon annotation per transcript\n",
                            "Please rerun the addAnnotation ",
                            "function with appropriate parameters.")
                
                    annGr <- .loadPrebuiltAnnotation(con,o,s,v,"transexon",
                        versioned)
                    message("Merging exons for ",o," from ",s," version ",v)
                    annList <- reduceTranscriptsExons(annGr)
                    ann <- as.data.frame(annList$model)
                    ann <- ann[,c(1,2,3,6,7,5,8,9)]
                    names(ann)[1] <- "chromosome"
                    ann$chromosome <- as.character(ann$chromosome)
                    ann <- ann[order(ann$chromosome,ann$start),]
                    nr <- .dropAnnotation(con,o,s,v,
                        "summarized_transcript_exon",versioned)
                    nr <- .insertContent(con,o,s,v,"summarized_transcript_exon",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,
                        "summarized_transcript_exon",versioned,out="id")
                    ann$content_id <- rep(nid,nrow(ann))
                    sfSumTrExon <- sf
                    sfSumTrExon$content_id <- rep(nid,nrow(sfSumTrExon))
                    dbWriteTable(con,"summarized_transcript_exon",ann,
                        row.names=FALSE,append=TRUE)
                    dbWriteTable(con,"seqinfo",sfSumTrExon,row.names=FALSE,
                        append=TRUE)
                            
                    activeLength <- annList$length
                    nr <- .dropAnnotation(con,o,s,v,"active_trans_exon_length",
                        versioned)
                    nr <- .insertContent(con,o,s,v,"active_trans_exon_length",
                        versioned)
                    nid <- .annotationExists(con,o,s,v,
                        "active_trans_exon_length",versioned,out="id")
                    active <- data.frame(
                        name=names(activeLength),
                        length=activeLength,
                        content_id=rep(nid,length(activeLength))
                    )
                    dbWriteTable(con,"active_trans_exon_length",active,
                        row.names=FALSE,append=TRUE)
                }
            }
        }
    }
    
    dbDisconnect(con)
    
    return(list(
        completed=completed,
        failed=failed
    ))
}

# GTF only!
addCustomAnnotation <- function(gtfFile,metadata,db=getDbPath(),rewrite=TRUE) {
    # Check metadata
    if (is.null(metadata$organism))
        stop("An organism name must be provided with metadata!")
    if (is.null(metadata$source)) {
        warning("A source should be provided with metadata! Using 'inhouse'...",
            immediate.=TRUE)
        metadata$source <- "inhouse"
    }
    if (is.null(metadata$version)) {
        warning("A version should be provided with metadata! Using today...",
            immediate.=TRUE)
        metadata$version <- format(Sys.Date(),"%Y%m%d")
    }
    if (is.null(metadata$chromInfo)) {
        warning("Chromosomal lengths should be provided with metadata! ",
            "Only chromosome names will be available... ",immediate.=TRUE)
        metadata$chromInfo <- NULL
    }
    else {
        str <- metadata$chromInfo
        if (is.character(str)) {
            out <- tryCatch(open(Rsamtools::BamFile(str)),error=function(e) e)
            if (inherits(out,"error")) # Not a BAM file, try to read.delim
                metadata$chromInfo <- read.delim(str,row.names=1)
            else
                metadata$chromInfo <- .chromInfoFromBAM(str)
        }
        if (!is.data.frame(metadata$chromInfo))
            stop("metadata$chromInfo must be a data frame!")
    }
    
    # Check database path
    if (!dir.exists(dirname(db)))
        dir.create(dirname(db),recursive=TRUE,mode="0755")
    
    # Initialize or open the annotation SQLite datatabase
    message("Opening sitadela SQLite database ",db)
    con <- initDatabase(db)
    
    parsed <- parseCustomGtf(gtfFile)
    
    s <- tolower(metadata$source)
    o <- tolower(metadata$organism)
    v <- metadata$version
    
    # Retrieve gene annotations
    if (.annotationExists(con,o,s,v,"gene") && !rewrite)
        message("Gene annotation for ",o," from ",s," version ",v," has ",
            "already been created and will be skipped.\nIf you wish to ",
            "recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving gene annotation for ",o," from ",s," version ",v,
            " from ",gtfFile)
        ann <- annotationFromCustomGtf(parsed,type="gene",asdf=TRUE)
        nr <- .dropAnnotation(con,o,s,v,"gene")
        nr <- .insertContent(con,o,s,v,"gene",0,1)
        nid <- .annotationExists(con,o,s,v,"gene",FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfGene <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfGene$content_id <- rep(nid,nrow(sfGene))
        dbWriteTable(con,"gene",ann,row.names=FALSE,append=TRUE)
        dbWriteTable(con,"seqinfo",sfGene,row.names=FALSE,append=TRUE)
    }
    
    # Retrieve transcript annotations
    if (.annotationExists(con,o,s,v,"transcript") && !rewrite)
        message("Transcript annotation for ",o," from ",s," version ",v," has ",
            "already been created and will be skipped.\nIf you wish to ",
            "recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving transcript annotation for ",o," from ",s,
            " version ",v)
        ann <- annotationFromCustomGtf(parsed,type="transcript",asdf=TRUE)
        nr <- .dropAnnotation(con,o,s,v,"transcript")
        nr <- .insertContent(con,o,s,v,"transcript",0,1)
        nid <- .annotationExists(con,o,s,v,"transcript",FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfTranscript <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfTranscript$content_id <- rep(nid,nrow(sfTranscript))
        dbWriteTable(con,"transcript",ann,row.names=FALSE,append=TRUE)
        dbWriteTable(con,"seqinfo",sfTranscript,row.names=FALSE,append=TRUE)
    }
    
    # Then summarize the transcripts and write again with type sum_transcript
    if (.annotationExists(con,o,s,v,"summarized_transcript") && !rewrite)
        message("Summarized transcript annotation for ",o," from ",s,
            " version ",v," has already been created and will be skipped.\nIf ",
            "you wish to recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving summarized transcript annotation for ",o," from ",s,
            " version ",v)
        ann <- annotationFromCustomGtf(parsed,type="transcript",summarized=TRUE,
            asdf=TRUE)
        nr <- .dropAnnotation(con,o,s,v,"summarized_transcript")
        nr <- .insertContent(con,o,s,v,"summarized_transcript",0,1)
        nid <- .annotationExists(con,o,s,v,"summarized_transcript",FALSE,
            out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfSumTranscript <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfSumTranscript$content_id <- rep(nid,nrow(sfSumTranscript))
        dbWriteTable(con,"summarized_transcript",ann,row.names=FALSE,
            append=TRUE)
        dbWriteTable(con,"seqinfo",sfSumTranscript,row.names=FALSE,append=TRUE)
    }
    
    # Retrieve 3' UTR annotations
    if (.annotationExists(con,o,s,v,"utr") && !rewrite)
        message("3' UTR annotation for ",o," from ",s," version ",v," has ",
            "already been created and will be skipped.\nIf you wish to ",
            "recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving 3' UTR annotation for ",o," from ",s," version ",v)
        ann <- annotationFromCustomGtf(parsed,type="utr",asdf=TRUE)
        if (nrow(ann) > 0) {
            nr <- .dropAnnotation(con,o,s,v,"utr")
            nr <- .insertContent(con,o,s,v,"utr",0,1)
            nid <- .annotationExists(con,o,s,v,"utr",FALSE,out="id")
            ann$content_id <- rep(nid,nrow(ann))
            sfUtr <- .chromInfoToSeqInfoDf(metadata$chromInfo)
            sfUtr$content_id <- rep(nid,nrow(sfUtr))
            dbWriteTable(con,"utr",ann,row.names=FALSE,append=TRUE)
            dbWriteTable(con,"seqinfo",sfUtr,row.names=FALSE,append=TRUE)
        }
        else
            message("3' UTR annotation for ",o," from ",s," version ",v," is ",
                "not available in the provided GTF file.")
    }
    
    # Then summarize the 3'UTRs and write again with type sum_transcript
    if (.annotationExists(con,o,s,v,"summarized_3utr") && !rewrite)
        message("Summarized 3' UTR annotation for ",o," from ",s," version ",v,
            " has already been created and will be skipped.\nIf you wish to ",
            "recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving summarized 3' UTR annotation per gene for ",o,
            " from ",s," version ",v)
        ann <- annotationFromCustomGtf(parsed,type="utr",summarized=TRUE,
            asdf=TRUE)
        
        if (nrow(ann) > 0) {
            activeLength <- attr(ann,"activeLength")
            
            nr <- .dropAnnotation(con,o,s,v,"summarized_3utr")
            nr <- .insertContent(con,o,s,v,"summarized_3utr",0,1)
            nid <- .annotationExists(con,o,s,v,"summarized_3utr",FALSE,out="id")
            ann$content_id <- rep(nid,nrow(ann))
            sfSumUtr <- .chromInfoToSeqInfoDf(metadata$chromInfo)
            sfSumUtr$content_id <- rep(nid,nrow(sfSumUtr))
            dbWriteTable(con,"summarized_3utr",ann,row.names=FALSE,append=TRUE)
            dbWriteTable(con,"seqinfo",sfSumUtr,row.names=FALSE,append=TRUE)
            
            nr <- .dropAnnotation(con,o,s,v,"active_utr_length")
            nr <- .insertContent(con,o,s,v,"active_utr_length",0,1)
            nid <- .annotationExists(con,o,s,v,"active_utr_length",FALSE,
                out="id")
            active <- data.frame(
                name=names(activeLength),
                length=activeLength,
                content_id=rep(nid,length(activeLength))
            )
            dbWriteTable(con,"active_utr_length",active,row.names=FALSE,
                append=TRUE)
        }
        else
            message("3' UTR annotation for ",o," from ",s," version ",v," is ",
                "not available in the provided GTF file.")
    }
    
    # Then summarize the 3'utrs per transcript and write again with 
    # type sum_utr
    if (.annotationExists(con,o,s,v,"summarized_3utr_transcript") && !rewrite)
        message("Summarized 3' UTR annotation per transcript for ",o," from ",s,
            " version ",v," has already been created and will be skipped.\nIf ",
            "you wish to recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving summarized 3' UTR annotation per transcript for ",o,
            " from ",s," version ",v)
        ann <- annotationFromCustomGtf(parsed,type="transutr",summarized=TRUE,
            asdf=TRUE)

        if (nrow(ann) > 0) {
            activeLength <- attr(ann,"activeLength")
            nr <- .dropAnnotation(con,o,s,v,"summarized_3utr_transcript")
            nr <- .insertContent(con,o,s,v,"summarized_3utr_transcript",0,1)
            nid <- .annotationExists(con,o,s,v,"summarized_3utr_transcript",
                FALSE,out="id")
            ann$content_id <- rep(nid,nrow(ann))
            sfSumUtrTranscript <- .chromInfoToSeqInfoDf(metadata$chromInfo)
            sfSumUtrTranscript$content_id <- rep(nid,nrow(sfSumUtrTranscript))
            dbWriteTable(con,"summarized_3utr_transcript",ann,row.names=FALSE,
                append=TRUE)
            dbWriteTable(con,"seqinfo",sfSumUtrTranscript,row.names=FALSE,
                append=TRUE)
            
            nr <- .dropAnnotation(con,o,s,v,"active_trans_utr_length")
            nr <- .insertContent(con,o,s,v,"active_trans_utr_length",0,1)
            nid <- .annotationExists(con,o,s,v,"active_trans_utr_length",
                FALSE,out="id")
            active <- data.frame(
                name=names(activeLength),
                length=activeLength,
                content_id=rep(nid,length(activeLength))
            )
            dbWriteTable(con,"active_trans_utr_length",active,row.names=FALSE,
                append=TRUE)
        }
        else
            message("3' UTR annotation for ",o," from ",s," version ",v," is ",
                "not available in the provided GTF file.")
    }
    
    # Retrieve exon annotations
    if (.annotationExists(con,o,s,v,"exon") && !rewrite)
        message("Exon annotation for ",o," from ",s," version ",v," has ",
            "already been created and will be skipped.\nIf you wish to ",
            "recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving exon annotation for ",o," from ",s," version ",v)
        ann <- annotationFromCustomGtf(parsed,type="exon",asdf=TRUE)
        nr <- .dropAnnotation(con,o,s,v,"exon")
        nr <- .insertContent(con,o,s,v,"exon",0,1)
        nid <- .annotationExists(con,o,s,v,"exon",FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfExon <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfExon$content_id <- rep(nid,nrow(sfExon))
        dbWriteTable(con,"exon",ann,row.names=FALSE,append=TRUE)
        dbWriteTable(con,"seqinfo",sfExon,row.names=FALSE,append=TRUE)
    }
    
    # Then summarize the exons and write again with type sum_exon
    if (.annotationExists(con,o,s,v,"summarized_exon") && !rewrite)
        message("Summarized exon annotation for ",o," from ",s," version ",v,
            " has already been created and will be skipped.\nIf you wish to ",
            "to recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving summarized exon annotation for ",o," from ",s,
            " version ",v)
        ann <- annotationFromCustomGtf(parsed,type="exon",summarized=TRUE,
            asdf=TRUE)
        activeLength <- attr(ann,"activeLength")
        
        nr <- .dropAnnotation(con,o,s,v,"summarized_exon")
        nr <- .insertContent(con,o,s,v,"summarized_exon",0,1)
        nid <- .annotationExists(con,o,s,v,"summarized_exon",FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfSumExon <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfSumExon$content_id <- rep(nid,nrow(sfSumExon))
        dbWriteTable(con,"summarized_exon",ann,row.names=FALSE,append=TRUE)
        dbWriteTable(con,"seqinfo",sfSumExon,row.names=FALSE,append=TRUE)

        nr <- .dropAnnotation(con,o,s,v,"active_length")
        nr <- .insertContent(con,o,s,v,"active_length",0,1)
        nid <- .annotationExists(con,o,s,v,"active_length",FALSE,out="id")
        active <- data.frame(
            name=names(activeLength),
            length=activeLength,
            content_id=rep(nid,length(activeLength))
        )
        dbWriteTable(con,"active_length",active,row.names=FALSE,append=TRUE)
    }
    
    if (.annotationExists(con,o,s,v,"transexon") && !rewrite)
        message("Extended exon annotation for ",o," from ",s," version ",v,
            " has already been created and will be skipped.\nIf you wish to ",
            "to recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving extended exon annotation for ",o," from ",s,
            " version ",v)
        ann <- annotationFromCustomGtf(parsed,type="transexon",asdf=TRUE)
        
        nr <- .dropAnnotation(con,o,s,v,"transexon")
        nr <- .insertContent(con,o,s,v,"transexon",0,1)
        nid <- .annotationExists(con,o,s,v,"transexon",FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfTransExon <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfTransExon$content_id <- rep(nid,nrow(sfExon))
        dbWriteTable(con,"transexon",ann,row.names=FALSE,append=TRUE)
        dbWriteTable(con,"seqinfo",sfTransExon,row.names=FALSE,append=TRUE)
    }
    
    # Then summarize the exons per transcript and write again with type 
    # summarized_transcript_exon
    if (.annotationExists(con,o,s,v,"summarized_transcript_exon")
        && !rewrite)
        message("Summarized exon annotation per transcript for ",o," from ",
            s," version ",v," has already been created and will be skipped.\n",
            "If you wish to recreate it choose rewrite = TRUE.")
    else {
        message("Retrieving summarized transcript exon annotation for ",o,
            " from ",s," version ",v)
        ann <- annotationFromCustomGtf(parsed,type="transexon",summarized=TRUE,
            asdf=TRUE)
        activeLength <- attr(ann,"activeLength")
        
        nr <- .dropAnnotation(con,o,s,v,"summarized_transcript_exon")
        nr <- .insertContent(con,o,s,v,"summarized_transcript_exon",0,1)
        nid <- .annotationExists(con,o,s,v,"summarized_transcript_exon",
            FALSE,out="id")
        ann$content_id <- rep(nid,nrow(ann))
        sfSumTransExon <- .chromInfoToSeqInfoDf(metadata$chromInfo)
        sfSumTransExon$content_id <- rep(nid,nrow(sfSumTransExon))
        dbWriteTable(con,"summarized_transcript_exon",ann,row.names=FALSE,
            append=TRUE)
        dbWriteTable(con,"seqinfo",sfSumTransExon,row.names=FALSE,append=TRUE)

        nr <- .dropAnnotation(con,o,s,v,"active_trans_exon_length")
        nr <- .insertContent(con,o,s,v,"active_trans_exon_length",0,1)
        nid <- .annotationExists(con,o,s,v,"active_trans_exon_length",
            FALSE,out="id")
        active <- data.frame(
            name=names(activeLength),
            length=activeLength,
            content_id=rep(nid,length(activeLength))
        )
        dbWriteTable(con,"active_trans_exon_length",active,row.names=FALSE,
            append=TRUE)    
    }
}

# level and type must be re-organized to one single argument, as it is not
# dependent anymore to metaseqR terminology. Suggested:
# gene       : GRanges with genes from start to end
# transcript : GRanges with (summarized) transcripts from start to end
# utr        : GRanges with (summarized) 3' UTRs grouped per gene
# transexon  : GRanges with (summarized) exons grouped per transcript
# transutr   : GRanges with (summarized) 3' UTRs grouped per transcript
# exon       : GRanges with (summarized) exons
loadAnnotation <- function(genome,refdb,type=c("gene","transcript","utr",
    "transutr","transexon","exon"),version="auto",wtv=FALSE,
    db=file.path(system.file(package="sitadela"),"annotation.sqlite"),
    summarized=FALSE,asdf=FALSE,rc=NULL) {
    if (!requireNamespace("RSQLite"))
        stop("R package RSQLite is required to load annotation from database!")
    
    genome <- tolower(genome[1])
    refdb <- tolower(refdb[1])
    type <- type[1]
    .checkTextArgs("type",type,c("gene","transcript","utr","transexon",
        "transutr","exon"),multiarg=FALSE)
    if (version != "auto")
        .checkNumArgs("version",version,"numeric")
    
    # Check if local storage has been set
    onTheFly <- FALSE
    if (file.exists(db)) {
        # Open the connection
        drv <- dbDriver("SQLite")
        con <- dbConnect(drv,dbname=db)
        
        # Is the general resource (organism, source) installed?
        if (!.annotationExists(con,genome,refdb)) {
            warning("The requested annotation does not seem to exist in the ",
                "database! It will be loaded on the fly.\nConsider importing ",
                "it by using addAnnotation.")
            onTheFly <- TRUE
        }
        
        # If main source exists, decide on version
        if (!onTheFly) {
            if (version != "auto") {
                if (!.annotationExists(con,genome,refdb,version,
                    .annotationTypeFromInputArgs(type))) {
                    warning("The requested annotation version does not seem ",
                        "to exist! Have you run addAnnotation or possibly ",
                        "mispelled? Will use newest existing version.",
                        immediate.=TRUE)
                    version <- "auto"
                }
            }
            if (version == "auto") {
                # Check if annotation exists, has been performed before
                vers <- .installedVersions(con,genome,refdb)
                vers <- sort(vers,decreasing=TRUE)
                version <- vers[1]
            }
            ann <- .loadPrebuiltAnnotation(con,genome,refdb,version,type,wtv,
                summarized)
            dbDisconnect(con)
            
            if (asdf) {
                a <- attr(ann,"activeLength")
                ann <- as.data.frame(unname(ann))
                ann <- ann[,c(1,2,3,6,7,5,8,9)]
                names(ann)[1] <- "chromosome"
                if (!is.null(a))
                    attr(ann,"activeLength") <- a
                return(ann)
            }
            else
                return(ann)
        }
    }
    else
        onTheFly <- TRUE
        
    if (onTheFly) {
        if (genome %in% getSupportedOrganisms()
            && refdb %in% getSupportedRefDbs()) {
            message("Getting latest annotation on the fly for ",genome," from ",
                refdb)
            ann <- .loadAnnotationOnTheFly(genome,refdb,type,wtv,rc)
            if (asdf) {
                a <- attr(ann,"activeLength")
                ann <- as.data.frame(unname(ann))
                ann <- ann[,c(1,2,3,6,7,5,8,9)]
                names(ann)[1] <- "chromosome"
                if (!is.null(a))
                    attr(ann,"activeLength") <- a
                return(ann)
            }
            else
                return(ann)
        }
        else {
            stop("genome and refdb not in supported automatically download ",
                "annotation options. Please use importCustomAnnotation.")
        }
    }
}

importCustomAnnotation <- function(gtfFile,metadata,
    type=c("gene","transcript","utr","transexon","transutr","exon")) {
    type <- tolower(type[1])
    # Check metadata
    if (is.null(metadata$organism)) {
        tmpOrg <- paste("species",format(Sys.Date(),"%Y%m%d"),sep="_")
        warning("An organism name must be provided with metadata for ",
            "reporting purposes! Using ",tmpOrg,immediate.=TRUE)
        metadata$organism <- tmpOrg
    }

    if (is.null(metadata$source)) {
        tmpSource <- paste("source",format(Sys.Date(),"%Y%m%d"),sep="_")
        warning("A source should be provided with metadata for reporting ",
            "purposes ! Using ",tmpSource,immediate.=TRUE)
        metadata$source <- tmpSource
    }
    if (is.null(metadata$version)) {
        tmpVer <- paste("version",format(Sys.Date(),"%Y%m%d"),sep="_")
        warning("A version should be provided with metadata for reporting ",
            "purposes! Using ",tmpVer,immediate.=TRUE)
        metadata$version <- tmpVer
    }
    if (is.null(metadata$chromInfo)) {
        warning("Chromosomal lengths should be provided with metadata! ",
            "Only chromosome names will be available... ",immediate.=TRUE)
        metadata$chromInfo <- NULL
    }
    else {
        str <- metadata$chromInfo
        if (is.character(str) && file.exists(str)) {
            out <- tryCatch(open(Rsamtools::BamFile(str)),error=function(e) e)
            if (inherits(out,"error")) # Not a BAM file, try to read.delim
                metadata$chromInfo <- read.delim(str,row.names=1)
            else
                metadata$chromInfo <- .chromInfoFromBAM(str)
        }
        if (!is.data.frame(metadata$chromInfo))
            stop("metadata$chromInfo must be a data frame!")
    }
    
    # For display meta information
    s <- metadata$source
    o <- metadata$organism
    v <- metadata$version
    
    # Parse the GTF file... If something wrong, it will crash here
    parsed <- parseCustomGtf(gtfFile)
    
    switch(type,
        gene = {
            message("Retrieving gene annotation for ",o," from ",s," version ",
                v," from ",gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="gene")
        },
        transcript = {
            message("Retrieving transcript annotation for ",o," from ",s,
                " version ",v," from ",gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="transcript")
        },
        utr = {
            message("Retrieving summarized 3' UTR annotation per gene ","for ",
                o," from ",s," version ",v," from ",gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="utr",summarized=TRUE)
        },
        transutr = {
            message("Retrieving summarized 3' UTR annotation per ",
                "transcript for ",o," from ",s," version ",v," from ",
                gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="transutr",
                summarized=TRUE)
        },
        transexon = {
            message("Retrieving summarized exon annotation per transcript ",
                "for ",o," from ",s," version ",v," from ",gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="transexon",
                summarized=TRUE)
        },
        exon = {
            message("Retrieving exon annotation for ",o," from ",s," version ",
                v," from ",gtfFile)
            annGr <- annotationFromCustomGtf(parsed,type="exon",summarized=TRUE)
        }
    )
    
    return(annGr)
}

removeAnnotation <- function(org,refdb,ver=NULL,db=NULL) {
    if (missing(db) || is.null(db))
        con <- initDatabase(getDbPath())
    else {
        if (!is.character(db) || !file.exists(db))
            stop("The path to the sitadela database must be a valid path ",
                "to an existing file! Assuming default...")
    }
    con <- initDatabase(db)
    content <- getInstalledAnnotations(con)
    orgs <- unique(as.character(content$organism))
    sources <- unique(as.character(content$source))
    versions <- unique(content$version)
    
    if (!(org %in% orgs)) {
        message(org," organism was not found in the database! Doing nothing...")
        return()
    }
    if (!(refdb %in% sources)) {
        message(refdb," source was not found in the database! Doing nothing...")
        return()
    }
    if (!is.null(ver) && !(ver %in% versions)) {
        message(ver," version was not found in the database! Doing nothing...")
        return()
    }
    
    # Reopen as getInstalledAnnotations closes the connection
    con <- initDatabase(db)
    nr <- .dropAnnotation(con,org,refdb,ver)
    dbDisconnect(con)
    return(nr)
}

getInstalledAnnotations <- function(obj=NULL) {
    con <- .validateDbCon(obj)
    content <- .browseContent(con)
    dbDisconnect(con)
    return(content[,-1])
}

getUserAnnotations <- function(obj=NULL) {
    con <- .validateDbCon(obj)
    userContent <- .browseUserContent(con)
    dbDisconnect(con)
    return(userContent[,-1])
}

getAnnotation <- function(org,type,refdb="ensembl",ver=NULL,tv=FALSE,rc=NULL) {
    org <- tolower(org)
    switch(refdb,
        ensembl = { return(getEnsemblAnnotation(org,type,ver,tv)) },
        ucsc = { return(getUcscAnnotation(org,type,refdb,tv,rc=rc)) },
        refseq = { return(getUcscAnnotation(org,type,refdb,tv,rc=rc)) },
        ncbi = { return(getUcscAnnotation(org,type,refdb,tv,rc=rc)) }
    )
}

getEnsemblAnnotation <- function(org,type,ver=NULL,tv=FALSE) {
    if (org=="tair10")
        dat <- "plants_mart"
    else
        dat <- "ENSEMBL_MART_ENSEMBL"
    host <- .getHost(org,ver)
    message("Using Ensembl host ",host)
    mart <- useMart(biomart=dat,host=host,dataset=.getDataset(org))

    chrsExp <- paste("^",.getValidChrs(org),"$",sep="",collapse="|")
    bm <- tryCatch(
        getBM(attributes=.getAttributes(org,type,ver,tv),mart=mart),
        error=function(e) {
            message("Caught error: ",e)
            message("This error is most probably related to httr package ",
                "internals! Using fallback solution...")
            .myGetBM(attributes=.getAttributes(org,type,ver,tv),mart=mart)
        },
        finally=""
    )
    if (type=="gene") {
        ann <- data.frame(
            chromosome=paste("chr",bm$chromosome_name,sep=""),
            start=bm$start_position,
            end=bm$end_position,
            gene_id=bm$ensembl_gene_id,
            gc_content=if (org %in% 
                c("hg18","hg19","mm9","rn5","dm3","danrer7")) 
                bm$percentage_gc_content else bm$percentage_gene_gc_content,
            strand=ifelse(bm$strand==1,"+","-"),
            gene_name=if (org %in% c("hg18","hg19","mm9")) bm$external_gene_id 
                else bm$external_gene_name,
            biotype=bm$gene_biotype
        )
        rownames(ann) <- ann$gene_id
    }
    else if (type=="transcript") {
        ann <- data.frame(
            chromosome=paste("chr",bm$chromosome_name,sep=""),
            start=bm$transcript_start,
            end=bm$transcript_end,
            transcript_id=bm$ensembl_transcript_id,
            gene_id=bm$ensembl_gene_id,
            strand=ifelse(bm$strand==1,"+","-"),
            gene_name=if (org %in% c("hg18","hg19","mm9")) 
                bm$external_gene_id else bm$external_gene_name,
            biotype=bm$gene_biotype
        )
        rownames(ann) <- as.character(ann$transcript_id)
    }
    else if (type=="utr") {
        ann <- data.frame(
            chromosome=paste("chr",bm$chromosome_name,sep=""),
            start=bm$`3_utr_start`,
            end=bm$`3_utr_end`,
            tstart=bm$transcript_start,
            tend=bm$transcript_end,
            transcript_id=bm$ensembl_transcript_id,
            gene_id=bm$ensembl_gene_id,
            strand=ifelse(bm$strand==1,"+","-"),
            gene_name=if (org %in% c("hg18","hg19","mm9","tair10")) 
                bm$external_gene_id else bm$external_gene_name,
            biotype=bm$gene_biotype
        )
        ann <- correctTranscripts(ann)
        ann <- ann[,c("chromosome","start","end","transcript_id","gene_id",
            "strand","gene_name","biotype")]
    }
    else if (type=="exon") {
        ann <- data.frame(
            chromosome=paste("chr",bm$chromosome_name,sep=""),
            start=bm$exon_chrom_start,
            end=bm$exon_chrom_end,
            exon_id=bm$ensembl_exon_id,
            gene_id=bm$ensembl_gene_id,
            strand=ifelse(bm$strand==1,"+","-"),
            gene_name=if (org %in% c("hg18","hg19","mm9")) 
                bm$external_gene_id else bm$external_gene_name,
            biotype=bm$gene_biotype
        )
        # At some point we should number the exons returned here, but it is not
        # easy as there is heavy overlap... We are doing this at the 
        # summarization level...
        rownames(ann) <- ann$exon_id
    }
    else if (type=="transexon") {
        ann <- data.frame(
            chromosome=paste("chr",bm$chromosome_name,sep=""),
            start=bm$exon_chrom_start,
            end=bm$exon_chrom_end,
            exon_id=bm$ensembl_exon_id,
            transcript_id=bm$ensembl_transcript_id,
            strand=ifelse(bm$strand==1,"+","-"),
            gene_name=if (org %in% c("hg18","hg19","mm9")) 
                bm$external_gene_id else bm$external_gene_name,
            biotype=bm$gene_biotype
        )
    }
    ann <- ann[order(ann$chromosome,ann$start),]
    ann <- ann[grep(chrsExp,ann$chromosome),]
    ann$chromosome <- as.character(ann$chromosome)
    
    return(ann)
}

getUcscAnnotation <- function(org,type,refdb="ucsc",versioned=FALSE,
    chunkSize=500,rc=NULL) {
    if (!requireNamespace("RMySQL")) {
        rmysqlPresent <- FALSE
        warning("R package RMySQL is not present! Annotation will be ",
            "retrieved by downloading temporary files from UCSC and the usage
            of a temporary SQLite database...",immediate.=TRUE)
    }
    else
        rmysqlPresent <- TRUE
    if (!requireNamespace("RSQLite"))
        stop("R package RSQLite is required to use annotation from UCSC!")
    
    if (org=="tair10") {
        warning("Arabidopsis thaliana genome is not supported by UCSC Genome ",
            "Browser database! Switching to Ensembl...",immediate.=TRUE)
        return(getEnsemblAnnotation("tair10",type))
    }
    
    validChrs <- .getValidChrs(org)
    chrsExp <- paste("^",paste(validChrs,collapse="$|^"),"$",sep="")

    dbOrg <- getUcscOrganism(org)
    if (rmysqlPresent) {
        # The UTR case is handled later
        if (type != "utr") {
            dbCreds <- .getUcscCredentials()
            drv <- dbDriver("MySQL")
            con <- dbConnect(drv,user=dbCreds[2],password=NULL,dbname=dbOrg,
                host=dbCreds[1])
            type2 <- ifelse(type=="transexon","exon",type)
            query <- .getUcscQuery(org,type2,refdb,versioned)
            rawAnn <- dbGetQuery(con,query)
            dbDisconnect(con)
        }
    }
    else {
        tmpSqlite <- getUcscDbl(org,refdb,versioned)
        
        if (type != "utr") { # Otherwise direct download is used
            drv <- dbDriver("SQLite")
            con <- dbConnect(drv,dbname=tmpSqlite)
            query <- .getUcscQuery(org,type,refdb,versioned)
            rawAnn <- dbGetQuery(con,query)
            dbDisconnect(con)
        }
    }
    if (type=="gene") {
        ann <- rawAnn
        ann <- ann[grep(chrsExp,ann$chromosome,perl=TRUE),]
        ann$chromosome <- as.character(ann$chromosome)
        rownames(ann) <- ann$gene_id
        gcContent <- getGcContent(ann,org)
        ann$gc_content <- gcContent
        rownames(ann) <- ann$gene_id
    }
    else if (type=="transcript") {
        ann <- rawAnn
        ann <- ann[grep(chrsExp,ann$chromosome,perl=TRUE),]
        d <- which(duplicated(ann))
        if (length(d) > 0)
            ann <- ann[-d,]
        ann$chromosome <- as.character(ann$chromosome)
        ann$transcript_id <- as.character(ann$transcript_id)
        ann$gene_id <- as.character(ann$transcript_id)
        # There are still duplicated transcript ids
        d <- which(duplicated(ann$transcript_id))
        iter <- 1
        while(length(d) > 0) {
            ann$transcript_id[d] <- paste(ann$transcript_id[d],iter,sep="_")
            iter <- iter + 1
            d <- which(duplicated(ann$transcript_id))
        }
        rownames(ann) <- ann$transcript_id
        ann <- ann[,c(1,2,3,4,8,5:7)]
    }
    else if (type=="exon" || type=="transexon") {
        rawAnn <- rawAnn[grep(chrsExp,rawAnn$chromosome,perl=TRUE),]
        exList <- cmclapply(as.list(seq_len(nrow(rawAnn))),function(x,d) {
            r <- d[x,,drop=FALSE]
            starts <- as.numeric(strsplit(r[,"start"],",")[[1]])
            ends <- as.numeric(strsplit(r[,"end"],",")[[1]])
            nexons <- length(starts)
            exonNumbering <- seq_len(nexons)
            if (r[,"strand"] == "-")
                exonNumbering <- rev(exonNumbering)
            ret <- data.frame(
                rep(r[,"chromosome"],nexons),
                starts,ends,
                paste(r[,"exon_id"],"_e",exonNumbering,sep=""),
                rep(r[,"strand"],nexons),
                rep(r[,"gene_id"],nexons),
                rep(r[,"gene_name"],nexons),
                rep(r[,"biotype"],nexons)
            )
            names(ret) <- names(r)
            rownames(ret) <- ret$exon_id
            return(ret)
        },rawAnn,rc=rc)
        
        # For some reason rbind takes ages for large datasets... We have to 
        # split in chunks of 1000
        N <- length(exList)
        mo <- N%%chunkSize
        if (mo == 0) {
            fl <- N/chunkSize
            fac <- factor(rep(seq_len(fl),each=chunkSize))
        }
        else {
            fl <- (N-mo)/chunkSize
            fac <- factor(c(rep(seq_len(fl),each=chunkSize),rep(fl,mo)))
        }
        exChunkedList <- split(exList,fac)
        # Merge the chunks
        tmp <- cmclapply(names(exChunkedList),function(n,X) {
            message("Binding chunk ",n,"...")
            return(do.call("rbind",X[[n]]))
        },exChunkedList,rc=rc)
        # Final merge
        message("Binding all chunks...")
        tmpAnn <- do.call("rbind",tmp)
        
        ann <- data.frame(
            chromosome=as.character(tmpAnn$chromosome),
            start=tmpAnn$start,
            end=tmpAnn$end,
            exon_id=as.character(tmpAnn$exon_id),
            gene_id=as.character(tmpAnn$gene_id),
            strand=as.character(tmpAnn$strand),
            gene_name=as.character(tmpAnn$gene_name),
            biotype=as.character(tmpAnn$biotype)
        )
        rownames(ann) <- ann$exon_id
        
        if (type == "transexon")
            names(ann)[5] <- "transcript_id"
    }
    else if (type=="utr") {
        # We are already supposed to have the necessary elements to construct
        # the required data frame. Should be something like
        # 1. Read the GTF as TxDb
        # 2. Import the GTF with rtracklayer
        # 3. Call the 3UTR function of TxDb on (1)
        # 4. Construct a map to add gene_name and other things (essentially
        #    the biotype if we make it, if not we have to connect it from the
        #    respective gene annotation during the pipeline execution)
        # 5. Add the mapped elements to the GRanges/data.frame
        # 6. Return a data.frame
        #
        # All the process is streamlined in getUcscUtr function
        
        preAnn <- getUcscUtr(org,refdb,versioned,.rmysql=rmysqlPresent)
        preAnn <- as.data.frame(preAnn)
        preAnn <- preAnn[,c(1,2,3,6,7,5,8,9)]
        preAnn <- preAnn[grep(chrsExp,preAnn$seqnames,perl=TRUE),]
        names(preAnn) <- c("chromosome","start","end","transcript_id","gene_id",
            "strand","gene_name","biotype")
        # preAnn now has exon names as rownames... OK...
        #rownames(preAnn) <- paste("T",seq_len(nrow(preAnn)),sep="_")
        ann <- preAnn
    }
    
    ann <- ann[order(ann$chromosome,ann$start),]
    return(ann)
}

getUcscUtr <- function(org,refdb="ucsc",versioned=FALSE,.rmysql=FALSE) {
    org <- tolower(org[1])
    refdb <- tolower(refdb[1])
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    .checkTextArgs("refdb",refdb,getSupportedUcscDbs())
    
    if (!requireNamespace("GenomicFeatures"))
        stop("Bioconductor package GenomicFeatures is required!")
        
    # If RefSeq and versioned, the we must either:
    # 1) Run an SQL query, fetch the results and write the genePred file so it
    #    can be parsed by genePredToGtf
    # 2) Download the refGene and gbCdnaInfo tables locally, create a local
    #    SQLite, run a concat query, create the genePred and parse...
    # The function should create the table and write .gz in session tmpdir
    if (refdb=="refseq" && versioned)
        tableUtr <- .makeUcscRefseqUtrTable(org,.rmysql)
    else {
        httpBase <- paste("http://hgdownload.soe.ucsc.edu/goldenPath/",
        getUcscOrganism(org),"/database/",sep="")
        tableUtr <- getUcscTableNameUtr(org,refdb) # Need one table
        message("  Retrieving table ",tableUtr," for 3'UTR annotation ",
            "generation from ",refdb," for ",org)
        download.file(paste(httpBase,paste(tableUtr,".txt.gz",sep=""),sep=""),
            file.path(tempdir(),paste(tableUtr,".txt.gz",sep="")),quiet=TRUE)
    }
    
    # Do the conversion stuff. AS there is no easy way to check if genePredToGtf
    # exists in the system, we should download it on the fly (once for the 
    # session). If no Linux machine, then problem.
    genePredToGtf <- file.path(tempdir(),"genePredToGtf")
    if (!file.exists(file.path(tempdir(),"genePredToGtf"))) {
        message("  Retrieving genePredToGtf tool")
        download.file(
        "http://hgdownload.soe.ucsc.edu/admin/exe/linux.x86_64/genePredToGtf",
            genePredToGtf,quiet=TRUE
        )
        system(paste("chmod 775",genePredToGtf))
    }
    
    # Then do the conversion... No need for windows case as Kent tools do not
    # work in Windows anyway
    gtfFile <- file.path(tempdir(),paste(tableUtr,".gtf",sep=""))
    message("  Converting ",tableUtr," to GTF")
    tmpName <- file.path(tempdir(),paste(format(Sys.time(),"%Y%m%d%H%M%S"),
        "tgtf",sep="."))
    commandUcsc <- paste(
        "zcat ",file.path(tempdir(),paste(tableUtr,".txt.gz",sep=""))," | ",
        "cut -f1-10 - | ",genePredToGtf," file stdin ",tmpName," -source=",
        tableUtr," -utr && grep -vP '\t\\.\t\\.\t' ",tmpName," > ",gtfFile,
        sep=""
    )
    commandRefSeq <- paste(
        "zcat ",file.path(tempdir(),paste(tableUtr,".txt.gz",sep=""))," | ",
        "cut -f2- | ",genePredToGtf," file stdin ",tmpName," -source=",
        tableUtr," -utr && grep -vP '\t\\.\t\\.\t' ",tmpName," > ",gtfFile,
        sep=""
    )
    command <- commandRefSeq
    # There is an exception for organisms that do not exist in UCSC databases
    # so we must use RefSeq
    ucscUnsup <- c("rn5","rn6","dm3","dm6","danrer7","danrer10","danrer11",
        "pantro4","pantro5","susscr3","susscr11","equcab2")
    if (refdb == "ucsc" && !(org %in% ucscUnsup))
        command <- commandUcsc
    
    # Run the command and process the data
    message("Executing: ",command)
    system(command)
    parsed <- parseCustomGtf(gtfFile)
    return(.makeGeneUtrFromTxDb(parsed$txdb,parsed$map,asdf=FALSE))
}

getGcContent <- function(ann,org) {
    if (missing(ann))
        stop("A valid annotation data frame must be provided in order to ",
            "retrieve GC-content.")
    org <- tolower(org[1])
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    # Convert annotation to GRanges
    message("Converting annotation to GenomicRanges object...")
    annGr <- tryCatch(GRanges(ann),error=function(e) {
        if (packageVersion("GenomicRanges")<1.14)
            return(GRanges(
                seqnames=Rle(ann[,1]),
                ranges=IRanges(start=ann[,2],end=ann[,3]),
                strand=Rle(ann[,6]),
                name=as.character(ann[,4])
            ))
        else
            return(makeGRangesFromDataFrame(
                df=ann,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            ))
    },finaly="")
    
    bsg <- loadBsGenome(org)
    if (is(bsg,"BSgenome")) {
        message("Getting DNA sequences...")
        seqs <- tryCatch(getSeq(bsg,names=annGr),error=function(e) {
            warning("Caught error ",e,immediate.=TRUE)
            message("Cannot get ",org," sequences! Returning NA GC content...")
            return(NA)
        },finally="")
        if (!is.na(seqs[1])) {
            message("Getting GC content...")
            freqMatrix <- alphabetFrequency(seqs,as.prob=TRUE,baseOnly=TRUE)
            gcContent <- apply(freqMatrix,1,function(x) round(100*sum(x[2:3]),
                digits=2))
        }
        else
            gcContent <- rep(NA,nrow(ann))
    }
    else
        gcContent <- rep(NA,nrow(ann))
    names(gcContent) <- as.character(ann[,4])
    return(gcContent)
}

getSeqInfo <- function(org,asSeqinfo=FALSE) {
    sf <- tryCatch(.chromInfoWrapperGID(getUcscOrganism(org)),
        error=function(e) {
            message("GenomeInfoDb chrom info fetch mechanism failed with the ",
                "following error: ")
            message(e)
            message("")
            message("Trying a direct download...")
            getChromInfo(getUcscOrganism(org))
        },finally="")
    rownames(sf) <- as.character(sf[,1])
    sf <- sf[.getValidChrs(org),]
    if (asSeqinfo)
        return(Seqinfo(seqnames=sf[,1],seqlengths=sf[,2],
            isCircular=sf[,3],genome=getUcscOrganism(org)))
    else
        return(data.frame(chromosome=sf[,1],length=as.integer(sf[,2])))
}

getUcscOrganism <- function(org) {
    switch(org,
        hg18 = { return("hg18") },
        hg19 = { return("hg19") },
        hg38 = { return("hg38") },
        mm9 = { return("mm9") },
        mm10 = { return("mm10") },
        rn5 = { return("rn5") },
        rn6 = { return("rn6") },
        dm3 = { return("dm3") },
        dm6 = { return("dm6") },
        danrer7 = { return("danRer7") },
        danrer10 = { return("danRer10") },
        danrer11 = { return("danRer11") },
        pantro4 = { return("panTro4") },
        pantro5 = { return("panTro5") },
        susscr3 = { return("susScr3") },
        susscr11 = { return("susScr11") },
        equcab2 = { return("equCab2") },
        equcab3 = { return("equCab3") },
        tair10 = { return("TAIR10") }
    )
}

getBsOrganism <- function(org) {
    switch(org,
        hg18 = {
            return("BSgenome.Hsapiens.UCSC.hg18")
        },
        hg19 = {
            return("BSgenome.Hsapiens.UCSC.hg19")
        },
        hg38 = {
            return("BSgenome.Hsapiens.UCSC.hg38")
        },
        mm9 = {
            return("BSgenome.Mmusculus.UCSC.mm9")
        },
        mm10 = {
            return("BSgenome.Mmusculus.UCSC.mm10")
        },
        rn5 = {
            return("BSgenome.Rnorvegicus.UCSC.rn5")
        },
        rn6 = {
            return("BSgenome.Rnorvegicus.UCSC.rn6")
        },
        dm3 = {
            return("BSgenome.Dmelanogaster.UCSC.dm3")
        },
        dm6 = {
            return("BSgenome.Dmelanogaster.UCSC.dm6")
        },
        danrer7 = {
            return("BSgenome.Drerio.UCSC.danRer7")
        },
        danrer10 = {
            return("BSgenome.Drerio.UCSC.danRer10")
        },
        danrer11 = {
            warning("danRer11 is not supported by BSgenome! Please use Ensembl",
                " as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
            #return("BSgenome.Drerio.UCSC.danRer11")
        },
        pantro4 = {
            warning("panTro4 is not supported by BSgenome! Please use Ensembl ",
                "as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
        },
        pantro5 = {
            return("BSgenome.Ptroglodytes.UCSC.panTro5")
        },
        susscr3 = {
            return("BSgenome.Sscrofa.UCSC.susScr3")
        },
        susscr11 = {
            warning("susScr11 is not supported by BSgenome! Please use ",
                "Ensembl as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
        },
        equcab2 = {
            warning("equCab2 is not supported by BSgenome! Please use Ensembl ",
                "as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
        },
        equcab3 = {
            warning("equCab3 is not supported by BSgenome! Please use Ensembl ",
                "as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
        },
        tair10 = {
            warning("TAIR10 is not supported by BSgenome! Please use Ensembl ",
                "as annotation source if GC content is important.",
                immediate.=TRUE)
            return(NA)
        }
    )
}

loadBsGenome <- function(org) {
    if (!requireNamespace("BiocManager"))
        stop("The Bioconductor package BiocManager is required to ",
            "proceed!")
    if (!requireNamespace("BSgenome"))
        stop("The Bioconductor package BSgenome is required to proceed!")
    bsOrg <- getBsOrganism(org)
    if (!is.na(bsOrg)) {
        if (bsOrg %in% BSgenome::installed.genomes())
            bsObj <- BSgenome::getBSgenome(getUcscOrganism(org))
        else {
            BiocManager::install(bsOrg,update=FALSE,ask=FALSE)
            bsObj <- BSgenome::getBSgenome(getUcscOrganism(org))
        }
        return(bsObj)
    }
    else
        return(NA)
}

getChromInfo <- function(org,
    goldenPath="http://hgdownload.cse.ucsc.edu/goldenPath/") {
    download.file(paste(goldenPath,org,"/database/chromInfo.txt.gz",sep=""),
        file.path(tempdir(),"chromInfo.txt.gz"),quiet=TRUE)
    chromInfo <- read.delim(file.path(tempdir(),"chromInfo.txt.gz"),
        header=FALSE)
    chromInfo <- chromInfo[,1,2]
    chromInfo[,1] <- as.character(chromInfo[,1])
    chromInfo$V3 <- rep(FALSE,nrow(chromInfo))
    m <- grep("M",chromInfo[,1])
    if (length(m) > 0)
        chromInfo$V3[m] <- TRUE
    return(chromInfo)
}

getSupportedRefDbs <- function() {
    return(c("ensembl","ucsc","refseq","ncbi"))
}

getSupportedOrganisms <- function() {
    return(c("hg18","hg19","hg38","mm9","mm10","rn5","rn6","dm3","dm6",
        "danrer7","danrer10","danrer11","pantro4","pantro5",#"pantro6",
        "susscr3","susscr11","equcab2","equcab3","tair10"))
}

getSupportedUcscDbs <- function() {
    return(c("ucsc","refseq","ncbi"))
}

setDbPath <- function(db=NULL) {
    if (is.null(db))
        db <- .defaultDbPath()
    else {
        if (!is.character(db)) {
            warning("The path to the sitadela database must be a valid path ",
                "to a file which\nwill be created if not existing! Assuming ",
                "default...",immediate..=TRUE)
            db <- .defaultDbPath()
        }
    }
    
    if (is.null(getOption("BioC"))) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
        options("BioC"=BioC)
    }

    BioC <- getOption("BioC")
    BioC$sitadela <- db
    options("BioC"=BioC)
}

getDbPath <- function() {
    db <- getOption("BioC")$sitadela
    if (!is.null(db))
        return(db)
    else
        return(.defaultDbPath())
}

importCustomGtf <- function(gtfFile,type=c("gene","transcript","utr",
    "transexon","transutr","exon")) {
    if (!requireNamespace("GenomicFeatures"))
        stop("Bioconductor package GenomicFeatures is required to build ",
            "custom annotation!")
        
    # Some argument checking
    type <- type[1]
    .checkTextArgs("type",type,c("gene","transcript","utr","transexon",
        "transutr","exon"),multiarg=FALSE)
    
    # Import the GTF with rtracklayer to create a map of available metadata
    message("  Importing GTF ",gtfFile," as GTF to make id map")
    desiredColumns <- c("type","gene_id","transcript_id","exon_id",
        "gene_name","gene_biotype")
    gr <- import(gtfFile,format="gtf",colnames=desiredColumns,
        feature.type=.GFF_FEATURE_TYPES)
    grdf <- as.data.frame(gr)
    grdf <- grdf[grdf$type=="exon",]

    # Need to map gene ids to names and biotypes. We need a collapsed 
    # structure exon_id - transcript_id - gene_id - gane_name - biotype
    message("  Making id map")
    map <- .makeIdMap(grdf)
    
    message("  Importing GTF ",gtfFile," as TxDb")
    txdb <- suppressWarnings(makeTxDbFromGFF(gtfFile))
    
    switch(type,
        gene = {
            return(.makeGeneGeneFromTxDb(txdb,map))
        },
        transcript = {
            return(.makeTranscriptGeneFromTxDb(txdb,map))
        },
        utr = {
            return(.makeGeneUtrFromTxDb(txdb,map))
        },
        transexon = {
            # Stub
        },
        transutr = {
            return(.makeTranscriptUtrFromTxDb(txdb,map))
        },
        exon = {
            return(.makeExonExonFromTxDb(txdb,map))
        }
    )
}

# parsed <- parseCustomGtf(gffFile)
parseCustomGtf <- function(gtfFile) {
    if (!requireNamespace("GenomicFeatures"))
        stop("Bioconductor package GenomicFeatures is required to parse ",
            "custom GTF file!")
    
    # Import the GTF with rtracklayer to create a map of available metadata
    message("  Importing GTF ",gtfFile," as GTF to make id map")
    desiredColumns <- c("type","gene_id","transcript_id","exon_id",
        "gene_name","gene_biotype")
    # Let it recognize automatically GFF/GTF
    gr <- import(gtfFile,colnames=desiredColumns,
        feature.type=.GFF_FEATURE_TYPES)
    grdf <- as.data.frame(gr)
    grdf <- grdf[grdf$type=="exon",]

    # Need to map gene ids to names and biotypes. We need a collapsed 
    # structure exon_id - transcript_id - gene_id - gane_name - biotype
    message("  Making id map")
    map <- .makeIdMap(grdf)
    
    message("  Importing GTF ",gtfFile," as TxDb")
    txdb <- suppressWarnings(makeTxDbFromGFF(gtfFile))
    
    return(list(txdb=txdb,map=map))
}

annotationFromCustomGtf <- function(parsed,type=c("gene","transcript","utr",
    "transexon","transutr","exon"),summarized=FALSE,asdf=FALSE) {
    # Some argument checking
    if (!is.logical(summarized))
        stop("summarized must be TRUE or FALSE")
    type <- type[1]
    .checkTextArgs("type",type,c("gene","transcript","utr","transexon",
        "transutr","exon"),multiarg=FALSE)
    
    txdb <- parsed$txdb
    map <- parsed$map
    
    switch(type,
        gene = {
            return(.makeGeneGeneFromTxDb(txdb,map,asdf))
        },
        transcript = {
            if (summarized)
                return(.makeSumTranscriptGeneFromTxDb(txdb,map,asdf))
            else
                return(.makeTranscriptGeneFromTxDb(txdb,map,asdf))
        },
        utr = {
            if (summarized)
                return(.makeSumGeneUtrFromTxDb(txdb,map,asdf))
            else
                return(.makeGeneUtrFromTxDb(txdb,map,asdf))
        },
        transexon = {
            if (summarized)
                return(.makeSumTranscriptExonFromTxDb(txdb,map,asdf))
            else
                return(.makeTranscriptExonFromTxDb(txdb,map,asdf))
        },
        transutr = {
            if (summarized)
                return(.makeSumTranscriptUtrFromTxDb(txdb,map,asdf))
            else
                return(.makeTranscriptUtrFromTxDb(txdb,map,asdf))
        },
        exon = {
            if (summarized)
                return(.makeSumGeneExonFromTxDb(txdb,map,asdf))
            else
                return(.makeExonExonFromTxDb(txdb,map,asdf))
        }
    )
}

cmclapply <- function(...,rc) {
    if (suppressWarnings(!requireNamespace("parallel")) 
        || .Platform$OS.type!="unix")
        m <- FALSE
    else {
        m <- TRUE
        ncores <- parallel::detectCores()
        if (ncores==1) 
            m <- FALSE
        else {
            if (!missing(rc) && !is.null(rc))
                ncores <- ceiling(rc*ncores)
            else 
                m <- FALSE
        }
    }
    if (m)
        return(mclapply(...,mc.cores=ncores,mc.set.seed=FALSE))
    else
        return(lapply(...))
}

.loadPrebuiltAnnotation <- function(con,genome,refdb,version,type,tv,
    summarized=FALSE) {
    metaType <- .annotationTypeFromInputArgs(type,summarized)
    cid <- .annotationExists(con,genome,refdb,version,metaType,tv,out="id")
    if (metaType == "summarized_exon")
        tName <- "active_length"
    else if (metaType == "summarized_3utr")
        tName <- "active_utr_length"
    else if (metaType == "summarized_3utr_transcript")
        tName <- "active_trans_utr_length"
    else if (metaType == "summarized_transcript_exon")
        tName <- "active_trans_exon_length"
    cida <- .annotationExists(con,genome,refdb,version,"active_length",tv,
        out="id")
        
    querySet <- .makeAnnotationQuerySet(metaType,cid,cida)
    
    preAnn <- dbGetQuery(con,querySet$main)
    preAnn$`_id` <- NULL
    preAnn$content_id <- NULL
    ann <- GRanges(preAnn)
    seqlevels(ann) <- unique(preAnn$chromosome)
    
    preSf <- dbGetQuery(con,querySet$seqinfo)
    preSf$`_id` <- NULL
    preSf$content_id <- NULL
    rownames(preSf) <- as.character(preSf[,1])
    if (genome %in% getSupportedOrganisms())
        sfg <- getUcscOrganism(genome)
    else
        sfg <- genome
    sf <- Seqinfo(seqnames=preSf[,1],seqlengths=preSf[,2],
        isCircular=rep(FALSE,nrow(preSf)),genome=sfg)
    
    if (length(sf) > 0) {
        if (length(seqlevels(ann)) != length(seqlevels(sf)))
            # If a subset, this is enough
            seqinfo(ann) <- sf[intersect(seqlevels(ann),seqlevels(sf))]
        else if (!all(seqlevels(ann) == seqlevels(sf))) {
            # Must also be sorted in the same way
            seqlevels(ann) <- seqlevels(sf)
            seqinfo(ann) <- sf
        }
        else
            seqinfo(ann) <- sf
    }
    
    ann <- .nameAnnotationFromMetaType(ann,metaType)
    
    preActive <- NULL
    if (!is.null(querySet$active)) {
        preActive <- dbGetQuery(con,querySet$active)
        preActive$`_id` <- NULL
        preActive$content_id <- NULL
        active <- as.integer(preActive$length)
        names(active) <- as.character(preActive$name)
        # FIXME: Will have to take care later of this
        #active <- active[names(ann)]
        attr(ann,"activeLength") <- active
    }
    
    return(ann)
}

.loadAnnotationOnTheFly <- function(genome,refdb,type,versioned=FALSE,rc=NULL) {
    message("Retrieving genome information for ",genome," from ",refdb)
    sf <- getSeqInfo(genome,asSeqinfo=TRUE)
    switch(type,
        gene = {
            message("Retrieving latest gene annotation for ",genome," from ",
                refdb)
            ann <- getAnnotation(genome,"gene",refdb=refdb,tv=versioned,rc=rc)
            annGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            names(annGr) <- as.character(annGr$gene_id)
        },
        transcript = {
            message("Retrieving latest transcript annotation for ",genome,
                " from ",refdb)
            ann <- getAnnotation(genome,"transcript",refdb=refdb,tv=versioned,
                rc=rc)
            annGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            names(annGr) <- as.character(annGr$transcript_id)
        },
        utr = {
            message("Retrieving latest 3' UTR annotation for ",genome," from ",
                refdb)
            ann <- getAnnotation(genome,"utr",refdb=refdb,tv=versioned,rc=rc)
            tmpGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            message("Merging 3' UTRs for ",genome," from ",refdb)
            annList <- reduceExons(tmpGr)
            annGr <- annList$model
            names(annGr) <- as.character(annGr$transcript_id)
            activeLength <- annList$length
            names(activeLength) <- unique(annGr$gene_id)
            attr(annGr,"activeLength") <- activeLength
        },
        transexon = {
            message("Retrieving latest transcrpit exon annotation for ",
                genome," from ",refdb)
            ann <- getAnnotation(genome,"transexon",refdb=refdb,tv=versioned,
                rc=rc)
            annGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            names(annGr) <- as.character(annGr$transcript_id)
        },
        transutr = {
            message("Retrieving latest 3' UTR annotation for ",genome," from ",
                refdb)
            ann <- getAnnotation(genome,"utr",refdb=refdb,tv=versioned,rc=rc)
            annGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            message("Merging 3' UTRs for ",genome," from ",refdb)
            annList <- reduceTranscriptsUtr(tmpGr)
            annGr <- annList$model
            names(annGr) <- as.character(annGr$transcript_id)
            activeLength <- annList$length
            names(activeLength) <- unique(annGr$transcript_id)
            attr(annGr,"activeLength") <- activeLength
        },
        exon = {
            message("Retrieving latest exon annotation for ",genome," from ",
                refdb)
            ann <- getAnnotation(genome,"exon",refdb=refdb,tv=versioned,rc=rc)
            annGr <- makeGRangesFromDataFrame(
                df=ann,
                seqinfo=sf,
                keep.extra.columns=TRUE,
                seqnames.field="chromosome"
            )
            names(annGr) <- as.character(annGr$exon_id)
        }
    )

    return(annGr)
}

.nameAnnotationFromMetaType <- function(ann,type) {
    switch(type,
        gene = {
            names(ann) <- as.character(ann$gene_id)
        },
        summarized_exon = {
            names(ann) <- as.character(ann$exon_id)
        },
        exon = {
            names(ann) <- as.character(ann$exon_id)
        },
        summarized_3utr = {
            names(ann) <- as.character(ann$transcript_id)
        },
        utr = {
            names(ann) <- as.character(ann$transcript_id)
        },
        summarized_transcript = {
            names(ann) <- as.character(ann$transcript_id)
        },
        transcript = {
            names(ann) <- as.character(ann$transcript_id)
        },
        summarized_3utr_transcript = {
            names(ann) <- as.character(ann$transcript_id)
        }
    )
    return(ann)
}

.annotationTypeFromInputArgs <- function(type,summarized=FALSE) {
    switch(type,
        gene = {
            return("gene")
        },
        transcript = {
            if (summarized)
                return("summarized_transcript")
            else
                return("transcript")
        },
        utr = {
            if (summarized)
                return("summarized_3utr")
            else
                return("utr")
        },
        transexon = {
            if (summarized)
                return("summarized_transcript_exon")
            else
                return("transexon")
        },
        transutr = {
            if (summarized)
                return("summarized_3utr_transcript")
            else
                return("utr")
        },
        exon = {
            if (summarized)
                return("summarized_exon")
            else
                return("exon")
        }
    )
}

.chromInfoFromBAM <- function(bam) {
    # Danger of including non-canonical chromosomes
    b <- scanBamHeader(bam)
    ci <- as.data.frame(b[[bam]]$targets)
    names(ci) <- "length"
    return(ci)
}

.chromInfoFromSeqinfo <- function(sf) {
    sf <- as.data.frame(sf)
    sf <- sf[,1,drop=FALSE]
    names(sf) <- "length"
    return(sf)
}

.chromInfoToSeqInfoDf <- function(ci,o="custom",circ=FALSE,asSeqinfo=FALSE) {
    if (asSeqinfo)
        return(Seqinfo(seqnames=rownames(ci),seqlengths=ci[,1],
            isCircular=rep(circ,nrow(ci)),genome=o))
    else
        return(data.frame(chromosome=rownames(ci),length=as.integer(ci[,1])))
}

.chromInfoWrapperGID <- function(o) {
    if (packageVersion("GenomeInfoDb")>=1.23)
        return(GenomeInfoDb::getChromInfoFromUCSC(o))
    else
        return(GenomeInfoDb::fetchExtendedChromInfoFromUCSC(o))
}

.validateDbCon <- function(obj) {
    # obj can be an already opened connection or the db file or missing. In the
    # latter case, the function looks at the package filesystem location
    if (missing(obj) || is.null(obj)) {
        db <-  getDbPath()
        drv <- dbDriver("SQLite")
        con <- tryCatch(dbConnect(drv,dbname=db),error=function(e) {
            message("Caught error: ",e)
            stop("Have you constructed the metaseqR annotation database?")
        },finally="")
    }
    if (is.character(obj) && file.exists(obj)) {
        drv <- dbDriver("SQLite")
        con <- tryCatch(dbConnect(drv,dbname=obj),error=function(e) {
            message("Caught error: ",e)
            stop("Is obj an SQLite database?")
        },finally="")
    }
    else if (is(obj,"SQLiteConnection"))
        con <- obj
    return(con)
}

.getValidChrs <- function(org) {
    switch(org,
        hg18 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        hg19 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        hg38 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        mm9 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX","chrY"
            ))
        },
        mm10 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX","chrY"
            ))
        },
        rn5 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX"
            ))
        },
        rn6 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX"
            ))
        },
        dm3 = {
            return(c(
                "chr2L","chr2LHet","chr2R","chr2RHet","chr3L","chr3LHet",
                "chr3R","chr3RHet","chr4","chrU","chrUextra","chrX","chrXHet",
                "chrYHet"
            ))
        },
        dm6 = {
            return(c(
                "chr2L","chr2R","chr3L","chr3R","chr4","chrX"
            ))
        },
        danrer7 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr3","chr4","chr5","chr6","chr7","chr8","chr9"
            ))
        },
        danrer10 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr3","chr4","chr5","chr6","chr7","chr8","chr9"
            ))
        },
        danrer11 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr3","chr4","chr5","chr6","chr7","chr8","chr9"
            ))
        },
        pantro4 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr20","chr21","chr22","chr2A","chr2B",
                "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        pantro5 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr20","chr21","chr22","chr2A","chr2B",
                "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        pantro6 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr20","chr21","chr22","chr2A","chr2B",
                "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        susscr3 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr2","chr3","chr4","chr5","chr6","chr7",
                "chr8","chr9","chrX","chrY"
            ))
        },
        susscr11 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr2","chr3","chr4","chr5","chr6","chr7",
                "chr8","chr9","chrX","chrY"
            ))
        },
        equcab2 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr26","chr27","chr28","chr29","chr3","chr30",
                "chr31","chr4","chr5","chr6","chr7","chr8","chr9","chrX"#,"chrY"
            ))
        },
        equcab3 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr26","chr27","chr28","chr29","chr3","chr30",
                "chr31","chr4","chr5","chr6","chr7","chr8","chr9","chrX"#,"chrY"
            ))
        },
        tair10 = {
            return(c(
                "chr1","chr2","chr3","chr4","chr5"
            ))
        }
    )
}

.getValidChrsWithMit <- function(org) {
    if (org %in% c("hg18","hg19","hg38","mm9","mm10","rn5","rn6","pantro4",
        "pantro5","pantro6","susscr3","susscr11","equcab2","equcab3"))
        return(c(.getValidChrs(org),"chrM"))
    else
        return(.getValidChrs(org))
}

.defaultDbPath <- function() {
    return(file.path(system.file(package="sitadela"),"annotation.sqlite"))
}

.collapseFailures <- function(f) {
    df <- data.frame(
        org=vapply(f,function(x) return(x$org),character(1)),
        refdb=vapply(f,function(x) return(x$refdb),character(1)),
        ver=vapply(f,function(x) return(x$ver),numeric(1)),
        tv=vapply(f,function(x) return(x$tv),logical(1))
    )
    
    uorgs <- unique(df$org)
    organisms <- vector("list",length(uorgs))
    names(organisms) <- uorgs
    for (o in uorgs) {
        organisms[[o]] <- 
            df$ver[which(df$org==o & df$refdb=="ensembl")]
        if (length(organisms[[o]]) == 0) {
            # Not an Ensembl failure, but there must be a version
            # which will be ignored during the build unless
            # forceDownload = TRUE
            vs <- .getUcscToEnsembl(o)
            organisms[[o]] <- vs[length(vs)]
        }
    }
    
    return(list(organisms=organisms,sources=unique(df$refdb)))
}
