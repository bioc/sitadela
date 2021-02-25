initDatabase <- function(db) {
    if (!requireNamespace("RSQLite"))
        stop("R package RSQLite is required to build the annotation database!")
    if (missing(db))
        stop("A database file must be provided!")
    drv <- dbDriver("SQLite")
    if (file.exists(db))
        # The database has been created at least with the tables defined
        con <- dbConnect(drv,dbname=db)
    else {
        # Create database file and define tables
        con <- dbConnect(drv,dbname=db)
        rs <- .initTables(con)
    }
    return(con)
}

getUcscTableNameUtr <- function(org,refdb) {
    switch(refdb,
        ucsc = {
            switch(org,
                hg18 = {
                    return("knownGene")
                },
                hg19 = {
                    return("knownGene")
                },
                hg38 = {
                    return("knownGene")
                },
                mm9 = {
                    return("knownGene")
                },
                mm10 = {
                    return("knownGene")
                },
                rn5 = {
                    return("mgcGenes")
                },
                rn6 = {
                    return("mgcGenes")
                },
                dm3 = {
                    return("flyBaseGene")
                },
                dm6 = {
                    warning("No UCSC Genome annotation for Drosophila ",
                        "melanogaster v6! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                danrer7 = {
                    return("mgcGenes")
                },
                danrer10 = {
                    return("mgcGenes")
                    
                },
                danrer11 = {
                    warning("No UCSC Genome annotation for Danio rerio v11! ",
                        "Will use RefSeq instead...",immediate.=TRUE)
                    return("refGene")
                },
                pantro4 = {
                    warning("No UCSC Genome annotation for Pan ",
                        "troglodytes! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                pantro5 = {
                    warning("No UCSC Genome annotation for Pan ",
                        "troglodytes! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                susscr3 = {
                    warning("No UCSC Genome annotation for Sus ",
                        "scrofa v3! Will use RefSeq instead...",
                        immediate.=TRUE)
                        return("refGene")
                },
                susscr11 = {
                    warning("No UCSC Genome annotation for Sus ",
                        "scrofa v11! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                equcab2 = {
                    warning("No UCSC Genome annotation for Equus ",
                        "caballus v2! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                equcab3 = {
                    warning("No UCSC Genome annotation for Equus ",
                        "caballus v3! Will use RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                }
            )
        },
        refseq = {
            return("refGene")
        },
        ncbi = {
            switch(org,
                hg18 = {
                    warning("No NCBI RefSeq Genome annotation for Homo ",
                        "sapiens hg18! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                hg19 = {
                    return("ncbiRefSeq")
                },
                hg38 = {
                    return("ncbiRefSeq")
                },
                mm9 = {
                    warning("No NCBI RefSeq Genome annotation for Mus ",
                        "musculus mm9! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                mm10 = {
                    return("ncbiRefSeq")
                },
                rn5 = {
                    warning("No NCBI RefSeq Genome annotation for Rattus ",
                        "norvegicus rn5! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                rn6 = {
                    return("ncbiRefSeq")
                },
                dm3 = {
                    warning("No NCBI RefSeq Genome annotation for Drosophila ",
                        "melanogaster dm3! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                dm6 = {
                    return("ncbiRefSeq")
                },
                danrer7 = {
                    warning("No NCBI RefSeq Genome annotation for Danio ",
                        "rerio danrer7! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                danrer10 = {
                    return("ncbiRefSeq")
                    
                },
                danrer11 = {
                    return("ncbiRefSeq")
                },
                pantro4 = {
                    warning("No NCBI RefSeq Genome annotation for Pan ",
                        " troglodytes pantro4! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                pantro5 = {
                    warning("No NCBI RefSeq Genome annotation for Pan ",
                        " troglodytes pantro5! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                susscr3 = {
                    warning("No NCBI RefSeq Genome annotation for Sus scrofa ",
                        " v3! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                susscr11 = {
                    warning("No NCBI RefSeq Genome annotation for Sus scrofa ",
                        " v11! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                equcab2 = {
                    warning("No NCBI RefSeq Genome annotation for Equus ",
                        " cabalus v2! Will use UCSC RefSeq instead...",
                        immediate.=TRUE)
                    return("refGene")
                },
                equcab3 = {
                    return("ncbiRefSeq")
                }
            )
        }
    )
}

getUcscDbl <- function(org,refdb="ucsc",versioned=FALSE) {
    org <- tolower(org[1])
    refdb <- tolower(refdb[1])
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    .checkTextArgs("refdb",refdb,getSupportedUcscDbs())
    
    if (!requireNamespace("RSQLite"))
        stop("R package RSQLite is required to use annotation from UCSC!")

    tableDefs <- .getUcscTabledef(org,refdb,"fields",versioned)
    fileList <- vector("list",length(tableDefs))
    names(fileList) <- names(tableDefs)
    
    # If versioning required, fetch gbCdnaInfo only once
    if (versioned && refdb=="refseq") {
        verInfo <- paste0("http://hgdownload.soe.ucsc.edu/goldenPath/hgFixed/",
            "database/gbCdnaInfo.txt.gz")
        if (!file.exists(file.path(tempdir(),"gbCdnaInfo.txt.gz")))
            download.file(verInfo,file.path(tempdir(),"gbCdnaInfo.txt.gz"),
                quiet=TRUE)
        # gbCdnaInfo should be the last element
        fileList <- fileList[-length(fileList)]
    }
    
    httpBase <- paste("http://hgdownload.soe.ucsc.edu/goldenPath/",
        getUcscOrganism(org),"/database/",sep="")
    for (n in names(fileList))
        fileList[[n]] <- paste(httpBase,n,".txt.gz",sep="")
        
    # Fill the fields for each table
    drv <- dbDriver("SQLite")
    dbTmp <- tempfile()
    con <- dbConnect(drv,dbname=dbTmp)
    message("  Retrieving tables for temporary SQLite ",refdb," ",org,
        " subset database")
    for (n in names(fileList)) {
        message("    Retrieving table ",n)
        download.file(fileList[[n]],file.path(tempdir(),
            paste(n,".txt.gz",sep="")),quiet=TRUE)
        sqlDf <- read.delim(file.path(tempdir(),paste(n,".txt.gz",sep="")),
            row.names=NULL,header=FALSE,strip.white=TRUE)
        names(sqlDf) <- tableDefs[[n]]
        dbWriteTable(con,n,sqlDf,row.names=FALSE)
    }
    
    # And now read also gbCdnaInfo in
    if (versioned && refdb=="refseq") {
        sqlDf <- read.delim(file.path(tempdir(),"gbCdnaInfo.txt.gz"),
            row.names=NULL,header=FALSE,strip.white=TRUE)
        names(sqlDf) <- tableDefs[["gbCdnaInfo"]]
        dbWriteTable(con,"gbCdnaInfo",sqlDf,row.names=FALSE)
    }
    
    dbDisconnect(con)
    return(dbTmp)
}

.initTables <- function(con) {
    queries <- .localTblDef()
    rs <- dbSendQuery(con,queries[[1]])
    if (dbHasCompleted(rs))
        dbClearResult(rs)
    for (n in names(queries)) {
        rs <- dbSendStatement(con,queries[[n]])
        if (dbHasCompleted(rs))
            dbClearResult(rs)
    }
}

.localTblDef <- function() {
    return(list(
        enable_fkey="PRAGMA foreign_keys=1;",
        #content=paste(
        #    "CREATE TABLE IF NOT EXISTS content (",
        #    "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
        #    "source TEXT,",
        #    "organism TEXT,",
        #    "version INTEGER,",
        #    "type TEXT,",
        #    "user INTEGER",
        #    ");"
        #),
        content=paste(
            "CREATE TABLE IF NOT EXISTS content (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "source TEXT,",
            "organism TEXT,",
            "version INTEGER,",
            "type TEXT,",
            "has_tv INTEGER,",
            "user INTEGER",
            ");"
        ),
        seqinfo=paste(
            "CREATE TABLE IF NOT EXISTS seqinfo (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "length INTEGER,",
            #"source TEXT,",
            #"organism TEXT,",
            #"version INTEGER,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        gene=paste(
            "CREATE TABLE IF NOT EXISTS gene (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "gene_id TEXT,",
            "gc_content REAL,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        transcript=paste(
            "CREATE TABLE IF NOT EXISTS transcript (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "transcript_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        exon=paste(
            "CREATE TABLE IF NOT EXISTS exon (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "exon_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        transexon=paste(
            "CREATE TABLE IF NOT EXISTS transexon (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "exon_id TEXT,",
            "transcript_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        utr=paste(
            "CREATE TABLE IF NOT EXISTS utr (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "transcript_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        summarized_transcript=paste(
            "CREATE TABLE IF NOT EXISTS summarized_transcript (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "transcript_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        summarized_exon=paste(
            "CREATE TABLE IF NOT EXISTS summarized_exon (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "exon_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        summarized_transcript_exon=paste(
            "CREATE TABLE IF NOT EXISTS summarized_transcript_exon (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "exon_id TEXT,",
            "transcript_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        summarized_3utr=paste(
            "CREATE TABLE IF NOT EXISTS summarized_3utr (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "transcript_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        summarized_3utr_transcript=paste(
            "CREATE TABLE IF NOT EXISTS summarized_3utr_transcript (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "chromosome TEXT,",
            "start INTEGER,",
            "end INTEGER,",
            "transcript_id TEXT,",
            "gene_id TEXT,",
            "strand TEXT,",
            "gene_name TEXT,",
            "biotype TEXT,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        active_length=paste(
            "CREATE TABLE IF NOT EXISTS active_length (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "name TEXT,",
            "length INTEGER,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        active_utr_length=paste(
            "CREATE TABLE IF NOT EXISTS active_utr_length (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "name TEXT,",
            "length INTEGER,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        active_trans_utr_length=paste(
            "CREATE TABLE IF NOT EXISTS active_trans_utr_length (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "name TEXT,",
            "length INTEGER,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        ),
        active_trans_exon_length=paste(
            "CREATE TABLE IF NOT EXISTS active_trans_exon_length (",
            "_id INTEGER PRIMARY KEY AUTOINCREMENT,",
            "name TEXT,",
            "length INTEGER,",
            "content_id INTEGER NOT NULL,",
            "FOREIGN KEY(content_id) REFERENCES content(_id) ON DELETE CASCADE",
            ");"
        )
    ))
}

.makeAnnotationQuerySet <- function(t,i,j=NULL) {
    mainQuery <- paste("SELECT * FROM ",t," WHERE content_id=",i,sep="")
    seqInfoQuery <- paste("SELECT * FROM seqinfo WHERE content_id=",i,sep="")
    activeQuery <- NULL
    if (t == "summarized_exon" && !is.null(j))
        activeQuery <- paste("SELECT * FROM active_length WHERE content_id=",
            j,sep="")
    else if (t == "summarized_3utr" && !is.null(j))
        activeQuery <- paste("SELECT * FROM active_utr_length WHERE ",
            "content_id=",j,sep="")
    else if (t == "summarized_3utr_transcript" && !is.null(j))
        activeQuery <- paste("SELECT * FROM active_trans_utr_length WHERE ",
            "content_id=",j,sep="")
    else if (t == "summarized_transcript_exon" && !is.null(j))
        activeQuery <- paste("SELECT * FROM active_trans_exon_length WHERE ",
            "content_id=",j,sep="")
    return(list(
        main=mainQuery,
        seqinfo=seqInfoQuery,
        active=activeQuery
    ))
}

.insertContent <- function(con,o,s,v,t,h=0,u=0) {
    query <- paste(
        "INSERT INTO content (source, organism, version, type, has_tv, user) ",
        "VALUES (",paste("'",s,"', ","'",o,"', ",v,", '",t,"', ",h,", ",u,
        sep=""),")",sep=""
    )
    nr <- dbExecute(con,query)
    return(nr)
}

.browseContent <- function(con) {
    return(dbGetQuery(con,"SELECT * FROM content"))
}

.browseUserContent <- function(con) {
    return(dbGetQuery(con,"SELECT * FROM content WHERE user=1"))
}

.annotationExists <- function(con,o,s,v=NULL,t=NULL,h=NULL,
    out=c("tf","nr","id")) {
    out <- out[1]
    if (!is.null(h) && is.logical(h))
        h <- ifelse(h,1,0)
    else
        h <- NULL
    query <- paste("SELECT _id FROM content WHERE source='",s,
        "' AND organism='",o,"'",sep="")
    if (!is.null(v))
        query <- paste(query," AND version=",v,sep="")
    if (!is.null(t))
        query <- paste(query," AND type='",t,"'",sep="")
    if (!is.null(h))
        query <- paste(query," AND has_tv=",h,sep="")
    res <- dbGetQuery(con,query)
    if (out == "tf")
        return(nrow(res) > 0)
    else if (out=="nr")
        return(nrow(res))
    else if (out == "id") {
        if (nrow(res) > 0)
            return(res[1,1])
    }
}

.containsVersionedGT <- function(con,o,s,only=FALSE) {
    query <- paste("SELECT has_tv FROM content WHERE source='",s,
        "' AND organism='",o,"'",sep="")
    res <- dbGetQuery(con,query)
    out <- as.integer(res[,1])
    if (!only)
        return(any(out==1))
    else
        return(all(out==1))
}

.dropAnnotation <- function(con,o,s,v,t,h) {
    # At least organism must exist
    if (missing(o))
        stop("At least an organism name must be provided for deletion!")
    # A basic deletion query based on organism. Since the content table is
    # connected with the rest through foreing keys, deletion from there should
    # be enough.
    query <- paste("DELETE FROM content WHERE organism='",o,"'",sep="")
    # Augment according to given arguments.
    if (!missing(s) && !is.null(s))
        query <- paste(query," AND source='",s,"'",sep="")
    if (!missing(v) && !is.null(v))
        query <- paste(query," AND version=",v,sep="")
    if (!missing(t) && !is.null(t))
        query <- paste(query," AND type='",t,"'",sep="")
    if (!missing(h) && !is.null(h))
        query <- paste(query," AND has_tv=",h,sep="")
    # Execute
    nr <- dbExecute(con,query)
    return(nr)
}

.installedVersions <- function(con,o,s) {
    query <- paste(
        "SELECT version FROM content WHERE source='",s,"' AND organism='",o,"'",
        sep=""
    )
    res <- dbGetQuery(con,query)
    if (nrow(res) > 0)
        return(as.numeric(res[,1]))
    else
        return(NA)
}

.getUcscTabledef <- function(org,refdb="ucsc",what="queries",versioned=FALSE) {
    org <- tolower(org[1])
    refdb <- tolower(refdb[1])
    what <- tolower(what[1])
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    .checkTextArgs("refdb",refdb,getSupportedUcscDbs())
    .checkTextArgs("what",what,c("queries","fields"))
    switch(refdb,
        ucsc = {
            return(.getUcscTabledefUcsc(org,what))
        },
        refseq = {
            return(.getUcscTabledefRefseq(org,what,versioned))
        },
        ncbi = {
            return(.getUcscTabledefNcbi(org,what))
        }
    )
}

.getUcscTblTpl <- function(tab,what="queries") {
    if (what=="queries") {
        switch(tab,
            knownCanonical = {
                return(paste(
                    "CREATE TABLE",
                    "`knownCanonical` (",
                    "`chrom` TEXT NOT NULL DEFAULT '',",
                    "`chromStart` INTEGER NOT NULL DEFAULT '0',",
                    "`chromEnd` INTEGER NOT NULL DEFAULT '0',",
                    "`clusterId` INTEGER NOT NULL DEFAULT '0',",
                    "`transcript` TEXT NOT NULL DEFAULT '',",
                    "`protein` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            knownGene = {
                return(paste(
                    "CREATE TABLE",
                    "`knownGene` (",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`chrom` TEXT NOT NULL DEFAULT '',",
                    "`strand` TEXT NOT NULL DEFAULT '',",
                    "`txStart` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`txEnd` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`exonCount` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL,",
                    "`proteinID` TEXT NOT NULL DEFAULT '',",
                    "`alignID` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            knownToRefSeq = {
                return(paste(
                    "CREATE TABLE",
                    "`knownToRefSeq` (",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`value` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            refFlat = {
                return(paste("CREATE TABLE",
                    "`refFlat` (",
                    "`geneName` TEXT NOT NULL,",
                    "`name` TEXT NOT NULL,",
                    "`chrom` TEXT NOT NULL,",
                    "`strand` TEXT NOT NULL,",
                    "`txStart` UNSIGNED INTEGER NOT NULL,",
                    "`txEnd` UNSIGNED INTEGER NOT NULL,",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL,",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL,",
                    "`exonCount` UNSIGNED INTEGER NOT NULL,",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
            refGene = {
                return(paste("CREATE TABLE",
                    "`refGene` (",
                    "`bin` UNSIGNED INTEGER NOT NULL,",
                    "`name` TEXT NOT NULL,",
                    "`chrom` TEXT NOT NULL,",
                    "`strand` TEXT NOT NULL,",
                    "`txStart` UNSIGNED INTEGER NOT NULL,",
                    "`txEnd` UNSIGNED INTEGER NOT NULL,",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL,",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL,",
                    "`exonCount` UNSIGNED INTEGER NOT NULL,",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL",
                    "`score` INTEGER NOT NULL,",
                    "`name2` TEXT NOT NULL,",
                    "`cdsStartStat` TEXT NOT NULL,",
                    "`cdsEndStat` TEXT NOT NULL,",
                    "`exonFrames` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
            knownToEnsembl = {
                return(paste(
                    "CREATE TABLE",
                    "`knownToEnsembl` (",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`value` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            ensemblSource = {
                return(paste(
                    "CREATE TABLE",
                    "`ensemblSource` (",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`source` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            mgcGenes = {
                return(paste(
                    "CREATE TABLE `mgcGenes` (",
                    "`bin` UNSIGNED INTEGER NOT NULL,",
                    "`name` TEXT NOT NULL,",
                    "`chrom` TEXT NOT NULL,",
                    "`strand` TEXT NOT NULL,",
                    "`txStart` UNSIGNED INTEGER NOT NULL,",
                    "`txEnd` UNSIGNED INTEGER NOT NULL,",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL,",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL,",
                    "`exonCount` UNSIGNED INTEGER NOT NULL,",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL,",
                    "`score` INTEGER DEFAULT NULL,",
                    "`name2` TEXT NOT NULL,",
                    "`cdsStartStat` TEXT NOT NULL,",
                    "`cdsEndStat` TEXT NOT NULL,",
                    "`exonFrames` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
            ensemblToGeneName = {
                return(paste(
                    "CREATE TABLE",
                    "`knownToGeneName` (",
                    "`name` TEXT NOT NULL,",
                    "`value` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
            flyBaseCanonical = {
                return(paste(
                    "CREATE TABLE",
                    "`flyBaseCanonical` (",
                    "`chrom` TEXT NOT NULL DEFAULT '',",
                    "`chromStart` INTEGER NOT NULL DEFAULT '0',",
                    "`chromEnd` INTEGER NOT NULL DEFAULT '0',",
                    "`clusterId` INTEGER unsigned NOT NULL DEFAULT '0',",
                    "`transcript` TEXT NOT NULL DEFAULT '',",
                    "`protein` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            flyBaseGene = {
                return(paste(
                    "CREATE TABLE",
                    "`flyBaseGene` (",
                    "`bin` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`chrom` TEXT NOT NULL DEFAULT '',",
                    "`strand` TEXT NOT NULL DEFAULT '',",
                    "`txStart` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`txEnd` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`exonCount` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
            flyBaseToRefSeq = {
                return(paste(
                    "CREATE TABLE",
                    "`flyBaseToRefSeq` (",
                    "`name` TEXT NOT NULL DEFAULT '',",
                    "`value` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            gbCdnaInfo = {
                return(paste(
                    "CREATE TABLE",
                    "`gbCdnaInfo` (",
                    "`bin` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`acc` TEXT NOT NULL DEFAULT '',",
                    "`version` UNSIGNED INTEGER NOT NULL DEFAULT '1',",
                    "`type` TEXT NOT NULL DEFAULT '',",
                    "`direction` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`source` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`organism` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`library` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`mrnaClone` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`sex` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`tissue` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`development` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cell` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`cds` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`keyword` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`description` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`geneName` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`productName` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`author` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`gi` UNSIGNED INTEGER NOT NULL DEFAULT '0',",
                    "`mol` TEXT NOT NULL DEFAULT ''",
                    ")",collapse=" "
                ))
            },
            ncbiRefSeq = {
                return(paste("CREATE TABLE",
                    "`ncbiRefSeq` (",
                    "`bin` UNSIGNED INTEGER NOT NULL,",
                    "`name` TEXT NOT NULL,",
                    "`chrom` TEXT NOT NULL,",
                    "`strand` TEXT NOT NULL,",
                    "`txStart` UNSIGNED INTEGER NOT NULL,",
                    "`txEnd` UNSIGNED INTEGER NOT NULL,",
                    "`cdsStart` UNSIGNED INTEGER NOT NULL,",
                    "`cdsEnd` UNSIGNED INTEGER NOT NULL,",
                    "`exonCount` UNSIGNED INTEGER NOT NULL,",
                    "`exonStarts` TEXT NOT NULL,",
                    "`exonEnds` TEXT NOT NULL",
                    "`score` INTEGER NOT NULL,",
                    "`name2` TEXT NOT NULL,",
                    "`cdsStartStat` TEXT NOT NULL,",
                    "`cdsEndStat` TEXT NOT NULL,",
                    "`exonFrames` TEXT NOT NULL",
                    ")",collapse=" "
                ))
            },
        )
    }
    else if (what=="fields") {
        switch(tab,
            knownCanonical = {
                return(c("chrom","chromStart","chromEnd","clusterId",
                "transcript","protein"))
            },
            knownGene = {
                return(c("name","chrom","strand","txStart","txEnd","cdsStart",
                    "cdsEnd","exonCount","exonStarts","exonEnds","proteinID",
                    "alignID"))
            },
            knownToRefSeq = {
                return(c("name","value"))
            },
            refFlat = {
                return(c("geneName","name","chrom","strand","txStart","txEnd",
                    "cdsStart","cdsEnd","exonCount","exonStarts","exonEnds"))
            },
            refGene = {
                return(c("bin","name","chrom","strand","txStart","txEnd",
                "cdsStart","cdsEnd","exonCount","exonStarts","exonEnds","score",
                "name2","cdsStartStat","cdsEndStat","exonFrames"))
            },
            knownToEnsembl = {
                return(c("name","value"))
            },
            ensemblSource = {
                return(c("name","source"))
            },
            mgcGenes = {
                return(c("bin","name","chrom","strand","txStart","txEnd",
                    "cdsStart","cdsEnd","exonCount","exonStarts","exonEnds",
                    "score","name2","cdsStartStat","cdsEndStat","exonFrames"
                ))
            },
            ensemblToGeneName = {
                return(c("name","value"))
            },
            flyBaseCanonical = {
                return(c("chrom","chromStart","chromEnd","clusterId",
                    "transcript","protein"))
            },
            flyBaseGene = {
                return(c("bin","name","chrom","strand","txStart","txEnd",
                    "cdsStart","cdsEnd","exonCount","exonStarts","exonEnds"))
            },
            flyBaseToRefSeq = {
                return(c("name","value"))
            },
            gbCdnaInfo = {
                return(c("bin","acc","version","moddate","type","direction",
                    "source","organism","library","mrnaClone","sex","tissue",
                    "development","cell","cds","keyword","description",
                    "geneName","productName","author","gi","mol"))
            },
            ncbiRefSeq = {
                return(c("bin","name","chrom","strand","txStart","txEnd",
                    "cdsStart","cdsEnd","exonCount","exonStarts","exonEnds",
                    "score","name2","cdsStartStat","cdsEndStat","exonFrames"))
            }
        )
    }
}

.getUcscQuery <- function(org,type,refdb="ucsc",versioned=FALSE) {
    type <- tolower(type[1])
    org <- tolower(org[1])
    refdb <- tolower(refdb[1])
    .checkTextArgs("type",type,c("gene","exon","transcript"))
    .checkTextArgs("org",org,getSupportedOrganisms(),multiarg=FALSE)
    .checkTextArgs("refdb",refdb,c("ucsc","refseq","ncbi"))
    switch(type,
        gene = {
            switch(refdb,
                ucsc = {
                    return(.getUcscQueryUcscGene(org))
                },
                refseq = {
                    return(.getUcscQueryRefseqGene(org,versioned=versioned))
                },
                ncbi = {
                    return(.getUcscQueryNcbiGene(org))
                }
            )
        },
        exon = {
            switch(refdb,
                ucsc = {
                    return(.getUcscQueryUcscExon(org))
                },
                refseq = {
                    return(.getUcscQueryRefseqExon(org,versioned=versioned))
                },
                ncbi = {
                    return(.getUcscQueryNcbiExon(org))
                }
            )
        },
        transcript = {
            switch(refdb,
                ucsc = {
                    return(.getUcscQueryUcscTranscript(org))
                },
                refseq = {
                    return(.getUcscQueryRefseqTranscript(org,
                        versioned=versioned))
                },
                ncbi = {
                    return(.getUcscQueryNcbiTranscript(org))
                }
            )
        }
    )
}

.makeUcscRefseqUtrTable <- function(org,.rmysql=FALSE) {
    vq <- .getUcscRefseqVersionedUtrQuery()
    if (.rmysql) {
        dbCreds <- .getUcscCredentials()
        drv <- dbDriver("MySQL")
        con <- dbConnect(drv,user=dbCreds[2],password=NULL,
            dbname=getUcscOrganism(org),host=dbCreds[1])
        rawAnn <- dbGetQuery(con,vq)
        dbDisconnect(con)
    }
    else {
        # Download tables, build sqlite and run vq
        tmpSqlite <- getUcscDbl(org=org,refdb="ucsc",versioned=TRUE)
        drv <- dbDriver("SQLite")
        con <- dbConnect(drv,dbname=tmpSqlite)
        rawAnn <- dbGetQuery(con,vq)
        dbDisconnect(con)
    }
    utrTmp <- tempfile()
    options(scipen=32) # genePredToGtf has problem with scientific notation
    write.table(rawAnn,file=gzfile(paste0(utrTmp,".txt.gz")),sep="\t",
        col.names=FALSE,row.names=FALSE,quote=FALSE)
    options(scipen=0)
    return(basename(utrTmp))
}

.getUcscCredentials <- function() {
    return(c(
        host="genome-mysql.cse.ucsc.edu",
        user="genome",
        password=""
    ))
}

#..getAllUcsc <- function(to) {
#    if (missing(to))
#        stop("Please provide a path to put the sqlite databases!")
#    if (!dir.exists(to))
#        dir.create(to,recursive=TRUE)
#        
#    for (refdb in getSupportedUcscDbs())
#        for (org in getSupportedOrganisms())
#            ..getUcscSqlite(org,refdb,to)
#}
#
#..getUcscSqlite <- function(org,refdb,to) {
#    if (missing(to))
#        stop("Please provide a path to put the sqlite databases!")
#    if (!dir.exists(to))
#        dir.create(to,recursive=TRUE)
#    
#    message("RETRIEVING ",org," FROM ",refdb)
#    dbTmp <- getUcscDbl(org=org,refdb=refdb)
#    name <- paste(org,"_",refdb,".sqlite",sep="")
#    file.copy(dbTmp,file.path(to,name),recursive=TRUE)
#    return()
#}
