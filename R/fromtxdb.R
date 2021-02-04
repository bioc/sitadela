# ann <- .makeGeneGeneFromTxDb(txdb,map,FALSE)
.makeGeneGeneFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- genes(txdb)
        # We need to add gc_content, gene_name, biotype from the map
        # Remove the exon_id and transcript_id columns from the map
        # for the gene case
        ir <- c(which(names(map)=="exon_id"),
            which(names(map)=="transcript_id"))
        if (length(ir) > 0)
            smap <- map[,-ir,drop=FALSE]
        dgt <- which(duplicated(smap))
        if (length(dgt) > 0)
            smap <- smap[-dgt,]
        
        # Add metadata from the map
        rownames(smap) <- smap$gene_id
        smap <- smap[names(gr),,drop=FALSE]
        gr$gene_name <- smap$gene_name
        gr$biotype <- smap$biotype
    }
    else {
        gr <- genes(txdb,columns=c("gene_id","tx_type"))
        gr$gene_id <- as.character(gr$gene_id)
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(gr$tx_type)
        gr$tx_type <- NULL
    }
    
    gr$gc_content = rep(50,length(gr))
    ann <- as.data.frame(gr)
    ann <- ann[,c(1:3,6,9,5,7,8)]
    names(ann)[1] <- "chromosome"
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeGeneExonFromTxDb(txdb,map,FALSE)
.makeGeneExonFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- exons(txdb,columns="exon_name")
        
        # There are cases where exons are unnamed and just the structure 
        useMap <- TRUE
        if (any(is.na(gr$exon_name)))
            useMap <- FALSE
        
        if (useMap) {
            names(gr) <- gr$exon_name
        
            # We need to add gene_id, gene_name, biotype from the map
            # Remove the transcript_id column from the map for the gene
            # case
            ir <- which(names(map)=="transcript_id")
            if (length(ir) > 0)
                smap <- map[,-ir,drop=FALSE]
            dgt <- which(duplicated(smap))
            if (length(dgt) > 0)
                smap <- smap[-dgt,]
            
            # Add metadata from the map
            rownames(smap) <- smap$exon_id
            smap <- smap[names(gr),,drop=FALSE]
            gr$gene_id <- smap$gene_id
            gr$gene_name <- smap$gene_name
            gr$biotype <- smap$biotype
        }
        else {
            # In order to create the exon annotation, we need to manually 
            # overlap and assign to genes
            ge <- genes(txdb,columns=c("gene_id","tx_type"))
            ov <- findOverlaps(gr,ge)
            exonPos <- queryHits(ov)
            genePos <- subjectHits(ov)
            levs <- unique(genePos)
            
            dup <- which(duplicated(exonPos))
            if (length(dup) > 0) {
                exonPos <- exonPos[-dup]
                genePos <- genePos[-dup]
            }
            
            S <- split(exonPos,factor(genePos,levels=levs))
            gr$gene_id <- gr$gene_name <- rep(names(ge)[levs],lengths(S))
            gr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],
                lengths(S))
            #gr$biotype <- rep("gene",length(gr))
            if (is(gr$gene_id,"CharacterList"))
                gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
            else
                gtmp <- gr$gene_id
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
            gr$exon_name <- names(gr)
        }
    }
    else {
        gr <- exons(txdb,columns=c("exon_name","gene_id","tx_type"))
        if (is(gr$gene_id,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
        else
            gtmp <- gr$gene_id
        if (any(is.na(gr$exon_name)))            
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
        else
            names(gr) <- gr$exon_name
        gr$gene_id <- gtmp
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(lapply(gr$tx_type,function(x) x[1]))
        gr$tx_type <- NULL
    }
    
    ann <- as.data.frame(unname(gr))
    ann <- ann[,c(1,2,3,6,7,5,8,9)]
    names(ann)[c(1,4)] <- c("chromosome","exon_id")
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    if (length(unique(ann$exon_id)) == length(ann$exon_id))
        # Should always be TRUE
        rownames(ann) <- as.character(ann$exon_id)
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeSumGeneExonFromTxDb(txdb,map,FALSE)
.makeSumGeneExonFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- exons(txdb,columns="exon_name")
        
        useMap <- TRUE
        if (any(is.na(gr$exon_name)))
            useMap <- FALSE
        
        if (useMap) {
            names(gr) <- gr$exon_name
            
            # We need to add gene_id, gene_name, biotype from the map
            # Remove the transcript_id column from the map for the gene
            # case
            ir <- which(names(map)=="transcript_id")
            if (length(ir) > 0)
                smap <- map[,-ir,drop=FALSE]
            dgt <- which(duplicated(smap))
            if (length(dgt) > 0)
                smap <- smap[-dgt,]
            
            # Add metadata from the map
            rownames(smap) <- smap$exon_id
            smap <- smap[names(gr),,drop=FALSE]
            gr$gene_id <- smap$gene_id
            gr$gene_name <- smap$gene_name
            gr$biotype <- smap$biotype
        }
        else {
            # In order to create the exon annotation, we need to manually 
            # overlap and assign to genes
            ge <- genes(txdb,columns=c("gene_id","tx_type"))
            ov <- findOverlaps(gr,ge)
            exonPos <- queryHits(ov)
            genePos <- subjectHits(ov)
            levs <- unique(genePos)
            
            dup <- which(duplicated(exonPos))
            if (length(dup) > 0) {
                exonPos <- exonPos[-dup]
                genePos <- genePos[-dup]
            }
            
            S <- split(exonPos,factor(genePos,levels=levs))
            gr$gene_id <- gr$gene_name <- rep(names(ge)[levs],lengths(S))
            gr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],
                lengths(S))
            if (is(gr$gene_id,"CharacterList"))
                gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
            else
                gtmp <- gr$gene_id
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
            gr$exon_name <- names(gr)
        }
    }
    else {
        gr <- exons(txdb,columns=c("exon_name","gene_id","tx_type"))
        if (is(gr$gene_id,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
        else
            gtmp <- gr$gene_id
        if (any(is.na(gr$exon_name)))            
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
        else
            names(gr) <- gr$exon_name
        gr$gene_id <- gtmp
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(lapply(gr$tx_type,function(x) x[1]))
        gr$tx_type <- NULL
    }
    
    ann <- as.data.frame(unname(gr))
    ann <- ann[,c(1,2,3,6,8,5,7,9)]
    names(ann)[c(1,4)] <- c("chromosome","exon_id")
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    message("  summarizing exons per gene for imported GTF")
    annList <- reduceExons(GRanges(ann))
    sexon <- annList$model
    names(sexon) <- as.character(sexon$exon_id)
    activeLength <- annList$length
    names(activeLength) <- unique(sexon$gene_id)
    attr(sexon,"activeLength") <- activeLength
    
    if (asdf) {
        eann <- as.data.frame(sexon)
        eann <- eann[,c(1,2,3,6,8,5,7,9)]
        names(eann)[c(1,4)] <- c("chromosome","exon_id")
        attr(eann,"activeLength") <- activeLength
        return(eann)
    }
    else
        return(sexon)
}

# ann <- .makeGeneUtrFromTxDb(txdb,map,FALSE)
.makeGeneUtrFromTxDb <- function(txdb,map,asdf) {
    utrList <- threeUTRsByTranscript(txdb,use.names=TRUE)
    utrGr <- unlist(utrList)
    
    # There may be no sufficient information to create UTRs
    if (length(utrGr) == 0) {
        warning("No UTR information was found in the provided GTF file! Will ",
            "return an empty object...",immediate.=TRUE)
        ann <- data.frame(chromosome=1,start=1,end=1,
            transcript_id=1,gene_id=1,strand=1,gene_name=1,
            biotype=1,row.names=1)
        # Strange but required to be compatible with GRanges
        ann <- ann[-1,]
        if (asdf)
            return(ann)
        else
            return(GRanges(ann))
    }
    
    utrGr$transcript_id <- names(utrGr)
    utrTmp <- as.data.frame(unname(utrGr))
    keep <- c("seqnames","start","end","transcript_id",
        "exon_rank","strand","exon_name")
    utr <- utrTmp[,keep]
    
    useMap <- TRUE
    if (any(is.na(utr$exon_name)) || is.null(map))
        # Impossible to take info from map...
        useMap <- FALSE
    else
        rownames(utr) <- paste(utr$exon_name,utr$transcript_id,sep="_")
        
    if (useMap) {
        rownames(map) <- paste(map$exon_id,map$transcript_id,sep="_")

        # Different case with map here
        smap <- map[rownames(utr),]
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the gene case
        ir <- which(names(smap)=="exon_id")
        if (length(ir) > 0)
            smap <- smap[,-ir,drop=FALSE]
        
        # Add metadata from the map
        utr$gene_id <- smap$gene_id
        utr$gene_name <- smap$gene_name
        utr$biotype <- smap$biotype
    }
    else {
        # Add metadata from the utr frame
        utr$gene_id <- utr$transcript_id
        utr$gene_name <- utr$gene_id
        
        # To get biotypes
        ge <- genes(txdb,columns=c("gene_id","tx_type"))
        ov <- findOverlaps(utrGr,ge)
        utrPos <- queryHits(ov)
        genePos <- subjectHits(ov)
        levs <- unique(genePos)
        
        dup <- which(duplicated(utrPos))
        if (length(dup) > 0) {
            utrPos <- utrPos[-dup]
            genePos <- genePos[-dup]
        }
        
        S <- split(utrPos,factor(genePos,levels=levs))
        utr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],lengths(S))
        #utr$biotype <- rep("gene",nrow(utr))
    }
    
    ann <- utr[,c(1,2,3,4,8,6,9,10)]
    names(ann)[1] <- "chromosome"
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeSumGeneUtrFromTxDb(txdb,map,FALSE)
.makeSumGeneUtrFromTxDb <- function(txdb,map,asdf) {
    utrList <- threeUTRsByTranscript(txdb,use.names=TRUE)
    utrGr <- unlist(utrList)
    
    if (length(utrGr) == 0) {
        warning("No UTR information was found in the provided GTF file! Will ",
            "return an empty object...",immediate.=TRUE)
        ann <- data.frame(chromosome=1,start=1,end=1,
            transcript_id=1,gene_id=1,strand=1,gene_name=1,
            biotype=1,row.names=1)
        # Strange but required to be compatible with GRanges
        ann <- ann[-1,]
        if (asdf)
            return(ann)
        else
            return(GRanges(ann))
    }
    
    utrGr$transcript_id <- names(utrGr)
    utrTmp <- as.data.frame(unname(utrGr))
    keep <- c("seqnames","start","end","transcript_id",
        "exon_rank","strand","exon_name")
    utr <- utrTmp[,keep]
    
    useMap <- TRUE
    if (any(is.na(utr$exon_name)) || is.null(map))
        useMap <- FALSE
    else
        rownames(utr) <- paste(utr$exon_name,utr$transcript_id,sep="_")
    
    if (useMap) {
        rownames(map) <- paste(map$exon_id,map$transcript_id,sep="_")
    
        # Different case with map here
        smap <- map[rownames(utr),]
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the gene case
        ir <- which(names(smap)=="exon_id")
        if (length(ir) > 0)
            smap <- smap[,-ir,drop=FALSE]
        
        # Add metadata from the map
        utr$gene_id <- smap$gene_id
        utr$gene_name <- smap$gene_name
        utr$biotype <- smap$biotype
    }
    else {
        # Add metadata from the utr frame
        utr$gene_id <- utr$transcript_id
        utr$gene_name <- utr$gene_id
        
        # To get biotypes
        ge <- genes(txdb,columns=c("gene_id","tx_type"))
        ov <- findOverlaps(utrGr,ge)
        utrPos <- queryHits(ov)
        genePos <- subjectHits(ov)
        levs <- unique(genePos)
        
        dup <- which(duplicated(utrPos))
        if (length(dup) > 0) {
            utrPos <- utrPos[-dup]
            genePos <- genePos[-dup]
        }
        
        S <- split(utrPos,factor(genePos,levels=levs))
        utr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],lengths(S))
    }
    ann <- utr[,c(1,2,3,4,8,6,9,10)]
    names(ann)[1] <- "chromosome"
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    message("  summarizing UTRs per gene for imported GTF")
    #s3utr <- reduceTranscripts(GRanges(ann))
    annList <- reduceTranscripts(GRanges(ann))
    s3utr <- annList$model
    names(s3utr) <- as.character(s3utr$transcript_id)
    activeLength <- annList$length
    names(activeLength) <- unique(as.character(s3utr$gene_id))
    
    if (asdf) {
        sann <- as.data.frame(s3utr)
        sann <- sann[,c(1,2,3,6,8,5,7,9)]
        if (useMap)
            names(sann)[c(1,4)] <- c("chromosome","gene_id")
        else {
            sann <- sann[,c(1,2,3,7,5,6,4,8)]
            names(sann)[c(1,4,7)] <- c("chromosome","gene_id","transcript_id")
        }
        attr(sann,"activeLength") <- activeLength
        return(sann)
    }
    else
        return(s3utr)
}

# ann <- .makeTranscriptGeneFromTxDb(txdb,map,FALSE)
.makeTranscriptGeneFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- transcripts(txdb,columns="tx_name")
        names(gr) <- gr$tx_name
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the transcript 
        # case
        ir <- which(names(map)=="exon_id")
        if (length(ir) > 0)
            smap <- map[,-ir,drop=FALSE]
        dgt <- which(duplicated(smap))
        if (length(dgt) > 0)
            smap <- smap[-dgt,]
        
        # Add metadata from the map
        rownames(smap) <- smap$transcript_id
        smap <- smap[names(gr),,drop=FALSE]
        gr$gene_id <- smap$gene_name
        gr$gene_name <- smap$gene_name
        gr$biotype <- smap$biotype
    }
    else {
        gr <- transcripts(txdb,columns=c("tx_name","gene_id","tx_type"))
        if (is(gr$gene_id,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
        else
            gtmp <- as.character(gr$gene_id)
        gr$gene_id <- gtmp
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(lapply(gr$tx_type,function(x) x[1]))
        gr$tx_type <- NULL
    }
    
    ann <- as.data.frame(gr)
    ann <- ann[,c(1,2,3,6,7,5,8,9)]
    names(ann)[c(1,4)] <- c("chromosome","transcript_id")
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeSumTranscriptGeneFromTxDb(txdb,map,FALSE)
.makeSumTranscriptGeneFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- transcripts(txdb,columns="tx_name")
        names(gr) <- gr$tx_name
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the transcript 
        # case
        ir <- which(names(map)=="exon_id")
        if (length(ir) > 0)
            smap <- map[,-ir,drop=FALSE]
        dgt <- which(duplicated(smap))
        if (length(dgt) > 0)
            smap <- smap[-dgt,]
        
        # Add metadata from the map
        rownames(smap) <- smap$transcript_id
        smap <- smap[names(gr),,drop=FALSE]
        gr$gene_id <- smap$gene_name
        gr$gene_name <- smap$gene_name
        gr$biotype <- smap$biotype
    }
    else {
        gr <- transcripts(txdb,columns=c("tx_name","gene_id","tx_type"))
        if (is(gr$gene_id,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
        else
            gtmp <- as.character(gr$gene_id)
        gr$gene_id <- gtmp
        #gr$gene_id <- as.character(gr$gene_id)
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(lapply(gr$tx_type,function(x) x[1]))
        gr$tx_type <- NULL
    }
    
    ann <- as.data.frame(gr)
    ann <- ann[,c(1,2,3,6,7,5,8,9)]
    names(ann)[c(1,4)] <- c("chromosome","transcript_id")
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    #stranscript <- reduceTranscripts(GRanges(ann))
    annList <- reduceTranscripts(GRanges(ann))
    stranscript <- annList$model
    names(stranscript) <- as.character(stranscript$transcript_id)
    activeLength <- annList$length
    #names(activeLength) <- as.character(stranscript$transcript_id)
    names(activeLength) <- unique(stranscript$gene_id)
    
    if (asdf) {
        sann <- as.data.frame(stranscript)
        sann <- sann[,c(1,2,3,6,8,5,7,9)]
        names(sann)[c(1,4)] <- c("chromosome","transcript_id")
        attr(sann,"activeLength") <- activeLength
        return(sann)
    }
    else
        return(stranscript)
}

# ann <- .makeTranscriptExonFromTxDb(txdb,map,FALSE)
.makeTranscriptExonFromTxDb <- function(txdb,map,asdf) {
    gr <- exonsBy(txdb,by="tx")
    tr <- transcripts(txdb,columns=c("tx_name","gene_id"))
    if (is(tr$gene_id,"CharacterList"))
        gtmp <- unlist(lapply(tr$gene_id,function(x) x[1]))
    else
        gtmp <- tr$gene_id
    names(gr) <- names(tr) <- tr$tx_name
        
    if (!is.null(map)) {
        grTmp <- as.data.frame(gr)
        grTmp$gene_id <- rep(gtmp,lengths(gr))
        names(grTmp)[2] <- "transcript_id"
        keep <- c("seqnames","start","end","exon_name","transcript_id","strand")
        ann <- grTmp[,keep]
        ann$transcript_id <- as.character(ann$transcript_id)
        rownames(ann) <- paste(ann$exon_name,ann$transcript_id,sep="_")
        
        # There are cases where exons are unnamed and just the structure 
        useMap <- TRUE
        if (any(is.na(gr$exon_name)))
            useMap <- FALSE
        
        if (useMap) {
            rownames(map) <- paste(map$exon_id,map$transcript_id,sep="_")

            # Different case with map here
            smap <- map[intersect(rownames(ann),rownames(map)),]
            ann <- ann[intersect(rownames(ann),rownames(map)),]
            
            # We need to add gene_id, gene_name, biotype from the map
            # Remove the exon_id column from the map for the gene case
            ir <- which(names(map)=="gene_id")
            if (length(ir) > 0)
                smap <- smap[,-ir,drop=FALSE]
            
            # Add metadata from the map
            ann$gene_name <- smap$gene_name
            ann$biotype <- smap$biotype
            
            names(ann)[c(1,4)] <- c("chromosome","exon_id")
            ann$chromosome <- as.character(ann$chromosome)
            ord <- order(ann$chromosome,ann$start)
            ann <- ann[ord,]
            smap <- smap[ord,]
            if (length(unique(ann$exon_id)) == length(ann$exon_id))
                rownames(ann) <- as.character(ann$exon_id)
            else
                rownames(ann) <- rownames(smap)
        }
        else {
            # In order to create the exon annotation, we need to manually 
            # overlap and assign to transcripts
            gr <- unlist(gr)
            ov <- findOverlaps(gr,tr)
            exonPos <- queryHits(ov)
            tranPos <- subjectHits(ov)
            levs <- unique(tranPos)
            
            dup <- which(duplicated(exonPos))
            if (length(dup) > 0) {
                exonPos <- exonPos[-dup]
                tranPos <- tranPos[-dup]
            }
            
            S <- split(exonPos,factor(tranPos,levels=levs))
            gr$transcript_id <- rep(names(tr)[levs],lengths(S))
            gr$gene_name <- rep(tr$gene_id[levs],lengths(S))
            gr$biotype <- rep("transcript",length(gr))
            if (is(gr$gene_name,"CharacterList"))
                gtmp <- unlist(lapply(gr$gene_name,function(x) x[1]))
            else
                gtmp <- gr$gene_name
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gr$transcript_id,sep="")
            gr$exon_name <- names(gr)
            gr$gene_name <- gtmp
            
            ann <- as.data.frame(unname(gr))
            ann <- ann[,c(1,2,3,7,9,5,10,11)]
            names(ann)[c(1,4)] <- c("chromosome","exon_id")
            ann$chromosome <- as.character(ann$chromosome)
            ord <- order(ann$chromosome,ann$start)
            ann <- ann[ord,]
            if (length(unique(ann$exon_id)) == length(ann$exon_id))
                # Should always be TRUE
                rownames(ann) <- as.character(ann$exon_id)
        }
    }
    else {
        # The same as above... some repetition, we check later
        gr <- unlist(gr)
        ov <- findOverlaps(gr,tr)
        exonPos <- queryHits(ov)
        tranPos <- subjectHits(ov)
        levs <- unique(tranPos)
        
        dup <- which(duplicated(exonPos))
        if (length(dup) > 0) {
            exonPos <- exonPos[-dup]
            tranPos <- tranPos[-dup]
        }
        
        S <- split(exonPos,factor(tranPos,levels=levs))
        gr$transcript_id <- rep(names(tr)[levs],lengths(S))
        gr$gene_name <- rep(tr$gene_id[levs],lengths(S))
        gr$biotype <- rep("transcript",length(gr))
        if (is(gr$gene_name,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_name,function(x) x[1]))
        else
            gtmp <- gr$gene_name
        names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
            gr$transcript_id,sep="")
        gr$exon_name <- names(gr)
        gr$gene_name <- gtmp
        
        ann <- as.data.frame(unname(gr))
        ann <- ann[,c(1,2,3,7,9,5,10,11)]
        names(ann)[c(1,4)] <- c("chromosome","exon_id")
        ann$chromosome <- as.character(ann$chromosome)
        ord <- order(ann$chromosome,ann$start)
        ann <- ann[ord,]
        if (length(unique(ann$exon_id)) == length(ann$exon_id))
            # Should always be TRUE
            rownames(ann) <- as.character(ann$exon_id)
    }
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeSumTranscriptExonFromTxDb(txdb,map,FALSE)
.makeSumTranscriptExonFromTxDb <- function(txdb,map,asdf) {
    # Test code rewrapping
    annGr <- .makeTranscriptExonFromTxDb(txdb,map,asdf=FALSE)
    
    # Do the rest
    annList <- reduceTranscriptsExons(annGr)
    strexon <- annList$model
    names(strexon) <- as.character(strexon$exon_id)
    activeLength <- annList$length
    names(activeLength) <- unique(strexon$transcript_id)
    
    if (asdf) {
        sann <- as.data.frame(strexon)
        sann <- sann[,c(1,2,3,6,7,5,8,9)]
        names(sann)[c(1,4)] <- c("chromosome","exon_id")
        attr(sann,"activeLength") <- activeLength
        return(sann)
    }
    else
        return(strexon)   
}

# ann <- .makeTranscriptUtrFromTxDb(txdb,map,FALSE)
.makeTranscriptUtrFromTxDb <- function(txdb,map,asdf) {
    utrList <- threeUTRsByTranscript(txdb,use.names=TRUE)
    utrGr <- unlist(utrList)
    
    if (length(utrGr) == 0) {
        warning("No UTR information was found in the provided GTF file! Will ",
            "return an empty object...",immediate.=TRUE)
        ann <- data.frame(chromosome=1,start=1,end=1,
            transcript_id=1,gene_id=1,strand=1,gene_name=1,
            biotype=1,row.names=1)
        # Strange but required to be compatible with GRanges
        ann <- ann[-1,]
        if (asdf)
            return(ann)
        else
            return(GRanges(ann))
    }
    
    utrGr$transcript_id <- names(utrGr)
    utrTmp <- as.data.frame(unname(utrGr))
    keep <- c("seqnames","start","end","transcript_id",
        "exon_rank","strand","exon_name")
    utr <- utrTmp[,keep]
    
    useMap <- TRUE
    if (any(is.na(utr$exon_name)) || is.null(map))
        useMap <- FALSE
    else
        rownames(utr) <- paste(utr$exon_name,utr$transcript_id,sep="_")
    
    if (useMap) {
        rownames(map) <- paste(map$exon_id,map$transcript_id,sep="_")
    
        # Different case with map here
        smap <- map[rownames(utr),]
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the gene case
        ir <- which(names(map)=="exon_id")
        if (length(ir) > 0)
            smap <- smap[,-ir,drop=FALSE]
        
        # Add metadata from the map
        #rownames(smap) <- smap$transcript_id
        #smap <- smap[rownames(utr),]
        utr$gene_id <- smap$gene_id
        utr$gene_name <- smap$gene_name
        utr$biotype <- smap$biotype
    }
    else {
        # Add metadata from the utr frame
        utr$gene_id <- utr$transcript_id
        utr$gene_name <- utr$gene_id
        
        # To get biotypes
        ge <- genes(txdb,columns=c("gene_id","tx_type"))
        ov <- findOverlaps(utrGr,ge)
        utrPos <- queryHits(ov)
        genePos <- subjectHits(ov)
        levs <- unique(genePos)
        
        dup <- which(duplicated(utrPos))
        if (length(dup) > 0) {
            utrPos <- utrPos[-dup]
            genePos <- genePos[-dup]
        }
        
        S <- split(utrPos,factor(genePos,levels=levs))
        utr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],lengths(S))
        #utr$biotype <- rep("gene",nrow(utr))
    }
    
    #ann <- utr[,c(1,2,3,4,7,6,8,9)]
    ann <- utr[,c(1,2,3,4,8,6,9,10)]
    names(ann)[1] <- "chromosome"
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

# ann <- .makeSumTranscriptUtrFromTxDb(txdb,map,FALSE)
.makeSumTranscriptUtrFromTxDb <- function(txdb,map,asdf) {
    utrList <- threeUTRsByTranscript(txdb,use.names=TRUE)
    utrGr <- unlist(utrList)
    
    if (length(utrGr) == 0) {
        warning("No UTR information was found in the provided GTF file! Will ",
            "return an empty object...",immediate.=TRUE)
        ann <- data.frame(chromosome=1,start=1,end=1,
            transcript_id=1,gene_id=1,strand=1,gene_name=1,
            biotype=1,row.names=1)
        # Strange but required to be compatible with GRanges
        ann <- ann[-1,]
        if (asdf)
            return(ann)
        else
            return(GRanges(ann))
    }
    
    utrGr$transcript_id <- names(utrGr)
    utrTmp <- as.data.frame(unname(utrGr))
    keep <- c("seqnames","start","end","transcript_id",
        "exon_rank","strand","exon_name")
    utr <- utrTmp[,keep]
    
    useMap <- TRUE
    if (any(is.na(utr$exon_name)) || is.null(map))
        useMap <- FALSE
    else
        rownames(utr) <- paste(utr$exon_name,utr$transcript_id,sep="_")
    
    if (useMap) {
        rownames(map) <- paste(map$exon_id,map$transcript_id,sep="_")
        
        # Different case with map here
        smap <- map[rownames(utr),]
        
        # We need to add gene_id, gene_name, biotype from the map
        # Remove the exon_id column from the map for the gene case
        ir <- which(names(map)=="exon_id")
        if (length(ir) > 0)
            smap <- smap[,-ir,drop=FALSE]
        
        # Add metadata from the map
        utr$gene_id <- smap$gene_id
        utr$gene_name <- smap$gene_name
        utr$biotype <- smap$biotype
    }
    else {
        # Add metadata from the utr frame
        utr$gene_id <- utr$transcript_id
        utr$gene_name <- utr$gene_id
        
        # To get biotypes
        ge <- genes(txdb,columns=c("gene_id","tx_type"))
        ov <- findOverlaps(utrGr,ge)
        utrPos <- queryHits(ov)
        genePos <- subjectHits(ov)
        levs <- unique(genePos)
        
        dup <- which(duplicated(utrPos))
        if (length(dup) > 0) {
            utrPos <- utrPos[-dup]
            genePos <- genePos[-dup]
        }
        
        S <- split(utrPos,factor(genePos,levels=levs))
        utr$biotype <- rep(unlist(ge$tx_type,use.names=FALSE)[levs],lengths(S))
    }
    
    #ann <- utr[,c(1,2,3,4,7,6,8,9)]
    ann <- utr[,c(1,2,3,4,8,6,9,10)]
    names(ann)[1] <- "chromosome"
    ann$chromosome <- as.character(ann$chromosome)
    ann <- ann[order(ann$chromosome,ann$start),]
    
    message("  summarizing UTRs per gene for imported GTF")
    #s3utrTranscript <- reduceTranscriptsUtr(GRanges(ann))
    annList <- reduceTranscriptsUtr(GRanges(ann))
    s3utrTranscript <- annList$model
    names(s3utrTranscript) <- 
        as.character(s3utrTranscript$transcript_id)
    activeLength <- annList$length
    #names(activeLength) <- as.character(s3utrTranscript$transcript_id)
    # Remember, gene_id in UTR's per transcript is the transcript
    names(activeLength) <- unique(as.character(s3utrTranscript$gene_id))
    
    if (asdf) {
        sann <- as.data.frame(s3utrTranscript)
        sann <- sann[,c(1,2,3,6,8,5,7,9)]
        names(sann)[c(1,4)] <- c("chromosome","transcript_id")
        attr(sann,"activeLength") <- activeLength
        return(sann)
    }
    else
        return(s3utrTranscript)
}

# ann <- .makeExonExonFromTxDb(txdb,map,FALSE)
.makeExonExonFromTxDb <- function(txdb,map,asdf) {
    if (!is.null(map)) {
        gr <- exons(txdb,columns="exon_name")
        
        # There are cases where exons are unnamed and just the structure 
        useMap <- TRUE
        if (any(is.na(gr$exon_name)))
            useMap <- FALSE
        
        if (useMap) {
            names(gr) <- gr$exon_name
            # We need to add gene_id, gene_name, biotype from the map
            # Remove the transcript_id column from the map for the gene
            # case
            ir <- which(names(map)=="transcript_id")
            if (length(ir) > 0)
                smap <- map[,-ir,drop=FALSE]
            dgt <- which(duplicated(smap))
            if (length(dgt) > 0)
                smap <- smap[-dgt,]
            
            # Add metadata from the map
            rownames(smap) <- smap$exon_id
            smap <- smap[names(gr),,drop=FALSE]
            gr$gene_id <- smap$gene_id
            gr$gene_name <- smap$gene_name
            gr$biotype <- smap$biotype
            
            ann <- as.data.frame(unname(gr))
            #ann <- ann[,c(1,2,3,6,8,5,7,9)]
            ann <- ann[,c(1,2,3,6,7,5,8,9)]
            names(ann)[c(1,4)] <- c("chromosome","exon_id")
            ann$chromosome <- as.character(ann$chromosome)
            ord <- order(ann$chromosome,ann$start)
            ann <- ann[ord,]
            smap <- smap[ord,]
            if (length(unique(ann$exon_id)) == length(ann$exon_id))
                rownames(ann) <- as.character(ann$exon_id)
            else
                rownames(ann) <- rownames(smap)
        }
        else {
            # In order to create the exon annotation, we need to manually 
            # overlap and assign to genes
            ge <- genes(txdb)
            ov <- findOverlaps(gr,ge)
            exonPos <- queryHits(ov)
            genePos <- subjectHits(ov)
            levs <- unique(genePos)
            
            dup <- which(duplicated(exonPos))
            if (length(dup) > 0) {
                exonPos <- exonPos[-dup]
                genePos <- genePos[-dup]
            }
            
            S <- split(exonPos,factor(genePos,levels=levs))
            gr$gene_id <- gr$gene_name <- rep(names(ge)[levs],lengths(S))
            gr$biotype <- rep("gene",length(gr))
            if (is(gr$gene_id,"CharacterList"))
                gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
            else
                gtmp <- gr$gene_id
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
            gr$exon_name <- names(gr)
            
            ann <- as.data.frame(unname(gr))
            ann <- ann[,c(1,2,3,6,7,5,8,9)]
            names(ann)[c(1,4)] <- c("chromosome","exon_id")
            ann$chromosome <- as.character(ann$chromosome)
            ord <- order(ann$chromosome,ann$start)
            ann <- ann[ord,]
            if (length(unique(ann$exon_id)) == length(ann$exon_id))
                # Should always be TRUE
                rownames(ann) <- as.character(ann$exon_id)
        }
    }
    else {
        gr <- exons(txdb,columns=c("exon_name","gene_id","tx_type"))
        if (is(gr$gene_id,"CharacterList"))
            gtmp <- unlist(lapply(gr$gene_id,function(x) x[1]))
        else
            gtmp <- gr$gene_id
        if (any(is.na(gr$exon_name)))            
            names(gr) <- paste(seqnames(gr),":",start(gr),"-",end(gr),"_",
                gtmp,sep="")
        else
            names(gr) <- gr$exon_name
        gr$gene_id <- gtmp
        gr$gene_name <- gr$gene_id
        gr$biotype <- unlist(lapply(gr$tx_type,function(x) x[1]))
        gr$tx_type <- NULL
        ann <- as.data.frame(unname(gr))
        ann <- ann[,c(1,2,3,6,7,5,8,9)]
        names(ann)[c(1,4)] <- c("chromosome","exon_id")
        ann$chromosome <- as.character(ann$chromosome)
        ord <- order(ann$chromosome,ann$start)
        ann <- ann[ord,]
        if (length(unique(ann$exon_id)) == length(ann$exon_id))
            # Should always be TRUE
            rownames(ann) <- as.character(ann$exon_id)
    }
    
    if (asdf)
        return(ann)
    else
        return(GRanges(ann))
}

.makeIdMap <- function(grdf) {
    # Some basic names must be present in order to make a map
    if (!any(c("exon_id","transcript_id","gene_id") %in% names(grdf))
        || any(is.na(grdf$exon_id))
        || any(is.na(grdf$transcript_id))
        || any(is.na(grdf$gene_id)))
        return(NULL)

    #hasGeneName <- hasBiotype <- FALSE
    if (!all(is.na(grdf$gene_name)) && !all(is.na(grdf$gene_biotype))) {
        map <- grdf[,c("exon_id","transcript_id","gene_id","gene_name",
            "gene_biotype")]
        names(map)[5] <- "biotype"
        #hasGeneName <- hasBiotype <- TRUE
    }
    else if (all(is.na(grdf$gene_name)) && !all(is.na(grdf$gene_biotype))) {
        map <- grdf[,c("exon_id","transcript_id","gene_id","gene_id",
            "gene_biotype")]
        names(map)[4:5] <- c("gene_name","biotype")
        #hasBiotype <- TRUE
    }
    else if (!all(is.na(grdf$gene_name)) && all(is.na(grdf$gene_biotype))) {
        map <- grdf[,c("exon_id","transcript_id","gene_id","gene_name")]
        map$gene_biotype <- rep("gene",nrow(map))
        names(map)[5] <- "biotype"
        #hasGeneName <- TRUE
    }
    else {
        map <- grdf[,c("exon_id","transcript_id","gene_id","gene_id")]
        map$gene_biotype <- rep("gene",nrow(map))
        names(map)[4:5] <- c("gene_name","biotype")
    }
    nag <- which(is.na(map$gene_name))
    if (length(nag) > 0)
        map$gene_name[nag] <- map$gene_id[nag]
    nab <- which(is.na(map$biotype))
    if (length(nab) > 0)
        map$biotype[nag] <- "gene"
    return(map)
}
