correctTranscripts <- function(ann) {
    rownames(ann) <- paste("T",seq_len(nrow(ann)),sep="_")
    len <- ann[,3] - ann[,2]
    len <- len[-which(is.na(len))]
    len[len==0] <- 1
    defUtrLen <- round(2^mean(log2(len)))
    nas <- which(is.na(ann$start))
    annNa <- ann[nas,]
    annNa$start <- annNa$tstart
    annNa$end <- annNa$tend
    tmp <- makeGRangesFromDataFrame(df=annNa)
    tmp <- flank(resize(tmp,width=1,fix="end"),start=FALSE,width=defUtrLen)
    ann[names(tmp),"start"] <- start(tmp)
    ann[names(tmp),"end"] <- end(tmp)
    return(ann)
}

reduceTranscripts <- function(gr) {
    # Get the gene ids to use as split factor
    gene <- unique(as.character(gr$gene_id))
    
    # Get the GRanges metadata to create a map for later
    meta <- elementMetadata(gr)
    if (is.null(gr$gene_name))
        meta$gene_name <- meta$gene_id
    if (is.null(gr$biotype))
        meta$biotype <- rep("gene",nrow(meta))
    
    # There will be a transcript_id
    ir <- which(names(meta) == "transcript_id")
    map <- meta[,-ir]
    
    # Remove duplicates and name the map
    d <- which(duplicated(map))
    if (length(d) > 0)
        map <- map[-d,]
    # In some cases, transcripts are recorded with multiple biotypes...
    if (length(map$gene_id) != length(unique(map$gene_id))) {
        mapp <- map
        ir <- which(names(mapp) == "biotype")
        mapp <- mapp[,-ir]
        d <- which(duplicated(mapp))
        if (length(d) > 0)
            map <- map[-d,]
    }
    rownames(map) <- map$gene_id
    map <- map[gene,]
    
    # Gene names and biotypes for later reconstruction
    gn <- as.character(map$gene_name)
    bt <- as.character(map$biotype)
    
    # Split the initial GRanges to apply rest operations
    grList <- split(gr,gr$gene_id)
    # Re-order for common reference with the map
    grList <- grList[gene]
    # Now, reduce to merge overlaping exons
    grList <- reduce(grList)
    
    # Start the reconstruction by getting new lengths and create names
    lens <- lengths(grList)
    inds <- unlist(lapply(lens,function(j) return(seq_len(j))),use.names=FALSE)
    grNew <- unname(unlist(grList))
    gene_id <- rep(gene,lens)
    transcript_id <- paste(gene_id,"MET",inds,sep="_")
    gene_name <- rep(gn,lens)
    biotype <- rep(bt,lens)
    newMeta <- DataFrame(
        transcript_id=transcript_id,
        gene_id=gene_id,
        gene_name=gene_name,
        biotype=biotype
    )
    mcols(grNew) <- newMeta
    
    # grNew is the GRanges to return. In order to get the activeLength, we split
    # again per gene_id in a temp variable
    tmp <- split(grNew,grNew$gene_id)
    tmp <- tmp[gene]
    len <- vapply(width(tmp),sum,integer(1))
    
    #return(grNew)
    return(list(model=grNew,length=len))
}

reduceTranscriptsUtr <- function(gr) {
    # Get the transcript ids to use ordering element
    trans <- unique(as.character(gr$transcript_id))
    
    # Get the GRanges metadata to create a map for later
    meta <- elementMetadata(gr)
    if (is.null(gr$gene_name))
        meta$gene_name <- meta$gene_id
    if (is.null(gr$biotype))
        meta$biotype <- rep("gene",nrow(meta))
    
    # Make the map and remove duplicates
    map <- meta
    d <- which(duplicated(map))
    if (length(d) > 0)
        map <- map[-d,,drop=FALSE]
    rownames(map) <- map$transcript_id
    map <- map[trans,]
    
    # Gene ids, names and biotypes for later reconstruction
    gi <- as.character(map$gene_id)
    gn <- as.character(map$gene_name)
    bt <- as.character(map$biotype)
    
    # Split the initial GRanges to apply rest operations
    grList <- split(gr,gr$transcript_id)
    # Re-order for common reference with the map
    grList <- grList[trans]
    # Now, reduce to merge overlaping exons
    grList <- reduce(grList)
    
    # Start the reconstruction by getting new lengths and create names
    lens <- lengths(grList)
    inds <- unlist(lapply(lens,function(j) return(seq_len(j))),use.names=FALSE)
    grNew <- unname(unlist(grList))
    transcript_id <- paste(rep(trans,lens),"MEU",inds,sep="_")
    gene_id <- rep(gi,lens)
    gene_name <- rep(gn,lens)
    biotype <- rep(bt,lens)
    newMeta <- DataFrame(
        transcript_id=transcript_id,
        #gene_id=gene_id,
        gene_id=rep(trans,lens),
        gene_name=gene_name,
        biotype=biotype
    )
    mcols(grNew) <- newMeta
    
    # grNew is the GRanges to return. In order to get the activeLength, we split
    # again per gene_id in a temp variable
    tmp <- split(grNew,grNew$gene_id)
    tmp <- tmp[trans]
    len <- vapply(width(tmp),sum,integer(1))
    
    #return(grNew)
    return(list(model=grNew,length=len))
}

reduceExons <- function(gr) {
    # Get the gene ids to use as split factor
    gene <- unique(as.character(gr$gene_id))
    
    # Get the GRanges metadata to create a map for later
    meta <- elementMetadata(gr)
    if (is.null(gr$gene_name))
        meta$gene_name <- meta$gene_id
    if (is.null(gr$biotype))
        meta$biotype <- rep("gene",nrow(meta))
    
    # There will be an exon_id
    ir <- which(names(meta) == "exon_id")
    map <- meta[,-ir]
    
    # Remove duplicates and name the map
    d <- which(duplicated(map))
    if (length(d) > 0)
        map <- map[-d,]
    rownames(map) <- map$gene_id
    map <- map[gene,]
    
    # Gene names and biotypes for later reconstruction
    gn <- as.character(map$gene_name)
    bt <- as.character(map$biotype)
    
    # Split the initial GRanges to apply rest operations
    grList <- split(gr,gr$gene_id)
    # Re-order for common reference with the map
    grList <- grList[gene]
    # Now, reduce to merge overlaping exons
    grList <- reduce(grList)
    
    # Start the reconstruction by getting new lengths and create names
    lens <- lengths(grList)
    strs <- as.character(runValue(strand(grList)))
    #inds <- unlist(lapply(lens,function(j) return(seq_len(j))),use.names=FALSE)
    inds <- unlist(lapply(seq_along(lens),function(j,L,S) {
        if (S[j] == "+")
            return(seq_len(L[j]))
        else
            return(rev(seq_len(L[j])))
    },lens,strs),use.names=FALSE)
    grNew <- unname(unlist(grList))
    gene_id <- rep(gene,lens)
    exon_id <- paste(gene_id,"MEX",inds,sep="_")
    gene_name <- rep(gn,lens)
    biotype <- rep(bt,lens)
    newMeta <- DataFrame(
        exon_id=exon_id,
        gene_id=gene_id,
        gene_name=gene_name,
        biotype=biotype
    )
    mcols(grNew) <- newMeta
    
    # grNew is the GRanges to return. In order to get the activeLength, we split
    # again per gene_id in a temp variable
    tmp <- split(grNew,grNew$gene_id)
    tmp <- tmp[gene]
    len <- vapply(width(tmp),sum,integer(1))
    
    return(list(model=grNew,length=len))
}

reduceTranscriptsExons <- function(gr) {
    # Get the transcript ids to use ordering element
    trans <- unique(as.character(gr$transcript_id))
    
    # Get the GRanges metadata to create a map for later
    meta <- elementMetadata(gr)
    if (is.null(gr$gene_name))
        meta$gene_name <- meta$gene_id
    if (is.null(gr$biotype))
        meta$biotype <- rep("gene",nrow(meta))
        
    # There will be an exon_id
    ir <- which(names(meta) == "exon_id")
    map <- meta[,-ir]
    
    # Make the map and remove duplicates
    d <- which(duplicated(map))
    if (length(d) > 0)
        map <- map[-d,,drop=FALSE]
    rownames(map) <- map$transcript_id
    map <- map[trans,]
    
    # Gene ids, names and biotypes for later reconstruction
    gn <- as.character(map$gene_name)
    bt <- as.character(map$biotype)
    
    # Split the initial GRanges to apply rest operations
    grList <- split(gr,gr$transcript_id)
    # Re-order for common reference with the map
    grList <- grList[trans]
    # Now, reduce to merge overlaping exons
    grList <- reduce(grList)
    
    # Start the reconstruction by getting new lengths and create names
    lens <- lengths(grList)
    strs <- as.character(runValue(strand(grList)))
    #inds <- unlist(lapply(lens,function(j) return(seq_len(j))),use.names=FALSE)
    inds <- unlist(lapply(seq_along(lens),function(j,L,S) {
        if (S[j] == "+")
            return(seq_len(L[j]))
        else
            return(rev(seq_len(L[j])))
    },lens,strs),use.names=FALSE)
    grNew <- unname(unlist(grList))
    exon_id <- paste(rep(trans,lens),"MTE",inds,sep="_")
    gene_name <- rep(gn,lens)
    biotype <- rep(bt,lens)
    newMeta <- DataFrame(
        exon_id=exon_id,
        transcript_id=rep(trans,lens),
        gene_name=gene_name,
        biotype=biotype
    )
    mcols(grNew) <- newMeta
    
    # grNew is the GRanges to return. In order to get the activeLength, we split
    # again per gene_id in a temp variable
    tmp <- split(grNew,grNew$transcript_id)
    tmp <- tmp[trans]
    len <- vapply(width(tmp),sum,integer(1))
    
    return(list(model=grNew,length=len))
}
