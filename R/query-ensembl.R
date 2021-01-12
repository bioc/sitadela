.getGeneAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9","tair10"))
        return(c(
            "chromosome_name",
            "start_position",
            "end_position",
            "ensembl_gene_id",
            "percentage_gc_content",
            "strand",
            "external_gene_id",
            "gene_biotype"
        ))
    else if (org %in% c("rn5","danrer7","dm3"))
        return(c(
            "chromosome_name",
            "start_position",
            "end_position",
            "ensembl_gene_id",
            "percentage_gc_content",
            "strand",
            "external_gene_name",
            "gene_biotype"
        ))
    else
        return(c(
            "chromosome_name",
            "start_position",
            "end_position",
            "ensembl_gene_id",
            "percentage_gene_gc_content",
            "strand",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getVersionedGeneAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getGeneAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getGeneAttributes(org))
        else
            return(c(
                "chromosome_name",
                "start_position",
                "end_position",
                "ensembl_gene_id_version",
                "percentage_gene_gc_content",
                "strand",
                "external_gene_name",
                "gene_biotype"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
                "chromosome_name",
                "start_position",
                "end_position",
                "ensembl_gene_id_version",
                "percentage_gene_gc_content",
                "strand",
                "external_gene_name",
                "gene_biotype"
            ))
}

.getTranscriptAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9","tair10"))
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "ensembl_transcript_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_id",
            "gene_biotype"
        ))
    else
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "ensembl_transcript_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getVersionedTranscriptAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getTranscriptAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getTranscriptAttributes(org))
        else
            return(c(
                "chromosome_name",
                "transcript_start",
                "transcript_end",
                "ensembl_transcript_id_version",
                "strand",
                "ensembl_gene_id_version",
                "external_gene_name",
                "gene_biotype"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "ensembl_transcript_id_version",
            "strand",
            "ensembl_gene_id_version",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getTranscriptUtrAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9","tair10"))
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "3_utr_start",
            "3_utr_end",
            "ensembl_transcript_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_id",
            "gene_biotype"
        ))
    else
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "3_utr_start",
            "3_utr_end",
            "ensembl_transcript_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getVersionedTranscriptUtrAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getTranscriptUtrAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getTranscriptUtrAttributes(org))
        else
            return(c(
                "chromosome_name",
                "transcript_start",
                "transcript_end",
                "3_utr_start",
                "3_utr_end",
                "ensembl_transcript_id_version",
                "strand",
                "ensembl_gene_id_version",
                "external_gene_name",
                "gene_biotype"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "chromosome_name",
            "transcript_start",
            "transcript_end",
            "3_utr_start",
            "3_utr_end",
            "ensembl_transcript_id_version",
            "strand",
            "ensembl_gene_id_version",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getExonAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9","tair10"))
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_id",
            "gene_biotype"
        ))
    else
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_gene_id",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getVersionedExonAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getExonAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getExonAttributes(org))
        else
            return(c(
                "chromosome_name",
                "exon_chrom_start",
                "exon_chrom_end",
                "ensembl_exon_id",
                "strand",
                "ensembl_gene_id_version",
                "external_gene_name",
                "gene_biotype"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_gene_id_version",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getTranscriptExonAttributes <- function(org) {
    if (org %in% c("hg18","hg19","mm9","tair10"))
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_transcript_id",
            "external_gene_id",
            "gene_biotype"
        ))
    else
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_transcript_id",
            "external_gene_name",
            "gene_biotype"
        ))
}

.getVersionedTranscriptExonAttributes <- function(org,ver) {
    if (org %in% .orgsWithNoVersion())
        return(.getTranscriptExonAttributes(org))
    else if (org %in% .orgsWithVersionAfter90()) {
        if (ver < 90)
            return(.getTranscriptExonAttributes(org))
        else
            return(c(
                "chromosome_name",
                "exon_chrom_start",
                "exon_chrom_end",
                "ensembl_exon_id",
                "strand",
                "ensembl_transcript_id_version",
                "external_gene_name",
                "gene_biotype"
            ))
    }
    else if (org %in% .orgsWithVersion())
        return(c(
            "chromosome_name",
            "exon_chrom_start",
            "exon_chrom_end",
            "ensembl_exon_id",
            "strand",
            "ensembl_transcript_id_version",
            "external_gene_name",
            "gene_biotype"
        ))
}

.ucscToEnsembl <- function() {
    return(list(
        hg18=67,
        hg19=74:75,
        hg38=76:102,
        mm9=67,
        mm10=74:102,
        rn5=74:79,
        rn6=80:102,
        dm3=c(67,74:78),
        dm6=79:100,
        danrer7=c(67,74:79),
        danrer10=80:91,
        danrer11=92:102,
        pantro4=c(67,74:90),
        pantro5=91:102,
        #pantro6=,
        susscr3=c(67,74:89),
        susscr11=90:102,
        equcab2=c(67,74:94),
        equcab3=c(95:102)
    ))
}

.getDataset <- function(org) {
    switch(org,
        hg18 = { return("hsapiens_gene_ensembl") },
        hg19 = { return("hsapiens_gene_ensembl") },
        hg38 = { return("hsapiens_gene_ensembl") },
        mm9 = { return("mmusculus_gene_ensembl") },
        mm10 = { return("mmusculus_gene_ensembl") },
        rn5 = { return("rnorvegicus_gene_ensembl") },
        rn6 = { return("rnorvegicus_gene_ensembl") },
        dm3 = { return("dmelanogaster_gene_ensembl") },
        dm6 = { return("dmelanogaster_gene_ensembl") },
        danrer7 = { return("drerio_gene_ensembl") },
        danrer10 = { return("drerio_gene_ensembl") },
        danrer11 = { return("drerio_gene_ensembl") },
        pantro4 = { return("ptroglodytes_gene_ensembl") },
        pantro5 = { return("ptroglodytes_gene_ensembl") },
        #pantro6 = { return("ptroglodytes_gene_ensembl") },
        susscr3 = { return("sscrofa_gene_ensembl") },
        susscr11 = { return("sscrofa_gene_ensembl") },
        equcab2 = { return("ecaballus_gene_ensembl") },
        equcab3 = { return("ecaballus_gene_ensembl") },
        tair10 = { return("athaliana_eg_gene") }
    )
}

.orgsWithNoVersion <- function() {
    return(c("hg18","hg19","mm9","rn5","dm3","dm6","danrer7","susscr3",
        "tair10"))
}

.orgsWithVersionAfter90 <- function() {
    return(c("hg38","mm10","rn6","danrer10","susscr11","equcab2"))
}

.orgsWithVersion <- function() {
    return(c("danrer11","equcab3"))
}

.getBiotypes <- function(org) {
    if (!(org %in% getSupportedOrganisms()))
        return(NULL)
    switch(org,
        hg18 = {
            return(c("unprocessed_pseudogene","pseudogene","miRNA",
                "retrotransposed","protein_coding","processed_pseudogene",
                "snRNA","snRNA_pseudogene","Mt_tRNA_pseudogene",
                "miRNA_pseudogene","misc_RNA","tRNA_pseudogene","snoRNA",
                "scRNA_pseudogene","rRNA_pseudogene","snoRNA_pseudogene","rRNA",
                "misc_RNA_pseudogene","IG_V_gene","IG_D_gene","IG_J_gene",
                "IG_C_gene","IG_pseudogene","scRNA"))
        },
        hg19 = {
            return(c("pseudogene","lincRNA","protein_coding","antisense",
                "processed_transcript","snRNA","sense_intronic","miRNA",
                "misc_RNA","snoRNA","rRNA","polymorphic_pseudogene",
                "sense_overlapping","3prime_overlapping_ncrna","TR_V_gene",
                "TR_V_pseudogene","TR_D_gene","TR_J_gene","TR_C_gene",
                "TR_J_pseudogene","IG_C_gene","IG_C_pseudogene","IG_J_gene",
                "IG_J_pseudogene","IG_D_gene","IG_V_gene","IG_V_pseudogene"))
        },
        hg38 = {
            return(c("protein_coding","polymorphic_pseudogene","lincRNA",
                "unprocessed_pseudogene","processed_pseudogene","antisense",
                "processed_transcript","transcribed_unprocessed_pseudogene",
                "sense_intronic","unitary_pseudogene","IG_V_gene",
                "IG_V_pseudogene","TR_V_gene","sense_overlapping",
                "transcribed_processed_pseudogene","miRNA","snRNA","misc_RNA",
                "rRNA","snoRNA","IG_J_pseudogene","IG_J_gene","IG_D_gene",
                "3prime_overlapping_ncrna","IG_C_gene","IG_C_pseudogene",
                "pseudogene","TR_V_pseudogene","Mt_tRNA","Mt_rRNA",
                "translated_processed_pseudogene","TR_J_gene","TR_C_gene",
                "TR_D_gene","TR_J_pseudogene","LRG_gene"))
        },
        mm9 = {
            return(c("pseudogene","snRNA","protein_coding","antisense","miRNA",
                "lincRNA","snoRNA","processed_transcript","misc_RNA","rRNA",
                "sense_overlapping","sense_intronic","polymorphic_pseudogene",
                "non_coding","3prime_overlapping_ncrna","IG_C_gene",
                "IG_J_gene","IG_D_gene","IG_V_gene","ncrna_host"))
        },
        mm10 = {
            return(c("pseudogene","snRNA","protein_coding","antisense","miRNA",
                "snoRNA","lincRNA","processed_transcript","misc_RNA","rRNA",
                "sense_intronic","sense_overlapping","polymorphic_pseudogene",
                "IG_C_gene","IG_J_gene","IG_D_gene","IG_LV_gene","IG_V_gene",
                "IG_V_pseudogene","TR_V_gene","TR_V_pseudogene",
                "3prime_overlapping_ncrna"))
        },
        dm3 = {
            return(c("protein_coding","ncRNA","snoRNA","pre_miRNA","pseudogene",
                "snRNA","tRNA","rRNA"))
        },
        dm6 = {
            return(c("protein_coding","ncRNA","snoRNA","pre_miRNA","pseudogene",
                "snRNA","tRNA","rRNA"))
        },
        rn5 = {
            return(c("protein_coding","pseudogene","processed_pseudogene",
                "miRNA","rRNA","misc_RNA"))
        },
        rn6 = {
            return(c("protein_coding","pseudogene","processed_pseudogene",
                "miRNA","rRNA","misc_RNA"))
        },
        danrer7 = {
            return(c("antisense","protein_coding","miRNA","snoRNA","rRNA",
                "lincRNA","processed_transcript","snRNA","pseudogene",
                "sense_intronic","misc_RNA","polymorphic_pseudogene",
                "IG_V_pseudogene","IG_C_pseudogene","IG_J_pseudogene",
                "non_coding","sense_overlapping"
            ))
        },
        danrer10 = {
            return(c("antisense","protein_coding","miRNA","snoRNA","rRNA",
                "lincRNA","processed_transcript","snRNA","pseudogene",
                "sense_intronic","misc_RNA","polymorphic_pseudogene",
                "IG_V_pseudogene","IG_C_pseudogene","IG_J_pseudogene",
                "non_coding","sense_overlapping"
            ))
        },
        danrer11 = {
            return(c("antisense","protein_coding","miRNA","snoRNA","rRNA",
                "lincRNA","processed_transcript","snRNA","pseudogene",
                "sense_intronic","misc_RNA","polymorphic_pseudogene",
                "IG_V_pseudogene","IG_C_pseudogene","IG_J_pseudogene",
                "non_coding","sense_overlapping"
            ))
        },
        pantro4 = {
            return(c("protein_coding","pseudogene","processed_pseudogene",
                "miRNA","rRNA","snRNA","snoRNA","misc_RNA"))
        },
        pantro5 = {
            return(c("protein_coding","pseudogene","processed_pseudogene",
                "miRNA","rRNA","snRNA","snoRNA","misc_RNA"))
        },
        pantro6 = {
            return(c("protein_coding","pseudogene","processed_pseudogene",
                "miRNA","rRNA","snRNA","snoRNA","misc_RNA"))
        },
        susscr3 = {
            return(c("antisense","protein_coding","lincRNA","pseudogene",
                "processed_transcript","miRNA","rRNA","snRNA","snoRNA",
                "misc_RNA","non_coding","IG_C_gene","IG_J_gene",
                "IG_V_gene","IG_V_pseudogene"))
        },
        equcab2 = {
            return(c("miRNA","misc_RNA","protein_coding","pseudogene","rRNA",
                "processed_pseudogene","snoRNA","snRNA"))
        },
        equcab3 = {
            return(c("miRNA","misc_RNA","protein_coding","pseudogene","rRNA",
                "processed_pseudogene","snoRNA","snRNA"))
        },
        tair10 = {
            return(c("miRNA","ncRNA","protein_coding","pseudogene","rRNA",
                "snoRNA","snRNA","transposable_element","tRNA"))
        }
    )
}
