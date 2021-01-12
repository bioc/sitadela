.getUcscTabledefRefseq <- function(org,what="queries",versioned=FALSE) {
    switch(org,
        hg18 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what)
            )
        },
        hg19 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        hg38 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what)
            )
        },
        mm9 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        mm10 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        rn5 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        rn6 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        dm3 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        dm6 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        danrer7 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        danrer10 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        pantro4 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        pantro5 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        #pantro6 = {
        #    def <- list(
        #        refFlat=.getUcscTblTpl("refFlat",what),
        #        ensemblToGeneName=
        #            .getUcscTblTpl("ensemblToGeneName",what),
        #        ensemblSource=
        #            .getUcscTblTpl("ensemblSource",what)
        #    )
        #},
        susscr3 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        susscr11 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        equcab2 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        },
        equcab3 = {
            def <- list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            )
        }
    )
    
    if (versioned)
        def <- c(def,gbCdnaInfo=.getUcscTblTpl("gbCdnaInfo",what))
    
    return(def)
}

.getUcscQueryRefseqGene <- function(org,versioned=FALSE) {
    switch(org,
        hg18 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should be the same as hg19 but is as hg18
            return(paste("SELECT  refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste("SELECT  refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        rn6 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        dm3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        dm6 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer7 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer10 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer11 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro4 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro5 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr11 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab2 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        }
    )
}

.getUcscQueryRefseqTranscript <- function(org,versioned=FALSE) {
    switch(org,
        hg18 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "ORDER BY `chromosome`, `start`"))
        },
        hg19 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should be the same as hg19 but is as hg18
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "ORDER BY `chromosome`, `start`"))
        },
        mm9 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "ORDER BY `chromosome`, `start`"))
        },
        rn5 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        rn6 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm3 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm6 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer7 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer10 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer11 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        pantro4 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        pantro5 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        susscr3 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        susscr11 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        equcab2 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        equcab3 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `transcript_id`,")
                else
                    "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        }
    )
}

.getUcscQueryRefseqExon <- function(org,versioned=FALSE) {
    switch(org,
        hg18 = {
            return(paste("SELECT  refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`", 
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should be the same as hg19 but is as hg18
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`", 
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`", 
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        rn6 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        dm3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        dm6 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer7 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer10 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer11 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro4 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro5 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr11 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab2 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab3 = {
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `exon_id`,")
                else
                    "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                if (versioned)
                    paste0("CONCAT(refFlat.name,'.',",
                        "hgFixed.gbCdnaInfo.version) AS `gene_id`,")
                else
                    "refFlat.name AS `gene_id`,",
                "refFlat.geneName AS `gene_name`,",
                "ensemblSource.source AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                if (versioned)
                    paste("INNER JOIN hgFixed.gbCdnaInfo",
                        "ON refFlat.name=hgFixed.gbCdnaInfo.acc")
                else "",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        }
    )
}
