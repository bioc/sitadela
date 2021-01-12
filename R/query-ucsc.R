.getUcscTabledefUcsc <- function(org,what="queries") {
    switch(org,
        hg18 = {
            return(list(
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownGene=.getUcscTblTpl("knownGene",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        hg19 = {
            return(list(
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownGene=.getUcscTblTpl("knownGene",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        hg38 = {
            return(list(
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownGene=.getUcscTblTpl("knownGene",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        mm9 = {
            return(list(
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownGene=.getUcscTblTpl("knownGene",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        mm10 = {
            return(list(
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownGene=.getUcscTblTpl("knownGene",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        rn5 = {
            return(list(
                mgcGenes=.getUcscTblTpl("mgcGenes",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        rn6 = {
            return(list(
                mgcGenes=.getUcscTblTpl("mgcGenes",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        dm3 = {
            return(list(
                flyBaseCanonical=
                    .getUcscTblTpl("flyBaseCanonical",what),
                flyBaseGene=
                    .getUcscTblTpl("flyBaseGene",what),
                flyBaseToRefSeq=
                    .getUcscTblTpl("flyBaseToRefSeq",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what),
                refFlat=.getUcscTblTpl("refFlat",what)
            ))
        },
        dm6 = {
            warning("No UCSC Genome annotation for Drosophila ",
                "melanogaster v6! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        danrer7 = {
            return(list(
                mgcGenes=.getUcscTblTpl("mgcGenes",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        danrer10 = {
            return(list(
                mgcGenes=.getUcscTblTpl("mgcGenes",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        pantro4 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        pantro5 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        #pantro6 = {
        #    warning("No UCSC Genome annotation for Pan ",
        #        "troglodytes! Will use RefSeq instead...",
        #        immediate.=TRUE)
        #    return(list(
        #        refFlat=.getUcscTblTpl("refFlat",what),
        #        ensemblToGeneName=
        #            .getUcscTblTpl("ensemblToGeneName",what),
        #        ensemblSource=
        #            .getUcscTblTpl("ensemblSource",what)
        #    ))
        #},
        susscr3 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        susscr11 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v11! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        equcab2 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v2! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        equcab3 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        }
    )
}

.getUcscQueryUcscGene <- function(org) {
    switch(org,
        hg18 = {
            return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownCanonical` INNER JOIN `knownGene`",
                "ON knownCanonical.transcript=knownGene.name",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY gene_id",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownCanonical` INNER JOIN `knownGene`",
                "ON knownCanonical.transcript=knownGene.name",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should have been like hg19 but it's like hg18
            return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownCanonical` INNER JOIN `knownGene`",
                "ON knownCanonical.transcript=knownGene.name",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY gene_id",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownCanonical` INNER JOIN `knownGene`",
                "ON knownCanonical.transcript=knownGene.name",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            #return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
            #   "`chromStart` AS `start`,",
            #   "`chromEnd` AS `end`,",
            #   "`transcript` AS `gene_id`,",
            #   "0 AS `gc_content`,",
            #   "knownGene.strand AS `strand`,",
            #   "`geneName` AS `gene_name`,",
            #   "`source` AS `biotype`",
            #   "FROM `knownCanonical` INNER JOIN `knownGene`", 
            #   "ON knownCanonical.transcript=knownGene.name",
            #   "INNER JOIN `knownToRefSeq`", 
            #   "ON knownCanonical.transcript=knownToRefSeq.name",
            #   "INNER JOIN `knownToEnsembl`",
            #   "ON knownCanonical.transcript=knownToEnsembl.name",
            #   "INNER JOIN `ensemblSource`",
            #   "ON knownToEnsembl.value=ensemblSource.name",
            #   "INNER JOIN `refFlat`",
            #   "ON knownToRefSeq.value=refFlat.name",
            #   "GROUP BY `gene_id`",
            #   "ORDER BY `chromosome`,`start`"))
            ## No Ensembl source...
            return(paste("SELECT knownCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownCanonical` INNER JOIN `knownGene`",
                "ON knownCanonical.transcript=knownGene.name",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY gene_id",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
            #    "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "0 AS `gc_content`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'gene' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        rn6 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
            #    "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "0 AS `gc_content`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'gene' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        dm3 = {
            return(paste("SELECT flyBaseCanonical.chrom AS `chromosome`,",
                "`chromStart` AS `start`,",
                "`chromEnd` AS `end`,",
                "`transcript` AS `gene_id`,",
                "0 AS `gc_content`,",
                "flyBaseGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `flyBaseCanonical` INNER JOIN `flyBaseGene`", 
                "ON flyBaseCanonical.transcript=flyBaseGene.name",
                "INNER JOIN `flyBaseToRefSeq`",
                "ON flyBaseCanonical.transcript=flyBaseToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON flyBaseToRefSeq.value=refFlat.name",
                "INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm6 = {
            warning("No UCSC Genome annotation for Drosophila ",
                "melanogaster v6! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`", 
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer7 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
            #    "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "0 AS `gc_content`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'gene' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer10 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
            #    "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "0 AS `gc_content`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'gene' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer11 = {
            warning("No UCSC Genome annotation for Danio rerio v11! Will use ",
                "RefSeq instead...",immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro4 = {
            warning("No UCSC Genome annotation for Pan troglodytes v4! Will ",
                "use RefSeq instead...",immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro5 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes v5! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr3 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr11 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v11! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab2 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v2! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab3 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`gene_id`,",
                "`gc_content`,`strand`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        }
    )
}

.getUcscQueryUcscTranscript <- function(org) {
    switch(org,
        hg18 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.txStart AS `start`,",
                "knownGene.txEnd AS `end`,",
                "knownGene.name AS `transcript_id`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownToRefSeq`", 
                "ON knownGene.name=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.txStart AS `start`,",
                "knownGene.txEnd AS `end`,",
                "knownGene.name AS `transcript_id`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownToRefSeq`", 
                "ON knownGene.name=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`",
                "ON knownGene.name=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat` ON", 
                "knownToRefSeq.value=refFlat.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should be the same as hg19 but is like hg18
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.txStart AS `start`,",
                "knownGene.txEnd AS `end`,",
                "knownGene.name AS `transcript_id`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownToRefSeq`",
                "ON knownGene.name=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.txStart AS `start`,",
                "knownGene.txEnd AS `end`,",
                "knownGene.name AS `transcript_id`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownToRefSeq`",
                "ON knownGene.name=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`",
                "ON knownGene.name=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat` ON",
                "knownToRefSeq.value=refFlat.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            #return(paste("SELECT knownGene.chrom AS `chromosome`,",
            #   "knownGene.txStart AS `start`,",
            #   "knownGene.txEnd AS `end`,",
            #   "knownGene.name AS `transcript_id`,",
            #   "knownGene.strand AS `strand`,",
            #   "`geneName` AS `gene_name`,",
            #   "`source` AS `biotype`",
            #   "FROM `knownGene` INNER JOIN `knownToRefSeq`", 
            #   "ON knownGene.name=knownToRefSeq.name",
            #   "INNER JOIN `knownToEnsembl`",
            #   "ON knownGene.name=knownToEnsembl.name",
            #   "INNER JOIN `ensemblSource`",
            #   "ON knownToEnsembl.value=ensemblSource.name",
            #   "INNER JOIN `refFlat` ON",
            #   "knownToRefSeq.value=refFlat.name",
            #   "GROUP BY `transcript_id`",
            #   "ORDER BY `chromosome`,`start`"))
            ## No ensemblSource...
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.txStart AS `start`,",
                "knownGene.txEnd AS `end`,",
                "knownGene.name AS `transcript_id`,",
                "knownGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownToRefSeq`",
                "ON knownGene.name=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            #return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `transcript_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`",
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `transcript_id`",
            #    "ORDER BY `chromosome`,`start`"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `transcript_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'transcript' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        rn6 = {
            #return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `transcript_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`",
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `transcript_id`",
            #    "ORDER BY `chromosome`,`start`"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `transcript_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'gene' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm3 = {
            return(paste("SELECT flyBaseGene.chrom AS `chromosome`,",
                "flyBaseGene.txStart AS `start`,",
                "flyBaseGene.txEnd AS `end`,",
                "flyBaseGene.name AS `transcript_id`,",
                "flyBaseGene.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `flyBaseGene` INNER JOIN `flyBaseToRefSeq`",
                "ON flyBaseGene.name=flyBaseToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON flyBaseToRefSeq.value=refFlat.name",
                "INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm6 = {
            warning("No UCSC Genome annotation for Drosophila ",
                "melanogaster v6! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer7 = {
            #return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `transcript_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `transcript_id`",
            #    "ORDER BY `chromosome`,`start`"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `transcript_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'transcript' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer10 = {
            #return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
            #    "`txStart` AS `start`,",
            #    "`txEnd` AS `end`,",
            #    "mgcGenes.name AS `transcript_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `transcript_id`",
            #    "ORDER BY `chromosome`,`start`"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "mgcGenes.name AS `transcript_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_name`,",
                "'transcript' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer11 = {
            warning("No UCSC Genome annotation for Danio rerio v11! Will use ",
                "RefSeq instead...",immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON refFlat.geneName=ensemblToGeneName.value",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        pantro4 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes v4! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        pantro5 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes v5! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        susscr3 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        susscr11 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v11! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        equcab2 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v2! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        equcab3 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v2! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        }
    )
}

.getUcscQueryUcscExon <- function(org) {
    switch(org,
        hg18 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.exonStarts AS `start`,",
                "knownGene.exonEnds AS `end`,",
                "knownGene.name AS `exon_id`,",
                "knownGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownCanonical`", 
                "ON knownGene.name=knownCanonical.transcript",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.exonStarts AS `start`,",
                "knownGene.exonEnds AS `end`,",
                "knownGene.name AS `exon_id`,",
                "knownGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownCanonical`", 
                "ON knownGene.name=knownCanonical.transcript",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`", 
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            # Should be the same as hg19 but is as hg18
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.exonStarts AS `start`,",
                "knownGene.exonEnds AS `end`,",
                "knownGene.name AS `exon_id`,",
                "knownGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownCanonical`", 
                "ON knownGene.name=knownCanonical.transcript",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.exonStarts AS `start`,",
                "knownGene.exonEnds AS `end`,",
                "knownGene.name AS `exon_id`,",
                "knownGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownCanonical`",
                "ON knownGene.name=knownCanonical.transcript",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            #return(paste("SELECT knownGene.chrom AS `chromosome`,",
            #   "knownGene.exonStarts AS `start`,",
            #   "knownGene.exonEnds AS `end`,",
            #   "knownGene.name AS `exon_id`,",
            #   "knownGene.strand AS `strand`,",
            #   "`transcript` AS `gene_id`,",
            #   "`geneName` AS `gene_name`,",
            #   "`source` AS `biotype`",
            #   "FROM `knownGene` INNER JOIN `knownCanonical`",
            #   "ON knownGene.name=knownCanonical.transcript",
            #   "INNER JOIN `knownToRefSeq`",
            #   "ON knownCanonical.transcript=knownToRefSeq.name",
            #   "INNER JOIN `knownToEnsembl`",
            #   "ON knownCanonical.transcript=knownToEnsembl.name",
            #   "INNER JOIN `ensemblSource`",
            #   "ON knownToEnsembl.value=ensemblSource.name",
            #   "INNER JOIN `refFlat`",
            #   "ON knownToRefSeq.value=refFlat.name",
            #   "GROUP BY knownGene.name",
            #   "ORDER BY `chromosome`,`start`"))
            ## No Ensembl source...
            return(paste("SELECT knownGene.chrom AS `chromosome`,",
                "knownGene.exonStarts AS `start`,",
                "knownGene.exonEnds AS `end`,",
                "knownGene.name AS `exon_id`,",
                "knownGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `knownGene` INNER JOIN `knownCanonical`", 
                "ON knownGene.name=knownCanonical.transcript",
                "INNER JOIN `knownToRefSeq`",
                "ON knownCanonical.transcript=knownToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON knownToRefSeq.value=refFlat.name",
                "GROUP BY knownGene.name",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            #return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
            #    "`exonStarts` AS `start`,",
            #    "`exonEnds` AS `end`,",
            #    "mgcGenes.name AS `exon_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`", 
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_id`",
            #    "ORDER BY `chromosome`,`start`"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "mgcGenes.name AS `exon_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_id`,",
                "mgcGenes.name AS `gene_name`,",
                "'exon' AS `biotype`",
                "FROM `mgcGenes`", 
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        rn6 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
            #    "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`exonStarts` AS `start`,",
            #    "`exonEnds` AS `end`,",
            #    "mgcGenes.name AS `exon_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`",
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`" ,
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT mgcGenes.chrom AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "mgcGenes.name AS `exon_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_id`,",
                "mgcGenes.name AS `gene_name`,",
                "'exon' AS `biotype`",
                "FROM `mgcGenes`", 
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm3 = {
            return(paste("SELECT flyBaseCanonical.chrom AS `chromosome`,",
                "flyBaseGene.exonStarts AS `start`,",
                "flyBaseGene.exonEnds AS `end`,",
                "`transcript` AS `exon_id`,",
                "flyBaseGene.strand AS `strand`,",
                "`transcript` AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `flyBaseCanonical` INNER JOIN `flyBaseGene` ON",
                "flyBaseCanonical.transcript=flyBaseGene.name",
                "INNER JOIN `flyBaseToRefSeq`",
                "ON flyBaseCanonical.transcript=flyBaseToRefSeq.name",
                "INNER JOIN `refFlat`",
                "ON flyBaseToRefSeq.value=refFlat.name",
                "INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm6 = {
            warning("No UCSC Genome annotation for Drosophila ",
                "melanogaster v6! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer7 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
            #    "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`exonStarts` AS `start`,",
            #    "`exonEnds` AS `end`,",
            #    "mgcGenes.name AS `exon_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`",
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #    "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "mgcGenes.name AS `exon_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_id`,",
                "mgcGenes.name AS `gene_name`,",
                "'exon' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer10 = {
            #return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
            #    "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
            #    "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
            #    "mgcGenes.chrom AS `chromosome`,",
            #    "`exonStarts` AS `start`,",
            #    "`exonEnds` AS `end`,",
            #    "mgcGenes.name AS `exon_id`,",
            #    "mgcGenes.strand AS `strand`,",
            #    "mgcGenes.name AS `gene_id`,",
            #    "`name2` AS `gene_name`,",
            #    "`source` AS `biotype`",
            #    "FROM `mgcGenes` INNER JOIN `ensemblToGeneName`",
            #    "ON mgcGenes.name2=ensemblToGeneName.value",
            #    "INNER JOIN `ensemblSource`",
            #    "ON ensemblToGeneName.name=ensemblSource.name",
            #    "GROUP BY `gene_name`",
            #   "ORDER BY `chromosome`,`start`) AS tmp"))
            ## Name 2 is empty - 11/6/2020
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "mgcGenes.chrom AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "mgcGenes.name AS `exon_id`,",
                "mgcGenes.strand AS `strand`,",
                "mgcGenes.name AS `gene_id`,",
                "mgcGenes.name AS `gene_name`,",
                "'exon' AS `biotype`",
                "FROM `mgcGenes`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        danrer11 = {
            warning("No UCSC Genome annotation for Danio rerio v11! Will use ",
                "RefSeq instead...",immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`", 
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro4 = {
            warning("No UCSC Genome annotation for Pan troglodytes v4! Will ",
                "use RefSeq instead...",immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        pantro5 = {
            warning("No UCSC Genome annotation for Pan ",
                "troglodytes v5! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr3 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        susscr11 = {
            warning("No UCSC Genome annotation for Sus ",
                "scrofa v11! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab2 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v2! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        },
        equcab3 = {
            warning("No UCSC Genome annotation for Equus ",
                "caballus v3! Will use RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT `chromosome`,`start`,`end`,`exon_id`,",
                "`strand`,`gene_id`,`gene_name`,`biotype` FROM",
                "(SELECT MAX(`txEnd` - `txStart`) AS `width`,",
                "refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `ensemblToGeneName`",
                "ON ensemblToGeneName.value=refFlat.geneName",
                "INNER JOIN `ensemblSource`",
                "ON ensemblToGeneName.name=ensemblSource.name",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`) AS tmp"))
        }
    )
}
