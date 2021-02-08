.getUcscTabledefNcbi <- function(org,what="queries") {
    switch(org,
        hg18 = {
            warning("No NCBI RefSeq Genome annotation for Homo ",
                "sapiens hg18! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what)
            ))
        },
        hg19 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        hg38 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        mm9 = {
            warning("No NCBI RefSeq Genome annotation for Mus ",
                "musculus mm9! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                knownToRefSeq=
                    .getUcscTblTpl("knownToRefSeq",what),
                knownCanonical=
                    .getUcscTblTpl("knownCanonical",what),
                knownToEnsembl=
                    .getUcscTblTpl("knownToEnsembl",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        mm10 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        rn5 = {
            warning("No NCBI RefSeq Genome annotation for Rattus ",
                "norvegicus rn5! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        rn6 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        dm3 = {
            warning("No NCBI RefSeq Genome annotation for Drosophila ",
                "melanogaster dm3! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        dm6 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        danrer7 = {
            warning("No NCBI RefSeq Genome annotation for Danio ",
                "rerio danrer7! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(list(
                refFlat=.getUcscTblTpl("refFlat",what),
                ensemblToGeneName=
                    .getUcscTblTpl("ensemblToGeneName",what),
                ensemblSource=
                    .getUcscTblTpl("ensemblSource",what)
            ))
        },
        danrer10 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        danrer11 = {
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        },
        pantro4 = {
            warning("No NCBI RefSeq Genome annotation for Pan troglodytes ",
                "pantro4! Will use UCSC RefSeq instead...",
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
            warning("No NCBI RefSeq Genome annotation for Pan troglodytes ",
                "pantro5! Will use UCSC RefSeq instead...",
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
        #    return(list(
        #        refFlat=.getUcscTblTpl("refFlat",what),
        #        ensemblToGeneName=
        #            .getUcscTblTpl("ensemblToGeneName",what),
        #        ensemblSource=
        #            .getUcscTblTpl("ensemblSource",what)
        #    ))
        #},
        susscr3 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v3! ",
                "Will use UCSC RefSeq instead...",
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
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v11! ",
                "Will use UCSC RefSeq instead...",
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
            warning("No NCBI RefSeq Genome annotation for Equus cabalus v2! ",
                "Will use UCSC RefSeq instead...",
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
            return(list(
                ncbiRefSeq=.getUcscTblTpl("ncbiRefSeq",what)
            ))
        }
    )
}


.getUcscQueryNcbiGene <- function(org) {
    switch(org,
        hg18 = {
            warning("No NCBI RefSeq Genome annotation for Homo ",
                "sapiens hg18! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        hg38 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        mm9 = {
            warning("No NCBI RefSeq Genome annotation for Mus ",
                "musculus mm9! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `gene_id`,",
                "0 AS `gc_content`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        rn5 = {
            warning("No NCBI RefSeq Genome annotation for Rattus ",
                "norvegicus rn5! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        rn6 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        dm3 = {
            warning("No NCBI RefSeq Genome annotation for Drosophila ",
                "melanogaster dm3! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        dm6 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        danrer7 = {
            warning("No NCBI RefSeq Genome annotation for Danio ",
                "rerio danrer7! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        danrer10 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        danrer11 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        pantro4 = {
            warning("No NCBI RefSeq Genome annotation for Pan troglodytes ",
                "pantro4! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        pantro5 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        susscr3 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v3! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        susscr11 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v11! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        equcab2 = {
            warning("No NCBI RefSeq Genome annotation for Equus cabalus v2! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        equcab3 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `gene_id`,",
                "0 AS `gc_content`,",
                "`strand` AS `strand`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        }
    )
}

.getUcscQueryNcbiTranscript <- function(org) {
    switch(org,
        hg18 = {
            warning("No NCBI RefSeq Genome annotation for Homo ",
                "sapiens hg18! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "ORDER BY `chromosome`, `start`"))
        },
        hg19 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        hg38 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        mm9 = {
            warning("No NCBI RefSeq Genome annotation for Mus ",
                "musculus mm9! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.txStart AS `start`,",
                "refFlat.txEnd AS `end`,",
                "refFlat.name AS `transcript_id`,",
                "refFlat.strand AS `strand`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        rn5 = {
            warning("No NCBI RefSeq Genome annotation for Rattus ",
                "norvegicus rn5! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        rn6 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        dm3 = {
            warning("No NCBI RefSeq Genome annotation for Drosophila ",
                "melanogaster dm3! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        dm6 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer7 = {
            warning("No NCBI RefSeq Genome annotation for Danio ",
                "rerio danrer7! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        danrer10 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        danrer11 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        pantro4 = {
            warning("No NCBI RefSeq Genome annotation for Pan troglodytes ",
                "pantro4! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        pantro5 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        },
        susscr3 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v3! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        susscr11 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v11! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        equcab2 = {
            warning("No NCBI RefSeq Genome annotation for Equus cabalus v2! ",
                "Will use UCSC RefSeq instead...",
                immediate.=TRUE)
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
        equcab3 = {
            return(paste("SELECT `chrom` AS `chromosome`,",
                "`txStart` AS `start`,",
                "`txEnd` AS `end`,",
                "`name` AS `transcript_id`,",
                "`strand` AS `strand`,",
                "name2 AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`",
                "GROUP BY `transcript_id`",
                "ORDER BY `chromosome`,`start`"))
        }
    )
}

.getUcscQueryNcbiExon <- function(org) {
    switch(org,
        hg18 = {
            warning("No NCBI RefSeq Genome annotation for Homo ",
                "sapiens hg18! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT  refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        hg19 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        hg38 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        mm9 = {
            warning("No NCBI RefSeq Genome annotation for Mus ",
                "musculus mm9! Will use UCSC RefSeq instead...",
                immediate.=TRUE)
            return(paste("SELECT refFlat.chrom AS `chromosome`,",
                "refFlat.exonStarts AS `start`,",
                "refFlat.exonEnds  AS `end`,",
                "refFlat.name AS `exon_id`,",
                "refFlat.strand AS `strand`,",
                "refFlat.name AS `gene_id`,",
                "`geneName` AS `gene_name`,",
                "`source` AS `biotype`",
                "FROM `refFlat` INNER JOIN `knownToRefSeq`",
                "ON refFlat.name=knownToRefSeq.value",
                "INNER JOIN `knownCanonical`",
                "ON knownToRefSeq.name=knownCanonical.transcript",
                "INNER JOIN `knownToEnsembl`",
                "ON knownCanonical.transcript=knownToEnsembl.name",
                "INNER JOIN `ensemblSource`",
                "ON knownToEnsembl.value=ensemblSource.name",
                "GROUP BY refFlat.name",
                "ORDER BY `chromosome`,`start`"))
        },
        mm10 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        rn5 = {
            warning("No NCBI RefSeq Genome annotation for Rattus ",
                "norvegicus rn5! Will use UCSC RefSeq instead...",
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
        rn6 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        dm3 = {
            warning("No NCBI RefSeq Genome annotation for Drosophila ",
                "melanogaster dm3! Will use UCSC RefSeq instead...",
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
        dm6 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        danrer7 = {
            warning("No NCBI RefSeq Genome annotation for Danio ",
                "rerio danrer7! Will use UCSC RefSeq instead...",
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
        danrer10 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        danrer11 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        pantro4 = {
            warning("No NCBI RefSeq Genome annotation for Pan troglodytes ",
                "pantro4! Will use UCSC RefSeq instead...",
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
        pantro5 = {
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        },
        susscr3 = {
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v3! ",
                "Will use UCSC RefSeq instead...",
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
            warning("No NCBI RefSeq Genome annotation for Sus scrofa v11! ",
                "Will use UCSC RefSeq instead...",
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
            warning("No NCBI RefSeq Genome annotation for Equus cabalus v2! ",
                "Will use UCSC RefSeq instead...",
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
            return(paste(
                "SELECT `chrom` AS `chromosome`,",
                "`exonStarts` AS `start`,",
                "`exonEnds` AS `end`,",
                "`name` AS `exon_id`,",
                "`strand` AS `strand`,",
                "`name` AS `gene_id`,",
                "`name2` AS `gene_name`,",
                "'NA' AS `biotype`",
                "FROM `ncbiRefSeq`", 
                "GROUP BY `gene_name`",
                "ORDER BY `chromosome`,`start`"
            ))
        }
    )
}
