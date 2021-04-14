library(biomaRt)
library(regioneR)
gene.symbols <- c("AR", "BRCA1", "BRCA2", "CDH1", "EP300", "EZH2", "FGFR2", "FGFR4", "GNMT", "HOXB13", 
                  "IGF2", "ITGA6", "PTEN", "STAT3", "TGFBR1", "WRN", "WT1")
ensembl <- useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
genes <- toGRanges(getBM(attributes=c('chromosome_name', 'start_position', 'end_position', 'hgnc_symbol'),
                         filters = 'hgnc_symbol', values =gene.symbols, mart = ensembl))
seqlevelsStyle(genes) <- "UCSC"

head(genes)


library(karyoploteR)
kp <- plotKaryotype(genome="hg38")
kpPlotMarkers(kp, data=genes, labels=genes$hgnc_symbol, text.orientation = "horizontal",
              r1=0.5, cex=0.8, adjust.label.position = TRUE)
kpAddMainTitle(kp, "18 Genes that are Associated with Prostate Cancer")