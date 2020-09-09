#' @title Plot Gtex data
#' @description Given a SummarizedExperiment object and a gene ENSEMBL, this function plots the mean expression level of this gene across different tissue subtype and different sex
#' @param se SummarizedExperiment
#' @param gene An ensembl id of gene
#' @param title title of plot, by default it is the gene ENSEMBL
#' @import ggplot2
#' @import SummarizedExperiment
#' @import dplyr
#' @return No return. Plots are shown on screen immediately.
#' @export
#' @example
#' plot_tissuesubtype_sex(se,"ENSG00000002586")


plot_tissuesubtype_sex <- function(se,gene,title){
        df <- data.frame(Tissue_subtype=SummarizedExperiment::colData(se)$Tissue_subtype,
                    Sex=SummarizedExperiment::colData(se)$Sex,
                    Expression_level=SummarizedExperiment::assay(se)[gene,])
        library(dplyr)
        df <- df %>% dplyr::group_by(Tissue_subtype,Sex) %>% dplyr::summarise(Mean_log_expression=mean(Expression_level))
        g <- ggplot2::ggplot(df,ggplot2::aes(x=Tissue_subtype,y=Mean_log_expression,color=Sex))+ggplot2::geom_point()+ggplot2::ggtitle(paste(gene,title))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(g)
}
