#' @title Plot Gtex data
#' @description Given a SummarizedExperiment object and a gene ENSEMBL, this function plots the mean expression level of this gene across different tissue subtype and different sex
#' @param expr_mat expression matrix
#' @param pheno phenotype data
#' @param gene An ensembl id of gene
#' @param title title of plot, by default it is the gene ENSEMBL
#' @import ggplot2
#' @import dplyr
#' @return No return. Plots are shown on screen immediately.
#' @export
#'


plot_tissuesubtype_sex <- function(expr_mat,pheno,gene,title){
        df <- data.frame(Tissue_subtype=pheno$Tissue_subtype,
                    Sex=pheno$Sex,
                    Expression_level=unlist(expr_mat[gene,]))
        library(dplyr)
        df <- df %>% dplyr::group_by(Tissue_subtype,Sex) %>% dplyr::summarise(Mean_log_expression=mean(Expression_level))
        g <- ggplot2::ggplot(df,ggplot2::aes(x=Tissue_subtype,y=Mean_log_expression,color=Sex))+ggplot2::geom_point()+ggplot2::ggtitle(paste(gene,title))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(g)
}
