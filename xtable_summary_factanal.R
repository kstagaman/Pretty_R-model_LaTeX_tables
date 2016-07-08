# xtable_summary_factanal.R

require(xtable)

xtable.summary.factanal <- function(factanal.obj,
                                    file.id,
                                    cutoff=0.4,
                                    alt.factor.names=NULL)
{
    uniq.mat <- t(as.matrix(factanal.obj$uniquenesses))
    rownames(uniq.mat) <- "Uniquenesses"
    colnames(uniq.mat) <- gsub("_", "-", colnames(uniq.mat))
    uniq.xtbl <- xtable(uniq.mat)
    print(uniq.xtbl,
          floating=FALSE,
          file=paste(file.id, "_factanal_uniq.tex", sep=""),
          sanitize.text.function=function(x){x})

    load.mat <- factanal.obj$loadings[,]
    load.text.mat <- matrix(, nrow=nrow(load.mat), ncol=ncol(load.mat))
    for(j in c(1:ncol(load.mat))) {
        for(i in c(1:nrow(load.mat))) {
            load.text.mat[i,j] <- ifelse(abs(load.mat[i,j]) > cutoff,
                                    paste("\\textbf{", signif(load.mat[i,j], 2), "}", sep=""),
                                    paste(signif(load.mat[i,j], 2)))
        }
    }
    colnames(load.text.mat) <- colnames(load.mat)
    rownames(load.text.mat) <- gsub("_", "-", rownames(load.mat))
    load.xtbl <- xtable(load.text.mat)
    if(!is.null(alt.factor.names)) {
        row.names(load.xtbl) <- alt.factor.names
    }
    print(load.xtbl,
          floating=FALSE,
          file=paste(file.id, "_factanal_loadings.tex", sep=""),
          sanitize.text.function=function(x){x})

    ss.load <- colSums(factanal.obj$loadings^2)
    prop.var <- colSums(factanal.obj$loadings^2) / dim(factanal.obj$loadings)[1]
    cum.var <- cumsum(colSums(factanal.obj$loadings^2) / dim(factanal.obj$loadings)[1])
    var.df <- rbind(ss.load, prop.var, cum.var)
    row.names(var.df) <- c("SS Loadings", "Proportion Var.", "Cumulative Var.")
    var.xtbl <- xtable(var.df)
    print(var.xtbl,
          floating=FALSE,
          file=paste(file.id, "_factanal_vars.tex", sep=""),
          sanitize.text.function=function(x){x})
}
