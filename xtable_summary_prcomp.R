# xtable_summary_prcomp.R

require(xtable)

xtable.summary.prcomp<- function(prcomp.obj,
                                 file.id,
                                 choices=c(1:3),
                                 cutoff=0.4,
                                 alt.factor.names=NULL)
{
    prcomp.df <- as.data.frame(prcomp.obj$rotation)[,choices]
    prcomp.text.df <- matrix(, nrow=nrow(prcomp.df), ncol=ncol(prcomp.df))
    for(j in c(1:ncol(prcomp.df))) {
        for(i in c(1:nrow(prcomp.df))) {
            prcomp.text.df[i,j] <- ifelse(abs(prcomp.df[i,j]) > cutoff,
                                          paste("\\textbf{",
                                                signif(prcomp.df[i,j], 2),
                                                "}",
                                                sep=""),
                                          paste(signif(prcomp.df[i,j], 2)))
        }
    }
    colnames(prcomp.text.df) <- colnames(prcomp.df)
    if(is.null(alt.factor.names)) {
        row.names(prcomp.text.df) <- row.names(prcomp.df)
    } else {
        row.names(prcomp.text.df) <- alt.factor.names
    }
    if(mean(diff(choices)) == 1) {
        pcs.text <- paste(choices[1], choices[length(choices)], sep="-")
    } else {
        pcs.text <- paste(choices, collapse="_")
    }
    print(xtable(prcomp.text.df),
          floating=FALSE,
          file=paste(file.id, "_PCs", pcs.text, ".tex", sep=""),
          sanitize.text.function=function(x){x})
}
