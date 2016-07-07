# xtable_summary_lm.R

require(xtable)

xtable.summary.lm <- function(lm.obj,
                              file.id,
                              alt.factor.names=NULL)
{
    lm.xtbl <- xtable(lm.obj)
    sum.lm <- summary(lm.obj)
    if(!is.null(alt.factor.names)) {
        if(length(row.names(lm.xtbl)) == length(alt.factor.names)) {
            row.names(lm.xtbl) <- alt.factor.names
        } else {
            stop("Length of alternate factor names must equal the length of the original factor names")
        }
    }

    factor.pvals <- c()
    factor.names <- c()
    for(i in c(1:nrow(lm.xtbl))) {
        factor.names[i] <- ifelse(lm.xtbl$`Pr(>|t|)`[i] < 0.05,
                                  paste("\\textbf{", row.names(lm.xtbl)[i], "}", sep=""),
                                  paste(row.names(lm.xtbl)[i]))
        factor.pvals[i] <- ifelse(lm.xtbl$`Pr(>|t|)`[i] < 0.05,
                                  paste("\\textbf{",
                                        signif(lm.xtbl$`Pr(>|t|)`[i], 3),
                                        "}",
                                        sep=""),
                                  paste(signif(lm.xtbl$`Pr(>|t|)`[i], 3)))
    }
    lm.xtbl$`Pr(>|t|)` <- factor.pvals
    row.names(lm.xtbl) <- factor.names

    print(lm.xtbl,
          floating=FALSE,
          file=paste(file.id, "_lm_factors.tex", sep=""),
          sanitize.text.function=function(x){x})

    sum.lm.p <- pf(sum.lm$fstatistic[1],
                   sum.lm$fstatistic[2],
                   sum.lm$fstatistic[3],
                   lower.tail=FALSE)
    names(sum.lm.p) <- NULL
    rsq.p.tbl <- data.frame(sum.lm$df[2],
                            sum.lm$sigma,
                            sum.lm$r.squared,
                            sum.lm$adj.r.squared,
                            sum.lm.p)
    rsq.p.xtbl <- xtable(rsq.p.tbl)
    names(rsq.p.xtbl) <- c("D.f.",
                           "Residual Std. Error",
                           "$R^2$",
                           "Adjusted $R^2$",
                           "$p$-value")
    print(rsq.p.xtbl,
          floating=FALSE,
          file=paste(file.id, "_lm_rsqP.tex", sep=""),
          sanitize.text.function=function(x){x},
          include.rownames=FALSE)
}
