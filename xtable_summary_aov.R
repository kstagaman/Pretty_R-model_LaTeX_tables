# xtable_summary_aov.R

require(xtable)

xtable.summary.aov <- function(aov.obj,
                               file.id,
                               alt.factor.names=NULL)
{
    aov.xtbl <- xtable(aov.obj)

    if(!is.null(alt.factor.names)) {
        if(length(row.names(aov.xtbl)) == length(alt.factor.names)) {
            row.names(aov.xtbl) <- alt.factor.names
        } else if (!("Residuals" %in% alt.factor.names)) {
            row.names(aov.xtbl) <- c(alt.factor.names, "Residuals")
        } else {
            stop("Length of alternate factor names must equal the length of the original factor names")
        }
    }

    factor.pvals <- c()
    factor.names <- c()
    for(i in c(1:(nrow(aov.xtbl)-1))) {
        factor.names[i] <- ifelse(aov.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{", row.names(aov.xtbl)[i], "}", sep=""),
                                  paste(row.names(aov.xtbl)[i]))
        factor.pvals[i] <- ifelse(aov.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{",
                                        signif(aov.xtbl$`Pr(>F)`[i], 3),
                                        "}",
                                        sep=""),
                                  paste(signif(aov.xtbl$`Pr(>F)`[i], 3)))
    }
    aov.xtbl$`Pr(>F)`[1:(nrow(aov.xtbl)-1)] <- factor.pvals
    row.names(aov.xtbl)[1:(nrow(aov.xtbl)-1)] <- factor.names

    names(aov.xtbl) <- c("D.f.", "Sum of Sqs.", "Mean Sqs.", "$F$ Value", "Pr($>F$)")
    print(aov.xtbl,
          floating=FALSE,
          file=paste(file.id, "_aov.tex", sep=""),
          sanitize.text.function=function(x){x})
}
