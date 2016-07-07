# xtable_summary_adonis.R

require(xtable)

xtable.summary.adonis <- function(adns.obj,
                                  file.id,
                                  alt.factor.names=NULL)
{
    adns.xtbl <- xtable(adns.obj$aov.tab, align=rep("r", ncol(adns.obj$aov.tab)+1))

    if(!is.null(alt.factor.names)) {
        if(length(row.names(adns.xtbl)) == length(alt.factor.names)) {
            row.names(adns.xtbl) <- alt.factor.names
        } else if (!("Residuals" %in% alt.factor.names)) {
            row.names(adns.xtbl) <- c(alt.factor.names, "Residuals", "Total")
        } else {
            stop("Length of alternate factor names must equal
                 the length of the original factor names")
        }
    }

    factor.pvals <- c()
    factor.names <- c()
    for(i in c(1:(nrow(adns.xtbl)-2))) {
        factor.names[i] <- ifelse(adns.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{", row.names(adns.xtbl)[i], "}", sep=""),
                                  paste(row.names(adns.xtbl)[i]))
        factor.pvals[i] <- ifelse(adns.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{",
                                        signif(adns.xtbl$`Pr(>F)`[i], 3),
                                        "}",
                                        sep=""),
                                  paste(signif(adns.xtbl$`Pr(>F)`[i], 3)))
    }
    adns.xtbl$`Pr(>F)`[1:(nrow(adns.xtbl)-2)] <- factor.pvals
    row.names(adns.xtbl)[1:(nrow(adns.xtbl)-2)] <- factor.names

    names(adns.xtbl) <- c("D.f.",
                          "Sum of Sqs.",
                          "Mean Sqs.",
                          "$F$ Model",
                          "$R^2$",
                          "Pr($>F$)")
    print(adns.xtbl,
          floating=FALSE,
          file=paste(file.id, "_adns.tex", sep=""),
          sanitize.text.function=function(x){x})
}
