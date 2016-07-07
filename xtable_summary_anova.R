# xtable_summary_anova.R

require(xtable)
require(stringr)

xtable.summary.anova <- function(anova.obj,
                                 file.id,
                                 alt.models.text=NULL)
{
    if(!is.null(alt.models.text)) {
        if(length(alt.models.text) != 1) {
            stop("Alternate models text must be a single string")
        } else {
            models.text <- alt.models.text
        }
    } else {
        models.text <- attributes(anova.obj)$heading[2]
    }

    heading.df <- data.frame("V1"=str_split(models.text, "\n")[[1]])
    names(heading.df) <- gsub("[\r\n]", "", attributes(anova.obj)$heading[1])
    heading.xtbl <- xtable(heading.df)
    print(heading.xtbl,
          floating=FALSE,
          file=paste(file.id, "_anovaHeading.tex", sep=""),
          sanitize.text.function=function(x){x},
          include.rownames=FALSE)

    anova.xtbl <- xtable(anova.obj)

    factor.pvals <- c()
    factor.names <- c()
    for(i in c(2:nrow(anova.xtbl))) {
        factor.names[i-1] <- ifelse(anova.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{", row.names(anova.xtbl)[i], "}", sep=""),
                                  paste(row.names(anova.xtbl)[i]))
        factor.pvals[i-1] <- ifelse(anova.xtbl$`Pr(>F)`[i] < 0.05,
                                  paste("\\textbf{",
                                        signif(anova.xtbl$`Pr(>F)`[i], 3),
                                        "}",
                                        sep=""),
                                  paste(signif(anova.xtbl$`Pr(>F)`[i], 3)))
    }
    anova.xtbl$`Pr(>F)`[2:nrow(anova.xtbl)] <- factor.pvals
    row.names(anova.xtbl)[2:nrow(anova.xtbl)] <- factor.names

    names(anova.xtbl) <- c("Res. D.f.", "RSS", "D.f.", "Sum. of Sq.", "$F$", "Pr($>F$)")
    print(anova.xtbl,
          floating=FALSE,
          file=paste(file.id, "_anova.tex", sep=""),
          sanitize.text.function=function(x){x})
}
