# xtable_summary.R

require(xtable)
source("xtable_summary_lm.R")
source("xtable_summary_aov.R")
source("xtable_summary_anova.R")
source("xtable_summary_adonis.R")
source("xtable_summary_factanal.R")
source("xtable_summary_prcomp.R")

xtable.summary<- function(model.obj,
                          file.id,
                          choices=c(1:3),
                          cutoff=0.4,
                          alt.factor.names=NULL,
                          alt.models.text=NULL)
{
    if(class(model.obj)[1] == "lm") {
        args <- list(model.obj, file.id, alt.factor.names)
        do.call(xtable.summary.lm, args)
    } else if(class(model.obj)[1] == "aov") {
        args <- list(model.obj, file.id, alt.factor.names)
        do.call(xtable.summary.aov, args)
    } else if(class(model.obj)[1] == "anova") {
        args <- list(model.obj, file.id, alt.models.text)
        do.call(xtable.summary.anova, args)
    } else if(class(model.obj)[1] == "adonis") {
        args <- list(model.obj, file.id, alt.factor.names)
        do.call(xtable.summary.adonis, args)
    } else if(class(model.obj)[1] == "factanal") {
        args <- list(model.obj, file.id, cutoff, alt.factor.names)
        do.call(xtable.summary.factanal, args)
    } else if(class(model.obj)[1] == "prcomp") {
        args <- list(model.obj, file.id, choices, cutoff, alt.factor.names)
        do.call(xtable.summary.prcomp, args)
    } else {
        obj.class <- class(model.obj)[1]
        message(paste("Objects of class '",
                      obj.class,
                      "' are not yet supported by xtable.summary()",
                      sep=""))
    }
}
