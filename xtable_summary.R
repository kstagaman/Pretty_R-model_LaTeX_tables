# xtable_summary.R

for(i in c(0:sys.nframe())) {
    if(!is.null(sys.frame(i)$ofile)) {
        if(grepl("xtable_summary.R", sys.frame(i)$ofile)) {
            script.dir <- dirname(sys.frame(i)$ofile)
        }
    }
}

require(xtable)
source(file.path(script.dir, "xtable_summary_lm.R"))
source(file.path(script.dir, "xtable_summary_aov.R"))
source(file.path(script.dir, "xtable_summary_anova.R"))
source(file.path(script.dir, "xtable_summary_adonis.R"))
source(file.path(script.dir, "xtable_summary_factanal.R"))
source(file.path(script.dir, "xtable_summary_prcomp.R"))

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
