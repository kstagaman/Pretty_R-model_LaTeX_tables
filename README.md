# Pretty_R-model_LaTeX_tables

These are a collection of R scripts that take common statistical models in R (lm, aov, anova, adonis, factanal, prcomp) and use  the `xtable-package` to make nice LaTeX tables, originally with the idea of including them in PDF made with the `knitr-package`.

The function `xtable.summary` will call the appropriate script for the model object as long as all the scripts are kept in the same directory. The main benefits of using these scripts instead of `xtable` by itself is modifying row names, bolding significant factors (or factors above a certain cutoff), and printing extra tables that are dropped by `xtable` when used alone (such as the full model *R*<sup>2</sup> and *p*-value for "lm" objects).