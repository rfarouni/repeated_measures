library(lavaan)
sem_lower <- '
 1.0   
.441   1.0   
.304  .182   1.0   
.274  .265  .084   1.0   
.270  .122  .198  .664   1.0'


sem_cov <- getCov(sem_lower,
                 names = c("Mothr_Ed", "Fathr_Ed", "FamilyInc", "Reading","Language"))

sem_model <- '  
  # measurement model
    Ksi =~ Mothr_Ed + Fathr_Ed + FamilyInc
    Eta =~ 1*Reading +  Language
  # regressions
    Eta ~ Ksi
# variances
    Eta ~~ NA*Eta'


sem_model_fit <- sem(model = sem_model,
                    sample.cov = sem_cov, 
                    sample.nobs = 148, 
                    std.lv = TRUE)


lavInspect(sem_model_fit, "est")

lavInspect(sem_model_fit, "std")

fitMeasures(sem_model_fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))
