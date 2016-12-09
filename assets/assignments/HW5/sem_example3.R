library(lavaan)
sem_lower <- '
 1.0
.441 1.0
.304 .182 1.0
.274 .265 .084 1.0
.270 .122 .198 .664 1.0
.162 .208 .057 .316 .420 1.0'


sem_cov <- getCov(sem_lower,
                  names = c("Mothr_Ed", "Fathr_Ed", "FamilyInc", "Reading","Language", "Motivate")

sem_model <- '
  # measurement model
    SES =~ Mothr_Ed + Fathr_Ed + FamilyInc
    Ability =~ 1*Reading +  Language
    LV_Mo =~ 1*Motivate
  # regressions
    Ability ~ SES + LV_Mo
   # variances
    Ability ~~ NA*Ability'


sem_model_fit <- sem(model = sem_model,
                     sample.cov = sem_cov, 
                     sample.nobs = 148, 
                     std.lv = TRUE)

fitMeasures(sem_model_fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))

lavInspect(sem_model_fit, "est")

lavInspect(sem_model_fit, "std")

