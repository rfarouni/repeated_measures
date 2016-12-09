library(lavaan)
sem_lower <- '
102.07
61.769  94.604
64.611  57.935  88.637 
2.530   2.340   2.128  0.4493
0.7293  0.8476  0.6293  0.0563  0.1175'


sem_cov <- getCov(sem_lower,
                  names = c("read", "write", "math", "locus", "motiv"))


sem_model <- '
# measurement model
achieve = ~ read + write + math
# regressions
achieve ~  locus + motiv'


sem_model_fit <- sem(model = sem_model,
                     sample.cov = sem_cov, 
                     sample.nobs = 600, 
                     std.lv = TRUE)

fitMeasures(sem_model_fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))

lavInspect(sem_model_fit, "est")

lavInspect(sem_model_fit, "std")

