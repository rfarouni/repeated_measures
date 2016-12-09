# Simulate Godfrey Thomson's "sampling model" of mental abilities, and perform
# factor analysis on the resulting test scores.


# MASS package used to make random multivariate normal vectors
require(MASS)


# Simulate the Thomson model
# Follow Thomson's original sampling-without-replacement scheme
# Pick a random number in 1:a for the number of shared abilities for each test
# Then draw a sample-without-replacement of that size from 1:a; those are the
# shared abilities summed in that test.
# Specific variance of each test is also random; draw a number in 1:q, and
# sum that many independent normals, with the same parameters as the
# abilities.
# Inputs: number of testees, number of tests, number of shared abilities, number
#     of specific abilities per test, mean of each ability, sd of each ability
# Calls: mvrnorm from library MASS (multivariate random normal generator)
# Outputs: matrix of test loadings on to general abilities, vector of number of
#     specific abilities per test, matrix of abilities-by-testees, matrix of
#     general+specific scores by testees, raw data (including measurement noise)
rthomson <- function(n,d,a,q,ability.mean=0,ability.sd=1) {
  # Using incomprehensible parameter names is bad
  # number of testees = n
  # number of tests = d
  # number of shared abilities = a
  # max. number of specific abilities per test = q
  
  # assign abilities to tests
  general.per.test = floor(runif(d,min=0,max=a)) + 1
  specifics.per.test = floor(runif(d,min=0,max=q)) + 1
  
  # Define the matrix assigning abilities to tests
  general.to.tests = matrix(0,a,d)
  # The use of a for loop here is maybe a little slower than some sort
  # of vectorization, but it's sanity-preserving; so is using the temporary
  # variable "abilties" to hold the sample.
  for (i in 1:d) {
    abilities = sample(1:a,size=general.per.test[i],replace=FALSE)
    general.to.tests[abilities,i] = 1
  }
  
  # Covariance matrix of the general abilities
  sigma = matrix(0,a,a)
  diag(sigma) = (ability.sd)^2
  mu=rep(ability.mean,a)
  x = mvrnorm(n,mu,sigma) # person-by-abilities matrix of abilities
  
  # The "general" part of the tests
  general.tests = x %*% general.to.tests
  
  specific.tests = matrix(0,n,d)
  noisy.tests = matrix(0,n,d)
  # Each test gets its own specific abilities, which are independent for each
  # person
  # Again, I could probably vectorize, but d is small and this is saner
  for (i in 1:d) {
    # Each test has noises.per.test disturbances, each of which has the
    # given sd; since these are all independent their variances add
    j = specifics.per.test[i]
    specifics = rnorm(n,mean=ability.mean*j,sd=ability.sd*sqrt(j))
    specific.tests[,i] = general.tests[,i] + specifics
    # Finally, for extra realism, some mean-zero trial-to-trial noise, so
    # that if we re-use this combination of general and specific ability
    # scores, we won't get the exact same test scores twice
    noises = rnorm(n,mean=0,sd=ability.sd)
    noisy.tests[,i] = specific.tests[,i] + noises
  }
  
  tm = list(data=noisy.tests,general.ability.pattern = general.to.tests,
            numbers.of.specifics = specifics.per.test,
            ability.matrix = x, specific.tests = specific.tests)
  return(tm)
}

library(lavaan)

tm = rthomson(50,10,500,50)




saveRDS(tm, "tm.rds")
rowSums(tm$general.ability.pattern)
df <-round(tm$data, 3)
colnames(df) <- c("test1" , "test2" , "test3" ,"test4" , "test5" , "test6" ,"test7" , "test8" , "test9", "test10")

write.csv(df,"ThomsonData.csv",row.names = FALSE)
df <- read.csv("ThomsonData.csv")
fa_model <- 'f =~ test1+ test2 + test3 + test4 + test5 + test6 +test7+ test8 + test9 + test10'

fa_model_fit <- cfa(model = fa_model,
                    data = df, 
                    std.lv = TRUE)

summary(fa_model_fit , fit.measures = TRUE)
The null hypothesis is that there is no difference between the patterns observed in these data and the model specified

Imagine a human population that posseses 500  distinct and indpendent mental abilities (latent traits) .
The abilities are normally distributed among its members (an individual is a point in this 500 dimensional space). 
We adminisiter 10 test items (test) to a sample of 50 test takers from this population. Here is the data



and a smaller number, say no more than 30, that are specific to an individual 
The data was generated such that each test item's score is linear combination of a random subset of these 400 shared abilities. 
In addition to the shared ability component, the observed score for each test item  contains a specific ability component. 

but where each individual has only a small random sample of these abilities

combination of general and specific ability
# scores
Let's assume that the number of skills and abilities that go into answering any particluar test questions are many. 

say 500, an 
each feature is a linear combination of a random sample of
a huge pool of completely independent features, plus some extra noise specific
to the feature


RMSEA  < 0.06   Absolute fit indices Favours parsimony model fits in comparison to no model at all 
CFI  > 0.96  Incremental Fit Indices  null model indicators are uncorrelated
SRMR < 0.09  standardized difference between the observed correlation and the predicted correlation
factanal(tm$data,1)