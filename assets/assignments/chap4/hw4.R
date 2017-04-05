library("dplyr")
library("tidyr")
library("ggplot2")
library("nlme")
library("lme4")
library("data.table")
# Step 2: Load data


NLSL_wide <- fread("NLSL.txt", na.strings = ".")
NLSL_wide

NLSL_wide <- NLSL_wide %>% 
  mutate(m_age1 = m_age,
         m_age2 = m_age + 2,
         m_age3 = m_age + 4,
         m_age4 = m_age + 6,
         c_age1 = age,
         c_age2 = age + 2,
         c_age3 = age + 4,
         c_age4 = age + 6) %>% 
  select(-c(m_age, age))




# Step 3: Rearrange the data

NLSL_long <- NLSL_wide %>% 
  reshape(direction = "long", 
          idvar = c("id"),
          varying = list(c(2:5), c(10:13), c(14:17)), 
          v.names = c("read_score", "mother_age", "child_age"), 
          times = c("1", "2", "3", "4")) %>%
  na.omit()


NLSL_long <- within(NLSL_long, {
  id <- factor(id)
  boy <- factor(boy)
  time <- factor(time)
  mother_age <- mother_age - 6
  child_age <- child_age - 6
})



ggplot(data = NLSL_long %>% filter(id %in% NLSL_long$id[1:36]) , 
       aes(x = child_age , 
           y = read_score, 
           group = boy)) +
  facet_wrap( ~ id, ncol=6) + 
  geom_line(aes(colour = boy), size = 0.2) +
  xlab("Age") +
  ylab("Reading Score") +
  ggtitle("NLSL Data") +
  theme(legend.position = "none")


# Step 7: Fit the model.  (I need help with this)

NLSL_fit <- lmer( read_score ~ child_age + I(child_age^2)+ (1 + child_age + I(child_age^2)|id),
                  data = NLSL_long)

# Not you will get something like this as a result:

# Error in lme.formula(fixed = read ~ childAge + I(childAge^2), random = ~1 +  : 
#  nlminb problem, convergence error code = 1
# message = iteration limit reached without convergence (10)


# This is not needed but I just like to see the data for the model

NLSY_fit

# Step 8: This is needed. In the summary, it will show the coefficients of the intercept and age and also the standard error of each.

summary(NLSL_fit )










# This is not needed but I just like to see the data

NLSY_wide

# This creates 3 new columns: childAge2, childAge3, & childAge4
# by adding 2, 4, & 6 to the child's age at occasion 1 childAge1

NLSY_wide$childAge2 <- NLSY_wide$childAge1 + 2
NLSY_wide$childAge3 <- NLSY_wide$childAge1 + 4
NLSY_wide$childAge4 <- NLSY_wide$childAge1 + 6

# This creates 3 new columns: motherAge2, motherAge3, & motherAge4
# by adding 2, 4, & 6 to the mother's age at occasion 1 motherAge1

NLSY_wide$motherAge2 <- NLSY_wide$motherAge1 + 2
NLSY_wide$motherAge3 <- NLSY_wide$motherAge1 + 4
NLSY_wide$motherAge4 <- NLSY_wide$motherAge1 + 6

# This is not needed but I just like to see the data with the new columns

NLSY_wide

# Rearrange the order of the columns to make thing easier to read

NLSY_wide <- NLSY_wide[,c(1:7,15:17,8,12:14,9:11)]

# This is not needed but I just like to see the data with the newly ordered columns

NLSY_wide



nlsyFit<-lme(fixed = read ~ 1 + age + age2 + sms + sms*age,
             data = nlsyLongFilt,
             random = ~1 + age + age2|id,
             method = "ML")

With this code, I get the following error: 
  
  Error in lme.formula(fixed = read ~ 1 + age + age2 + sms + sms * age, : 
                         nlminb problem, convergence error code = 1
                       message = iteration limit reached without convergence (10)
                       