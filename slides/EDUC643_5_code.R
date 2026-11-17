############################################################################
# Estimating bivariate relationship and check assumptions
## Created by: David Liebowitz
### First created: 12/15/22
### Last update: 11/16/26
### inputs: belonging.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, broom, modelsummary)


i_am("slides/EDUC643_5_code.R")

# Import the data and implement previously identified data-cleaning steps
pb <- read.csv(here("data/belonging.csv")) %>%
  select(belong = t2__dv_composite__belonging,
         extraversion = t2__dv_composite__personality_extrovert,
         neuroticism = t2__dv_composite__personality_neuroticism,
         school = college_name) %>%
  drop_na()

# Going to add an id variable
pb <- rownames_to_column(pb, "id")

################################
###  Bivariate relationship
################################

# Plot bivariate relationship

lm_plot <- ggplot(pb, aes(x=extraversion, y=belong)) +
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm") +
  xlab("Extraversion") +
  theme_minimal(base_size = 16)

lm_plot

# Fit regression
fit <- lm(belong ~ extraversion, data=pb)

# As an alternative to summary, can use tidy from the `broom` package to more easily manipulate results post-estimation
# Automatically returns a dataframe
tidy(fit)

# Can also use good old summary:
summary(fit)


###################################
### Working with residuals
###################################

# Extract raw residuals and fitted/predicted values
pb$predict <- predict(fit)
pb$resid <- residuals(fit)

# Extract standardized and studentized residuals
pb$std_resid <- rstandard(fit)
pb$stu_resid <- rstudent(fit)

# Produce some summary statistics for raw and standardized residuals
summary(pb$resid)
sd(pb$resid)

summary(pb$std_resid)
sd(pb$std_resid)

# Generate residuals vs. fitted plot
ggplot(pb, aes(x = predict, y = resid)) +
  geom_point(alpha=0.2) +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Raw Residuals") + xlab("Fitted values") +
  theme_minimal(base_size = 16)

##############
## Assessing residual normality

# via boxplot
boxplot(rstudent(fit))

# via histogram
ggplot(pb, aes(x = resid)) +
  geom_histogram(binwidth = 0.25) +
  theme_minimal(base_size = 16)

# via Q-Q plot
ggplot(pb) +
  stat_qq(aes(sample=stu_resid)) +
  geom_abline(color='blue') +
  theme_minimal(base_size = 16)


####################################################################################
## Some solutions to violations of homoscedasticity and independence assumptions
####################################################################################

# Use modelsumamry to report different types of standard errors
modelsummary(fit,
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE",
             coef_rename = c("extraversion" = "Extraversion (0-5)"),
             vcov =  list("iid", "robust"))

### Students in our data are clustered within 11 colleges and universities
# Look at the studentized residuals by school
ggplot(pb, aes(x = school, y = stu_resid)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  theme_minimal(base_size = 16)

# Then fit a regression in which I include these clusters as predictors
lm_school <- lm(belong ~ extraversion + as.factor(school), data=pb)

# Finally, implement a post-hoc correction to the standard errors
modelsummary(lm_school,
             stars=T,
             coef_omit = "school",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE",
             coef_rename = c("extraversion" = "Extraversion (0-5)"),
             vcov =  list("iid", ~ school))

## As an alternative, can use estimatr::lm_robust
## You'll see that to get the exact same standard errors, you need to specify se_type== "HC3"
hetero <- estimatr::lm_robust(belong ~ extraversion, data=pb)
summary(hetero)

cluster <- estimatr::lm_robust(belong ~ extraversion + as.factor(school), clusters = school, data=pb)
summary(cluster)
