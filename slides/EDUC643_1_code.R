############################################################################
# Importing personality and belonging data and estimating bivariate relationship
## Created by: David Liebowitz
### First created: 12/15/22
### Last update: 11/16/26
### inputs: belonging.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, broom, modelsummary)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC643_1_code.R")

# Let's first access the data
# These data come from Stubblebine, Gopalan & Brady (2024) and were posted publicly
# by the authors on the Open Science Framework: https://osf.io/mv6ek/
# We use select() to keep only the variables we need and give them shorter names
pb <- read.csv(here("data/belonging.csv")) %>%
        select(belong = t2__dv_composite__belonging,
               extraversion = t2__dv_composite__personality_extrovert,
               neuroticism = t2__dv_composite__personality_neuroticism,
               school = college_name,
               first_gen = t1__generation_br_combined_recoded)



#############################################################
### Data Cleaning
#############################################################

# We start with some simple data-cleaning steps. You will get light exposure to these in this class,
# and fully learn via intensive practice on applied research projects

# Understand data structure
head(pb)
str(pb)


# Check for missingness
sapply(pb, function(x) sum(is.na(x)))

# Can also do this variable by variable
sum(is.na(pb$extraversion))

# Some students did not complete the end-of-year personality items; we'll focus on those
# cases that had complete records for all variables
pb <- pb %>% drop_na()

# Can also do this for particular variables
pb <- filter(pb, !is.na(extraversion))

# Get a quick sense of some basic summary statistics
quantile(pb$extraversion)
summary(pb$belong)
summary(pb$neuroticism)

# Something looks funny with first_gen, let's check it out
ggplot(data=pb, aes(first_gen)) +
    geom_bar()

# The codebook tells us that 0 = "Unknown", so re-code values of 0 as NA
pb <- pb %>% mutate(first_gen = ifelse(first_gen==0, NA, first_gen))

sum(is.na(pb$first_gen))

# first_gen isn't part of our main analysis, so we won't drop these students


#######################################################
### Visualization of bivariate relationship
#######################################################

# With more than 4,000 students, many points overlap, so we make them partially transparent
lm_plot <- ggplot(data=pb, aes(x=extraversion, y=belong)) +
              geom_point(alpha=0.2) +
              xlab("Extraversion (0-5)") +
              ylab("Sense of belonging (0-6)") +
              theme_minimal(base_size = 16)


lm_plot

# Add line of best fit, the method is linear model (lm) and we are not including confidence intervals (se=F)
lm_plot +
  geom_smooth(method='lm', se=F)



####################################################################
## Bivariate regression
####################################################################

# Fitting a relationship using Ordinary Least Squares (OLS)
fit <- lm(belong ~ extraversion, data=pb)
summary(fit)

# In addition to the handy display that summary returns, there are several other elements of the model fit that R stores under the hood

# Look at the structure of the object
str(fit)

# Can get descriptive values on residuals
quantile(fit$residuals)

# Can assign the fitted values to a variable in your original data set; NOTE: need to make sure that these are of same length!
pb$predicted_belong <- fit$fitted.values

# Can get the degrees of freedom for the mdoel
fit$df

# Can also extract the coefficients to use later on
beta1_hat <- fit$coeff[2]  # <- we're extracting the second of the coefficients in the list; i.e., the slope on extraversion

# Produce confidence intervals
tidy(fit, conf.int=T)

#Another way of producing CIs
confint(fit)

#############################################################
# Pass output to table
#############################################################

# Produce the table
modelsummary(fit, stars=T,
             gof_omit = "Adj.|AIC|BIC|Log",
             coef_rename = c("extraversion" = "Extraversion (0-5)"))

# Produce the table and export it to a Word file
modelsummary(fit, stars=T,
             gof_omit = "Adj.|AIC|BIC|Log",
             coef_rename = c("extraversion" = "Extraversion (0-5)"),
             output = "Tables/belong_extraversion.docx") # <- need to set up your directory structure to have a Tables file in your root directory

############################################################
# Calculate Pearson's product-moment correlation
############################################################

# Standardize the variables
pb <- pb  %>%
  mutate(belong_std = (belong - mean(belong)) / sd(belong))
pb <- pb %>%
  mutate(extraversion_std =
           (extraversion - mean(extraversion)) / sd(extraversion))

# Graph the standardized versions of the data
std <- ggplot(pb, aes(extraversion_std, belong_std)) +
  geom_point(alpha=0.2) +
  geom_smooth(method='lm', se=F) +
  theme_minimal(base_size=16)

std

# Calculate the correlation coefficient
cor(pb$belong, pb$extraversion)


