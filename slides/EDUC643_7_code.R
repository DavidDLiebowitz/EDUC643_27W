############################################################################
# Multiple regression
## Created by: David Liebowitz
### First created: 1/23/23
### Last update: 11/16/26
### inputs: belonging.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, modelsummary, marginaleffects)


i_am("slides/EDUC643_7_code.R")

# Import the data and implement previously identified data-cleaning steps
pb <- read.csv(here("data/belonging.csv")) %>% 
  select(belong = t2__dv_composite__belonging,
         extraversion = t2__dv_composite__personality_extrovert,
         neuroticism = t2__dv_composite__personality_neuroticism,
         agreeableness = t2__dv_composite__personality_agreeable,
         conscientiousness = t2__dv_composite__personality_conscientious,
         openness = t2__dv_composite__personality_open,
         school = college_name) %>%
  drop_na()

# Going to add an id variable
pb <- rownames_to_column(pb, "id")

#################################################
###  First, estimate the bivariate relationship
################################################

### Some univariate stats

summary(pb$belong)
sd(pb$belong)
summary(pb$extraversion)

### Some univariate displays

BEL <- ggplot(pb, aes(belong)) +
  geom_histogram() +
  theme_minimal(base_size = 16)
EXT <- ggplot(pb, aes(extraversion)) +
  geom_histogram() +
  theme_minimal(base_size = 16)

gridExtra::grid.arrange(BEL, EXT, ncol=2)

### Some bivariate statistics
cor(pb$belong, pb$extraversion)


# Bivariate visualization
lm_plot <- ggplot(pb, aes(x=extraversion, y=belong)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm") +
  xlab("Extraversion") +
  ylab("Sense of belonging") +
  theme_minimal(base_size = 16)

lm_plot

# Estimate the bivariate relationship
fit <- lm(belong ~ extraversion, data=pb)
summary(fit)

# Examine the bivariate regression assumptions

pb$predict <- predict(fit)
pb$resid <- resid(fit)
pb$stu_resid <- rstudent(fit)

# Plot the raw residuals
ggplot(pb, aes(x = resid)) + 
  geom_histogram(binwidth = 0.25) +
  theme_minimal(base_size = 16)

# Residuals v fitted plot
ggplot(pb, aes(x = predict, y = stu_resid)) + 
  geom_point(alpha=0.2) +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Studentized Residuals") + xlab("Fitted values") +
  theme_minimal(base_size = 16)


#################################################
###  Now, multiple regression
################################################

### The relationship between belong and neuroticism
bel_neuro <- ggplot(pb, aes(x=neuroticism, y=belong)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method = "lm") +
  xlab("Neuroticism") +
  ylab("Sense of belonging") +
  theme_minimal(base_size = 16)

bel_neuro

# And the bivariate relationship
summary(lm(belong ~ neuroticism, data=pb))

# Finally, the multivariate relationship
fit2 <- lm(belong ~ extraversion + neuroticism, data=pb)

fit2

#####################################################################################
## You do not need to follow the below, but if you'd like to see how to 
## create lines of best fit by group, it is as follows
## We'll explicitly learn how to do this in Unit 4

# First, I'm creating the simulated data
set.seed(1234)
library(MASS)
### build the g1
mu <- c(0,0)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g1 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g1$group <- c("Hi GDP")

### build the g2
mu <- c(3,3)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g2 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g2$group <- c("Mid GDP")

### build the g3
mu <- c(6,6)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g3 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g3$group <- c("Lo GDP")

# the combined data of all three groups
df <- rbind(g1,g2,g3)

# Changing the variable names for display purposes
df <- df %>% mutate(`% GDP on Health Care` = V1 + 5.2)
df <- df %>% rename(`Early mortality` = V2)

# Plotting the bivariate relationship
ggplot(df, aes(`% GDP on Health Care`, `Early mortality`)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  theme_minimal(base_size=16)

# Plotting the bivariate relationship with Simpson's Paradox revealed
ggplot(df) +
    geom_point(aes(x=`% GDP on Health Care`, y=`Early mortality`, col=group)) +
    geom_smooth(aes(x=`% GDP on Health Care`, y=`Early mortality`, col=group), method='lm', se=F) +
    geom_smooth(aes(x=`% GDP on Health Care`, y=`Early mortality`), method='lm', se=F, color="gray") +
    theme_minimal(base_size=16) + 
    annotate('text', x=10.7, y=-3, label="Hi GDP", color="#F8766D", size=5) +
    annotate('text', x=13, y=-1, label="Mid GDP", color="#619CFF", size=5) +
    annotate('text', x=15, y=2.5, label="Low GDP", color="#00BA38", size=5) +
    theme(legend.position="none")

###############################################################################################################

# Preparing a nice regression table -- on screen

# modelsummary reports the F-statistic ("F") by default, so we leave it out of gof_omit
modelsummary(list(fit, fit2),
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE",
             coef_rename = c("extraversion" = "Extraversion (0-5)", "neuroticism" = "Neuroticism (0-5)"),
             escape=F)  



# Preparing a nice regression table -- output to .doc
modelsummary(list(fit, fit2),
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE",
             coef_rename = c("extraversion" = "Extraversion (0-5)", "neuroticism" = "Neuroticism (0-5)"),
             output = "slides/belong_multiple.docx")

## Creating a prototypical plot

# Use our multiple regression of belonging on extraversion and neuroticism
fit2 <- lm(belong ~ extraversion + neuroticism, data=pb)

# Use plot_predictions() from the marginaleffects package:
# the first variable in `condition` goes on the x-axis, and we supply
# prototypical values for the second (all other predictors are held at their means)
proto <- plot_predictions(fit2, 
                          condition = list("extraversion", "neuroticism" = c(2,3,4))) + 
  xlab("Extraversion") + ylab("Predicted sense of belonging") +
  scale_color_discrete(name = "Neuroticism",
                       labels=c("Low (2)","Middle (3)","High (4)"),
                       aesthetics = c("colour", "fill")) +
  theme_minimal(base_size=16)

proto

#####################################
## Creating a correlation matrix 
######################################

# First, limit our data to only the variables of interest
cordat <- pb %>%
  dplyr::select(belong, extraversion, neuroticism,
                agreeableness, conscientiousness, openness)

## Note, when you load the MASS package (to generate the simulated data) it has its own function involving select
## If you try to use the select function after the MASS package, you'll get an error
## Unless you specifically tell R you want to use the select function from dplyr (which is what I'm doing above with the dplyr:: line)

# I'm going to rename some, so they fit nicely on the screen
cordat <- cordat %>%
  rename(Belong = belong,
         Extra = extraversion,
         Neuro = neuroticism,
         Agree = agreeableness,
         Consc = conscientiousness,
         Open = openness)

#Produce the correlation matrix
datasummary_correlation(cordat,
                        fmt = 3,
                        notes = "Notes: cells report Pearson correlation coefficients.",
                        escape=F)

### Don't expect you to know how to do this ####
# Can also produce a heat map

# First, I need to create an object that contains the correlation coefficients
cormat <- round(cor(cordat),3)
cormat[upper.tri(cormat)] <- NA

# The melt command is a quick way to reshape the data from wide-to-long, which is necessary here
melt_corm <- reshape2::melt(cormat, na.rm=T)
ggplot(data = melt_corm, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal(base_size = 16)
