############################################################################
# Interactions
## Created by: David Liebowitz
### First created: 2/13/23
### Last update: 2/19/25
### inputs: dibels_long.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, modelsummary, marginaleffects)


i_am("slides/EDUC643_13_code.R")

###########################################
### Data management
###########################################

# Import the data 
dibels_long <- read.csv(here("data/dibels_long.csv"))

# Clean up the Title I flag
dibels_long <- dibels_long %>%
  mutate(title1 = case_when(school_titlei=="Missing" ~ "Missing",
                            school_titlei=="Not a Title I school" ~ "Not Title I",
                            school_titlei=="Title I schoolwide eligible-Title I targeted assistance program" |
                              school_titlei=="Title I schoolwide eligible school-No program" |
                              school_titlei=="Title I schoolwide school" ~ "Title I schoolwide",
                            school_titlei=="Title I targeted assistance eligible school-No program" |
                              school_titlei=="Title I targeted assistance school" ~ "Title I targeted"))

dibels_long$title1 <- factor(dibels_long$title1,
                             levels=c("Not Title I", "Title I schoolwide", "Title I targeted", "Missing"))

#############################################
### Prior analyses
#############################################

fit3 <- lm(mean_orf ~ period, dibels_long)
fit4 <- lm(mean_orf ~ period + as.factor(grade) + school_enroll, data=dibels_long)

# Add row indicating presence of covariates
row <- tribble(~term, ~Bivariate, ~Multivariate,
               "<b>Covariates?</b>", "<b>No</b>", "<b>Yes</b>")
attr(row, 'position') <- c(9)

# Produce table
modelsummary(list(fit3, fit4),
             stars=T,
             escape=F,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_omit = "grade|school_enroll",
             coef_rename = c("periody1_moy" = "Winter 2020",
                             "periody2_boy" = "Fall 2020",
                             "periody2_moy" = "Winter 2021"),
             add_rows = row,
             notes=c("Cells report coefficients and heteroscedastic-robust standard errors in parentheses. Each observation is a school-grade-test value. Covariates include grade-level and total school enrollment."))

# Visualize main effects model
# Use plot_predictions() from the marginaleffects package
proto1 <- plot_predictions(fit4, condition = list("frpl_prop", "period")) +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  scale_color_discrete(name = "Period",
                       breaks=c("y1_boy", "y1_moy", 
                                "y2_boy", "y2_moy"),
                       labels=c("Fall 2019","Winter 2020",
                                "Fall 2020", "Winter 2021"),
                                aesthetics = c("colour", "fill")) +
  theme_minimal(base_size=16)

proto1

######################################
### Interactions
######################################

######################################
##  Categorical X continuous

# Period as predictor
summary(lm(mean_orf ~ period, dibels_long))

# *FRPL_PROP* as predictor
summary(lm(mean_orf ~ frpl_prop, dibels_long))

# Both
summary(lm(mean_orf ~ period + frpl_prop, dibels_long))

# Include interaction
summary(lm(mean_orf ~ period * frpl_prop, dibels_long))

# Generate fitted values, including interaction
fit5 <- lm(mean_orf ~ period * frpl_prop, data=dibels_long)
# Use plot_predictions() from the marginaleffects package
proto2 <- plot_predictions(fit5, condition = list("frpl_prop", "period")) +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  coord_cartesian(ylim = c(35, 110)) +
  scale_color_discrete(name = "Period",
                       breaks=c("y1_boy", "y1_moy", 
                                "y2_boy", "y2_moy"),
                       labels=c("Fall 2019","Winter 2020",
                                "Fall 2020", "Winter 2021"),
                                aesthetics = c("colour", "fill")) +
  theme_minimal(base_size=16)

proto2

## Can also just sub-set data by groups
# Fall 2019
summary(lm(mean_orf ~ frpl_prop, data=subset(dibels_long, period=="y1_boy")))

# Winter 2021
summary(lm(mean_orf ~ frpl_prop, data=subset(dibels_long, period=="y2_moy")))


###################################
### Categorical x categorical

## Review categories
table(dibels_long$title1, exclude=NULL)
dibels_long %>% group_by(title1) %>% summarize(mean= mean(mean_orf))

# Title I as predictor
summary(lm(mean_orf ~ title1, dibels_long))

# Both
summary(lm(mean_orf ~ period + title1, dibels_long))

# Interaction
summary(lm(mean_orf ~ period * title1, dibels_long))

## Visualize results
fit6 <- lm(mean_orf ~ period * title1, dibels_long)

# draw=FALSE asks plot_predictions() for the predicted values (and CIs)
# so we can build our own plot from them
df6 <- plot_predictions(fit6,
                        condition = list(period=c("y2_boy", "y2_moy"),
                                  title1 = c("Not Title I", "Title I schoolwide", 
                                             "Title I targeted", "Missing")),
                        draw = FALSE)

# Show results for each category
categ <- ggplot(data=df6, 
                aes(x=period, y=estimate, 
                    ymin=conf.low, ymax=conf.high, 
                    group=title1, color=title1)) + 
  geom_pointrange(position=position_dodge(width=0.2)) +
  ylab("Predicted ORF") + xlab(" ") +
  scale_x_discrete(labels= c("y2_boy" = "Fall 2020",
                             "y2_moy" = "Winter 2021")) +
  coord_cartesian(ylim = c(0, 110)) +
  theme_minimal(base_size=16) +
  theme(legend.title = element_blank())

categ

# Can add connecting lines
categ + geom_line(position=position_dodge(width=0.2))



###################################
### Continuous x continuous

# School enrollment as predictor
summary(lm(mean_orf ~ school_enroll, dibels_long))

# Both
summary(lm(mean_orf ~ frpl_prop + school_enroll, dibels_long))

# Interaction
summary(lm(mean_orf ~ frpl_prop * school_enroll, dibels_long))

# Prototypical values?
quantile(dibels_long$school_enroll, probs = seq(0, 1, 0.1))


# Visualizing -- select prototypical values
fit7 <- lm(mean_orf ~ frpl_prop * school_enroll, dibels_long)

# Use plot_predictions() at our prototypical values of school enrollment;
# vcov = FALSE draws just the fitted lines (no confidence bands)
cont <- plot_predictions(fit7, 
                         condition = list("frpl_prop", "school_enroll" = c(120, 326, 600)),
                         vcov = FALSE) +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  coord_cartesian(ylim = c(35, 100)) +
  scale_color_discrete(name = "School Enrollment",
                       label=c("~10th pctile (120 stu.)", 
                               "Median (326 stu.)", 
                               "~90th pctile (600 stu.)"),
                       aesthetics = c("colour", "fill")) +
  theme_minimal(base_size=16)

cont

# could also add a 95% confidence interval band (plot_predictions' default)

plot_predictions(fit7, 
                 condition = list("frpl_prop", "school_enroll" = c(120, 326, 600))) +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  coord_cartesian(ylim = c(35, 100)) +
  scale_color_discrete(name = "School Enrollment",
                       label=c("~10th pctile (120 stu.)", 
                               "Median (326 stu.)", 
                               "~90th pctile (600 stu.)"),
                       aesthetics = c("colour", "fill")) +
  theme_minimal(base_size=16)
