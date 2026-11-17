############################################################################
# Create the continuous version of the Stubblebine, Gopalan & Brady (2024) data
## Created by: David Liebowitz
### First created: 11/17/26
### inputs: belonging_osf_raw.csv (Data.csv from https://osf.io/mv6ek/)
### outputs: belonging.csv
######################################################
#
# The end-of-first-year (t2) composites for belonging, extraversion and
# neuroticism are averages of a few Likert items, so they can only take a
# handful of values. For teaching purposes, we add uniform random noise
# ("jitter") to make them truly continuous:
#   - belonging (originally 1-6, steps of 0.25): +/- 0.125
#   - extraversion & neuroticism (originally 1-5, steps of 0.5): +/- 0.25
# i.e., up to half the distance between adjacent possible scores.
#
# At the BOTTOM of each scale, students at the minimum (1) are instead spread
# downward toward 0: their noise is drawn from U(-(1 + half_step), half_step)
# and anything below 0 is set to exactly 0. This extends each scale's floor to
# 0 (0-6 for belonging; 0-5 for personality) and produces a small number of
# true zeros, which we use in Unit 5 to discuss why log(0) is a problem.
# At the TOP of each scale, values pushed past the maximum are reflected back
# inside (e.g., 6.05 -> 5.95).

library(here)

raw <- read.csv(here("data/belonging_osf_raw.csv"))

jitter_bounded <- function(x, half_step, hi) {
  lo_noise <- ifelse(!is.na(x) & x == 1, -(1 + half_step), -half_step)
  y <- x + runif(length(x), 0, 1) * (half_step - lo_noise) + lo_noise
  y <- ifelse(y > hi, 2 * hi - y, y)
  y <- pmax(y, 0)
  ifelse(is.na(x), NA, y)
}

set.seed(643)
out <- raw
out$t2__dv_composite__belonging <-
  jitter_bounded(raw$t2__dv_composite__belonging, 0.125, 6)
out$t2__dv_composite__personality_extrovert <-
  jitter_bounded(raw$t2__dv_composite__personality_extrovert, 0.25, 5)
out$t2__dv_composite__personality_neuroticism <-
  jitter_bounded(raw$t2__dv_composite__personality_neuroticism, 0.25, 5)

write.csv(out, here("data/belonging.csv"), row.names = FALSE)
