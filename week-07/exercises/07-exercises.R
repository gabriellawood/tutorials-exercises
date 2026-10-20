# ============================================================================
# Skill Building in R: Independent linear regression exercises
# Estimated time: 60 minutes
# Dataset: ggplot2::mpg
# ============================================================================
#
# Write code below each prompt and write your interpretations as comments 
# beginning with #. 
#
# Goal: apply the complete single-predictor regression workflow to a new dataset.
# We will fit TWO SEPARATE models of highway fuel economy:
#   1. A numeric predictor: engine displacement.
#   2. A categorical predictor: transmission group.
#
# All code required for these tasks appeared in the tutorial. Adapt it to the
# variable names below. 

library(tidyverse)
library(broom)

theme_set(theme_bw(base_size = 12))

# DATA 
# mpg contains 234 vehicle configurations for 38 car models, using EPA fuel
# economy data for model years 1999 and 2008. A row is a vehicle configuration,
# not a unique individual car or an independently sampled manufacturer.
#
# Variables used here:
#   hwy       = highway fuel economy, in miles per gallon (mpg).
#               Higher values mean a vehicle travels farther per gallon.
#   displ     = engine displacement (engine size), in liters (L).
#   trans     = detailed transmission description.
#   model     = car model name.
#   manufacturer = manufacturer name.
#
# Documentation: https://ggplot2.tidyverse.org/reference/mpg.html
# To open the help page in R: ?ggplot2::mpg
ggplot2::mpg
cars_dat <- ggplot2::mpg

# ----------------------------------------------------------------------------
# 1. Inspect and prepare the data 
# ----------------------------------------------------------------------------
# Research question:
# How does mean highway fuel economy vary with engine displacement?
#
# a. Use glimpse() to inspect mpg.
# b. Identify the response and predictor indicated by the question in a comment. Give their units.
# c. Use summarize() to count the total rows and missing values in hwy and displ.
# d. Create cars_dat by removing rows missing hwy or displ. Retain all columns.
#    State how many rows remain. Explain why missing values in an unused
#    variable would not require removal of a row for this model.

# Your code and answers:
#a.
glimpse(mpg)
glimpse(cars_dat)
#b. The response is the hwy(highway fuel economy) in miles per gallon and the
# predictor is the displ(engine displacement) in liters. 
#c. 
car_dat <- mpg |>
  summarise(
    n_rows = n(), 
    missing_hwy = sum(is.na(hwy)), 
    missing_displ = sum(is.na(displ))
  )
nrow(cars_dat)
# There are 234 rows that remain. There is no need to remove missing values in a 
# row that is not being used. 

# ----------------------------------------------------------------------------
# 2. Explore the numeric association 
# ----------------------------------------------------------------------------
# a. Make a scatterplot of highway fuel economy versus engine displacement.
# 	 Put the predictor on the x-axis, label both axes with units, 
# 	 and use alpha to reduce overlap.
# b. Describe the direction, approximate shape, and scatter in 1-2 sentences.
#    Does a straight-line model look like a reasonable starting point?
# c. Calculate the correlation between engine displacement and highway fuel economy.

# Your code and answers:
#a.
ggplot(cars_dat, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(
    x = "Engine Displacement(L)", 
    y = "Highway Fuel Economy(mpg)"
  )
#b. The direction of the scatter plot is a strong negative association because the 
# smaller the engine, the larger the hwy and vice versa, the larger the engine the 
# lower the hwy. A straight line is a fine starting point but I think there are
# stronger models for this data. 
#c. 
cars_dat |> 
  summarise(correlation = cor(displ, hwy))
# ~0.766, strong negative association

# ----------------------------------------------------------------------------
# 3. Fit and interpret a linear model with a single numeric predictor 
# ----------------------------------------------------------------------------
# a. Fit an lm() model using highway fuel economy as the response and 
#		 engine displacement as the predictor.
#    Name it hwy_model. 
# b. View summary(hwy_model) and tidy(hwy_model).
# c. Interpret the slope in a sentence with units. 
# d. Interpret the intercept. Is an engine displacement of 0 L in the observed
#    range? 
# e. Use glance() and select() to display nobs, r.squared, and sigma.
#    Interpret R-squared and sigma in words, including sigma's units.
# f. Use the coefficient table to assess the evidence against a zero slope.
#    Does a small p-value show the model fits well?

# Your code and answers:
#a. 
hwy_model <- lm(hwy ~ displ, data = cars_dat)
#b. 
summary(hwy_model)
tidy(hwy_model)
#c. The increase in in engine displacement is due to -3.53 mpg predicted hwy. 
#d. 
range(cars_dat$displ)
# The intercept is 35.7 mpg, 0L is outside the observed range of 1.6-7.0 L. 
#e. 
glance(hwy_model) |>
  select(nobs, r.squared, sigma)
# Nobs is 234, R-squared value is 0.587, and sigma is 3.84 mpg. The model shows 
# 58.7% observed hwy variation to predicted mean. The estimated error is 3.84 mpg
# but that does not equal the 58.7%. 
#f. The p-value is 2.04e-46 which shows evidence against a zero slope and a p-value
# can't show causation of model fit.

# ----------------------------------------------------------------------------
# 4. Fitted values, residuals, predictions, and plot
# ----------------------------------------------------------------------------
# a. Use augment(hwy_model, data = cars_dat) to create a data frame containing
#    fitted values and residuals. Save it as fitted_hwy.
#    Display displ, hwy, .fitted, and .resid for the first six observations.
# b. For the FIRST observation, calculate observed minus fitted fuel economy.
#    Check that your answer matches .resid. Did the model overpredict or
#    underpredict highway fuel economy for this configuration?
#    A positive residual means the model underpredicted;
#    a negative residual means the model overpredicted.
#    Code hints: Start with fitted_hwy |> slice_head(n = 1).
#    Use transmute() to display hwy, .fitted, .resid, and a new column
#    calculated as hwy - .fitted.
# c. Create a tibble called new_engines with 100 evenly spaced displ values
#    spanning the observed range in cars_dat. Use seq().
# d. Use augment() with newdata = new_engines, interval = "confidence", and
#    conf.level = 0.95 to create hwy_predictions.
# e. Plot the original observations, a prediction line, and a shaded 95%
#    confidence band. Use .fitted, .lower, and .upper from hwy_predictions.
# f. Check the observed displ range. Would a prediction at 10 L be outside
#    the range of observed values (extrapolation)?
#
# Your code and answers:
#a. 
fitted_hwy <- augment(hwy_model, data = cars_dat)
fitted_hwy |>
  select(displ, hwy, .fitted, .resid) |>
  slice_head(n = 6)
fitted_hwy
#b. 
fitted_hwy |>
  slice_head(n = 1) |> 
  transmute(
    observed_mpg = hwy, 
    fitted_mgp = .fitted, 
    residual_mpg = .resid, 
    observed_minus_fitted_mpg = hwy - .fitted
  )
# The model overpredicted with the resid being -0.343. 
#c.
new_engines <- tibble(
  displ = seq(
    min(cars_dat$displ),
    max(cars_dat$displ),
    length.out = 100
  )
)
new_engines
#d. 
hwy_predications <- augment(
  hwy_model,
  newdata = new_engines, 
  interval = "confidence", 
  conf.level = 0.95
)
hwy_predications
#e. 
ggplot(cars_dat, aes(x = displ, y = hwy)) +
  geom_ribbon(hwy_predications,
            mapping = aes(x = displ, ymin = .lower, ymax = .upper),
    inherit.aes = FALSE,
    fill = "skyblue", alpha = 0.3
  ) +
  geom_point(alpha = 0.5) +
  geom_line(hwy_predications, 
            mapping = aes(y = .fitted),
            color = "skyblue", linewidth = 1
          ) +
  labs(
    x = "Engine Displacement(L)", 
    y = "Highway Fuel Economy(mpg)"
    )
#f. 
range(cars_dat$displ)
# 10L would be outside the range of 1.6-7.0 L.

# # ----------------------------------------------------------------------------
# 5. Diagnostics
# ----------------------------------------------------------------------------
# Run plot(hwy_model) to inspect the four default diagnostic plots.
# In the Console, you may need to press Enter to advance through the plots.
#
# a. RESIDUALS VERSUS FITTED — Linearity
#    Are residuals scattered around zero without a clear curved pattern?
#    Use the red smooth line to help identify patterns.
#    What would systematic curvature suggest about whether a straight line
#    adequately describes the mean relationship?
#
# b. NORMAL Q-Q — Normality of errors
#    Do the points roughly follow the reference line?
#    Describe any departures, particularly in the tails.
#    Are normally distributed errors a reasonable assumption here?
#    Remember: the response variable itself does not need to be normal.
#
# c. SCALE-LOCATION — Equal variance of errors
#    Is the spread roughly similar across fitted values, with a roughly
#    horizontal red smooth line?
#    Does the plot show an increasing or decreasing spread?
#    What would this suggest about the equal-variance assumption?
#
# d. RESIDUALS VERSUS LEVERAGE — Potentially influential observations
#    Look for observations with both high leverage and large standardized
#    residuals. Are any points near or beyond the Cook's distance contours,
#    if shown?
#    These points may strongly affect the fitted model. This is an influence
#    check, rather than a separate assumption of linear regression.
#    Why should you investigate flagged observations rather than
#    automatically delete them?
#
# Your code and answers:
plot(hwy_model)
#a. The smooth red line shows some curvature within the graph. It shows more 
# distribution of the data rather than a straight line that misses parts
# of the relationship.
#b. The points are mostly following the reference line but then break off partly
# on the ends. Normally distributed errors are not completely reasonable as a normal 
# distribution is not required.
#c. The smooth red line and points plotted follow a somewhat similar spread, but 
# the red line still misses some variation with the data. The plot shows a slight
# increase in the spread but mostly neutral. The equal-variance assumption is questionable
# with the variation in data. 
#d. There are a majority of the points near the Cook's distance line but also a large
# amount of the data and points are past the Cook's distance. Investigating a flagged
# observation rather than deleting it helps with realizing where in the data there could
# be a missing value or a wrong data point submitted. It might also help with conclusions
# of the data.

# ----------------------------------------------------------------------------
# 6. Communicate your conclusion 
# ----------------------------------------------------------------------------
# Write 2-3 sentences about the hwy_model:
#   - State the estimated association with direction, magnitude, and units.
#   - Mention one diagnostic concern and what it means for interpretation.

# Your conclusion:
# Based on the sampled vehicle configurations, a 1L increase in displ is associated
# with a 3.53 mpg lower predicted mean hwy. In the residual plots, the straight lines
# in the models miss important data and the slope is simplified compared to detailed
# points plotted. 

# ----------------------------------------------------------------------------
# 7. A categorical predictor: transmission group 
# ----------------------------------------------------------------------------
# How does mean highway fuel economy differ between automatic
# and manual transmission configurations in this dataset?
#
# Run this supplied preparation code. It creates a two-level factor from the
# detailed transmission descriptions. Automatic is explicitly the reference.
# It uses the SAME source dataset as the numeric model.

cars_trans <- cars_dat |>
  filter(!is.na(trans)) |>
  mutate(
    transmission_group = case_when(
      str_starts(trans, "auto") ~ "automatic",
      str_starts(trans, "manual") ~ "manual",
      .default = NA_character_
    ),
    transmission_group = factor(
      transmission_group,
      levels = c("automatic", "manual")
    )
  ) |>
  drop_na(transmission_group)

# a. Summarize sample size and mean hwy for each transmission_group.
#    Make a boxplot of hwy by transmission_group, with jittered observations.
# b. Fit trans_model: hwy ~ transmission_group. Display tidy(trans_model).
# c. Interpret (Intercept) and transmission_groupmanual in words and mpg units.
#    Is transmission_groupmanual the manual-group mean or a difference?
# d. Use augment() with newdata = tibble(transmission_group =
#    c("automatic", "manual")) to obtain predictions for both groups.
# e. Run plot(trans_model). Why does residuals-versus-fitted show two vertical
#    stacks? What can their spread tell us? Why is checking for a curved
#    relationship less useful than in the numeric-predictor model?
# f. Does this model's group difference account for engine displacement?
#    Can we conclude switching a car's transmission would change its fuel
#    economy by this amount? Explain in one sentence.

# Your code and answers:

#a.
transmission_means <-cars_trans |>
  group_by(transmission_group) |>
  summarize(
    n_configurations = n(), 
    mean_hwy = mean(hwy))
transmission_means
# Automatic n = 157 and mean_hwy is 22.3 mpg. Manual n = 77 and mean_hwy is 25.8 mpg.
ggplot(cars_trans,aes(x = transmission_group, y = hwy)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.5) +
  labs(
    x = "Transmission group", 
    y = "Highway fuel economy (mpg)"
    )

#b.
trans_model <- lm(hwy ~ transmission_group, data = cars_trans)
tidy(trans_model)
#c. The predicted mean for automatic configurations is about 22.293 mpg. The
# manual-minus-automatic mean difference, about is +3.486 mpg. The manual-group mean
# is 25.779 mpg.
#d.
trans_predictions <- augment(
  trans_model,
  newdata = tibble(transmission_group = c("automatic", "manual"))
  ) |>
  select(transmission_group, .fitted)
trans_predictions

trans_predictions |>
  left_join(transmission_means, by = "transmission_group") |>
  mutate(difference = .fitted - mean_hwy)
# Differences should be zero up to floating-point rounding.
#e.
plot(trans_model)
#Why does residuals-versus-fitted show two vertical
#    stacks? What can their spread tell us? Why is checking for a curved
#    relationship less useful than in the numeric-predictor model?
# There are two fitted values and one estimated mean per group. The first graph 
# has two vertical lines of points to check equal variance but there is no continuous 
# predictor on which to check curvature. The curved relationship is less useful than a 
# numeric-predictor because it shows less of the data with less detail. It is smaller
# amounts of data rather than numeric amounts.

# f. This model does not account for engine displacement. We can't conclude if 
# switching a car's transmission would change its hwy due to the unaccounted data
# needed.

# ============================================================================
# OPTIONAL CHALLENGES: outside the 60-minute core
# ============================================================================
# A. Predict mean hwy at displ = 3 L, with a 95% confidence interval. Then obtain
#    a 95% prediction interval for an individual configuration at 3 L.
#    Which interval is wider, and why?
#
# B. Add .fitted and .resid to cars_trans using augment(trans_model,
#    data = cars_trans). Plot residuals by transmission_group and make a normal
#    Q-Q plot faceted by transmission_group. What does each plot assess?
#
# C. Color the original hwy-versus-displ scatterplot by class. What structure
#    does the pooled numeric model omit? Discuss without fitting a new model.
#
# Data reference: ggplot2::mpg (EPA fuel economy data).
# https://ggplot2.tidyverse.org/reference/mpg.html
