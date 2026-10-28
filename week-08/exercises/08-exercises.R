# ============================================================================
# Multiple regression in R 
#
# The exercises use palmerpenguins::penguins, restricted to Chinstrap penguins.
# Requires tidyverse, broom, and palmerpenguins to be installed.
# ============================================================================

	library(tidyverse)
	library(broom)
	library(palmerpenguins)
	
	theme_set(theme_bw(base_size = 12))

# ----------------------------------------------------------------------------
# 1. Prepare and explore the data
# ----------------------------------------------------------------------------
# Question: How does mean flipper length vary with body mass and sex
# in Chinstrap penguins?
#
# a. Create chinstrap_dat: keep Chinstrap penguins, remove missing values
#    in flipper_length_mm, body_mass_g, bill_depth_mm, and sex, and make
#    female the reference level for sex. Use this sample throughout.
chinstrap_dat <- penguins |>
	  filter(species == "Chinstrap") |>
	  drop_na(flipper_length_mm, body_mass_g, bill_depth_mm, sex) |>
	  mutate(sex = factor(sex, levels = c("female", "male")))
	
	glimpse(chinstrap_dat)

# b. Count the penguins of each sex.
	chinstrap_dat |>
	  count(sex)

# Answer: 34 individuals for each sex, male and female. 

# c. Plot body mass (x) and flipper length (y), colored by sex.
  ggplot(chinstrap_dat, 
         aes(x = body_mass_g, 
             y = flipper_length_mm, 
            color = sex)) +
    geom_point(alpha = 0.5) +
    labs(
      x = "Body Mass(g)", 
      y = "Flipper Length(mm)", 
      color = "Sex", 
      title = "Body Mass and Flipper Length by Sex"
    ) +
    theme_classic()
# d. Describe the relationship and the overlap in body-mass ranges.
	# The females tend to have a lower body mass and smaller flipper length
  # and the males tend to show the opposite. But, there is some overlap where
  # similar body mass in different sexes show a similar flipper length. 

# ----------------------------------------------------------------------------
# 2. Fit and interpret an additive model 
# ----------------------------------------------------------------------------
# a. Fit an additive model using body mass and sex as predictors.
	flipper_model_add <- lm(flipper_length_mm ~ body_mass_g + sex, 
	                        chinstrap_dat)
tidy(flipper_model_add)

# b. Examine coefficients with 95% confidence intervals.
 add_coefficients <- tidy(flipper_model_add, conf.int = TRUE)
 add_coefficients

# c. Interpret each coefficient, including units and what is held constant.
 # The intercept is 161 mm, the predicted mean female flipper length with a 
 # mass of 0 g. 
 # body_mass_g is 0.00864 mm/g, mean flipper length increases 0.00864 mm per 1 g 
 # of body mass increase and this is constant for each sex.
 # sexmale is 4.62 mm, male predicated mean flipper length is 4.62 mm larger than
 # a female mean flipper length observed at the same body mass.
	
# d. Calculate the predicted change for a 100 g increase in body mass.
	chinstrap_change <- add_coefficients |> 
	  filter(term == "body_mass_g") |>
	  transmute(chinstrap_change_mm = estimate * 100)
chinstrap_change
# About 0.864 mm greater mean flipper length per 100 g increase in body mass.

# e. Does the sexmale confidence interval include zero? Interpret it.
 add_coefficients |>
   filter(term == "sexmale") |>
   select(term, estimate, conf.low, conf.high, p.value)
# Estimate: 4.62, Low: 1.64, High: 7.60, P.Value: 0.00292. The interval is 
 # 1.64 to 7.60 which means it does not include 0. There is evidence of greater
 # mean flipper length for males at similar body mass. 

# f. What assumption is made about slopes?
 # Males and females only share one slope line, they are not identical but they
 # run parallel. 
 
# ----------------------------------------------------------------------------
# 3. Generate and plot predictions 
# ----------------------------------------------------------------------------
# a. Generate 100 masses within each sex's observed body-mass range.
  chinstrap_masses <- chinstrap_dat |>
   group_by(sex) |>
   reframe(
     body_mass_g = seq(min(body_mass_g), 
                       max(body_mass_g), 
                       length.out = 100)
   )


# b. Generate predicted means and 95% confidence intervals.
	chinstrap_predictions <- predict(
	  flipper_model_add, 
	  newdata = chinstrap_masses, 
	  interval = "confidence", 
	  level = 0.95) |>
	  as_tibble()
	  
	prediction_bands <- bind_cols(chinstrap_masses, chinstrap_predictions)
	head(prediction_bands)

# c. Plot observations, predictions, and confidence ribbons.
  chinstrap_additive <- ggplot(chinstrap_dat, 
                               aes(x = body_mass_g, y = flipper_length_mm,
                                   color = sex)) +
    geom_ribbon(prediction_bands, 
                mapping = aes(x = body_mass_g, ymin = lwr, ymax = upr,
                                      fill = sex, group = sex), 
                inherit.aes = FALSE, alpha = 0.15) +
    geom_point(alpha = 0.5) +
    geom_line(prediction_bands, 
              mapping = aes(y = fit, group = sex), 
              linewidth = 1) +
    labs(
      x = "Body Mass(g)", 
      y = "Flipper Length(mm)", 
      color = "Sex", fill = "Sex", 
      title = "Chinstrap Additive Model", 
      subtitle = "Predicted Means With 95% Pointwise Confidence Intervals"
    )
chinstrap_additive
# d. Why are the predicted lines parallel?
	# The lines are parallel due to the sexmale coefficients shifting the line 
# vertically but not changing its slope. 

# e. What do the shaded bands describe?
	# The shaded regions represent the allowed error in predicted flipper length]
# at each body mass for each sex.

# ----------------------------------------------------------------------------
# 4. Fit and evaluate an interaction model 
# ----------------------------------------------------------------------------
# a. Fit a model that allows the body-mass slope to differ by sex.
	flipper_model_int <- lm(flipper_length_mm ~ body_mass_g * sex, 
	                        chinstrap_dat)
tidy(flipper_model_int)

# b. Inspect coefficients and confidence intervals.
int_coefficients <- tidy(flipper_model_int, conf.int = TRUE)
int_coefficients
	
# c. Report the female slope, slope difference, and male slope.
	female_slope <- int_coefficients |>
	  filter(term == "body_mass_g") |>
	  pull(estimate)

	slope_difference <- int_coefficients |>
	  filter(term == "body_mass_g:sexmale") |>
	  pull(estimate)
	
	slope_summary <- tibble(
	  female_slope_mm = female_slope, 
	  male_minus_female_slope_mm = slope_difference, 
	  male_slope_mm = female_slope + slope_difference
	)
# Female slope is 0.00488 mm/g, male slope is 0.01097 mm/g, and slope difference
	#is 0.00608 mm/g. The male slope is steeper. 
	  
# d. Explain the change in the sexmale interpretation.
	# The sexmale is constant for each sex in the additive model but for the 
	# interactive model, the sexmale if the difference at a body mass of 0 g. 

# e. Compare AIC values and calculate the difference.
	aic_comparison <- AIC(flipper_model_add, flipper_model_int) |>
	  as_tibble(rownames = "model") |>
	  mutate(delta_AIC = AIC -min(AIC))
aic_comparison
# Additive AIC is 422 and interaction AIC is 421, but the additive model is 
# higher by 0.430. Overall they are within the range of 2 from each other so 
# either model works but the additive is the more simpler option.

# f. Examine uncertainty in the interaction coefficient.

# g. Which model would you select, and why?
# I would select the additive model because it is the simpler option and AIC
# comparison is similar compared to both models. There is no evidence that the
# slopes differ between sexes. 
# ----------------------------------------------------------------------------
# 5. Examine model diagnostics 
# ----------------------------------------------------------------------------
# Use the additive model for the following diagnostics.
# a. Inspect all four default diagnostic plots.
plot(flipper_model_add)

# b. Obtain fitted values and residuals.
chinstrap_results <- augment(flipper_model_add, chinstrap_dat)

# c. Plot residuals against body mass, colored by sex.
chinstrap_residuals <- ggplot(chinstrap_results,
  mapping = aes(x = body_mass_g, y = .resid, color = sex)
) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Body Mass(g)",
    y = "Residual(mm)",
    color = "Sex",
    title = "Additive, Residuals Versus Body Mass"
  )
chinstrap_residuals

# d. Describe any patterns in the residuals, including curvature,
#    changes in spread, or differences between sexes.
# The points are very widely scattered throughout the plot. There seems to be
# opposing trends by sex meaning a smaller slope for females than males. There
# is more detaching on the tail ends of the data. There does seem to be
# a small decreasing curve.

# e. Examine the Q-Q plot. Are there substantial departures from the line?
# There are departures at the tail ends of the graph, but not huge noticable ones.

# ----------------------------------------------------------------------------
# 6. Center and standardize numeric predictors 
# ----------------------------------------------------------------------------
# New question: How does mean flipper length vary with body mass and
# bill depth?
#
# a. Fit the model in original units.
chinstrap_numeric <- lm(
  flipper_length_mm ~ body_mass_g + bill_depth_mm,
  chinstrap_dat
)

# b. Center each predictor using its mean.
chinstrap_dat <- chinstrap_dat |>
  mutate(
    body_mass_c = body_mass_g - mean(body_mass_g),
    bill_depth_c = bill_depth_mm - mean(bill_depth_mm)
  )

# c. Fit a model with centered predictors.
chinstrap_centered <- lm(
  flipper_length_mm ~ body_mass_c + bill_depth_c,
  data = chinstrap_dat
)
# d. Standardize both predictors: subtract the mean, then divide by SD.
chinstrap_dat <- chinstrap_dat |>
  mutate(
    body_mass_z = (body_mass_g - mean(body_mass_g)) / sd(body_mass_g),
    bill_depth_z = (bill_depth_mm - mean(bill_depth_mm)) / sd(bill_depth_mm)
  )
# e. Fit a model with standardized predictors.
chinstrap_standardized <- lm(
  flipper_length_mm ~ body_mass_z + bill_depth_z,
  data = chinstrap_dat
)
# f. Examine the coefficients from all three models.
#    Interpret the intercept in the centered model and both slopes
#    in the standardized model. Include units and explain what
#    is held constant.
coefficient_comparison <- bind_rows(
  original = tidy(chinstrap_numeric, conf.int = TRUE),
  centered = tidy(chinstrap_centered, conf.int = TRUE),
  standardized = tidy(chinstrap_standardized, conf.int = TRUE),
  .id = "model") |>
  select(model, term, estimate, std.error, conf.low, conf.high)
coefficient_comparison
# The centered intercept is approximately 195.824 mm: predicted
# mean flipper length at the sample's mean mass (3733.088 g) and mean
# bill depth (18.421 mm). 
# The body_mass_z coefficient is approximately 3.269 mm: a one-SD
# increase in body mass (about 384.335 g) is associated with a predicted
# 3.269 mm increase in mean flipper length, holding bill depth constant.
# The bill_depth_z coefficient is approximately 2.161 mm per one-SD 
# increase in bill depth (about 1.135 mm), holding body mass constant.

# g. Verify that the fitted values are equal across all three models,
# allowing for numerical rounding.
all.equal(
  fitted(chinstrap_numeric),
  fitted(chinstrap_centered)
)

all.equal(
  fitted(chinstrap_numeric),
  fitted(chinstrap_standardized)
)
# The answer is true.
# h. Explain what happens to the slopes when you center or
#    standardize the predictors.	
# Centering shifts the predictor origin. A 1 g or 1 mm increment still
# has its original size, so the additive-model slopes are unchanged.
# Standardizing changes to units of SD. Each new slope
# equals the original slope multiplied by that predictor's sample SD.
# Flipper length was not standardized. The response remains in mm.
# Centered and standardized models have the same intercept because
# both assign zero to each predictor's sample mean.
	
# i. Would standardizing fix curvature or unequal residual variance?
# No. Centering and standardizing predictors leave fitted values
# and residuals unchanged for these equivalent additive linear models.
# They also do not remove correlation between the two predictors.

# ============================================================================
# Optional extension if time:
# compare additive and interaction predictions visually
# ============================================================================
# Generate predictions on the same grid and plot models side by side.
	
