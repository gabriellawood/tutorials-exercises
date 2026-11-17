	# Linear and generalized additive mixed-effects models in R
	#
	# In the tutorial, we modeled log beak height using log wing length and taxon.
	# Here, we model log eelgrass biomass using depth and site. Each row is
	# one quadrat. Depth is already centered at 3 m in the setup below.
	#

	# Load packages for data wrangling, plotting, and fitting GAMMs.
	library(dplyr)
	library(ggplot2)
	library(mgcv)
	
	# Set the seed so everyone simulates the same dataset.
	set.seed(316)
	
	# Simulate one observation per quadrat at 12 eelgrass sites. 
	# All sites span 1–5 m, with different sample sizes. 
	# Create one row per site with its quadrat count and the values used to 
	# create group-level intercepts and slopes. 
	sites <- tibble::tibble(
	  site = factor(sprintf("Site %02d", 1:12)),
	  n_quadrats = c(5, 6, 8, 9, 10, 12, 14, 16, 18, 20, 24, 28),
	  intercept_shift = c(9, -8, 3, 6, -6, 4, -4, 1, 7, -3, -2, -7),
	  slope_shift = c(1, -2, 3, -4, 2, -3, 4, -1, 3, 1, -3, -2)
	)
	
	# Generate one row per quadrat. Depth ranges from 1 to 5 m, so depth_c is zero at 3 m.
	# Add Gaussian noise to the shared curve and site adjustments to simulate log biomass.
	eelgrass <- sites |>
	  group_by(site) |>
	  reframe(
	    depth_m = seq(1, 5, length.out = first(n_quadrats)),
	    depth_c = depth_m - 3,
	    log_biomass = 4.2 + first(intercept_shift) / 20 +
	      (-0.35 + first(slope_shift) / 20) * depth_c +
	      0.16 * depth_c^2 + rnorm(length(depth_c), sd = 0.14),
	    biomass = exp(log_biomass)
	  )
	
	# 1. Plot the data and compare complete pooling and no pooling----------------------------
	
	# Identify the response, predictor, and grouping variable. Make one plot
	# with a line for all quadrats and another with a separate line per site.
	# What does each approach assume about differences among sites?
	count(eelgrass, site)
# Log biomass is the response and depth(m) is the predictor. One plot uses each
# site as an individual observation where the other plot uses every site for one observation.
	
	# Plot 1
	ggplot(eelgrass, aes(x = depth_m, y = log_biomass)) +
	  geom_point(aes(color = site)) +
	  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	  labs(
	    x = "Depth(m)", 
	    y = "Log Eelgrass Biomass"
	  )
	# Plot 2
	ggplot(eelgrass, aes(x = depth_m, y = log_biomass, color = site)) +
	  geom_point() +
	  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
	  labs(
	    x = "Depth(m)", 
	    y = "Log Eelgrass Biomass"
	  )
	
	# 2. Fit and interpret a random intercept -----------------------
	
	# Fit one shared depth slope with a random intercept for site.
	# Interpret the overall intercept, depth slope, site SD, and
	# residual SD. What does the random adjustment for Site 01 mean?
	m_intercept <- lme4::lmer(
	  log_biomass ~ depth_c + (1 | site), 
	  data = eelgrass
	)
summary(m_intercept)

lme4::fixef(m_intercept)
# The intercept is 4.443 for the predicted log biomass at 3m, centered depth = 0. 
# The slope is -0.358 meaning every additional m, log biomass decreases by that amount.
lme4::ranef(m_intercept)$site
# Site 01 is 0.369, sitting above overall line. Site 12 is -0.379, sitting below overall
# line. 
coef(m_intercept)$site
# Site 01 predicted log biomass is 4.812 and Site 12 predicted log biomass is 4.064. 

	# 3. Plot overall and site predictions --------------------
	
	# Use predict() to generate predictions (like the tutorial). 
	# Plot the overall fitted line and the site-specific lines. 
	# Why are the lines parallel? 
	# Check the residuals against fitted values and inspect the Q–Q plot.
  eelgrass <- eelgrass |>
    mutate(
      fit_intercept = predict(m_intercept),
      fit_overall = predict(m_intercept, re.form = NA)
  )

  ggplot(eelgrass, aes(depth_m, log_biomass)) +
    geom_point(aes(colour = site), alpha = 0.5) +
    geom_line(aes(y = fit_intercept, colour = site)) +
    geom_line(aes(y = fit_overall), colour = "black") +
    labs(
      x = "Depth (m)", 
      y = "Log eelgrass biomass", 
      colour = "Site"
    )
# The colored lines use each sites adjusted intercepts rather than the black line using
# the shared intercepts. = Partial pooling
  
  plot(m_intercept)
  qqnorm(residuals(m_intercept))
  qqline(residuals(m_intercept))
# Residuals vs. Fitted: The points tend to show a small concave up curvature with negative 
# values falling in the middle and positive values spreading to the ends. 
  
#QQ Plot: The points show departure from both end points on the graph and they don't
# follow the line fully, some points sit below and some sit above. 
  
	# 4. Fit a random slope and check the fit------------------------
	
	# Fit a model that lets the relationship between eelgrass biomass and depth vary by site 
	# 	(random slope); also include a random intercept. 
	# Compare fitted lines with the random-intercept only model. 
	# Inspect residual spread and the Q–Q plot.
	# Then look for patterns in residuals vs. depth within sites.
  m_slopes <- lme4::lmer(
    log_biomass ~ depth_c + (1 + depth_c | site),
    data = eelgrass
  )
summary(m_slopes)
lme4::isSingular(m_slopes)
# Came back as FALSE
eelgrass <- eelgrass |>
  mutate(
    fit_slopes = predict(m_slopes),
    resid_slopes = residuals(m_slopes)
  )
  ggplot(eelgrass, aes(depth_m, log_biomass)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(y = fit_intercept, colour = "Random intercept")) +
    geom_line(aes(y = fit_slopes, colour = "Random intercept + slope")) +
    facet_wrap(~ site) +
    labs(
      x = "Depth (m)", 
      y = "Log eelgrass biomass", 
      colour = NULL
    )
	
plot(m_slopes)
# The residuals tend to show a similar concave up pattern as before. 
qqnorm(eelgrass$resid_slopes)
qqline(eelgrass$resid_slopes)
# The points are still not fully following the line as some sit below and some sit
# above. There is also decent departure from the line at the end points once again. 
  ggplot(eelgrass, aes(depth_m, resid_slopes)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ site) +
    labs(
      x = "Depth (m)", 
      y = "Residual (log units)"
    )
# There is a strong concave up curve shown in all of the plots showing there was too
# little biomass recorded at the deepest and shallowest depths and too much biomass
# recorded for the middle depths. 

	# 5. Fit and check two GAMMs ---------------------------------------------
	# Fit a shared smooth of depth with a random intercept for site.
	# Then fit a second GAMM that also includes a random slope for depth.
	# Plot the fitted curves from both GAMMs within each site.
	#
	# At Site 12, compare residuals from the linear mixed model and the
	# GAMM with random intercepts and slopes. Does the U-shape weaken
	# after adding the smooth?
	#
	# Run k.check() on both GAMMs. Focus on the depth smooth and explain
	# whether the results give you a reason to try a larger k.
	
  m_gamm_int <- mgcv::gam(
    log_biomass ~ s(depth_c, k = 5) + s(site, bs = "re"),
    data = eelgrass,
    family = gaussian(),
    method = "REML"
  )
  
  m_gamm <- mgcv::gam(
    log_biomass ~ s(depth_c, k = 5) +
      s(site, bs = "re") +
      s(site, depth_c, bs = "re"),
    data = eelgrass,
    family = gaussian(),
    method = "REML"
  )
  
  eelgrass <- eelgrass |>
    mutate(
      fit_gamm_int = predict(m_gamm_int, type = "response"),
      fit_gamm = predict(m_gamm, type = "response"),
      resid_gamm = residuals(m_gamm, type = "response")
    )
  
  ggplot(eelgrass, aes(depth_m, log_biomass)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(y = fit_gamm_int, colour = "Random intercept")) +
    geom_line(aes(y = fit_gamm, colour = "Random intercept + slope")) +
    facet_wrap(~ site) +
    labs(
      x = "Depth (m)", 
      y = "Log eelgrass biomass", 
      colour = NULL
    )
  
  residual_comparison <- bind_rows(
    eelgrass |> transmute(site, depth_m, residual = resid_slopes,
                          model = "Linear mixed model"),
    eelgrass |> transmute(site, depth_m, residual = resid_gamm,
                          model = "GAMM")) |>
    filter(site == "Site 12")
  
  ggplot(residual_comparison, aes(depth_m, residual)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    facet_wrap(~ model) +
    labs(
      x = "Depth (m)", 
      y = "Residual (log units)"
    )
# Site 12 shows clear concave up curvature, underpredicts high and low depths but 
# overpredicts medium depths. The GAMM is more evenly scattered and no clear concave up.
	
  mgcv::k.check(m_gamm_int)
  mgcv::k.check(m_gamm)
# The max edf = k' = 4, edf = 3.70/3.86, k-index = 1.17/0.93, p-value = 0.9775/0.1225
  
  
	# 6. Summarize the results --------------------------------------------------
	# In three sentences, describe how predicted log biomass changes with
	# depth, how the fitted curves differ among sites, and what the Site 12
	# residual plots show about adding a smooth.
  
# The log biomass generally showed a pattern of high to low to high changes at low, med, 
# and high depths. The fitted curves showed the same pattern but site-specific graphs
# differed. Site 12 showed a strong concave up curvature for mixed model but more evenly 
# spread for the GAMM plot. 

	# Optional if you have time in class: refit using nlme -------------------
	
	# Try writing the same random-intercept model with a separate random
	# formula. Compare the fixed coefficients with lme4::fixef(m_intercept).
	
	
	# Optional if you have time class: fit a shared smooth with gamm() -----
