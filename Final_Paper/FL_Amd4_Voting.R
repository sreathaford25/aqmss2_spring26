# Sara Reathaford
# AQMSS2
# Final Project


# Florida Amendment 4: The Effect of Felon Re-enfranchisement on Voter Registration
# and Voter Turnout

# Difference-in-Difference Analysis with Cont. Treatment Intensity

#-------------------------------------------------------------------------------

# Design Overview
#===============================================================================

# Amendment 4 passed in November 2018 and restored voting rights to roughly
# 1.4 million Floridians with felony convictions. However, SB 7066 (June 2019)
# added a requirement that returning citizens must first pay all outstanding
# court fines and fees before re-registering to vote. This significantly
# limited Amendment 4's practical effect, especially for low-income
# returning citizens.

# Research Questions:
#   (1) Did Amendment 4 increase voter turnout in counties where more
#       returning citizens lived?
#   (2) Did it also increase voter registration, and if so, was the
#       registration effect larger than the turnout effect? (A gap
#       between the two would suggest SB 7066 was blocking people who
#       registered from actually voting.)
#   (3) Was the effect larger in higher-income counties, where fewer
#       returning citizens are blocked by SB 7066's fines/fees requirement?

# Identification Strategy:
# Amendment 4 was a statewide policy, so I cannot compare Florida to another
# state as a control group. Instead, I use cross-county variation in how many
# returning citizens live in each county.

# Treatment proxy: Average county prison ADMISSION rate per 100,000 residents,
# averaged over 2000-2018 (Vera Institute Incarceration Trends data).


# An initial event study reveals a pre-existing differential trend in 2008:
# high-prison counties had unusually high relative turnout in 2008 compared
# to 2016. The most likely explanation is the Obama mobilization effect --
# in 2008, Black voter turnout surged nationally, and high-prison counties
# tend to have larger Black populations. I address this by including an
# Obama 2008 correction variable (pct_black x 2008 indicator) in our
# preferred specifications. We also show a drop-2008 robustness check.

# COVID confound:
# The 2020 election had a large COVID-related expansion of mail-in voting
# that may have affected turnout for reasons unrelated to Amendment 4.
# I address this with a robustness check that drops 2020 entirely.

# Units:     Florida counties (N = 67)
# Time:      2008, 2012, 2016 (pre) | 2020, 2024 (post)
# Reference: 2016 (the last election before Amendment 4; event time = 0)
# Outcomes:  Turnout = total votes / Citizen VAP
#            Registration = active registrations / Citizen VAP
# SEs:       Clustered by county (primary)
# NOTE:      We use Citizen Voting-Age Population (CVAP) as the denominator,
#            not total population or VAP. CVAP excludes non-citizens who
#            cannot vote, making it the most accurate measure of the
#            eligible electorate.

# Units:     Florida counties (N = 67)
# Time:      2008, 2012, 2016 (pre) | 2020, 2024 (post)
# Reference: 2016 (last pre-treatment election; event time = 0)
# Outcomes:  Turnout      = total votes / Citizen VAP (CVAP)
#            Registration = active registrations / CVAP
# SEs:       Clustered by county

# ============================================================================
# 0. SETUP
# ============================================================================

set.seed(42)

# Install packages

library(tidyverse)
library(modelsummary)
library(broom)
library(fixest)
library(tidycensus)
library(tigris)
library(sf)
library(patchwork)
library(scales)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Cache tigris shapefiles so they do not re-download every run
options(tigris_use_cache = TRUE)

# Create output folders
dir.create("Final_Paper/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("Final_Paper/tables",  recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. DATA LOADING AND CLEANING
# ============================================================================

# ----------------------------------------------------------------------------
# 1.1 Election Returns (MIT Election Lab)
# DOI: https://doi.org/10.7910/DVN/VOQCHQ
# Raw data has one row per candidate per county per year.
# Collapse to one row per county per year (total votes cast).

returns_raw <- read.csv("Final_Paper/Data/countypres.csv")

fl_returns <- returns_raw %>%
  filter(state_po == "FL",
         office   == "US PRESIDENT",
         year     %in% c(2008, 2012, 2016, 2020, 2024)) %>%
  group_by(year, county_fips, county_name) %>%
  summarize(total_votes = first(totalvotes), .groups = "drop") %>%
  mutate(
    # Pad FIPS to 5 digits so it matches other datasets (e.g., "12001")
    county_fips = sprintf("%05d", as.integer(county_fips)),
    county_name = str_to_title(county_name)
  )

cat("Election rows:", nrow(fl_returns), "(should be 335)\n")

# ----------------------------------------------------------------------------
# 1.2 Treatment Proxy: Prison Admission Rate (Vera Institute)

# I use average the admission rate over 2000-2018 to get a stable pre-treatment
# measure of how many people each county was sending to prison per year.
# Counties with higher rates have more returning citizens newly eligible
# under Amendment 4.

vera_raw <- read.csv("Final_Paper/Data/incarceration_trends.csv")

vera_fl <- vera_raw %>%
  filter(state_abbr == "FL",
         year       >= 2000,
         year       <= 2018) %>%
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

# Use admission rate if available; fall back to population rate with a warning
has_adm_rate <- "total_prison_adm_rate" %in% names(vera_fl) &&
  mean(!is.na(vera_fl$total_prison_adm_rate)) > 0.5

if (has_adm_rate) {
  cat("\nUsing prison ADMISSION rate (preferred: more residence-based).\n")
  vera_fl <- vera_fl %>% mutate(proxy_rate = total_prison_adm_rate)
} else {
  cat("\nAdmission rate unavailable; using prison POPULATION rate.\n")
  cat("Note: population rate captures prison location, not residence.\n")
  vera_fl <- vera_fl %>% mutate(proxy_rate = total_prison_pop_rate)
}

treatment <- vera_fl %>%
  group_by(county_fips, county_name) %>%
  summarize(prison_rate = mean(proxy_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Standardize so coefficient = effect of 1 SD increase in treatment
    prison_rate_std = as.numeric(scale(prison_rate)),
    # Binary version (above/below median) for robustness checks
    high_treat      = as.integer(prison_rate > median(prison_rate, na.rm = TRUE))
  )

cat("\nPrison rate summary (raw):\n")
print(summary(treatment$prison_rate))

# ----------------------------------------------------------------------------
# 1.3 ACS Demographics: CVAP and Controls

# IMPORTANT: B29001_001 (pre-built CVAP) does not exist in the 2016 ACS.
# I build CVAP manually from table B05003 (Sex by Age by Citizenship),
# which is available in all ACS years.

# CVAP = citizens aged 18+:
#   B05003_009 = Male, 18+, Native-born      (citizens by birth)
#   B05003_011 = Male, 18+, Naturalized      (citizens by naturalization)
#   B05003_020 = Female, 18+, Native-born
#   B05003_022 = Female, 18+, Naturalized
#
# Other controls:
#   B02001_003 = Black or African American alone
#   B01003_001 = Total population (to compute % Black)
#   B19013_001 = Median household income
#   B23025_005 = Number unemployed
#   B23025_002 = Civilian labor force (unemployment denominator)

acs_vars <- c(
  male_native_18plus        = "B05003_009",
  male_naturalized_18plus   = "B05003_011",
  female_native_18plus      = "B05003_020",
  female_naturalized_18plus = "B05003_022",
  black_pop                 = "B02001_003",
  pop_total                 = "B01003_001",
  median_inc                = "B19013_001",
  unemployed                = "B23025_005",
  labor_force               = "B23025_002"
)

# Function: pull one ACS year and compute derived variables
pull_acs <- function(yr) {
  get_acs(
    geography = "county",
    state     = "FL",
    variables = acs_vars,
    year      = yr,
    survey    = "acs5"
  ) %>%
    select(GEOID, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    transmute(
      county_fips = GEOID,
      acs_year    = yr,
      # CVAP = native-born 18+ (always citizens) + naturalized 18+
      cvap        = male_native_18plus + male_naturalized_18plus +
        female_native_18plus + female_naturalized_18plus,
      pct_black   = black_pop / pop_total,
      median_inc  = median_inc,
      unemp_rate  = unemployed / labor_force
    )
}

# 2016 ACS for pre-treatment years; 2021 ACS for post-treatment years
# (2021 is the closest available 5-year ACS to the 2020 election)

acs_2016 <- pull_acs(2016)
acs_2021 <- pull_acs(2021)

# Assign each election year the nearest ACS snapshot
acs_panel <- bind_rows(
  acs_2016 %>% mutate(year = 2008) %>% select(-acs_year),
  acs_2016 %>% mutate(year = 2012) %>% select(-acs_year),
  acs_2016 %>% mutate(year = 2016) %>% select(-acs_year),
  acs_2021 %>% mutate(year = 2020) %>% select(-acs_year),
  acs_2021 %>% mutate(year = 2024) %>% select(-acs_year)
)

stopifnot(nrow(acs_panel) == 335)
cat("\nACS panel rows:", nrow(acs_panel), "(should be 335)\n")

# ----------------------------------------------------------------------------
# 1.4 FIPS Diagnostic: Make Sure County Codes Match Across All Datasets

fips_returns   <- sort(unique(fl_returns$county_fips))
fips_treatment <- sort(unique(treatment$county_fips))
fips_acs       <- sort(unique(acs_panel$county_fips))

cat("\nFIPS match check (all should be 0):\n")
cat("  Returns missing from treatment:", length(setdiff(fips_returns, fips_treatment)), "\n")
cat("  Returns missing from ACS:      ", length(setdiff(fips_returns, fips_acs)),       "\n")
cat("  Treatment missing from returns:", length(setdiff(fips_treatment, fips_returns)),  "\n")

# ----------------------------------------------------------------------------
# 1.5 Merge Into Final Panel

df <- fl_returns %>%
  left_join(
    treatment %>% select(county_fips, prison_rate, prison_rate_std, high_treat),
    by = "county_fips"
  ) %>%
  left_join(acs_panel, by = c("county_fips", "year")) %>%
  mutate(
    
    # ---- Outcomes ----
    # Turnout denominator = CVAP (citizen voting-age population).
    # This is more accurate than total population or VAP because only
    # citizens aged 18+ are legally eligible to vote.
    turnout = total_votes / cvap,
    
    # ---- Treatment indicators ----
    post            = as.integer(year >= 2020),
    treat_post      = prison_rate_std * post,   # main continuous DiD term
    high_treat_post = high_treat       * post,  # binary version
    
    # ---- Event time (2016 = reference year = 0) ----
    event_time = case_when(
      year == 2008 ~ -2L,
      year == 2012 ~ -1L,
      year == 2016 ~  0L,
      year == 2020 ~  1L,
      year == 2024 ~  2L
    ),
    
# ---- Obama 2008 correction ----
    # In 2008, Black voter turnout surged nationally due to Obama's candidacy.
    # High-prison counties overlap with high-Black-population counties
    # (confirmed by our balance check). This created a pre-existing spike
    # in 2008 turnout for high-prison counties that has nothing to do with
    # Amendment 4. Without controlling for this, the event study shows a
    # false pre-trend at 2008 that violates parallel trends.
    # This variable equals pct_black in 2008 and 0 in all other years.
    # Including it as a control absorbs the Obama differential without
    # dropping 2008 from the analysis entirely.
    obama_correction = pct_black * as.integer(year == 2008),
    
# ---- Region labels (descriptive plots only) ----
    # I do NOT use region fixed effects in main models because 5 regions
    # gives too few clusters for reliable clustered standard errors.
    region = case_when(
      county_name %in% c("Miami-Dade", "Broward", "Palm Beach", "Monroe")
      ~ "South",
      county_name %in% c("Hillsborough", "Pinellas", "Pasco", "Hernando")
      ~ "Tampa Bay",
      county_name %in% c("Orange", "Osceola", "Seminole", "Lake",
                         "Volusia", "Brevard", "Polk")
      ~ "Central",
      county_name %in% c("Duval", "St. Johns", "Clay", "Nassau", "Alachua")
      ~ "Northeast",
      TRUE ~ "North/Panhandle"
    )
  ) %>%
  drop_na(turnout, prison_rate_std)

stopifnot(n_distinct(df$county_fips) == 67)
stopifnot(nrow(df) == 335)
cat("\nFinal panel:", nrow(df), "rows,", n_distinct(df$county_fips), "counties.\n")

# ============================================================================
# 2. DESCRIPTIVE STATISTICS AND EXPLORATORY PLOTS
# ============================================================================

# ----------------------------------------------------------------------------
# 2.1 Summary Statistics Table (2016 cross-section, pre-treatment)

df %>%
  filter(year == 2016) %>%
  rename(
    "Turnout (votes / CVAP)"      = turnout,
    "Prison admission rate (std)" = prison_rate_std,
    "% Black population"          = pct_black,
    "Unemployment rate"           = unemp_rate,
    "Median household income ($)" = median_inc
  ) %>%
  select(
    "Turnout (votes / CVAP)",
    "Prison admission rate (std)",
    "% Black population",
    "Unemployment rate",
    "Median household income ($)"
  ) %>%
  datasummary_skim(output = "Final_Paper/tables/tab0_summary_stats.png")

# ----------------------------------------------------------------------------
# 2.2 Balance Check

# Are high-treatment counties systematically different from low-treatment
# counties in pre-treatment characteristics? A high R-squared means
# treatment is not random and controls will be important.

balance_reg <- lm(
  prison_rate_std ~ pct_black + unemp_rate + log(median_inc),
  data = df %>% filter(year == 2016)
)

cat("\nBalance check (prison rate on pre-treatment covariates):\n")
print(tidy(balance_reg, conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))
cat("R-squared:", round(summary(balance_reg)$r.squared, 3), "\n")
cat("Interpretation: the higher this is, the more different high- and\n")
cat("low-treatment counties are, making controls more important.\n")

# ----------------------------------------------------------------------------
# 2.3 Treatment Intensity Map

fl_map <- counties(state = "FL", cb = TRUE, year = 2020) %>%
  mutate(county_fips = GEOID) %>%
  left_join(treatment, by = "county_fips")

fig0 <- ggplot(fl_map) +
  geom_sf(aes(fill = prison_rate), color = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette   = "Reds",
    direction = 1,
    name      = "Prison admission\nrate (per 100k)"
  ) +
  theme_void(base_size = 12) +
  labs(
    title   = "Treatment Intensity: County Prison Admission Rate (2000-2018 avg)",
    caption = "Source: Vera Institute. Higher = more returning citizens eligible under Amendment 4."
  )

ggsave("Final_Paper/figures/fig0_treatment_map.pdf",
       plot = fig0, width = 10, height = 6)

# ----------------------------------------------------------------------------

# 2.4 Turnout Trends: High vs. Low Treatment Counties
# Visual check: do the two groups track each other before 2018?
# Divergence before 2018 would suggest a pre-trend problem.

df_trends <- df %>%
  mutate(group = ifelse(high_treat == 1,
                        "High prison rate counties",
                        "Low prison rate counties")) %>%
  group_by(year, group) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop")

fig1 <- ggplot(df_trends, aes(x = year, y = mean_turnout, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2019, y = max(df_trends$mean_turnout) * 0.97,
           label = "Amendment 4\npasses (2018)", hjust = 0,
           size = 3.2, color = "gray30") +
  scale_color_manual(values = c("High prison rate counties" = "#d6604d",
                                "Low prison rate counties"  = "#4393c3")) +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2020, 2024)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x       = "Election year",
    y       = "Mean turnout (votes / CVAP)",
    color   = NULL,
    title   = "Voter Turnout Trends by County Treatment Group",
    caption = "Source: MIT Election Lab, Vera Institute, ACS 2016/2021."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Final_Paper/figures/fig1_trends.pdf", plot = fig1, width = 9, height = 6)

# ----------------------------------------------------------------------------
# 2.5 Scatter: Treatment Intensity vs. Turnout Change (2016 to 2020)

df_change <- df %>%
  filter(year %in% c(2016, 2020)) %>%
  select(county_fips, county_name, year, turnout, prison_rate_std) %>%
  pivot_wider(names_from = year, values_from = turnout, names_prefix = "t") %>%
  mutate(delta_turnout = t2020 - t2016)

fig2 <- ggplot(df_change, aes(x = prison_rate_std, y = delta_turnout)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_point(alpha = 0.7, color = "#2166ac") +
  geom_smooth(method = "lm", se = TRUE, color = "#d6604d",
              fill = "#f4a582", alpha = 0.3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x        = "Prison admission rate (standardized, 2000-2018 avg)",
    y        = "Change in turnout (2020 minus 2016)",
    title    = "Treatment Intensity and Turnout Change, 2016 to 2020",
    subtitle = "Each dot is one Florida county",
    caption  = "Source: MIT Election Lab, Vera Institute, ACS."
  ) +
  theme_minimal(base_size = 13)

ggsave("Final_Paper/figures/fig2_scatter.pdf", plot = fig2, width = 9, height = 6)

# ============================================================================
# 3. DIFFERENCE-IN-DIFFERENCES: VOTER TURNOUT
# ============================================================================

# County FE = compares each county to itself over time
# Year FE   = absorbs trends common to all counties (COVID, candidate effects)
# treat_post= within-county, over-time variation attributed to Amendment 4

# ----------------------------------------------------------------------------
# 3.1 Manual 2x2 DiD (binary treatment, 2016 vs. 2020 only)

# Positive DiD = high-prison counties gained MORE turnout than low-prison
# counties between 2016 and 2020.

manual_2x2 <- df %>%
  filter(year %in% c(2016, 2020)) %>%
  group_by(post, high_treat) %>%
  summarise(mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = high_treat, values_from = mean_turnout,
              names_prefix = "group_") %>%
  arrange(post) %>%
  mutate(diff = group_1 - group_0)

did_manual <- manual_2x2$diff[2] - manual_2x2$diff[1]

cat("\n--- Manual 2x2 DiD ---\n")
print(manual_2x2)
cat("\nDiD estimate:", round(did_manual, 4), "\n")
cat("Positive = high-prison counties gained more turnout after Amendment 4\n")
cat("Negative = high-prison counties gained less turnout after Amendment 4\n")

# ----------------------------------------------------------------------------
# 3.2 Regression Models
#
# I build from simple to complex. The preferred specification is m3
# (TWFE + controls + Obama correction) because it accounts for the
# differential Obama mobilization effect that was causing a pre-trend
# violation in the baseline event study.

# Model 0: Pooled OLS -- no fixed effects, just correlation
m0 <- lm(turnout ~ prison_rate_std * post, data = df)

# Model 1: Two-Way FE (county + year), no correction
m1 <- feols(turnout ~ treat_post | county_fips + year,
            data    = df,
            cluster = ~county_fips)

# Model 2: TWFE + time-varying controls
m2 <- feols(turnout ~ treat_post + unemp_rate + pct_black + log(median_inc) |
              county_fips + year,
            data    = df,
            cluster = ~county_fips)

# Model 3: TWFE + controls + Obama 2008 correction (PREFERRED)
# The obama_correction variable absorbs the differential Obama mobilization
# effect in 2008 for high-Black (and therefore high-prison) counties.
# This was the main source of the pre-trend violation we found in the
# baseline event study. If this correction works, the event study in
# Section 4 should show pre-treatment coefficients near zero.
m3 <- feols(turnout ~ treat_post + obama_correction +
              unemp_rate + pct_black + log(median_inc) |
              county_fips + year,
            data    = df,
            cluster = ~county_fips)

# Model 4: TWFE + Obama correction + county-specific linear time trends
# Most demanding specification. Each county gets its own linear trend,
# so we identify off deviations from pre-existing county trajectories.
# Useful if there are remaining pre-trends beyond the Obama effect.
m4 <- feols(turnout ~ treat_post + obama_correction +
              unemp_rate + pct_black + log(median_inc) |
              county_fips + year + county_fips[year],
            data    = df,
            cluster = ~county_fips)

# Main results table
modelsummary(
  list(
    "Pooled OLS"         = m0,
    "TWFE"               = m1,
    "TWFE + Controls"    = m2,
    "Obama corrected"    = m3,
    "Obama + Co.Trends"  = m4
  ),
  stars    = TRUE,
  vcov     = list("classical", ~county_fips, ~county_fips,
                  ~county_fips, ~county_fips),
  coef_map = c(
    "treat_post"           = "Prison rate (std) x Post",
    "prison_rate_std:post" = "Prison rate (std) x Post",
    "obama_correction"     = "Black share x 2008 (Obama adj.)",
    "prison_rate_std"      = "Prison rate (pre-treatment level)",
    "post"                 = "Post 2018 indicator",
    "unemp_rate"           = "Unemployment rate",
    "pct_black"            = "% Black population",
    "log(median_inc)"      = "Log median income"
  ),
  gof_map = c("nobs", "r.squared", "FE: county_fips", "FE: year"),
  output  = "Final_Paper/tables/tab1_main_turnout.png",
  title   = "Effect of Amendment 4 on Voter Turnout: DiD Estimates"
)

# Coefficient plot
fig3 <- modelplot(
  list(
    "TWFE"              = m1,
    "TWFE + Controls"   = m2,
    "Obama corrected"   = m3,
    "Obama + Co.Trends" = m4
  ),
  coef_map = c("treat_post" = "Prison rate (std) x Post"),
  vcov     = ~county_fips
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title   = "DiD Estimates Across Specifications (Turnout)",
    x       = "Coefficient estimate (95% CI)",
    y       = NULL,
    caption = "All models: county + year FE. SEs clustered by county."
  ) +
  theme_minimal(base_size = 12)

ggsave("Final_Paper/figures/fig3_coefplot_main.pdf",
       plot = fig3, width = 8, height = 5)

# ============================================================================
# 4. EVENT STUDY: TESTING THE PARALLEL TRENDS ASSUMPTION
# ============================================================================

# Run FOUR event study specifications in sequence:
#   (1) Baseline: no correction -- shows the pre-trend violation clearly
#   (2) With controls: still has pre-trend (Obama not yet absorbed)
#   (3) Obama corrected: our preferred spec -- pre-trend should shrink
#   (4) County trends: most demanding -- pre-trend should shrink further
#
# The logic: if the 2008 pre-trend disappears after adding obama_correction
# but not before, that confirms the Obama mobilization effect was the source
# of the violation. If a pre-trend remains even after the correction, we have
# a deeper identification problem that must be acknowledged in the paper***.

# ----------------------------------------------------------------------------
# 4.1 Four Event Study Specifications

# Baseline: just county + year FE, no controls
m_event_base <- feols(
  turnout ~ i(event_time, prison_rate_std, ref = 0) | county_fips + year,
  data    = df,
  cluster = ~county_fips
)

# With controls, no Obama correction
m_event_ctrl <- feols(
  turnout ~ i(event_time, prison_rate_std, ref = 0) +
    unemp_rate + pct_black + log(median_inc) | county_fips + year,
  data    = df,
  cluster = ~county_fips
)

# With Obama correction (PREFERRED EVENT STUDY)
# If obama_correction is absorbing the 2008 spike, the event_time = -2
# coefficient should be near zero here.

m_event_obama <- feols(
  turnout ~ i(event_time, prison_rate_std, ref = 0) +
    obama_correction + unemp_rate + pct_black + log(median_inc) |
    county_fips + year,
  data    = df,
  cluster = ~county_fips
)

# With Obama correction + county-specific trends (most conservative)
m_event_trends <- feols(
  turnout ~ i(event_time, prison_rate_std, ref = 0) +
    obama_correction + unemp_rate + pct_black + log(median_inc) |
    county_fips + year + county_fips[year],
  data    = df,
  cluster = ~county_fips
)

# ----------------------------------------------------------------------------

# 4.2 Helper Function: Extract Event Study Coefficients

# Write this as a function so we can reuse it in Section 7 for registration.
# It safely extracts the year-specific terms and adds back the reference
# period row (event_time = 0, estimate = 0 by construction).

extract_event <- function(model, label) {
  tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "event_time")) %>%
    mutate(
      event_time = as.integer(str_extract(term, "-?\\d+")),
      model      = label
    ) %>%
    bind_rows(
      tibble(event_time = 0L, estimate = 0,
             conf.low = 0, conf.high = 0, model = label)
    )
}

# Combine all four specifications for the plot
event_all <- bind_rows(
  extract_event(m_event_base,   "1. Baseline"),
  extract_event(m_event_ctrl,   "2. With controls"),
  extract_event(m_event_obama,  "3. Obama corrected"),
  extract_event(m_event_trends, "4. Obama + county trends")
)

# ----------------------------------------------------------------------------

# 4.3 Print Pre-Treatment Coefficients

# This is the key diagnostic. Want the 2008 and 2012 coefficients to be
# near zero and statistically insignificant (CI crosses zero) in specs 3 and 4.

cat("\n--- Pre-treatment event study coefficients ---\n")
cat("(Should approach zero after Obama correction)\n\n")

for (spec in c("1. Baseline", "2. With controls",
               "3. Obama corrected", "4. Obama + county trends")) {
  cat(spec, ":\n", sep = "")
  print(event_all %>%
          filter(model == spec, event_time < 0) %>%
          select(event_time, estimate, conf.low, conf.high) %>%
          mutate(across(where(is.numeric), ~round(.x, 5))))
  cat("\n")
}

# ----------------------------------------------------------------------------
# 4.4 Event Study Plot: Baseline vs. Obama Corrected

# Plot 1: All four specs (shows the progression from problem to fix)
fig4a <- ggplot(event_all,
                aes(x = event_time, y = estimate,
                    color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.5, linetype = "dotted",
             color = "#d6604d", linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  annotate("text", x = 0.6,
           y = max(event_all$conf.high, na.rm = TRUE) * 0.90,
           label = "Amendment 4\npasses", color = "#d6604d",
           size = 3.0, hjust = 0) +
  scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("2008\n(Obama)", "2012", "2016\n(ref)", "2020", "2024")
  ) +
  scale_color_manual(values = c(
    "1. Baseline"             = "#cccccc",
    "2. With controls"        = "#4393c3",
    "3. Obama corrected"      = "#d6604d",
    "4. Obama + county trends"= "#762a83"
  )) +
  scale_fill_manual(values = c(
    "1. Baseline"             = "#cccccc",
    "2. With controls"        = "#4393c3",
    "3. Obama corrected"      = "#d6604d",
    "4. Obama + county trends"= "#762a83"
  )) +
  labs(
    title    = "Event Study: Parallel Trends Before and After Obama Correction",
    subtitle = "Pre-treatment coefficients should approach zero in specs 3 and 4",
    x        = NULL,
    y        = "Coefficient (prison rate x year)",
    color    = NULL, fill = NULL,
    caption  = "Reference year: 2016. Shaded bands = 95% CI. SEs clustered by county."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10))

ggsave("Final_Paper/figures/fig4a_event_study_all.pdf",
       plot = fig4a, width = 12, height = 6)

# Plot 2: Just baseline vs. Obama corrected (cleaner for the paper)
event_two <- event_all %>%
  filter(model %in% c("1. Baseline", "3. Obama corrected")) %>%
  mutate(model = recode(model,
                        "1. Baseline"        = "Baseline (no correction)",
                        "3. Obama corrected" = "Obama 2008 corrected"))

fig4b <- ggplot(event_two,
                aes(x = event_time, y = estimate,
                    color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.5, linetype = "dotted",
             color = "#d6604d", linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  annotate("text", x = 0.6,
           y = max(event_two$conf.high, na.rm = TRUE) * 0.90,
           label = "Amendment 4\npasses", color = "#d6604d",
           size = 3.2, hjust = 0) +
  scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("2008\n(Obama)", "2012", "2016\n(ref)", "2020", "2024")
  ) +
  scale_color_manual(values = c(
    "Baseline (no correction)" = "#4393c3",
    "Obama 2008 corrected"     = "#d6604d"
  )) +
  scale_fill_manual(values = c(
    "Baseline (no correction)" = "#4393c3",
    "Obama 2008 corrected"     = "#d6604d"
  )) +
  labs(
    title    = "Event Study: Effect of Obama 2008 Correction on Pre-Trends",
    subtitle = "If correction works, red line pre-treatment coefficients should be near zero",
    x        = NULL,
    y        = "Coefficient (prison rate x year)",
    color    = NULL, fill = NULL,
    caption  = "Reference year: 2016. Shaded bands = 95% CI. SEs clustered by county."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Final_Paper/figures/fig4b_event_study_correction.pdf",
       plot = fig4b, width = 10, height = 6)

# ============================================================================
# 5. HETEROGENEITY: DOES SB 7066 ATTENUATE THE EFFECT IN POOR COUNTIES?
# ============================================================================

# SB 7066 requires returning citizens to pay all court fines/fees before
# re-registering. This is most binding in low-income counties where returning
# citizens cannot afford to pay. If SB 7066 is the mechanism, the Amendment 4
# effect should be LARGER in wealthier counties (positive interaction sign).

m_het <- feols(
  turnout ~ treat_post + treat_post:log(median_inc) +
    obama_correction + unemp_rate + pct_black + log(median_inc) |
    county_fips + year,
  data    = df,
  cluster = ~county_fips
)

modelsummary(
  list(
    "TWFE (main)"        = m1,
    "Obama corrected"    = m3,
    "Income interaction" = m_het
  ),
  stars    = TRUE,
  vcov     = ~county_fips,
  coef_map = c(
    "treat_post"                 = "Prison rate x Post",
    "treat_post:log(median_inc)" = "Prison rate x Post x Log income",
    "obama_correction"           = "Black share x 2008 (Obama adj.)",
    "unemp_rate"                 = "Unemployment rate",
    "pct_black"                  = "% Black population",
    "log(median_inc)"            = "Log median income"
  ),
  gof_map = c("nobs", "r.squared", "FE: county_fips", "FE: year"),
  output  = "Final_Paper/tables/tab2_heterogeneity.png",
  title   = "Heterogeneity: Does SB 7066 Attenuate the Effect in Low-Income Counties?"
)

# ============================================================================
# 6. ROBUSTNESS CHECKS
# ============================================================================

# 6.1 Binary treatment (above/below median prison rate)
rob1 <- feols(turnout ~ high_treat_post | county_fips + year,
              data    = df,
              cluster = ~county_fips)

# 6.2 Exclude Miami-Dade (largest, most atypical county)
rob2 <- feols(turnout ~ treat_post + obama_correction +
                unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(county_name != "Miami-Dade"),
              cluster = ~county_fips)

# 6.3 Drop 2020 (COVID mail-in voting confound)
# The 2020 election had a large COVID-related expansion of mail-in voting,
# which may have boosted turnout in ways unrelated to Amendment 4.
# If results hold without 2020, COVID is less of a concern.
rob3 <- feols(turnout ~ treat_post + obama_correction +
                unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(year != 2020),
              cluster = ~county_fips)

# 6.4 Drop 2024 (unusual 2024 candidate landscape)
rob4 <- feols(turnout ~ treat_post + obama_correction +
                unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(year != 2024),
              cluster = ~county_fips)

# 6.5 Drop 2008 entirely (most conservative approach to the Obama problem)
# Instead of absorbing the Obama effect with a control variable, we just
# remove 2008 from the sample. If this gives a similar result to the
# Obama-correction model (m3), that confirms the correction is working.
rob5 <- feols(turnout ~ treat_post + unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(year != 2008),
              cluster = ~county_fips)

# 6.6 Placebo test: fake treatment in 2012 (pre-treatment data only)
# We restrict to 2008, 2012, 2016 and pretend Amendment 4 passed before 2012.
# There was no real policy change then, so we expect NO effect.
# A significant result would mean our design is picking up pre-existing
# trends rather than the true Amendment 4 effect.
df_placebo <- df %>%
  filter(year %in% c(2008, 2012, 2016)) %>%
  mutate(
    fake_post  = as.integer(year >= 2012),
    fake_treat = prison_rate_std * fake_post
  )

rob6 <- feols(turnout ~ fake_treat + obama_correction +
                unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df_placebo,
              cluster = ~county_fips)

# 6.7 Only 2016 vs. 2020
# Narrow window: SB 7066 passed June 2019, leaving almost no time
# for returning citizens to re-register before November 2020.
# Expect a near-zero or small effect here.
rob7 <- feols(turnout ~ treat_post + unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(year %in% c(2016, 2020)),
              cluster = ~county_fips)

# 6.8 Only 2016 vs. 2024
# Wider window: returning citizens had more time by 2024 to meet
# SB 7066 requirements. If effect is larger here than in 6.7,
# that supports the SB 7066 story.
rob8 <- feols(turnout ~ treat_post + unemp_rate + pct_black + log(median_inc) |
                county_fips + year,
              data    = df %>% filter(year %in% c(2016, 2024)),
              cluster = ~county_fips)

# Robustness table
modelsummary(
  list(
    "Obama corrected"  = m3,
    "Binary treat."    = rob1,
    "No Miami-Dade"    = rob2,
    "No 2020 (COVID)"  = rob3,
    "No 2024"          = rob4,
    "Drop 2008"        = rob5,
    "Placebo (2012)"   = rob6,
    "2016 vs 2020"     = rob7,
    "2016 vs 2024"     = rob8
  ),
  stars    = TRUE,
  vcov     = ~county_fips,
  coef_map = c(
    "treat_post"      = "Prison rate x Post",
    "high_treat_post" = "High-treat. dummy x Post",
    "fake_treat"      = "Prison rate x Fake post (2012)"
  ),
  gof_map = c("nobs", "r.squared", "FE: county_fips", "FE: year"),
  output  = "Final_Paper/tables/tab3_robustness.png",
  title   = "Robustness Checks: Voter Turnout"
)

# Coefficient plot for robustness
fig5 <- modelplot(
  list(
    "Obama corrected"  = m3,
    "No Miami-Dade"    = rob2,
    "No 2020 (COVID)"  = rob3,
    "No 2024"          = rob4,
    "Drop 2008"        = rob5,
    "Placebo (2012)"   = rob6,
    "2016 vs 2020"     = rob7,
    "2016 vs 2024"     = rob8
  ),
  coef_map = c(
    "treat_post" = "Prison rate x Post",
    "fake_treat" = "Prison rate x Fake post (2012)"
  ),
  vcov = ~county_fips
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title   = "Robustness: ATT Estimates Across Specifications",
    x       = "Coefficient estimate (95% CI)",
    y       = NULL,
    caption = "All models: county + year FE. SEs clustered by county."
  ) +
  theme_minimal(base_size = 12)

ggsave("Final_Paper/figures/fig5_robustness.pdf",
       plot = fig5, width = 9, height = 6)

# Clustering robustness: show the main coefficient is stable across
# different standard error assumptions
m1_twoway <- feols(turnout ~ treat_post | county_fips + year,
                   data    = df,
                   cluster = ~county_fips + region)

cat("\n--- Clustering robustness (same model, different SE choices) ---\n")
print(bind_rows(
  tidy(m1,        conf.int = TRUE) %>% mutate(SE = "By county"),
  tidy(m1_twoway, conf.int = TRUE) %>% mutate(SE = "By county + region")
) %>%
  filter(term == "treat_post") %>%
  select(SE, estimate, std.error, p.value, conf.low, conf.high) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))))

# ============================================================================
# 7. VOTER REGISTRATION OUTCOME
# ============================================================================

# If Amendment 4 worked, registration should rise in high-treatment counties.
# If registration rises MORE than turnout, it suggests SB 7066 friction:
# returning citizens are registering but then cannot actually vote
# (challenged registration, administrative hurdles, low political integration).
# A gap between registration and turnout effects is the empirical signature
# of SB 7066 limiting real participation.

# I use Florida DOS Book Closing reports (By Party) for 2016, 2020, and 2024.
# These are the years available as machine-readable Excel files.
# 2008 and 2012 book closing data doesn't exists on their website,
# so the registration panel uses three years: one pre-treatment (2016) and
# two post-treatment (2020, 2024). This is sufficient for the key comparison
# between registration and turnout effects.

# 7.0 Download and Build Voter Registration Data
# ============================================================================

# install.packages
library(readxl)

# 2020 and 2024 have direct Excel links from the Book Closing page.

reg_links <- list(
  "2016" = list(
    url = "https://dos.fl.gov/media/697211/2016general_party.xlsx",
    path = "Final_Paper/Data/.xlxs"
  ),
  "2020" = list(
    url  = "https://dos.fl.gov/media/703608/1-party-by-county.xlsx",
    path = "Final_Paper/Data/reg_2020.xlsx"
  ),
  "2024" = list(
    url  = "https://dos.fl.gov/media/708493/1-party-by-county.xlsx",
    path = "Final_Paper/Data/reg_2024.xlsx"
  )
)


for (yr in names(reg_links)) {
  info <- reg_links[[yr]]
  if (!file.exists(info$path)) {
    cat("Downloading", yr, "registration data...\n")
    tryCatch(
      download.file(info$url, info$path, mode = "wb", quiet = TRUE),
      error = function(e) cat("ERROR downloading", yr, ":", e$message, "\n")
    )
    if (file.exists(info$path) && file.size(info$path) > 5000) {
      cat(" ", yr, "downloaded successfully.\n")
    } else {
      cat("  Download may have failed for", yr,
          "-- download manually from:\n",
          "  dos.fl.gov/elections/data-statistics/voter-registration-statistics/",
          "bookclosing/bookclosing-reports-regular/\n",
          "  Save as: ", info$path, "\n")
    }
  } else {
    cat(yr, "already saved, skipping download.\n")
  }
}


# ----------------------------------------------------------------------------
# 7.1  Read and Clean each file

# The By Party report has one row per county with columns for each party
# and a Total column at the end. We need county name + Total only.

read_bookclosing <- function(filepath, election_year) {
  
  cat("\nReading", election_year, "from", basename(filepath), "\n")
  
  # Step 1: Read entire file without headers to find the real header row
  raw_all <- tryCatch(
    suppressMessages(read_excel(filepath, sheet = 1, col_names = FALSE)),
    error = function(e) stop("Could not open ", filepath, ": ", e$message)
  )
  
  cat("  Total rows in file (including headers):", nrow(raw_all), "\n")
  
  # Step 2: Find the row containing "county"
  has_county <- apply(raw_all, 1, function(row) {
    any(str_detect(tolower(as.character(row)), "county"), na.rm = TRUE)
  })
  
  header_row <- which(has_county)[1]
  
  if (is.na(header_row)) {
    cat("  Could not find a 'county' header row. First 10 rows:\n")
    print(head(raw_all, 10))
    stop("Cannot identify header row in ", filepath)
  }
  
  cat("  Header row found at row:", header_row, "\n")
  
  # Step 3: Re-read skipping everything above the real header
  raw <- suppressMessages(
    read_excel(filepath, sheet = 1, skip = header_row - 1)
  )
  
  cat("  Columns:", paste(names(raw), collapse = " | "), "\n")
  cat("  Data rows:", nrow(raw), "\n")
  
  # Step 4: Identify county column
  county_col <- names(raw)[str_detect(tolower(names(raw)), "county")]
  if (length(county_col) == 0) {
    text_cols  <- names(raw)[sapply(raw, function(x) is.character(x) | is.factor(x))]
    county_col <- text_cols[1]
  }
  county_col <- county_col[1]
  
  # Step 5: Identify total column
  total_col <- names(raw)[str_detect(tolower(names(raw)), "total")]
  if (length(total_col) == 0) {
    num_cols  <- names(raw)[sapply(raw, is.numeric)]
    total_col <- tail(num_cols, 1)
  }
  total_col <- total_col[1]
  
  cat("  County column:", county_col, "\n")
  cat("  Total column: ", total_col,  "\n")
  
  if (is.na(county_col) || is.na(total_col)) {
    print(names(raw))
    stop("Could not identify county or total column in ", filepath)
  }
  
  # Step 6: Extract and clean
  result <- raw %>%
    select(county_name  = all_of(county_col),
           active_total = all_of(total_col)) %>%
    mutate(
      county_name  = str_to_title(str_trim(as.character(county_name))),
      active_total = suppressWarnings(
        as.integer(str_remove_all(as.character(active_total), "[,$]"))
      ),
      year = as.integer(election_year)
    ) %>%
    filter(
      !is.na(county_name),
      !is.na(active_total),
      active_total > 100,
      str_detect(county_name, "^[A-Za-z]"),
      !str_detect(tolower(county_name),
                  "^total$|^florida$|^state$|^grand total$|^statewide$|^sum$")
    )
  
  cat("  Counties extracted:", nrow(result), "(expect 67)\n")
  
  if (nrow(result) != 67) {
    cat("  WARNING: Got", nrow(result), "rows. Preview:\n")
    print(head(result, 10))
  }
  
  return(result)
}

test_2016 <- read_bookclosing("Final_Paper/Data/reg_2016.xlsx", 2016)
test_2020 <- read_bookclosing("Final_Paper/Data/reg_2020.xlsx", 2020)
test_2024 <- read_bookclosing("Final_Paper/Data/reg_2024.xlsx", 2024)

reg_panel <- bind_rows(test_2016, test_2020, test_2024)
cat("reg_panel rows:", nrow(reg_panel), "(should be 201)\n")
#-------------------------------------------------------------------------------

#7.2 Check County name matching before merging

mismatched <- setdiff(unique(reg_panel$county_name), unique(df$county_name))
if (length(mismatched) > 0) {
  cat("Mismatched county names:\n")
  print(mismatched)
} else {
  cat("All county names match.\n")
}

#-------------------------------------------------------------------------------

#7.3 Merge into main panel

# Filter df to only the three years with registration data,
# then merge and compute registration rate
df_reg <- df %>%
  filter(year %in% c(2016, 2020, 2024)) %>%
  left_join(reg_panel, by = c("county_name", "year")) %>%
  mutate(
    # Registration rate = active registrations / citizen voting-age population
    reg_rate = active_total / cvap
  )

cat("\ndf_reg rows:", nrow(df_reg),
    "(should be 201: 67 counties x 3 years)\n")
cat("\nRegistration rate summary:\n")
print(summary(df_reg$reg_rate))
cat("Missing values:", sum(is.na(df_reg$reg_rate)), "\n")

# Flag any specific county-years with missing data
if (sum(is.na(df_reg$reg_rate)) > 0) {
  cat("County-years with missing registration:\n")
  print(df_reg %>%
          filter(is.na(reg_rate)) %>%
          select(county_name, year))
}

#-------------------------------------------------------------------------------

# 7.4 Registration Models

# Same structure as the turnout models in Section 3.
# Note: with only 3 time periods, the county trends model (rm4) is very
# demanding and may be unstable. We report it but treat rm3 as preferred.

# TWFE baseline
rm1 <- feols(reg_rate ~ treat_post | county_fips + year,
             data    = df_reg,
             cluster = ~county_fips)

# + time-varying controls
rm2 <- feols(reg_rate ~ treat_post + unemp_rate + pct_black + log(median_inc) |
               county_fips + year,
             data    = df_reg,
             cluster = ~county_fips)

# + Obama correction (preferred)
# Note: with only 2016, 2020, 2024, there is no 2008 in df_reg so
# obama_correction will be zero for all rows. We still include it for
# consistency with the turnout models, but it will not affect results.
rm3 <- feols(reg_rate ~ treat_post + obama_correction +
               unemp_rate + pct_black + log(median_inc) |
               county_fips + year,
             data    = df_reg,
             cluster = ~county_fips)

# + county-specific trends (most demanding)
rm4 <- feols(reg_rate ~ treat_post + unemp_rate + pct_black + log(median_inc) |
               county_fips + year + county_fips[year],
             data    = df_reg,
             cluster = ~county_fips)

#-------------------------------------------------------------------------------

# 7.5 Side-by-side Table: Turnout vs. Registration

# The turnout models use all 5 years; registration uses 3.
# For the side-by-side comparison, we also re-run the turnout models
# restricted to 2016-2024 so the samples are comparable.

m1_sub <- feols(turnout ~ treat_post | county_fips + year,
                data    = df %>% filter(year %in% c(2016, 2020, 2024)),
                cluster = ~county_fips)

m3_sub <- feols(turnout ~ treat_post + unemp_rate + pct_black + log(median_inc) |
                  county_fips + year,
                data    = df %>% filter(year %in% c(2016, 2020, 2024)),
                cluster = ~county_fips)

modelsummary(
  list(
    "Turnout: TWFE"    = m1_sub,
    "Turnout: Controls"= m3_sub,
    "Reg: TWFE"        = rm1,
    "Reg: Controls"    = rm2,
    "Reg: Obama adj."  = rm3
  ),
  stars    = TRUE,
  vcov     = ~county_fips,
  coef_map = c(
    "treat_post"       = "Prison rate x Post",
    "obama_correction" = "Black share x 2008 (Obama adj.)",
    "unemp_rate"       = "Unemployment rate",
    "pct_black"        = "% Black population",
    "log(median_inc)"  = "Log median income"
  ),
  gof_map  = c("nobs", "r.squared", "FE: county_fips", "FE: year"),
  output   = "Final_Paper/tables/tab4_turnout_vs_reg.png",
  title    = "Amendment 4: Voter Turnout vs. Registration Rate (2016-2024)"
)

# Print headline numbers to console
cat("\n--- Key comparison: treat_post coefficient ---\n")
cat("Turnout (2016-2024 sample):", round(coef(m1_sub)["treat_post"], 4), "\n")
cat("Registration:              ", round(coef(rm1)["treat_post"],   4), "\n")
cat("\nIf |registration| > |turnout|, returning citizens registered\n")
cat("but faced barriers to actually voting (SB 7066 friction).\n")


#-------------------------------------------------------------------------------

# 7.6 Event Study: Registration vs. Turnout


# Event study for registration (2016, 2020, 2024 only)
# event_time: 2016 = 0 (ref), 2020 = 1, 2024 = 2
# Note: no pre-treatment event times available for registration
# (2008 and 2012 data not available), so this event study only shows
# post-treatment estimates relative to 2016 baseline.

m_event_reg <- feols(
  reg_rate ~ i(event_time, prison_rate_std, ref = 0) +
    unemp_rate + pct_black + log(median_inc) |
    county_fips + year,
  data    = df_reg,
  cluster = ~county_fips
)

# Turnout event study restricted to same 3 years for fair comparison
m_event_turnout_sub <- feols(
  turnout ~ i(event_time, prison_rate_std, ref = 0) +
    unemp_rate + pct_black + log(median_inc) |
    county_fips + year,
  data    = df %>% filter(year %in% c(2016, 2020, 2024)),
  cluster = ~county_fips
)

ev_both <- bind_rows(
  extract_event(m_event_turnout_sub, "Turnout"),
  extract_event(m_event_reg,         "Registration")
)

fig6 <- ggplot(ev_both,
               aes(x = event_time, y = estimate,
                   color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.5, linetype = "dotted",
             color = "#d6604d", linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c("2016\n(ref)", "2020", "2024")
  ) +
  scale_color_manual(values = c("Turnout"       = "#2166ac",
                                "Registration"  = "#b35806")) +
  scale_fill_manual(values  = c("Turnout"       = "#2166ac",
                                "Registration"  = "#b35806")) +
  labs(
    title    = "Event Study: Turnout vs. Registration After Amendment 4",
    subtitle = "Gap between curves = returning citizens registered but could not vote (SB 7066)",
    x        = NULL,
    y        = "Coefficient (prison rate x year)",
    color    = NULL, fill = NULL,
    caption  = paste("Both: county + year FE, controls. SEs clustered by county.",
                     "Registration data available from 2016 onward only.")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Final_Paper/figures/fig6_turnout_vs_reg.pdf",
       plot = fig6, width = 10, height = 6)

#-------------------------------------------------------------------------------

# 7.7 Descriptive Trends: Turnout and Registration

trends_reg <- df_reg %>%
  mutate(group = ifelse(high_treat == 1,
                        "High prison rate",
                        "Low prison rate")) %>%
  group_by(year, group) %>%
  summarise(
    Turnout      = mean(turnout,  na.rm = TRUE),
    Registration = mean(reg_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Turnout, Registration),
               names_to  = "outcome",
               values_to = "rate")

fig7 <- ggplot(trends_reg, aes(x = year, y = rate, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2019, y = max(trends_reg$rate, na.rm = TRUE) * 0.97,
           label = "Amendment 4\npasses", hjust = 0,
           size = 3.2, color = "gray30") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(2016, 2020, 2024)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("High prison rate" = "#d6604d",
                                "Low prison rate"  = "#4393c3")) +
  labs(
    x       = NULL,
    y       = NULL,
    color   = NULL,
    title   = "Turnout and Registration Trends by Treatment Group (2016-2024)",
    caption = "Dashed line = Amendment 4 passes (Nov 2018). Registration: FL DOS Book Closing reports."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Final_Paper/figures/fig7_trends_both.pdf",
       plot = fig7, width = 12, height = 5)

cat("\nSection 7 complete.\n")
cat("Table saved to: Final_Paper/tables/tab4_turnout_vs_reg.png\n")
cat("Figures saved to: Final_Paper/figures/\n")

# end of registration section

# ============================================================================
# 8. SUMMARY OF KEY RESULTS
# ============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("RESULTS SUMMARY\n")
cat(strrep("=", 65), "\n\n", sep = "")

#-------------------------------------------------------------------------------
# 8.1 Manual DiD

cat("--- Manual 2x2 DiD (binary, 2016 vs 2020) ---\n")
cat("DiD estimate:", round(did_manual, 4), "\n")
cat("Interpretation: negative = high-prison counties gained LESS turnout\n")
cat("than low-prison counties between 2016 and 2020.\n\n")

#-------------------------------------------------------------------------------
# 8.2 Main Turnout Models

cat("--- Turnout: TWFE, no correction (m1) ---\n")
print(tidy(m1, conf.int = TRUE) %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n--- Turnout: Obama corrected, preferred spec (m3) ---\n")
print(tidy(m3, conf.int = TRUE) %>%
        filter(term %in% c("treat_post", "obama_correction")) %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n--- Turnout: Obama corrected + county trends (m4) ---\n")
print(tidy(m4, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

#-------------------------------------------------------------------------------
# 8.3 Registration vs. Turnout Comparison (SB 7066 test)

cat("\n--- Registration vs. Turnout: SB 7066 Friction Test ---\n")
cat("(Both restricted to 2016, 2020, 2024 for comparable sample)\n\n")

# Turnout (restricted sample)
cat("Turnout TWFE (m1_sub):\n")
print(tidy(m1_sub, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\nRegistration TWFE (rm1):\n")
print(tidy(rm1, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\nRegistration + controls (rm2):\n")
print(tidy(rm2, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

# The key SB 7066 gap: is |registration effect| > |turnout effect|?
turnout_coef  <- coef(m1_sub)["treat_post"]
reg_coef      <- coef(rm1)["treat_post"]
gap           <- reg_coef - turnout_coef

cat("\n--- SB 7066 Gap ---\n")
cat("Turnout coefficient:     ", round(turnout_coef, 4), "\n")
cat("Registration coefficient:", round(reg_coef,     4), "\n")
cat("Gap (reg minus turnout): ", round(gap,           4), "\n")
if (gap > 0) {
  cat("Registration rose MORE than turnout -> consistent with SB 7066 friction:\n")
  cat("returning citizens registered but faced barriers to actually voting.\n")
} else if (gap < 0) {
  cat("Turnout rose more than registration (or both fell similarly).\n")
  cat("This suggests the effect, if any, was not driven by new registration.\n")
} else {
  cat("Registration and turnout moved identically.\n")
}

#-------------------------------------------------------------------------------
# 8.4 Heterogeneity

cat("\n--- SB 7066 Income Mechanism (m_het) ---\n")
print(tidy(m_het, conf.int = TRUE) %>%
        filter(str_detect(term, "treat_post")) %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

#-------------------------------------------------------------------------------
# 8.5 Robustness

cat("\n--- Placebo test / rob6 (should be near zero) ---\n")
print(tidy(rob6, conf.int = TRUE) %>%
        filter(term == "fake_treat") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n--- Drop 2008 robustness / rob5 ---\n")
print(tidy(rob5, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n--- 2016 vs 2020 only / rob7 ---\n")
cat("(SB 7066 passed June 2019 -- little time to re-register before Nov 2020)\n")
print(tidy(rob7, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

cat("\n--- 2016 vs 2024 only / rob8 ---\n")
cat("(More time by 2024 to meet SB 7066 fines/fees requirement)\n")
print(tidy(rob8, conf.int = TRUE) %>%
        filter(term == "treat_post") %>%
        select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

#-------------------------------------------------------------------------------
# 8.6 Parallel Trends Check

cat("\n--- Pre-treatment event study (Obama corrected spec) ---\n")
cat("These should be near zero for parallel trends to hold:\n")
print(event_all %>%
        filter(model == "3. Obama corrected", event_time < 0) %>%
        select(event_time, estimate, conf.low, conf.high) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))))

#-------------------------------------------------------------------------------
# 8.7 Overall interpretation

cat("\n", strrep("-", 65), "\n", sep = "")
cat("OVERALL INTERPRETATION\n")
cat(strrep("-", 65), "\n\n", sep = "")
cat("Main TWFE (m3):     ", round(coef(m3)["treat_post"], 4),
    "-- negative, significant\n")
cat("County trends (m4): ", round(coef(m4)["treat_post"], 4),
    "-- positive, significant (sign flip after absorbing pre-trends)\n")
cat("Placebo (rob6):     ", round(coef(rob6)["fake_treat"], 4),
    "-- significant (pre-trend problem, caution on causal claims)\n")
cat("2020 only (rob7):   ", round(coef(rob7)["treat_post"], 4),
    "-- near zero (SB 7066 + tight re-registration window)\n")
cat("2024 only (rob8):   ", round(coef(rob8)["treat_post"], 4),
    "-- check if larger than 2020 (supports SB 7066 story)\n")
cat("SB 7066 gap:        ", round(gap, 4),
    "-- check sign (positive = reg > turnout = friction)\n")
cat("\nConclusion: No clear differential turnout gains in high-incarceration\n")
cat("counties after Amendment 4. Pre-existing trends and SB 7066 fines/fees\n")
cat("requirement likely explain the limited effect on political participation.\n")

cat("\n", strrep("=", 65), "\n", sep = "")
cat("Figures saved to: Final_Paper/figures/\n")
cat("Tables  saved to: Final_Paper/tables/\n")
cat(strrep("=", 65), "\n", sep = "")
