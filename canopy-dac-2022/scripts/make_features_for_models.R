## Prepping data for modeling
library(dplyr)
library(here)
library(readr)

load(here("data/complete_canopy_2022.RData"))


features_df = clean_data_full %>%
  select(
    # school characteristics
    school_id,
    grades_elementary, grades_middle, grades_high, locale,
    school_descriptor_district, school_descriptor_charter, school_descriptor_independent,
    
    # demographics
    diversity_staff = confidential_teaching_staff_diversity,
    diversity_leads = confidential_leadership_team_diversity,
    black_percent, hispanic_percent, white_percent,
    ell_percent, frpl_percent, swd_percent,
    n_students = nces_total_enrollment
    
  ) %>%
  mutate(
    bipoc_percent = 1 - white_percent,
    across(c(ends_with("percent"), n_students), scale, .names = "{.col}_scaled"),
    across(starts_with("grades"), as.integer),
    across(starts_with("diversity"), factor,
           levels = c("0 - 24% people of color", "25 - 49% people of color", 
             "50 - 74% people of color", "75 - 100% people of color"),
           labels = c("<25% POC", "25-49% POC", "50-74% POC", ">74% POC")
          )
  ) %>%
  select(-ends_with("percent"), -n_students)


write_rds(features_df, file = here("data/features_for_models.rds"))
