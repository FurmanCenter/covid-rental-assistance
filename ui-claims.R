library(tidyverse)

UI_TAKEUP_RATE <- 0.70


ui_claims1 <- read_csv("data/nys_ui-claims_industry_2020-05-23.csv", col_types = "cn") %>% 
  filter(!ind_group_ui %in% c("Total (Including Out-of-State Residents)")) %>% 
  group_by(ind_group_ui) %>% 
  summarise(claims = sum(oty_change_claims_mar14_may23)) %>% 
  ungroup()

# There are a lot of unclassified claims. We'll pull these out and redistribute
unclassified_claims <- ui_claims1 %>% filter(ind_group_ui == "Unclassified") %>% pull()

ui_claims2 <- ui_claims1 %>% 
  filter(ind_group_ui != "Unclassified") %>% 
  # The % of claims for this mgmt of companies/enterprises are so high (>100%)
  # that we decided to group it with this larger category
  mutate(
    ind_group_ui = recode(
      ind_group_ui,
      "Management of Companies and Enterprises" = "Professional, Scientific and Technical Services"
    )
  ) %>%
  group_by(ind_group_ui) %>% 
  summarise(claims = sum(claims)) %>% 
  # Incorporate the unclassified the claims across all industries following the
  # existing distribution
  mutate(claims = claims + (unclassified_claims * (claims / sum(claims))))


acs_ind_people <- read_csv("data/ipums_acs-2018-1yr_ny.csv.gz") %>% 
  rename_all(str_to_lower) %>% 
  mutate(
    ind_group_ui = case_when(
      between(ind, 8660, 8690) ~ "Accommodation and Food Services",
      between(ind, 7580, 7790) ~ "Administrative and Support Services",
      between(ind, 0170, 0290) ~ "Agriculture, Forestry, Fishing and Hunting",
      between(ind, 8560, 8590) ~ "Arts, Entertainment and Recreation",
      ind == 0770              ~ "Construction/Utilities",
      between(ind, 0570, 0690) ~ "Construction/Utilities",
      between(ind, 7860, 7890) ~ "Educational Services",
      between(ind, 6870, 6992) ~ "Finance and Insurance",
      between(ind, 7970, 8470) ~ "Health Care and Social Assistance",
      between(ind, 6470, 6780) ~ "Information",
      between(ind, 1070, 3990) ~ "Manufacturing",
      between(ind, 0370, 0490) ~ "Mining",
      between(ind, 8770, 9290) ~ "Other Services",
      between(ind, 7270, 7490) ~ "Professional, Scientific and Technical Services",
      # this "Management of Companies and Enterprises" is 7570, but the estimated % of
      # job loss is so high (>100%) that we are grouping it with the above category
      ind == 7570              ~ "Professional, Scientific and Technical Services", 
      between(ind, 9370, 9590) ~ "Public Administration (Including Government)",
      between(ind, 7071, 7190) ~ "Real Estate and Rental and Leasing",
      between(ind, 4670, 5790) ~ "Retail Trade",
      between(ind, 6070, 6390) ~ "Transportation and Warehousing",
      between(ind, 4070, 4590) ~ "Wholesale Trade"
    )
  ) %>% 
  filter(
    !is.na(ind_group_ui), # excluded IND codes are only Military
    !incwage %in% c(999999, 999998, 0) # keep only people with wages
  ) %>% 
  group_by(ind_group_ui) %>% 
  summarise(persons = sum(perwt)) %>% 
  ungroup()


ui_ind_job_loss <- acs_ind_people %>% 
  full_join(ui_claims2, by = "ind_group_ui") %>% 
  mutate(
    # Using the UI takeup rate, we scale up claims to estimated jobs lost
    job_loss_pct = (claims * (1 / UI_TAKEUP_RATE)) / persons
  ) 

write_csv(ui_ind_job_loss, "data/nys-ui-ind-job-loss.csv")
