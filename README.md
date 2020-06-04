Covid-19 Rental Assistance
================

Maxwell Austensen ([@austensen](https://github.com/austensen))

The analysis presented here appears in this blog post:

[*Understanding the Potential Magnitude of Rent Shortfalls in New York Due to COVID*](https://furmancenter.org/thestoop/entry/understanding-the-potential-magnitude-of-rent-shortfalls-in-new-york)

``` r
# Install required packages 

# pkgs <- c(
#   "tidyverse",
#   "hrbrthemes",
#   "srvyr",
#   "knitr",
#   "rmarkdown",
#   "gt",
#   "furrr",
#   "tictoc"
# )

# install.packages(pkgs)
```

``` r
library(tidyverse) # general data manipulation and graphing
library(scales) # number formatting
library(srvyr) # survey functions
library(gt) # making tables
library(furrr) # parallel processing
library(tictoc) # timing operations

# Load custom functions to help with plotting
source("R/utils.R")

# No scientific notation in outputs
options(scipen = 999)

# To deal with the random assignment of job loss and UI takeup for individuals,
# we need to run generate the results multiple times with different random seeds
# and then average the results. To speed this up we use {furrr} to run this in
# parrallel across multiple cores.
plan(multiprocess)
```

### Repeated Iterations of Analysis

``` r
# Number of iterations to run when compiling results
ITERATIONS <- 100
```

In our analysis we randomly assign individuals in the data to job loss
status and UI recipiency with probabilities based on the
industry-specific job loss rates and the UI benefit recipiency rate. To
ensure that our final results are not unduly influenced by random
variation in those assignment, we repeat the analysis 100 times and
average the results.

### NYS Unemployment Insurance Claims

We use New York State initial unemployment insurance claims data to
estimate the percent of workers that have lost their job due to covid by
industry category. We accesses these data form the [New York State
Department of
Labor](https://labor.ny.gov/stats/weekly-ui-claims-report.shtm) and from
[this PDF
file](https://labor.ny.gov/stats/PDFs/Research-Notes-Initial-Claims-WE-5232020.pdf#page=4)
we [transcribed the data](/data/nys_ui-claims_industry_2020-05-23.csv)
for the “Over-the-Year Change in Initial Claims by Industry - Cumulative
Weeks Ending March 14, 21, 28, April 4, 11, 18, 25, May 2, 9, 16, 23”.
They note that “These represent the cumulative number of initial claims
since they started increasing as a result of the COVID-19 pandemic.”

In the the file [`ui-claims.R`](ui-claims.R) we take these count of
claims and proportionally redistribute the sizable number of
“unclassified” claims across all the industries. Then we

``` r
# Create the estimated job loss rates from UI claims
source("ui-claims.R")

ui_ind_job_loss <- read_csv("data/nys-ui-ind-job-loss.csv", col_types = "cddd")

gt(ui_ind_job_loss) %>% 
  cols_label(
    ind_group_ui = "Industry Group", 
    persons = "Wage Earners (ACS)", 
    claims = "Initial Claims (NYS UI)", 
    job_loss_pct = "Job Loss Percent"
  ) %>% 
  fmt_number(columns = vars(persons, claims), decimal = 0) %>% 
  fmt_percent(columns = vars(job_loss_pct), decimal = 1)
```

<!--html_preserve-->

<div id="orhrecjqhd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Industry
Group

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Wage Earners
(ACS)

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Initial Claims (NYS
UI)

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Job Loss Percent

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Accommodation and Food Services

</td>

<td class="gt_row gt_right">

762,727

</td>

<td class="gt_row gt_right">

435,573

</td>

<td class="gt_row gt_right">

81.6%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Administrative and Support Services

</td>

<td class="gt_row gt_right">

356,649

</td>

<td class="gt_row gt_right">

206,215

</td>

<td class="gt_row gt_right">

82.6%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Agriculture, Forestry, Fishing and Hunting

</td>

<td class="gt_row gt_right">

43,967

</td>

<td class="gt_row gt_right">

2,134

</td>

<td class="gt_row gt_right">

6.9%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Arts, Entertainment and Recreation

</td>

<td class="gt_row gt_right">

265,910

</td>

<td class="gt_row gt_right">

84,012

</td>

<td class="gt_row gt_right">

45.1%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Construction/Utilities

</td>

<td class="gt_row gt_right">

595,351

</td>

<td class="gt_row gt_right">

185,665

</td>

<td class="gt_row gt_right">

44.6%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Educational Services

</td>

<td class="gt_row gt_right">

1,165,995

</td>

<td class="gt_row gt_right">

74,297

</td>

<td class="gt_row gt_right">

9.1%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Finance and Insurance

</td>

<td class="gt_row gt_right">

552,769

</td>

<td class="gt_row gt_right">

17,092

</td>

<td class="gt_row gt_right">

4.4%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Health Care and Social Assistance

</td>

<td class="gt_row gt_right">

1,599,160

</td>

<td class="gt_row gt_right">

295,182

</td>

<td class="gt_row gt_right">

26.4%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Information

</td>

<td class="gt_row gt_right">

269,137

</td>

<td class="gt_row gt_right">

84,737

</td>

<td class="gt_row gt_right">

45.0%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Manufacturing

</td>

<td class="gt_row gt_right">

581,133

</td>

<td class="gt_row gt_right">

121,732

</td>

<td class="gt_row gt_right">

29.9%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Mining

</td>

<td class="gt_row gt_right">

4,242

</td>

<td class="gt_row gt_right">

1,307

</td>

<td class="gt_row gt_right">

44.0%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Other Services

</td>

<td class="gt_row gt_right">

433,263

</td>

<td class="gt_row gt_right">

138,522

</td>

<td class="gt_row gt_right">

45.7%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Professional, Scientific and Technical Services

</td>

<td class="gt_row gt_right">

791,750

</td>

<td class="gt_row gt_right">

97,962

</td>

<td class="gt_row gt_right">

17.7%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Public Administration (Including Government)

</td>

<td class="gt_row gt_right">

473,640

</td>

<td class="gt_row gt_right">

15,074

</td>

<td class="gt_row gt_right">

4.5%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Real Estate and Rental and Leasing

</td>

<td class="gt_row gt_right">

209,662

</td>

<td class="gt_row gt_right">

38,055

</td>

<td class="gt_row gt_right">

25.9%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Retail Trade

</td>

<td class="gt_row gt_right">

1,020,767

</td>

<td class="gt_row gt_right">

322,133

</td>

<td class="gt_row gt_right">

45.1%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Transportation and Warehousing

</td>

<td class="gt_row gt_right">

469,527

</td>

<td class="gt_row gt_right">

103,330

</td>

<td class="gt_row gt_right">

31.4%

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Wholesale
Trade

</td>

<td class="gt_row gt_right">

215,201

</td>

<td class="gt_row gt_right">

81,846

</td>

<td class="gt_row gt_right">

54.3%

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

### Unemployment Insurance Recipiency Rate

``` r
# Assumption about % of people who lost jobs that will receipve UI benefits
UI_TAKEUP_RATE <- 0.67
```

Not every person who losses their job will receive unemployment
insurance. The reasons for this include:

  - income eligibility  
  - employment type eligibility  
  - immigration status  
  - administrative burden  
  - never applying

For this analysis we are following the work from [The Century
Foundation](https://tcf.org/content/commentary/covid-stimulus-3-0-ui-reaction)
and using a UI recipiency rate of 67% for New York State.

### HUD Area Median Income (AMI)

For our parts of our analysis we restrict to only renter households with
incomes below 80% of the Area Median Income (AMI). To incorporate this
information we use a file that the Furman Center has prepared from
[public HUD data
sources](https://www.huduser.gov/portal/datasets/il.html#2018_data) that
provides the 80% AMI income cutoff for households of different sizes for
every metro area. We’ll join this unto the IPUMS data using the county
and persons columns (AMIs vary depending on the number of people in the
household).

``` r
hud_ami <- read_csv("data/hud-2018-ami.csv", col_types = "ddddd")
```

To incorporate the AMI data we need to assign PUMAs to counties first.
Since PUMAs don’t always nest within counties we use this crosswalk file
created by
[geocorr](http://mcdc.missouri.edu/applications/geocorr2014.html). This
file is created using counts of housing units at the census block level
to determine an allocation factor of PUMAs to counties (the share of a
PUMA’s housing units that fall within a given county). We then assign
each PUMA to the county that contains the plurality of its housing
units.

``` r
puma_county_xwalk <- "data/geocorr_puma2010_county2010.csv" %>% 
  read_csv(col_names = c("state", "puma", "county", "afact"), col_types = "ddd____d", skip = 2) %>% 
  mutate(county = as.numeric(str_sub(county, -3))) %>% 
  arrange(state, puma, desc(afact)) %>% 
  distinct(state, puma, .keep_all = TRUE)
```

### IPUMS American Community Survey Microdata

All data for this analysis comes from [*IPUMS USA, University of
Minnesota*](https://usa.ipums.org/). To build on this analysis and/or
replicate it for a different geography, you can sign up for a free
account and download your own extract of the data. From the IPUMS USA
page, go to *Select Data* and choose the variables. In addition to to
automatically pre-selected variables, you’ll to select the following
other variables: `statefip`, `puma`, `numprec`, `age`, `ind`, `inctot`,
`incwage`, `hhincome`, `ownershp`, `rentgrs`, . Then click *Change
Samples* to select the data sample you want to use (for this analysis we
have used ACS 2018 1-year). Once you have all your variables and samples
selected, click *View Cart* and then *Create Extract*. The default
options here are fine (format: `.csv`, structure: `Rectangular
(person)`), and by default you’ll download data for the whole country.
You can click *Select Cases*, then `statefip` to export data for only
the states you select. Once the request is complete, download the file
to the `/data` folder and adjust the following section of code to
reflect your file name and filter to your desired geography.

``` r
# Read in IPUMS USA ACS microdata, standardize the column names
ipums_raw <- read_csv("data/ipums_acs-2018-1yr_ny.csv.gz") %>% rename_all(str_to_lower)
```

### Prepare Data

First we join the UI claims and HUD AMI data onto the IPUMS data, and
create a variety of new variables related to incomes, rents, and
household members that will be used in determining eligibility of UI and
stimulus benefits and accessing rental assistance need.

For determining the amount of the stimulus payment received, we have to
make a number of simplifying assumptions due to limitations of the data.
We calculate eligibility as though each individual is considered alone,
and do not account for joint filing. We use each person’s total pre-tax
income, and if they are the head of the household and there are children
in that household, we begin the phaseout of their payment at $112,000,
and for all others $75,000 is used. For each child we add $500.

For unemployment insurance benefits, we also have to make some
simplifying assumptions due to lack of information and eligibility based
on their job, immigration status, and quarterly wages. In determining
eligibility we assume each person’s highest quarterly wages to simply be
one quarter of their total annual wages, and then apply the eligibility
criteria as defined be New York State. We start by determining
eligibility and the amount of UI benefits based solely on wages for
every person, and then below once job loss assumptions are incorporated
we adjust these based on job losses and UI recipiency
rates.

``` r
# Create all the universal person- and household-level variables for analysis

ipums_clean <- ipums_raw %>% 
  left_join(puma_county_xwalk, by = c("statefip" = "state", "puma")) %>% 
  mutate(persons = if_else(numprec > 8, 8, numprec)) %>% 
  left_join(hud_ami, by = c("statefip" = "state", "county", "persons")) %>% 
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
      ind == 7570              ~ "Professional, Scientific and Technical Services",
      between(ind, 9370, 9590) ~ "Public Administration (Including Government)",
      between(ind, 7071, 7190) ~ "Real Estate and Rental and Leasing",
      between(ind, 4670, 5790) ~ "Retail Trade",
      between(ind, 6070, 6390) ~ "Transportation and Warehousing",
      between(ind, 4070, 4590) ~ "Wholesale Trade"
    )
  ) %>% 
  left_join(ui_ind_job_loss, by = "ind_group_ui") %>% 
  filter(
    # Remove group quarters population
    gq %in% 1:2 
  ) %>% 
  mutate(
    # Set missing values
    inc_wages = incwage %>% na_if(999999) %>% na_if(999998) %>% na_if(0),
    
    # Household income
    hh_inc_nom = case_when(
      hhincome <= 0 ~ 0,
      hhincome == 9999999 ~ NA_real_, 
      TRUE ~ hhincome
    ),
    hh_inc_grp = cut(
      hh_inc_nom, c(-Inf, 15e3, 30e3, 50e3, 75e3, 112.5e3, 150e3, Inf),
      c("< $15k", "$15k - 30k", "$30k - $50k", "$50k - $75k", "$75k - $112.5k", "$112.5k - $150k", ">= $150k"),
      include.lowest = TRUE, 
      ordered_result = TRUE
    ),
  
    # Various renter variables. These are household level variables, and will
    # only be used later after filtering to one row per household.
    is_renter = (ownershp == 2),
    gross_rent_nom = if_else(is_renter, rentgrs, NA_real_),
    gross_rent_grp = cut(
      gross_rent_nom, 
      c(-Inf, 600, 1000, 1400, 1800, 2200, Inf),
      c("< $600", "$600 - 1,000", "$1,000 - $1,400", "$1,400 - $1,800", "$1,800 - $2,200", ">= $2,200"),
      include.lowest = TRUE, 
      ordered_result = TRUE
    ),
    rent_burden = gross_rent_nom / (hh_inc_nom / 12),
    is_rent_burdened = (rent_burden > 0.30),
    is_rent_burdened_sev = (rent_burden > 0.50),
    is_rent_burdened_mod = (is_rent_burdened) & (!is_rent_burdened_sev),
    target_burden = if_else(is_rent_burdened, rent_burden, 0.3),
    
    child = (age < 18),
    adult = (age >= 18),
    per_type = case_when(
      child ~ "Children",
      adult ~ "Adults"
    )
  ) %>% 
  group_by(serial) %>% 
  mutate(
    num_adults = sum(adult),
    num_children = sum(child),
  ) %>% 
  ungroup() %>% 
  mutate(
    
    # IRS payment
    per_inc_tot = inctot %>% na_if(9999999) %>% if_else(. < 0, 0, .),
    phase_out_start = case_when(
      child ~ NA_real_,
      pernum == 1 & num_children > 0 ~ 112.5e3,
      TRUE ~ 75e3
    ),
    aid_benefit = case_when(
      child ~ 500,
      per_inc_tot <= phase_out_start ~ 1200,
      TRUE ~ 1200 - ((per_inc_tot - phase_out_start)*0.05)
    ) %>% 
      if_else(. < 0, 0, .),
    
    # UI benefits
    # NOTE: these are adjusted below for the different job loss assumptions
    inc_wages_qtr = inc_wages/4,
    
    ui_benefits_month_reg = case_when(
      inc_wages_qtr <= 2600 ~ 0,
      inc_wages_qtr <= 3575 ~ if_else(inc_wages_qtr/25 < 104, 104, inc_wages_qtr/25),
      inc_wages_qtr > 3575 ~ if_else(inc_wages_qtr/26 < 143, 143, inc_wages_qtr/26),
      TRUE ~ 0
    ) %>% 
      if_else(. > 504, 504, .) * 4,
    
    ui_benefits_month_600 = if_else(ui_benefits_month_reg > 0, 600, 0) * 4,
    
  ) %>% 
  # Group by household and categorize households based or occupations of members
  group_by(serial) %>% 
  mutate(
    
    # Total household income from wages
    hh_inc_wages = sum(inc_wages, na.rm = TRUE),
    
    hh_aid_benefits = sum(aid_benefit)
    
  ) %>% 
  ungroup()
```

The analysis is set up to make is easy to use different methodologies
for assigning individuals to job loss status. Previous analysis of ours
used occupations to designate individuals as being vulnerable to lost
employment/wages, and other such as the Terner Center and the Joint
Center for Housing Studies have used industries in the same way.

This function takes the dataset created above with an additional
variable called `risk_group` added that identifies an individual as
being affected by job loss, and then using that variable in conjunction
with the general information above to create new variables related to
income loss, UI and stimulus benefits, and rental assistance need.

As noted above when discussing the unemployment insurance recipiency
rate, not everyone who loses their job will receive UI benefits for a
variety of reasons. We use 67% as the UI recipiency rate, and randomly
assign those who lost their jobs to UI receipt with this likelihood
(adjusting for those who are deemed ineligible due to wage
calculations).

For this analysis rental assistance need is defined as the amount of
money required to bring households back to the rent-to-income ratio that
they had before loss of income due to job loss, capped at 30% for those
households that were previously below that level, and capped at 100%% of
their gross rent.

``` r
add_risk_vars <- function(.data) {
  
  .data <- .data %>% 
    mutate(
      # If a person has no wages they are excluded from the 
      risk_group = if_else(is.na(inc_wages), NA, risk_group),
      risk_group_lab = if_else(risk_group, "More vulnerable", "Less vulnerable"),
      risk_wages = if_else(risk_group, inc_wages, NA_real_),
    )
  
  # We have a UI_TAKEUP_RATE that we use to determine who gets UI benefits, but
  # that number includes people who are inelligible. So we need to determine the
  # % of people who lost jobs who are ineligible and subtract that from the
  # UI_TAKEUP_RATE used to determine UI receipt among the eligible population
  # who lost jobs.
  ui_ineligible_pct <- .data %>% 
    filter(risk_group) %>% 
    summarise(
      jobs_lost = sum(perwt),
      ui_ineligible = sum(perwt * (ui_benefits_month_reg == 0)),
      ui_ineligible_pct = ui_ineligible / jobs_lost
    ) %>% 
    pull(ui_ineligible_pct)
  
  UI_TAKEUP_RATE_ADJ <- UI_TAKEUP_RATE /(1 - ui_ineligible_pct)
  
  .data %>% 
    mutate(
      # Set UI benefits to 0 if haven't lost job
      ui_benefits_month_reg = if_else(!risk_group | is.na(risk_group), 0, ui_benefits_month_reg),
      ui_benefits_month_600 = if_else(!risk_group | is.na(risk_group), 0, ui_benefits_month_600),
      
      # First apply the random assignment using the adjusted UI takeup rate
      ui_takeup = runif(n()) < (UI_TAKEUP_RATE_ADJ),
      # Then set all the people that are ineligible as not getting UI
      ui_takeup = if_else(ui_benefits_month_reg == 0, FALSE, ui_takeup),
      
      # Now the final UI takeup rate among those who lost there job should now
      # reflect the UI_TAKEUP_RATE
      
      # For those who don't takeup UI, set benefits to 0
      ui_benefits_month_reg = if_else(ui_takeup, ui_benefits_month_reg, 0),
      ui_benefits_month_600 = if_else(ui_takeup, ui_benefits_month_600, 0)
  
    ) %>%
    # Group by household and categorize households based or occupations of members
    group_by(serial) %>%
    mutate(
  
      # Household with at least one wage earner in a more vulnerable occupation
  
      # If there are no members with wages then NA, if there are any at-risk
      # people with wages then TRUE, if there are people with wages but none of
      # them are at risk then FALSE
      hh_any_risk = case_when(
        all(is.na(risk_group)) ~ NA, # no wage earners
        any(risk_group, na.rm = TRUE) ~ TRUE, # any wage earners are at risk
        all(!risk_group, na.rm = TRUE) ~ FALSE # all wage earners are at NOT at risk
      ),
  
      # The total wages for each household that come from vulnerable occupations
      hh_risk_wages = sum(risk_wages, na.rm = TRUE),
  
      # The percent of household income that comes from wages from vulnerable occupations
      hh_risk_wages_pct = sum(risk_wages, na.rm = TRUE) / na_if(hh_inc_nom, 0),
  
      # Sum up UI benefits by household
      hh_ui_benefits_month_reg = sum(ui_benefits_month_reg),
      hh_ui_benefits_month_600 = sum(ui_benefits_month_600),
      hh_ui_benefits_month_all = hh_ui_benefits_month_reg + hh_ui_benefits_month_600,
  
    ) %>%
    ungroup() %>%
    mutate(
  
      risk_burden = gross_rent_nom / ((hh_inc_nom - hh_risk_wages) / 12),
      risk_burden_aid = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_aid_benefits),
      risk_burden_ui_reg = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_ui_benefits_month_reg),
      risk_burden_ui_all = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_ui_benefits_month_all),
      risk_burden_aid_ui_all = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_aid_benefits + hh_ui_benefits_month_all),
  
      risk_rent_need = ((gross_rent_nom/risk_burden) - (gross_rent_nom/target_burden)),
      risk_rent_need_aid = ((gross_rent_nom/risk_burden_aid) - (gross_rent_nom/target_burden)),
      risk_rent_need_ui_reg = ((gross_rent_nom/risk_burden_ui_reg) - (gross_rent_nom/target_burden)),
      risk_rent_need_ui_all = ((gross_rent_nom/risk_burden_ui_all) - (gross_rent_nom/target_burden)),
      risk_rent_need_aid_ui_all = ((gross_rent_nom/risk_burden_aid_ui_all) - (gross_rent_nom/target_burden)),
  
    ) %>%
    # Make some adjustments to all the rent_need columns
    mutate_at(
      vars(matches("risk_.*rent_need.*")),
      ~{
        # note that rent need is expressed as a negative number at first, after this it is positive
        case_when(
          is.na(.) ~ 0, # missing change to zero (missing if no cash rent, etc.)
          . > 0    ~ 0, # positive value for need means they don't have need, set to zero
          -. > gross_rent_nom ~ gross_rent_nom, # cap needs at the full rent value
          TRUE ~ -. # change the negative numbers to positive
        )
      }
    )
  
}
```

This function takes a dataframe as created by the above
`add_risk_vars()` function, and summarizes the household level variables
(using survey weights) of interest for a single month.

``` r
# Preset some arguments for cleaner code below
survey_total_ci90 <- partial(survey_total, vartype = "ci", level = 0.90)
survey_mean_ci90 <- partial(survey_mean, vartype = "ci", level = 0.90)

# Summarize the household-level data using survey weights to get estimates for our various indicators. This function is designed so that it can be applied to a dataset created by the above add_risk_vars() function.
summarise_assistance_stats <- function(.data) {
  .data %>% 
    as_survey_design(weights = hhwt) %>% 
    summarise(
      renter_households = survey_total_ci90(1),
      
      hh_hh_inc_nom_tot_monthly = survey_total_ci90(hh_inc_nom/12),
      hh_total_wages_tot_monthly = survey_total_ci90(hh_inc_wages/12, na.rm = TRUE),
      hh_risk_wages_tot_monthly = survey_total_ci90(hh_risk_wages/12),
      hh_rent_tot_monthly = survey_total_ci90(gross_rent_nom),
      
      hh_hh_inc_nom_avg_monthly = survey_mean_ci90(hh_inc_nom/12),
      hh_total_wages_avg_monthly = survey_mean_ci90(hh_inc_wages/12, na.rm = TRUE),
      hh_risk_wages_avg_monthly = survey_mean_ci90(hh_risk_wages/12),
      hh_rent_avg_monthly = survey_mean_ci90(gross_rent_nom),
      
      # Stimulus Payments
      hh_aid_benefits_tot = survey_total_ci90(hh_aid_benefits),
      hh_aid_benefits_avg = survey_mean_ci90(hh_aid_benefits),
      
      # UI Benefits
      hh_ui_benefits_monthly_reg_tot = survey_total_ci90(hh_ui_benefits_month_reg),
      hh_ui_benefits_monthly_600_tot = survey_total_ci90(hh_ui_benefits_month_600),
      hh_ui_benefits_monthly_reg_avg = survey_mean_ci90(hh_ui_benefits_month_reg),
      hh_ui_benefits_monthly_600_avg = survey_mean_ci90(hh_ui_benefits_month_600),
      
      # Rental ASsistance Need (aggregate)
      hh_rent_need_tot_monthly = survey_total_ci90(risk_rent_need),
      hh_rent_need_aid_tot_monthly = survey_total_ci90(risk_rent_need_aid),
      hh_rent_need_ui_reg_tot_monthly = survey_total_ci90(risk_rent_need_ui_reg),
      hh_rent_need_ui_all_tot_monthly = survey_total_ci90(risk_rent_need_ui_all),
      hh_rent_need_aid_ui_all_tot_monthly = survey_total_ci90(risk_rent_need_aid_ui_all),
      
      # Rental ASsistance Need (average)
      hh_rent_need_avg_monthly = survey_mean_ci90(risk_rent_need),
      hh_rent_need_aid_avg_monthly = survey_mean_ci90(risk_rent_need_aid),
      hh_rent_need_ui_reg_avg_monthly = survey_mean_ci90(risk_rent_need_ui_reg),
      hh_rent_need_ui_all_avg_monthly = survey_mean_ci90(risk_rent_need_ui_all),
      hh_rent_need_aid_ui_all_avg_monthly = survey_mean_ci90(risk_rent_need_aid_ui_all)
    ) %>% 
    pivot_longer(everything()) %>% 
    mutate(
      type = case_when(
        str_detect(name, "_low") ~ "low",
        str_detect(name, "_upp") ~ "upp",
        TRUE ~ "est"
      ),
      name = str_remove(name, "(_low|_upp)")
    ) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(moe_90 = upp - est) %>% 
    select(-low, -upp) %>% 
    mutate(
      name = recode(
        name,
        "renter_households" = "Total Households",
        "hh_hh_inc_nom_tot_monthly" = "Monthly Aggregate Household Income",
        "hh_total_wages_tot_monthly" = "Monthly Aggregate Wages",
        "hh_risk_wages_tot_monthly" = "Monthly Aggregate Lost Wages",
        "hh_rent_tot_monthly" = "Monthly Aggregate Gross Rent",
        "hh_hh_inc_nom_avg_monthly" = "Monthly Average Household Income",
        "hh_total_wages_avg_monthly" = "Monthly Average Wages",
        "hh_risk_wages_avg_monthly" = "Monthly Average Lost Wages",
        "hh_rent_avg_monthly" = "Monthly Average Gross Rent",
        
        "hh_aid_benefits_tot" = "Aggregate Stimulus Benefits",
        "hh_aid_benefits_avg" = "Average Stimulus Benefits",
        
        "hh_ui_benefits_monthly_reg_tot" = "Monthly Aggregate Regular UI Benefits",
        "hh_ui_benefits_monthly_reg_avg" = "Monthly Average Regular UI Benefits",
        "hh_ui_benefits_monthly_600_tot" = "Monthly Aggregate Enhanced UI Benefits",
        "hh_ui_benefits_monthly_600_avg" = "Monthly Average Enhanced UI Benefits",
        
        "hh_rent_need_tot_monthly" = "Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)",
        "hh_rent_need_aid_tot_monthly" = "Monthly Aggregate Rental Assistance Need (After Stimulus)",
        "hh_rent_need_ui_reg_tot_monthly" = "Monthly Aggregate Rental Assistance Need (After Regular UI)",
        "hh_rent_need_ui_all_tot_monthly" = "Monthly Aggregate Rental Assistance Need (After Enhanced UI)",
        "hh_rent_need_aid_ui_all_tot_monthly" = "Monthly Aggregate Rental Assistance Need (After Enhanced UI and Stimulus)",
        
        "hh_rent_need_avg_monthly" = "Monthly Average Rental Assistance Need (Without Stimulus or UI)",
        "hh_rent_need_aid_avg_monthly" = "Monthly Average Rental Assistance Need (After Stimulus)",
        "hh_rent_need_ui_reg_avg_monthly" = "Monthly Average Rental Assistance Need (After Regular UI)",
        "hh_rent_need_ui_all_avg_monthly" = "Monthly Average Rental Assistance Need (After Enhanced UI)",
        "hh_rent_need_aid_ui_all_avg_monthly" = "Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)"
      )
    )
}
```

This function sets out a few different versions of the results that we
would like to use in the final tables. The function takes the
`ipums_clean` dataset and a value to use as the seed for random number
generation. It then adds the relevant `risk_group` variable to indicate
which individuals should be identified as having lost their job. Next we
apply the `add_risk_vars()` function to create the variables we need.
Then filter to just one row per household and only those households with
at least one member assumed to lose their job. Then summarise the
results using the `summarise_assistance_stats()` function, and add a new
variable to identify what assumptions are used in this version of
results. We do this for a variety of assumptions, and then stack the
results into a single table.

To determine which individuals to assume lost their job based on the UI
claims data we assign each person a random number between 0 and 1
(uniform distribution) and then compare that number to the estimated job
loss rate in their industry and mark them as having lost their job if
their random number is low the job loss rate.

``` r
generate_results <- function(seed, .data) {
  set.seed(seed)
  
  fc_ui_jobs_data <- .data %>% 
    mutate(risk_group = runif(n()) < job_loss_pct) %>% 
    add_risk_vars()
  
  fc_ui_jobs <- fc_ui_jobs_data %>% 
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    summarise_assistance_stats() %>% 
    mutate(type = "fc_ui_jobs")
  
  fc_ui_jobs_noui <- fc_ui_jobs_data %>%
    # Include only those who don't get UI benefits
    filter(hh_ui_benefits_month_reg == 0) %>%
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>%
    summarise_assistance_stats() %>%
    mutate(type = "fc_ui_jobs_noui")
  
  fc_ui_jobs_preburdened <- fc_ui_jobs_data %>%
    # Include only those who were rent burdneded pre-covid
    filter(is_rent_burdened) %>%
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>%
    summarise_assistance_stats() %>%
    mutate(type = "fc_ui_jobs_preburdened")
  
  fc_ui_jobs_presevburdened <- fc_ui_jobs_data %>%
    # Include only those who were severely rent burdneded pre-covid
    filter(is_rent_burdened_sev) %>%
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>%
    summarise_assistance_stats() %>%
    mutate(type = "fc_ui_jobs_presevburdened")
  
  fc_ui_jobs_return25 <- .data %>% 
    # Assume that 25% of people who lost jobs return to them
    mutate(risk_group = runif(n()) < (job_loss_pct - (job_loss_pct*0.25))) %>% 
    add_risk_vars() %>% 
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    summarise_assistance_stats() %>% 
    mutate(type = "fc_ui_jobs_return25")
  
  fc_ui_jobs_return50 <- .data %>% 
    # Assume that 50% of people who lost jobs return to them
    mutate(risk_group = runif(n()) < (job_loss_pct - (job_loss_pct*0.50))) %>% 
    add_risk_vars() %>% 
    filter(
      pernum == 1, # keep only one row per household
      hh_any_risk  # keep only vulnerable households
    ) %>% 
    summarise_assistance_stats() %>% 
    mutate(type = "fc_ui_jobs_return50")
  
  
  bind_rows(
    fc_ui_jobs,
    fc_ui_jobs_noui,
    fc_ui_jobs_preburdened,
    fc_ui_jobs_presevburdened,
    fc_ui_jobs_return25,
    fc_ui_jobs_return50
  ) %>% 
    mutate(seed = seed)
  
}
```

This function takes the dataframe created by the above
`generate_results()` function and reformat the data and creates a table
of the results.

> *NOTE: The margin of error columns are currently being hidden for
> readability, but un-commenting those portions and remove the
> `cols_hide()` line will add them
back.*

``` r
# Take the agerage of all the results for the multiple iterations and then
# create a single table to compare the methodologies
table_rows <- c(
  "Total Households",
  "Monthly Aggregate Household Income",
  "Monthly Aggregate Wages",
  "Monthly Aggregate Lost Wages",
  "Monthly Aggregate Gross Rent",
  "Monthly Average Household Income",
  "Monthly Average Wages",
  "Monthly Average Lost Wages",
  "Monthly Average Gross Rent",
          
  "Aggregate Stimulus Benefits",
  "Average Stimulus Benefits",
          
  "Monthly Aggregate Regular UI Benefits",
  "Monthly Average Regular UI Benefits",
  "Monthly Aggregate Enhanced UI Benefits",
  "Monthly Average Enhanced UI Benefits",
          
  "Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)",
  "Monthly Aggregate Rental Assistance Need (After Stimulus)",
  "Monthly Aggregate Rental Assistance Need (After Regular UI)",
  "Monthly Aggregate Rental Assistance Need (After Enhanced UI)",
  "Monthly Aggregate Rental Assistance Need (After Enhanced UI and Stimulus)",
          
  "Monthly Average Rental Assistance Need (Without Stimulus or UI)",
  "Monthly Average Rental Assistance Need (After Stimulus)",
  "Monthly Average Rental Assistance Need (After Regular UI)",
  "Monthly Average Rental Assistance Need (After Enhanced UI)",
  "Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)"
)

make_table <- function(.data) {
  .data %>% 
    group_by(type, name) %>% 
    summarise_at(vars(est, moe_90), mean) %>% 
    pivot_longer(cols = all_of(c("est", "moe_90")), names_to = "measure") %>% 
    unite(type_measure,type, measure, sep = "__") %>% 
    pivot_wider(name, type_measure) %>% 
    mutate(name = ordered(name, levels = table_rows)) %>% 
    arrange(name) %>% 
    gt() %>%
    tab_spanner(label = md("**UI Job Loss, No Return**"), columns = starts_with("fc_ui_jobs")) %>% 
    tab_spanner(label = md("**UI Job Loss, No Return, No UI HHs**"), columns = starts_with("fc_ui_jobs_noui")) %>% 
    tab_spanner(label = md("**UI Job Loss, No Return, Rent Burdened HHs**"), columns = starts_with("fc_ui_jobs_preburdened")) %>% 
    tab_spanner(label = md("**UI Job Loss, No Return, Severely Rent Burdened HHs**"), columns = starts_with("fc_ui_jobs_presevburdened")) %>% 
    tab_spanner(label = md("**UI Job Loss, 25% Return**"), columns = starts_with("fc_ui_jobs_return25")) %>% 
    tab_spanner(label = md("**UI Job Loss, 50% Return**"), columns = starts_with("fc_ui_jobs_return50")) %>% 
    cols_label(
      name = "", 
      fc_ui_jobs__est = "Estimate", fc_ui_jobs__moe_90 = "MOE (90%)",
      fc_ui_jobs_noui__est = "Estimate", fc_ui_jobs_noui__moe_90 = "MOE (90%)",
      fc_ui_jobs_preburdened__est = "Estimate", fc_ui_jobs_preburdened__moe_90 = "MOE (90%)",
      fc_ui_jobs_presevburdened__est = "Estimate", fc_ui_jobs_presevburdened__moe_90 = "MOE (90%)",
      fc_ui_jobs_return25__est = "Estimate", fc_ui_jobs_return25__moe_90 = "MOE (90%)",
      fc_ui_jobs_return50__est = "Estimate", fc_ui_jobs_return50__moe_90 = "MOE (90%)",
    ) %>% 
    # Show/Hide MOE columns
    cols_hide(columns = ends_with("moe_90")) %>%
    tab_row_group(group = "Total", rows = matches("Total")) %>% 
    tab_row_group(group = "Aggregate", rows = matches("Aggregate")) %>% 
    tab_row_group(group = "Average", rows = matches("Average")) %>% 
    row_group_order(c("Total", "Aggregate", "Average")) %>% 
    cols_align("left", columns = vars(name)) %>% 
    fmt_number(columns = matches("(est|moe).*"), rows = str_detect(name, "^Total"), decimal = 0) %>% 
    fmt_currency(columns = matches("(est|moe).*"), rows = str_detect(name, "Aggregate|Average"), suffixing = c(NA, "M", "B", "T")) %>% 
    tab_source_note(str_glue("Notes: Assignment of individuals to UI take-up status is done randomly with a {percent(UI_TAKEUP_RATE)} likelihood, accounting for those deemed ineligible based on income criteria. Assignment of individuals to job loss status is done randomly with a likelihood based on the industry specific UI claims and job loss estimates. All of the results show here are the averages of {ITERATIONS} iterations of the estimations."))

}
```

-----

<br>

## Results Tables

### All NYS Affected Renter Households

``` r
table_nys <- seq_len(ITERATIONS) %>% 
  future_map_dfr(
    .f = generate_results, 
    .data = filter(ipums_clean, is_renter)
  )
```

``` r
table_nys %>% 
  filter(str_detect(name, "(Total|Aggregate)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households", 
    subtitle = "New York State - Aggregates"
  )
```

<!--html_preserve-->


<div id="kjuudrpbqc" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter
Households

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York State -
Aggregates

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

1,156,787

</td>

<td class="gt_row gt_right">

327,172

</td>

<td class="gt_row gt_right">

483,433

</td>

<td class="gt_row gt_right">

226,931

</td>

<td class="gt_row gt_right">

910,930

</td>

<td class="gt_row gt_right">

637,089

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Household Income

</td>

<td class="gt_row gt_right">

$8.06B

</td>

<td class="gt_row gt_right">

$1.71B

</td>

<td class="gt_row gt_right">

$1.54B

</td>

<td class="gt_row gt_right">

$444.12M

</td>

<td class="gt_row gt_right">

$6.41B

</td>

<td class="gt_row gt_right">

$4.53B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Wages

</td>

<td class="gt_row gt_right">

$7.43B

</td>

<td class="gt_row gt_right">

$1.51B

</td>

<td class="gt_row gt_right">

$1.41B

</td>

<td class="gt_row gt_right">

$393.15M

</td>

<td class="gt_row gt_right">

$5.91B

</td>

<td class="gt_row gt_right">

$4.19B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Lost Wages

</td>

<td class="gt_row gt_right">

$4.88B

</td>

<td class="gt_row gt_right">

$793.49M

</td>

<td class="gt_row gt_right">

$1.09B

</td>

<td class="gt_row gt_right">

$329.78M

</td>

<td class="gt_row gt_right">

$3.67B

</td>

<td class="gt_row gt_right">

$2.45B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Gross Rent

</td>

<td class="gt_row gt_right">

$1.74B

</td>

<td class="gt_row gt_right">

$453.19M

</td>

<td class="gt_row gt_right">

$773.25M

</td>

<td class="gt_row gt_right">

$362.60M

</td>

<td class="gt_row gt_right">

$1.37B

</td>

<td class="gt_row gt_right">

$965.86M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Aggregate Stimulus Benefits

</td>

<td class="gt_row gt_right">

$3.22B

</td>

<td class="gt_row gt_right">

$913.66M

</td>

<td class="gt_row gt_right">

$1.35B

</td>

<td class="gt_row gt_right">

$621.83M

</td>

<td class="gt_row gt_right">

$2.58B

</td>

<td class="gt_row gt_right">

$1.85B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Regular UI Benefits

</td>

<td class="gt_row gt_right">

$1.29B

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$360.81M

</td>

<td class="gt_row gt_right">

$106.70M

</td>

<td class="gt_row gt_right">

$967.27M

</td>

<td class="gt_row gt_right">

$643.43M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$2.27B

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$791.32M

</td>

<td class="gt_row gt_right">

$289.71M

</td>

<td class="gt_row gt_right">

$1.71B

</td>

<td class="gt_row gt_right">

$1.13B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$1.23B

</td>

<td class="gt_row gt_right">

$237.29M

</td>

<td class="gt_row gt_right">

$631.63M

</td>

<td class="gt_row gt_right">

$258.46M

</td>

<td class="gt_row gt_right">

$931.77M

</td>

<td class="gt_row gt_right">

$626.90M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$545.83M

</td>

<td class="gt_row gt_right">

$83.74M

</td>

<td class="gt_row gt_right">

$216.62M

</td>

<td class="gt_row gt_right">

$44.01M

</td>

<td class="gt_row gt_right">

$394.86M

</td>

<td class="gt_row gt_right">

$254.96M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$992.15M

</td>

<td class="gt_row gt_right">

$237.29M

</td>

<td class="gt_row gt_right">

$549.92M

</td>

<td class="gt_row gt_right">

$207.66M

</td>

<td class="gt_row gt_right">

$742.77M

</td>

<td class="gt_row gt_right">

$493.90M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$407.08M

</td>

<td class="gt_row gt_right">

$237.29M

</td>

<td class="gt_row gt_right">

$204.63M

</td>

<td class="gt_row gt_right">

$79.96M

</td>

<td class="gt_row gt_right">

$310.06M

</td>

<td class="gt_row gt_right">

$211.37M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI and
Stimulus)

</td>

<td class="gt_row gt_right">

$175.64M

</td>

<td class="gt_row gt_right">

$83.74M

</td>

<td class="gt_row gt_right">

$57.64M

</td>

<td class="gt_row gt_right">

$8.03M

</td>

<td class="gt_row gt_right">

$130.70M

</td>

<td class="gt_row gt_right">

$86.52M

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
table_nys %>% 
  filter(str_detect(name, "(Total|Average)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households", 
    subtitle = "New York State - Averages"
  )
```

<!--html_preserve-->


<div id="hifidddemd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter
Households

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York State -
Averages

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

1,156,787

</td>

<td class="gt_row gt_right">

327,172

</td>

<td class="gt_row gt_right">

483,433

</td>

<td class="gt_row gt_right">

226,931

</td>

<td class="gt_row gt_right">

910,930

</td>

<td class="gt_row gt_right">

637,089

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Household Income

</td>

<td class="gt_row gt_right">

$6,965.72

</td>

<td class="gt_row gt_right">

$5,237.73

</td>

<td class="gt_row gt_right">

$3,194.85

</td>

<td class="gt_row gt_right">

$1,957.08

</td>

<td class="gt_row gt_right">

$7,033.04

</td>

<td class="gt_row gt_right">

$7,116.89

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Wages

</td>

<td class="gt_row gt_right">

$6,419.68

</td>

<td class="gt_row gt_right">

$4,604.13

</td>

<td class="gt_row gt_right">

$2,907.95

</td>

<td class="gt_row gt_right">

$1,732.47

</td>

<td class="gt_row gt_right">

$6,488.32

</td>

<td class="gt_row gt_right">

$6,579.76

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Lost Wages

</td>

<td class="gt_row gt_right">

$4,220.14

</td>

<td class="gt_row gt_right">

$2,424.76

</td>

<td class="gt_row gt_right">

$2,261.28

</td>

<td class="gt_row gt_right">

$1,453.21

</td>

<td class="gt_row gt_right">

$4,028.01

</td>

<td class="gt_row gt_right">

$3,852.65

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Gross Rent

</td>

<td class="gt_row gt_right">

$1,503.92

</td>

<td class="gt_row gt_right">

$1,385.15

</td>

<td class="gt_row gt_right">

$1,599.49

</td>

<td class="gt_row gt_right">

$1,597.84

</td>

<td class="gt_row gt_right">

$1,508.74

</td>

<td class="gt_row gt_right">

$1,516.04

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Average Stimulus Benefits

</td>

<td class="gt_row gt_right">

$2,782.19

</td>

<td class="gt_row gt_right">

$2,792.69

</td>

<td class="gt_row gt_right">

$2,786.90

</td>

<td class="gt_row gt_right">

$2,740.30

</td>

<td class="gt_row gt_right">

$2,836.43

</td>

<td class="gt_row gt_right">

$2,897.11

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Regular UI Benefits

</td>

<td class="gt_row gt_right">

$1,114.14

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$746.36

</td>

<td class="gt_row gt_right">

$470.18

</td>

<td class="gt_row gt_right">

$1,061.85

</td>

<td class="gt_row gt_right">

$1,009.94

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1,964.88

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$1,636.92

</td>

<td class="gt_row gt_right">

$1,276.65

</td>

<td class="gt_row gt_right">

$1,874.53

</td>

<td class="gt_row gt_right">

$1,780.74

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$1,060.61

</td>

<td class="gt_row gt_right">

$725.16

</td>

<td class="gt_row gt_right">

$1,306.54

</td>

<td class="gt_row gt_right">

$1,138.93

</td>

<td class="gt_row gt_right">

$1,022.87

</td>

<td class="gt_row gt_right">

$983.98

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$471.84

</td>

<td class="gt_row gt_right">

$255.85

</td>

<td class="gt_row gt_right">

$448.06

</td>

<td class="gt_row gt_right">

$193.89

</td>

<td class="gt_row gt_right">

$433.46

</td>

<td class="gt_row gt_right">

$400.16

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$857.67

</td>

<td class="gt_row gt_right">

$725.16

</td>

<td class="gt_row gt_right">

$1,137.51

</td>

<td class="gt_row gt_right">

$915.07

</td>

<td class="gt_row gt_right">

$815.39

</td>

<td class="gt_row gt_right">

$775.21

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$351.89

</td>

<td class="gt_row gt_right">

$725.16

</td>

<td class="gt_row gt_right">

$423.26

</td>

<td class="gt_row gt_right">

$352.35

</td>

<td class="gt_row gt_right">

$340.37

</td>

<td class="gt_row gt_right">

$331.75

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)

</td>

<td class="gt_row gt_right">

$151.83

</td>

<td class="gt_row gt_right">

$255.85

</td>

<td class="gt_row gt_right">

$119.20

</td>

<td class="gt_row gt_right">

$35.39

</td>

<td class="gt_row gt_right">

$143.47

</td>

<td class="gt_row gt_right">

$135.80

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

-----

<br>

### All NYS Affected Renter Households with Incomes below 80% of AMI

``` r
table_nys_lt80ami <- seq_len(ITERATIONS) %>% 
  future_map_dfr(
    .f = generate_results, 
    .data = filter(ipums_clean, is_renter, hh_inc_nom < hud_inc_lim80)
  )
```

``` r
table_nys_lt80ami %>% 
  filter(str_detect(name, "(Total|Aggregate)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households with Incomes below 80% of AMI", 
    subtitle = "New York State - Aggregates"
  )
```

<!--html_preserve-->


<div id="mwvhooydcp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter Households with Incomes below 80% of
AMI

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York State -
Aggregates

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

628,276

</td>

<td class="gt_row gt_right">

176,942

</td>

<td class="gt_row gt_right">

430,084

</td>

<td class="gt_row gt_right">

223,120

</td>

<td class="gt_row gt_right">

491,541

</td>

<td class="gt_row gt_right">

342,178

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Household Income

</td>

<td class="gt_row gt_right">

$1.97B

</td>

<td class="gt_row gt_right">

$397.43M

</td>

<td class="gt_row gt_right">

$1.16B

</td>

<td class="gt_row gt_right">

$424.52M

</td>

<td class="gt_row gt_right">

$1.56B

</td>

<td class="gt_row gt_right">

$1.10B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Wages

</td>

<td class="gt_row gt_right">

$1.77B

</td>

<td class="gt_row gt_right">

$313.97M

</td>

<td class="gt_row gt_right">

$1.04B

</td>

<td class="gt_row gt_right">

$375.05M

</td>

<td class="gt_row gt_right">

$1.40B

</td>

<td class="gt_row gt_right">

$993.45M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Lost Wages

</td>

<td class="gt_row gt_right">

$1.34B

</td>

<td class="gt_row gt_right">

$154.91M

</td>

<td class="gt_row gt_right">

$821.95M

</td>

<td class="gt_row gt_right">

$315.50M

</td>

<td class="gt_row gt_right">

$1.01B

</td>

<td class="gt_row gt_right">

$677.92M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Gross Rent

</td>

<td class="gt_row gt_right">

$795.95M

</td>

<td class="gt_row gt_right">

$214.00M

</td>

<td class="gt_row gt_right">

$630.03M

</td>

<td class="gt_row gt_right">

$351.04M

</td>

<td class="gt_row gt_right">

$625.81M

</td>

<td class="gt_row gt_right">

$437.96M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Aggregate Stimulus Benefits

</td>

<td class="gt_row gt_right">

$1.86B

</td>

<td class="gt_row gt_right">

$517.64M

</td>

<td class="gt_row gt_right">

$1.23B

</td>

<td class="gt_row gt_right">

$612.57M

</td>

<td class="gt_row gt_right">

$1.48B

</td>

<td class="gt_row gt_right">

$1.05B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Regular UI Benefits

</td>

<td class="gt_row gt_right">

$526.41M

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$317.55M

</td>

<td class="gt_row gt_right">

$114.61M

</td>

<td class="gt_row gt_right">

$395.38M

</td>

<td class="gt_row gt_right">

$264.81M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1.20B

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$761.28M

</td>

<td class="gt_row gt_right">

$317.37M

</td>

<td class="gt_row gt_right">

$899.68M

</td>

<td class="gt_row gt_right">

$601.82M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$628.80M

</td>

<td class="gt_row gt_right">

$94.95M

</td>

<td class="gt_row gt_right">

$501.11M

</td>

<td class="gt_row gt_right">

$249.02M

</td>

<td class="gt_row gt_right">

$483.68M

</td>

<td class="gt_row gt_right">

$330.57M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$160.34M

</td>

<td class="gt_row gt_right">

$11.74M

</td>

<td class="gt_row gt_right">

$123.09M

</td>

<td class="gt_row gt_right">

$37.78M

</td>

<td class="gt_row gt_right">

$117.23M

</td>

<td class="gt_row gt_right">

$76.12M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$516.12M

</td>

<td class="gt_row gt_right">

$94.95M

</td>

<td class="gt_row gt_right">

$421.69M

</td>

<td class="gt_row gt_right">

$192.98M

</td>

<td class="gt_row gt_right">

$390.83M

</td>

<td class="gt_row gt_right">

$264.01M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$107.73M

</td>

<td class="gt_row gt_right">

$94.95M

</td>

<td class="gt_row gt_right">

$93.83M

</td>

<td class="gt_row gt_right">

$56.41M

</td>

<td class="gt_row gt_right">

$84.80M

</td>

<td class="gt_row gt_right">

$59.54M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI and
Stimulus)

</td>

<td class="gt_row gt_right">

$11.74M

</td>

<td class="gt_row gt_right">

$11.74M

</td>

<td class="gt_row gt_right">

$9.10M

</td>

<td class="gt_row gt_right">

$3.07M

</td>

<td class="gt_row gt_right">

$9.01M

</td>

<td class="gt_row gt_right">

$6.12M

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
table_nys_lt80ami %>% 
  filter(str_detect(name, "(Total|Average)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households with Incomes below 80% of AMI", 
    subtitle = "New York State - Averages"
  )
```

<!--html_preserve-->


<div id="yeezlarrsn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter Households with Incomes below 80% of
AMI

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York State -
Averages

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

628,276

</td>

<td class="gt_row gt_right">

176,942

</td>

<td class="gt_row gt_right">

430,084

</td>

<td class="gt_row gt_right">

223,120

</td>

<td class="gt_row gt_right">

491,541

</td>

<td class="gt_row gt_right">

342,178

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Household Income

</td>

<td class="gt_row gt_right">

$3,134.79

</td>

<td class="gt_row gt_right">

$2,245.79

</td>

<td class="gt_row gt_right">

$2,686.06

</td>

<td class="gt_row gt_right">

$1,902.64

</td>

<td class="gt_row gt_right">

$3,173.87

</td>

<td class="gt_row gt_right">

$3,216.32

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Wages

</td>

<td class="gt_row gt_right">

$2,814.90

</td>

<td class="gt_row gt_right">

$1,774.07

</td>

<td class="gt_row gt_right">

$2,425.61

</td>

<td class="gt_row gt_right">

$1,680.94

</td>

<td class="gt_row gt_right">

$2,856.89

</td>

<td class="gt_row gt_right">

$2,903.31

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Lost Wages

</td>

<td class="gt_row gt_right">

$2,140.75

</td>

<td class="gt_row gt_right">

$875.00

</td>

<td class="gt_row gt_right">

$1,911.10

</td>

<td class="gt_row gt_right">

$1,414.03

</td>

<td class="gt_row gt_right">

$2,057.52

</td>

<td class="gt_row gt_right">

$1,981.22

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Gross Rent

</td>

<td class="gt_row gt_right">

$1,266.89

</td>

<td class="gt_row gt_right">

$1,209.32

</td>

<td class="gt_row gt_right">

$1,464.90

</td>

<td class="gt_row gt_right">

$1,573.31

</td>

<td class="gt_row gt_right">

$1,273.16

</td>

<td class="gt_row gt_right">

$1,279.92

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Average Stimulus Benefits

</td>

<td class="gt_row gt_right">

$2,964.97

</td>

<td class="gt_row gt_right">

$2,925.52

</td>

<td class="gt_row gt_right">

$2,857.08

</td>

<td class="gt_row gt_right">

$2,745.54

</td>

<td class="gt_row gt_right">

$3,010.00

</td>

<td class="gt_row gt_right">

$3,061.67

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Regular UI Benefits

</td>

<td class="gt_row gt_right">

$837.87

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$738.35

</td>

<td class="gt_row gt_right">

$513.66

</td>

<td class="gt_row gt_right">

$804.36

</td>

<td class="gt_row gt_right">

$773.92

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1,907.46

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$1,770.15

</td>

<td class="gt_row gt_right">

$1,422.43

</td>

<td class="gt_row gt_right">

$1,830.35

</td>

<td class="gt_row gt_right">

$1,758.86

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$1,000.84

</td>

<td class="gt_row gt_right">

$536.38

</td>

<td class="gt_row gt_right">

$1,165.14

</td>

<td class="gt_row gt_right">

$1,116.07

</td>

<td class="gt_row gt_right">

$983.98

</td>

<td class="gt_row gt_right">

$966.09

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$255.20

</td>

<td class="gt_row gt_right">

$66.26

</td>

<td class="gt_row gt_right">

$286.17

</td>

<td class="gt_row gt_right">

$169.31

</td>

<td class="gt_row gt_right">

$238.47

</td>

<td class="gt_row gt_right">

$222.45

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$821.48

</td>

<td class="gt_row gt_right">

$536.38

</td>

<td class="gt_row gt_right">

$980.46

</td>

<td class="gt_row gt_right">

$864.90

</td>

<td class="gt_row gt_right">

$795.08

</td>

<td class="gt_row gt_right">

$771.57

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$171.45

</td>

<td class="gt_row gt_right">

$536.38

</td>

<td class="gt_row gt_right">

$218.14

</td>

<td class="gt_row gt_right">

$252.83

</td>

<td class="gt_row gt_right">

$172.49

</td>

<td class="gt_row gt_right">

$174.00

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)

</td>

<td class="gt_row gt_right">

$18.68

</td>

<td class="gt_row gt_right">

$66.26

</td>

<td class="gt_row gt_right">

$21.14

</td>

<td class="gt_row gt_right">

$13.75

</td>

<td class="gt_row gt_right">

$18.32

</td>

<td class="gt_row gt_right">

$17.88

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

-----

<br>

### All NYC Affected Renter Households

``` r
nyc_counties <- c(5, 47, 61, 81, 85)

table_nyc <- seq_len(ITERATIONS) %>% 
  future_map_dfr(
    .f = generate_results, 
    .data = filter(ipums_clean, is_renter, county %in% nyc_counties)
  )
```

``` r
table_nyc %>% 
  filter(str_detect(name, "(Total|Aggregate)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households", 
    subtitle = "New York City - Aggregates"
  )
```

<!--html_preserve-->


<div id="uqxyzxfuqi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter
Households

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York City -
Aggregates

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

734,894

</td>

<td class="gt_row gt_right">

208,730

</td>

<td class="gt_row gt_right">

313,977

</td>

<td class="gt_row gt_right">

142,032

</td>

<td class="gt_row gt_right">

579,707

</td>

<td class="gt_row gt_right">

406,081

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Household Income

</td>

<td class="gt_row gt_right">

$5.75B

</td>

<td class="gt_row gt_right">

$1.29B

</td>

<td class="gt_row gt_right">

$1.10B

</td>

<td class="gt_row gt_right">

$300.76M

</td>

<td class="gt_row gt_right">

$4.57B

</td>

<td class="gt_row gt_right">

$3.23B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Wages

</td>

<td class="gt_row gt_right">

$5.32B

</td>

<td class="gt_row gt_right">

$1.14B

</td>

<td class="gt_row gt_right">

$1.01B

</td>

<td class="gt_row gt_right">

$269.55M

</td>

<td class="gt_row gt_right">

$4.23B

</td>

<td class="gt_row gt_right">

$2.99B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Lost Wages

</td>

<td class="gt_row gt_right">

$3.46B

</td>

<td class="gt_row gt_right">

$622.83M

</td>

<td class="gt_row gt_right">

$783.41M

</td>

<td class="gt_row gt_right">

$226.63M

</td>

<td class="gt_row gt_right">

$2.60B

</td>

<td class="gt_row gt_right">

$1.73B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Gross Rent

</td>

<td class="gt_row gt_right">

$1.23B

</td>

<td class="gt_row gt_right">

$327.00M

</td>

<td class="gt_row gt_right">

$543.17M

</td>

<td class="gt_row gt_right">

$244.01M

</td>

<td class="gt_row gt_right">

$971.89M

</td>

<td class="gt_row gt_right">

$682.82M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Aggregate Stimulus Benefits

</td>

<td class="gt_row gt_right">

$2.07B

</td>

<td class="gt_row gt_right">

$590.35M

</td>

<td class="gt_row gt_right">

$890.93M

</td>

<td class="gt_row gt_right">

$393.39M

</td>

<td class="gt_row gt_right">

$1.66B

</td>

<td class="gt_row gt_right">

$1.19B

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Regular UI Benefits

</td>

<td class="gt_row gt_right">

$846.06M

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$250.96M

</td>

<td class="gt_row gt_right">

$73.09M

</td>

<td class="gt_row gt_right">

$636.55M

</td>

<td class="gt_row gt_right">

$424.56M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1.45B

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$526.07M

</td>

<td class="gt_row gt_right">

$191.85M

</td>

<td class="gt_row gt_right">

$1.09B

</td>

<td class="gt_row gt_right">

$725.01M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$870.31M

</td>

<td class="gt_row gt_right">

$179.72M

</td>

<td class="gt_row gt_right">

$449.86M

</td>

<td class="gt_row gt_right">

$176.86M

</td>

<td class="gt_row gt_right">

$661.16M

</td>

<td class="gt_row gt_right">

$444.51M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$411.99M

</td>

<td class="gt_row gt_right">

$71.25M

</td>

<td class="gt_row gt_right">

$166.47M

</td>

<td class="gt_row gt_right">

$34.16M

</td>

<td class="gt_row gt_right">

$297.51M

</td>

<td class="gt_row gt_right">

$190.16M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$711.63M

</td>

<td class="gt_row gt_right">

$179.72M

</td>

<td class="gt_row gt_right">

$392.55M

</td>

<td class="gt_row gt_right">

$141.98M

</td>

<td class="gt_row gt_right">

$532.40M

</td>

<td class="gt_row gt_right">

$352.95M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$318.41M

</td>

<td class="gt_row gt_right">

$179.72M

</td>

<td class="gt_row gt_right">

$154.89M

</td>

<td class="gt_row gt_right">

$54.26M

</td>

<td class="gt_row gt_right">

$241.28M

</td>

<td class="gt_row gt_right">

$162.29M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI and
Stimulus)

</td>

<td class="gt_row gt_right">

$149.93M

</td>

<td class="gt_row gt_right">

$71.25M

</td>

<td class="gt_row gt_right">

$49.31M

</td>

<td class="gt_row gt_right">

$7.08M

</td>

<td class="gt_row gt_right">

$111.60M

</td>

<td class="gt_row gt_right">

$73.03M

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
table_nyc %>% 
  filter(str_detect(name, "(Total|Average)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households", 
    subtitle = "New York City - Averages"
  )
```

<!--html_preserve-->

<div id="geztzlwyxh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter
Households

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York City -
Averages

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

734,894

</td>

<td class="gt_row gt_right">

208,730

</td>

<td class="gt_row gt_right">

313,977

</td>

<td class="gt_row gt_right">

142,032

</td>

<td class="gt_row gt_right">

579,707

</td>

<td class="gt_row gt_right">

406,081

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Household Income

</td>

<td class="gt_row gt_right">

$7,820.26

</td>

<td class="gt_row gt_right">

$6,159.79

</td>

<td class="gt_row gt_right">

$3,514.46

</td>

<td class="gt_row gt_right">

$2,117.59

</td>

<td class="gt_row gt_right">

$7,887.03

</td>

<td class="gt_row gt_right">

$7,953.01

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Wages

</td>

<td class="gt_row gt_right">

$7,232.89

</td>

<td class="gt_row gt_right">

$5,482.58

</td>

<td class="gt_row gt_right">

$3,227.98

</td>

<td class="gt_row gt_right">

$1,897.83

</td>

<td class="gt_row gt_right">

$7,304.35

</td>

<td class="gt_row gt_right">

$7,374.72

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Lost Wages

</td>

<td class="gt_row gt_right">

$4,711.76

</td>

<td class="gt_row gt_right">

$2,983.24

</td>

<td class="gt_row gt_right">

$2,495.04

</td>

<td class="gt_row gt_right">

$1,595.57

</td>

<td class="gt_row gt_right">

$4,490.51

</td>

<td class="gt_row gt_right">

$4,264.78

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Gross Rent

</td>

<td class="gt_row gt_right">

$1,672.77

</td>

<td class="gt_row gt_right">

$1,566.65

</td>

<td class="gt_row gt_right">

$1,729.97

</td>

<td class="gt_row gt_right">

$1,718.01

</td>

<td class="gt_row gt_right">

$1,676.52

</td>

<td class="gt_row gt_right">

$1,681.47

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Average Stimulus Benefits

</td>

<td class="gt_row gt_right">

$2,810.74

</td>

<td class="gt_row gt_right">

$2,828.47

</td>

<td class="gt_row gt_right">

$2,837.71

</td>

<td class="gt_row gt_right">

$2,769.88

</td>

<td class="gt_row gt_right">

$2,862.37

</td>

<td class="gt_row gt_right">

$2,924.29

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Regular UI Benefits

</td>

<td class="gt_row gt_right">

$1,151.25

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$799.31

</td>

<td class="gt_row gt_right">

$514.64

</td>

<td class="gt_row gt_right">

$1,098.08

</td>

<td class="gt_row gt_right">

$1,045.54

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1,966.51

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$1,675.57

</td>

<td class="gt_row gt_right">

$1,350.87

</td>

<td class="gt_row gt_right">

$1,875.12

</td>

<td class="gt_row gt_right">

$1,785.43

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$1,184.25

</td>

<td class="gt_row gt_right">

$860.81

</td>

<td class="gt_row gt_right">

$1,432.75

</td>

<td class="gt_row gt_right">

$1,245.16

</td>

<td class="gt_row gt_right">

$1,140.47

</td>

<td class="gt_row gt_right">

$1,094.58

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$560.59

</td>

<td class="gt_row gt_right">

$341.22

</td>

<td class="gt_row gt_right">

$530.14

</td>

<td class="gt_row gt_right">

$240.45

</td>

<td class="gt_row gt_right">

$513.16

</td>

<td class="gt_row gt_right">

$468.23

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$968.32

</td>

<td class="gt_row gt_right">

$860.81

</td>

<td class="gt_row gt_right">

$1,250.24

</td>

<td class="gt_row gt_right">

$999.60

</td>

<td class="gt_row gt_right">

$918.36

</td>

<td class="gt_row gt_right">

$869.12

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$433.25

</td>

<td class="gt_row gt_right">

$860.81

</td>

<td class="gt_row gt_right">

$493.27

</td>

<td class="gt_row gt_right">

$381.93

</td>

<td class="gt_row gt_right">

$416.17

</td>

<td class="gt_row gt_right">

$399.63

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)

</td>

<td class="gt_row gt_right">

$203.99

</td>

<td class="gt_row gt_right">

$341.22

</td>

<td class="gt_row gt_right">

$156.99

</td>

<td class="gt_row gt_right">

$49.84

</td>

<td class="gt_row gt_right">

$192.47

</td>

<td class="gt_row gt_right">

$179.84

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

-----

<br>

### All NYC Affected Renter Households with Incomes below 80% of AMI

``` r
nyc_counties <- c(5, 47, 61, 81, 85)

table_nyc_lt80ami <- seq_len(ITERATIONS) %>% 
  future_map_dfr(
    .f = generate_results, 
    .data = filter(ipums_clean, is_renter, hh_inc_nom < hud_inc_lim80, county %in% nyc_counties)
  )
```

``` r
table_nyc_lt80ami %>% 
  filter(str_detect(name, "(Total|Aggregate)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households with Incomes below 80% of AMI", 
    subtitle = "New York City - Aggregates"
  )
```

<!--html_preserve-->

<div id="qjilosjdim" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter Households with Incomes below 80% of
AMI

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York City -
Aggregates

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

390,971

</td>

<td class="gt_row gt_right">

111,527

</td>

<td class="gt_row gt_right">

273,411

</td>

<td class="gt_row gt_right">

139,706

</td>

<td class="gt_row gt_right">

306,718

</td>

<td class="gt_row gt_right">

212,318

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Household Income

</td>

<td class="gt_row gt_right">

$1.32B

</td>

<td class="gt_row gt_right">

$288.85M

</td>

<td class="gt_row gt_right">

$795.84M

</td>

<td class="gt_row gt_right">

$287.41M

</td>

<td class="gt_row gt_right">

$1.05B

</td>

<td class="gt_row gt_right">

$735.05M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Wages

</td>

<td class="gt_row gt_right">

$1.19B

</td>

<td class="gt_row gt_right">

$235.26M

</td>

<td class="gt_row gt_right">

$723.12M

</td>

<td class="gt_row gt_right">

$256.99M

</td>

<td class="gt_row gt_right">

$947.99M

</td>

<td class="gt_row gt_right">

$664.98M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Lost Wages

</td>

<td class="gt_row gt_right">

$898.06M

</td>

<td class="gt_row gt_right">

$125.19M

</td>

<td class="gt_row gt_right">

$567.57M

</td>

<td class="gt_row gt_right">

$216.83M

</td>

<td class="gt_row gt_right">

$675.89M

</td>

<td class="gt_row gt_right">

$446.92M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Gross Rent

</td>

<td class="gt_row gt_right">

$537.93M

</td>

<td class="gt_row gt_right">

$149.53M

</td>

<td class="gt_row gt_right">

$430.94M

</td>

<td class="gt_row gt_right">

$236.27M

</td>

<td class="gt_row gt_right">

$423.96M

</td>

<td class="gt_row gt_right">

$294.83M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Aggregate Stimulus Benefits

</td>

<td class="gt_row gt_right">

$1.21B

</td>

<td class="gt_row gt_right">

$339.75M

</td>

<td class="gt_row gt_right">

$803.28M

</td>

<td class="gt_row gt_right">

$387.66M

</td>

<td class="gt_row gt_right">

$961.92M

</td>

<td class="gt_row gt_right">

$675.77M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Regular UI Benefits

</td>

<td class="gt_row gt_right">

$338.80M

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$212.53M

</td>

<td class="gt_row gt_right">

$77.86M

</td>

<td class="gt_row gt_right">

$254.76M

</td>

<td class="gt_row gt_right">

$169.09M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$745.56M

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$490.67M

</td>

<td class="gt_row gt_right">

$208.62M

</td>

<td class="gt_row gt_right">

$560.23M

</td>

<td class="gt_row gt_right">

$372.43M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$427.43M

</td>

<td class="gt_row gt_right">

$72.71M

</td>

<td class="gt_row gt_right">

$346.71M

</td>

<td class="gt_row gt_right">

$170.28M

</td>

<td class="gt_row gt_right">

$329.20M

</td>

<td class="gt_row gt_right">

$223.78M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$114.77M

</td>

<td class="gt_row gt_right">

$12.49M

</td>

<td class="gt_row gt_right">

$90.95M

</td>

<td class="gt_row gt_right">

$29.94M

</td>

<td class="gt_row gt_right">

$83.11M

</td>

<td class="gt_row gt_right">

$52.98M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$351.66M

</td>

<td class="gt_row gt_right">

$72.71M

</td>

<td class="gt_row gt_right">

$292.17M

</td>

<td class="gt_row gt_right">

$132.13M

</td>

<td class="gt_row gt_right">

$267.08M

</td>

<td class="gt_row gt_right">

$178.38M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$83.05M

</td>

<td class="gt_row gt_right">

$72.71M

</td>

<td class="gt_row gt_right">

$71.35M

</td>

<td class="gt_row gt_right">

$38.86M

</td>

<td class="gt_row gt_right">

$65.11M

</td>

<td class="gt_row gt_right">

$44.15M

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Aggregate Rental Assistance Need (After Enhanced UI and
Stimulus)

</td>

<td class="gt_row gt_right">

$12.49M

</td>

<td class="gt_row gt_right">

$12.49M

</td>

<td class="gt_row gt_right">

$10.03M

</td>

<td class="gt_row gt_right">

$3.70M

</td>

<td class="gt_row gt_right">

$9.06M

</td>

<td class="gt_row gt_right">

$5.98M

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
table_nyc_lt80ami %>% 
  filter(str_detect(name, "(Total|Average)")) %>% 
  make_table() %>% 
  tab_header(
    title = "Affected Renter Households with Incomes below 80% of AMI", 
    subtitle = "New York City - Averages"
  )
```

<!--html_preserve-->


<div id="jbomwqpnbt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="7" class="gt_heading gt_title gt_font_normal" style>

Affected Renter Households with Incomes below 80% of
AMI

</th>

</tr>

<tr>

<th colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

New York City -
Averages

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, No UI
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Rent
Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, No Return, Severely
Rent Burdened
HHs</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 25%
Return</strong></span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1">

<span class="gt_column_spanner"><strong>UI Job Loss, 50%
Return</strong></span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Estimate

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Total Households

</td>

<td class="gt_row gt_right">

390,971

</td>

<td class="gt_row gt_right">

111,527

</td>

<td class="gt_row gt_right">

273,411

</td>

<td class="gt_row gt_right">

139,706

</td>

<td class="gt_row gt_right">

306,718

</td>

<td class="gt_row gt_right">

212,318

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Household Income

</td>

<td class="gt_row gt_right">

$3,382.38

</td>

<td class="gt_row gt_right">

$2,589.47

</td>

<td class="gt_row gt_right">

$2,910.91

</td>

<td class="gt_row gt_right">

$2,057.39

</td>

<td class="gt_row gt_right">

$3,428.73

</td>

<td class="gt_row gt_right">

$3,462.06

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Wages

</td>

<td class="gt_row gt_right">

$3,040.21

</td>

<td class="gt_row gt_right">

$2,108.84

</td>

<td class="gt_row gt_right">

$2,644.94

</td>

<td class="gt_row gt_right">

$1,839.62

</td>

<td class="gt_row gt_right">

$3,090.86

</td>

<td class="gt_row gt_right">

$3,132.04

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Lost Wages

</td>

<td class="gt_row gt_right">

$2,297.06

</td>

<td class="gt_row gt_right">

$1,121.93

</td>

<td class="gt_row gt_right">

$2,075.98

</td>

<td class="gt_row gt_right">

$1,552.17

</td>

<td class="gt_row gt_right">

$2,203.67

</td>

<td class="gt_row gt_right">

$2,105.00

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Gross Rent

</td>

<td class="gt_row gt_right">

$1,375.89

</td>

<td class="gt_row gt_right">

$1,340.72

</td>

<td class="gt_row gt_right">

$1,576.18

</td>

<td class="gt_row gt_right">

$1,691.21

</td>

<td class="gt_row gt_right">

$1,382.28

</td>

<td class="gt_row gt_right">

$1,388.65

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Average Stimulus Benefits

</td>

<td class="gt_row gt_right">

$3,086.40

</td>

<td class="gt_row gt_right">

$3,046.30

</td>

<td class="gt_row gt_right">

$2,938.03

</td>

<td class="gt_row gt_right">

$2,774.94

</td>

<td class="gt_row gt_right">

$3,136.30

</td>

<td class="gt_row gt_right">

$3,182.90

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Regular UI Benefits

</td>

<td class="gt_row gt_right">

$866.57

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$777.35

</td>

<td class="gt_row gt_right">

$557.31

</td>

<td class="gt_row gt_right">

$830.63

</td>

<td class="gt_row gt_right">

$796.42

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Enhanced UI Benefits

</td>

<td class="gt_row gt_right">

$1,906.97

</td>

<td class="gt_row gt_right">

$0.00

</td>

<td class="gt_row gt_right">

$1,794.61

</td>

<td class="gt_row gt_right">

$1,493.31

</td>

<td class="gt_row gt_right">

$1,826.54

</td>

<td class="gt_row gt_right">

$1,754.18

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (Without Stimulus or UI)

</td>

<td class="gt_row gt_right">

$1,093.27

</td>

<td class="gt_row gt_right">

$651.75

</td>

<td class="gt_row gt_right">

$1,268.13

</td>

<td class="gt_row gt_right">

$1,218.89

</td>

<td class="gt_row gt_right">

$1,073.33

</td>

<td class="gt_row gt_right">

$1,054.02

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Stimulus)

</td>

<td class="gt_row gt_right">

$293.54

</td>

<td class="gt_row gt_right">

$111.89

</td>

<td class="gt_row gt_right">

$332.69

</td>

<td class="gt_row gt_right">

$214.36

</td>

<td class="gt_row gt_right">

$270.93

</td>

<td class="gt_row gt_right">

$249.58

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Regular UI)

</td>

<td class="gt_row gt_right">

$899.46

</td>

<td class="gt_row gt_right">

$651.75

</td>

<td class="gt_row gt_right">

$1,068.65

</td>

<td class="gt_row gt_right">

$945.85

</td>

<td class="gt_row gt_right">

$870.78

</td>

<td class="gt_row gt_right">

$840.16

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI)

</td>

<td class="gt_row gt_right">

$212.43

</td>

<td class="gt_row gt_right">

$651.75

</td>

<td class="gt_row gt_right">

$260.98

</td>

<td class="gt_row gt_right">

$278.16

</td>

<td class="gt_row gt_right">

$212.30

</td>

<td class="gt_row gt_right">

$207.91

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Monthly Average Rental Assistance Need (After Enhanced UI and Stimulus)

</td>

<td class="gt_row gt_right">

$31.95

</td>

<td class="gt_row gt_right">

$111.89

</td>

<td class="gt_row gt_right">

$36.68

</td>

<td class="gt_row gt_right">

$26.49

</td>

<td class="gt_row gt_right">

$29.55

</td>

<td class="gt_row gt_right">

$28.18

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Notes: Assignment of individuals to UI take-up status is done randomly
with a 67% likelihood, accounting for those deemed ineligible based on
income criteria. Assignment of individuals to job loss status is done
randomly with a likelihood based on the industry specific UI claims and
job loss estimates. All of the results show here are the averages of 100
iterations of the estimations.

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

-----

<br>

## Plots

These plots present a few of the key estimates from the above tables.
For NYS and NYC, and for all renter households and just those below 80%
AMI pre-covid, we show the total monthly rental assistance need with and
without the enhanced UI benefits under the CARES
Act.

``` r
plot_keep_types <- c("fc_ui_jobs", "fc_ui_jobs_return25", "fc_ui_jobs_return50")

plot_keep_names <- c(
  "Monthly Aggregate Rental Assistance Need (After Enhanced UI)" = "With Enhanced Benefits",
  "Monthly Aggregate Rental Assistance Need (After Regular UI)" = "Without Enhanced Benefits"
)

plot_source_tables <- list(
  "New York State\nAll Renter Households" = table_nys,
  "New York State\n<80%AMI Renter Households" = table_nys_lt80ami,
  "New York City\nAll Renter Households" = table_nyc,
  "New York City\n<80%AMI Renter Households" = table_nyc_lt80ami
)

plot_data <- plot_source_tables %>% 
  imap_dfr(~{
    .x %>% 
      filter(
        type %in% plot_keep_types,
        name %in% names(plot_keep_names)
      ) %>% 
      transmute(
        group = .y,
        type,
        name = recode(name, !!!plot_keep_names),
        est,
        moe_90
      )
  }) %>% 
  mutate(
    group = ordered(group, levels = names(plot_source_tables)),
    name = ordered(name, levels = plot_keep_names),
  )
```

``` r
p <- plot_data %>% 
  filter(type == "fc_ui_jobs") %>% 
  group_by(group, name) %>% 
  summarise_at(vars(est, moe_90), mean) %>% 
  fc_col_plot_cluster(
    x = group, 
    y = est, 
    fill = name, 
    y_limits = c(0, 1.1e9), 
    ymin = est - moe_90,
    ymax = est + moe_90
  ) +
  scale_fill_manual(values = c("#D1D1D1", "#559C9E")) +
  labs(
    title = "Total Monthly Rental Assistance Need",
    subtitle = "Assuming No Job Recovery",
    y = NULL, x = NULL,
    fill = NULL,
    caption = str_glue(
      "Notes: All results include estimates of households that do not claim UI benefits but are likely to have experienced job loss.
      Error bars represent 90% confidence intervals, and value labels reflect point estimates. 
      
      Sources: American Community Survey (2018) via IPUMS USA, NYS Dept. of Labor, NYU Furman Center"
    )
  )

plot_save_include("img/rental-assistance-need-0-recovery.png")
```

![](img/rental-assistance-need-0-recovery.png)<!-- -->

``` r
p <- plot_data %>% 
  filter(type == "fc_ui_jobs_return25") %>% 
  group_by(group, name) %>% 
  summarise_at(vars(est, moe_90), mean) %>% 
  fc_col_plot_cluster(
    x = group, 
    y = est, 
    fill = name, 
    y_limits = c(0, 1.1e9), 
    ymin = est - moe_90,
    ymax = est + moe_90
  ) +
  scale_fill_manual(values = c("#D1D1D1", "#559C9E")) +
  labs(
    title = "Total Monthly Rental Assistance Need",
    subtitle = "Assuming 25% Job Recovery",
    y = NULL, x = NULL,
    fill = NULL,
    caption = str_glue(
      "Notes: All results include estimates of households that do not claim UI benefits but are likely to have experienced job loss.
      Error bars represent 90% confidence intervals, and value labels reflect point estimates. 
      
      Sources: American Community Survey (2018) via IPUMS USA, NYS Dept. of Labor, NYU Furman Center"
    )
  )

plot_save_include("img/rental-assistance-need-25-recovery.png")
```

![](img/rental-assistance-need-25-recovery.png)<!-- -->

``` r
p <- plot_data %>% 
  filter(type == "fc_ui_jobs_return50") %>% 
  group_by(group, name) %>% 
  summarise_at(vars(est, moe_90), mean) %>% 
  fc_col_plot_cluster(
    x = group, 
    y = est, 
    fill = name, 
    y_limits = c(0, 1.1e9), 
    ymin = est - moe_90,
    ymax = est + moe_90
  ) +
  scale_fill_manual(values = c("#D1D1D1", "#559C9E")) +
  labs(
    title = "Total Monthly Rental Assistance Need",
    subtitle = "Assuming 50% Job Recovery",
    y = NULL, x = NULL,
    fill = NULL,
    caption = str_glue(
      "Notes: All results include estimates of households that do not claim UI benefits but are likely to have experienced job loss.
      Error bars represent 90% confidence intervals, and value labels reflect point estimates. 
      
      Sources: American Community Survey (2018) via IPUMS USA, NYS Dept. of Labor, NYU Furman Center"
    )
  )
  
plot_save_include("img/rental-assistance-need-50-recovery.png")
```

![](img/rental-assistance-need-50-recovery.png)<!-- -->

-----

<br>

### NYS basic stats

``` r
ipums_clean %>% 
  filter(pernum == 1) %>% 
  as_survey_design(weights = "hhwt") %>% 
  summarise(
    hh_renter_num = survey_total(is_renter),
    hh_owner_num = survey_total(!is_renter),
    hh_renter_lt80ami_num = survey_total(is_renter & (hh_inc_nom < hud_inc_lim80)),
    hh_owner_lt80ami_num = survey_total(!is_renter & (hh_inc_nom < hud_inc_lim80))
  ) %>% 
  select(-ends_with("_se")) %>% 
  pivot_longer(everything())
```

    ## # A tibble: 4 x 2
    ##   name                    value
    ##   <chr>                   <dbl>
    ## 1 hh_renter_num         3413871
    ## 2 hh_owner_num          3953014
    ## 3 hh_renter_lt80ami_num 2161441
    ## 4 hh_owner_lt80ami_num  1216448

### NYC basic stats

``` r
ipums_clean %>% 
  filter(pernum == 1) %>% 
  filter(county %in% nyc_counties) %>% 
  as_survey_design(weights = "hhwt") %>% 
  summarise(
    hh_renter_num = survey_total(is_renter),
    hh_owner_num = survey_total(!is_renter),
    hh_renter_lt80ami_num = survey_total(is_renter & (hh_inc_nom < hud_inc_lim80)),
    hh_owner_lt80ami_num = survey_total(!is_renter & (hh_inc_nom < hud_inc_lim80))
  ) %>% 
  select(-ends_with("_se")) %>% 
  pivot_longer(everything())
```

    ## # A tibble: 4 x 2
    ##   name                    value
    ##   <chr>                   <dbl>
    ## 1 hh_renter_num         2141712
    ## 2 hh_owner_num          1042746
    ## 3 hh_renter_lt80ami_num 1329925
    ## 4 hh_owner_lt80ami_num   377162
