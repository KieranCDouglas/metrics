### --- About --- ###
### Project Title: SIEPR Data Task
### Script Name: douglas_kieran_code.R
### Created By: Kieran Douglas
### Last Edited By: Kieran Douglas
### Date: 10/13/2025
### Description: This is an empirical evaluation of the Small Business Training Program and its effect on firm productivity

### --- Package installation and loading data --- ###
install.packages(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich", "lubridate", "ggthemes", "gtsummary","gt", "modelsummary"))
lapply(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich", "lubridate", "ggthemes", "gtsummary", "gt", "modelsummary"), library, character.only = TRUE)
# Change path names for replication
firm_data <- read_csv("/Users/kieran/Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/firm_information.csv")
ag_sales <- read_csv("/Users/kieran/Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/aggregate_firm_sales.csv")
# Since the monthly data was provided across many .csv files, we need to merge them to one workable file.
monthly_files <- list.files(path = "/Users/kieran/Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/monthly_data", pattern = "\\.csv$", full.names = TRUE)
monthly_data <- map_dfr(monthly_files, read_csv)

### --- Data cleaning --- ###
# Due to inconsistencies in the day that each report is made, I am only going to care about year and month and create a new column for each df containing Y/m format.
ag_sales$date <- format(ymd(ag_sales$date), "%Y-%m")
# I noticed issues with dates in the monthly firm data where some were reported as Y/d/m rather than Y/m/d. To generate the new Y/m column I first need to standardize dates so it doesnt generate NA values by coersion.
monthly_data$date_standardized <- parse_date_time(monthly_data$date, orders = c("ymd", "ydm", "dmy", "mdy"))
monthly_data$date <- format(ymd(monthly_data$date_standardized), "%Y-%m")

# Now that I have a formatted Y/m dates column I want to merge each df by firm name and date
# Since only date and ag_sales have both firm id and date, I will conduct a two step merge
merged_step1 <- reduce(list(ag_sales, monthly_data), ~ left_join(.x, .y, by = c("firm_id", "date")))

# The merged df has additional rows which raises red flags, perhaps duplicates are present!
table(duplicated(ag_sales[, c("firm_id", "date")]))
table(duplicated(monthly_data[, c("firm_id", "date")]))

merged_step1 %>%
  group_by(firm_id, date) %>%
  filter(n() > 1) %>%
  distinct(firm_id, date)

# Duplicate rows are present for monthly_data and upon further inspection is looks like some firms reported multiple times in the same month. I am only interested in one report per month so I will keep the one that happened latest. 
merged_step1 <- merged_step1 %>%
  arrange(firm_id, date) %>%
  group_by(firm_id, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Now I am going to merge the firm_data df, including sector and name for each row.
merged_step2 <- merged_step1 %>%
  left_join(firm_data, by = "firm_id")

# Now that everything has been merged, I will conduct some general data cleaning to make everything usable. 
# I first want to decide what to do with missing data. The question of whether to impute or drop will be addressed by determining the nature of the missingness for each variable.  
# Sales
merged_step2$miss_sales_t <- is.na(merged_step2$sales_t)
glm_sales   <- glm(miss_sales_t ~ employment_t + wage_bill_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_sales)
# Employment
merged_step2$miss_employment_t <- is.na(merged_step2$employment_t)
glm_employment   <- glm(miss_employment_t ~ sales_t + wage_bill_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_employment)
# Wage
merged_step2$miss_wage_bill_t <- is.na(merged_step2$wage_bill_t)
miss_wage_bill_t   <- glm(miss_wage_bill_t ~ employment_t + sales_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(miss_wage_bill_t)
# Revenue
merged_step2$miss_revenue_t <- is.na(merged_step2$revenue_t)
glm_revenue   <- glm(miss_revenue_t ~ employment_t + wage_bill_t + sales_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_revenue)

# None of the regressions show statistically significant coeffs. This suggests that the probability of missingness is unrelated to the included observables. 
# This probably means that the NA values are missing completely at random and provides justification for dropping missing values, especially since I will still be left with a large number of observations (n=47696) and am only interested in complete cases.
merged_clean <- merged_step2 %>% 
  na.omit(merged_step2) 
# I am interested in a Two Way Fixed Effects Difference in Differences estimation technique which relies on a few assumptions, one of which I am especially worried about. This is the assumption that treatment effects are homogeneous. The fact that firms seem to join or opt out of the training program at staggered times is cause for concern.
# Because of this I will limit my analysis to firms who either adopted the training in 2013 (the first year the data show people started using it) and those who never adopted it (as my control group).
adoption <- merged_clean %>%
  filter(adopt_t == 1) %>%
  group_by(firm_id) %>%
  summarize(first_adopt_month = min(date), .groups = "drop")

# Extract year from first_adopt_month.
adoption <- adoption %>%
  mutate(first_adopt_year = as.numeric(substr(first_adopt_month, 1, 4)))

# Rejoin merged_clean df. 
merged_clean <- merged_clean %>%
  left_join(adoption, by = "firm_id")

# Filter exclusively for either 2013 program adopters or never adopters using first_adopt_year == 2013 since its already scored as numeric.
filtered_data <- merged_clean %>%
  filter(first_adopt_year == 2013 | is.na(first_adopt_year))

# At this point I have wrangled the data to exclude cases of staggered adoption, included leads for the parallel trends assumption, and merged the separate df by simplified date and firm_id.
# The next step will be to refine the data, generate additional variables, and ensure readiness for analysis. I will convert date to a proper date in standard format by changing the day to 01 for every cell (because again, we really care about month and year). 
# Additionally, since the program is for small businesses with <=100 employees, i will filter out those with more than 100. 
merged_clean <- filtered_data %>%
  select(firm_id, sales_t, date, employment_t, wage_bill_t, revenue_t, adopt_t, firm_name, firm_sector) %>%
  filter(employment_t <= 100) %>%
  group_by(firm_id) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    rev_per_employee = revenue_t / employment_t,
    wagerev_ratio = wage_bill_t / revenue_t,
    salesgrowth = (sales_t - lag(sales_t)) / lag(sales_t),
    revgrowth = (revenue_t - lag(revenue_t)) / lag(revenue_t),
    rev_per_employee_change = (rev_per_employee - lag(rev_per_employee)) / lag(rev_per_employee),
    date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d")
  ) %>%
  ungroup()
  
# I will also create a firm performance index (despite having my qualms with their interpretability) for the sake of exploration and potential added robustness to my analysis. In this index, I only include change variables since firm size can paint a weird picture.
# The index normalizes included variables by z-score and shows the number of standard deviations the firm is from the mean.
index_vars <- c(
  "rev_per_employee_change", "salesgrowth", "revgrowth"
)
merged_clean <- merged_clean %>%
  mutate(across(all_of(index_vars), scale, .names = "{.col}_norm"))
merged_clean <- merged_clean %>%
  rowwise() %>%
  mutate(
    performance_index = mean(
      c_across(ends_with("_norm")),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# I will also create an "ever adopted" variable for assumption checks
firm_adoption_status <- merged_clean %>%
  group_by(firm_id) %>%
  summarize(ever_adopted = as.integer(any(adopt_t == 1)))
merged_clean <- merged_clean %>%
  left_join(firm_adoption_status, by = "firm_id")

### --- Pre-Checks --- ###
# Before my primary analysis, I will check the some assumptions of TWFE DiD
# The first graphic demonstrates similar trends in average revenue growth across adopters and non-adopters, with observable confidence interval overlap.
parallel_check <- merged_clean %>%
  group_by(date, ever_adopted) %>%
  summarize(mean_revgrowth = mean(revgrowth, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(date, ever_adopted, fill = list(mean_revgrowth = NA)) %>% 
  mutate(mean_revgrowth = mean_revgrowth*100)

ggplot(parallel_check, aes(x = date, y = mean_revgrowth, color = factor(ever_adopted, labels = c("Not Adopted", "Adopted")), group = ever_adopted)) +
  geom_smooth(size = 1.6) +
  labs(title = "Parallel Trends Assumption Check 1", color = "Adoption Status") +
  theme_fivethirtyeight() +
  xlim(as.Date("2010-01-01"), as.Date("2012-12-31")) +
  ylim(0,10) +
  scale_color_manual(values = c("Not Adopted" = "#D4AFB9", "Adopted" = "#7EC4CF"))
# The second graphic demonstrates an earnings gap between adopters and non-adopters prior to the start of the adoption period, but similar trajectory nonetheless.
parallel_check <- merged_clean %>%
  group_by(date, ever_adopted) %>%
  summarize(salesmed = median(sales_t, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(date, ever_adopted, fill = list(salesmed = NA)) 

ggplot(parallel_check, aes(x = date, y = salesmed, color = factor(ever_adopted, labels = c("Not Adopted", "Adopted")), group = ever_adopted)) +
  geom_smooth(size = 1.6) +
  labs(title = "Parallel Trends Assumption Check 2", color = "Adoption Status") +
  theme_fivethirtyeight() +
  xlim(as.Date("2010-01-01"), as.Date("2012-12-31")) +
  scale_color_manual(values = c("Not Adopted" = "#D4AFB9", "Adopted" = "#7EC4CF"))


# To supplement the imagery, I will run a pre-trend linear regression. Here, I am interested in trends over time with the interaction term between time and whether or not the firm was at some point treated.
# To do this, I created a pre-2013 df and regressed an interaction term between date and adoption on each potential outcome variable of interest.
# The general lack of statistically significant coefficients indicates that the parallel trends assumption holds for these data. 
pre_df <- merged_clean %>% 
  filter(date < as.Date("2012-12-31"))

pre_df <- pre_df %>% 
  mutate(year = year(date), 
         month = month(date), 
         date_pre = format(date, "%Y-%m"))
# Revenue
revenue_pretrend <- lm(revenue_t ~ factor(date_pre) * ever_adopted, data = pre_df)
summary(revenue_pretrend)
# Revenue growth
revgrowth_pretrend <- lm(revgrowth ~ factor(date_pre) * ever_adopted, data = pre_df)
summary(revgrowth_pretrend)
# Performance index
index_pretrend <- lm(performance_index ~ factor(date_pre) * ever_adopted, data = pre_df)
summary(index_pretrend)
# Wage/revenue ratio
wagerev_pretrend <- lm(wagerev_ratio ~ factor(date_pre) * ever_adopted, data = pre_df)
summary(wagerev_pretrend)

### --- Analysis --- ###
# Now that assumptions have been validated, it is time to run the main Two Way Fixed Effects Difference in Differences model for this analysis.
# The main outcome of interest is a gold standard measure of firm productivity:revenue per employee. My initial concern with using this variable was obscured findings due to firm characteristics affecting base level revenues, but fixed effects should take care of this!
# Using rev_per_employee as my primary outcome of interest, I regress adoption status on it controlling for fixed effects through firm_id and date using clustered standard errors at the firm level 
model_revperemp <- feols(data = merged_clean, rev_per_employee ~ adopt_t | firm_id + date, cluster = ~firm_id)
summary(model_revperemp)
# For the sake of robustness I will also run an OLS regression to see if findings hold (they do).
model_revperemp_ols <- lm(data = merged_clean, rev_per_employee ~ adopt_t + firm_id + firm_sector + date)
summary(model_revperemp_ols)

etable(model_revperemp)

# Aside from my primary outcome of interest, I am going to explore a few other relationships to hopefully strengthen my case.
# To start, I will regress the treatment on my performance index (obviously controlling for all the other stuff).
model_performanceindex <- feols(data = merged_clean, performance_index ~ adopt_t | firm_id + date, cluster =  ~firm_id)
summary(model_performanceindex)
etable(model_performanceindex)
# I am also interested to see if opting in causes firms to change their employment numbers.
# The answer to this seems to be no!
model_employ <- feols(data = merged_clean, employment_t ~ adopt_t | firm_id + date, cluster =  ~firm_id)
summary(model_employ)
etable(model_employ)
# On the subject of employment, I will explore how the jobs training program may affect wages.
# seems like jobs training programs lead to increased cost of labor (a=0.001). This is interesting! 
model_wage <- feols(data = merged_clean, wage_bill_t ~ adopt_t | firm_id + date, cluster =  ~firm_id)
summary(model_wage)
etable(model_wage)
# I will now make a few graphics to demonstrate general characteristics of the data in addition to visualizations of the effect that the jobs training program had on business outcomes.
# Start by factoring adoption status for interpretability
merged_clean$ever_adopted_factor <- factor(
  merged_clean$ever_adopted,
  levels = c(0, 1),
  labels = c("Not Adopted", "Adopted"))
# Create a df for graphics with modified variables to help with interpretation.
graphics <- merged_clean %>%
  group_by(date, ever_adopted) %>%
  summarize(
    mean_perempgrow = mean(rev_per_employee, na.rm = TRUE),
    mean_employment = mean(employment_t, na.rm = TRUE),
    mean_perfindex = mean(performance_index, na.rm = TRUE),
    mean_salesgrowth = mean(salesgrowth, na.rm = TRUE),
    .groups = "drop") %>%
  complete(date, ever_adopted, fill = list(
    mean_perempgrow = NA,
    mean_employment = NA,
    mean_perfindex = NA,
    mean_salesgrowth = NA)) %>%
  mutate(
    mean_perempgrow = mean_perempgrow * 100,
    ever_adopted_factor = factor(
      ever_adopted,
      levels = c(0, 1),
      labels = c("Not Adopted", "Adopted")))
# Now make some graphs!
# Create a graphic showing time trends of revenue per employee by adoption status
ggplot(data = merged_clean, mapping = aes(x = date, y = rev_per_employee, color = ever_adopted_factor, group = ever_adopted_factor)) +
  geom_smooth() +
  facet_wrap(~ ever_adopted_factor) +
  scale_color_manual(values = c("Not Adopted" = "#D4AFB9", "Adopted" = "#7EC4CF")) +
  theme_fivethirtyeight() +
  labs(title = "Revenue Per Employee By Adoption Status", x = "Date", y = "revenue Per Employee", color = "Ever Adopted") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", color = "#9CADCE", size = .5) 
  
ggplot(
  data = graphics, 
  mapping = aes(x = date, y = mean_perfindex, color = ever_adopted_factor, group = ever_adopted_factor)) +
  geom_smooth() +
  facet_wrap(~ ever_adopted_factor) +
  scale_color_manual(values = c("Not Adopted" = "#D4AFB9", "Adopted" = "#7EC4CF")) +
  theme_fivethirtyeight() +
  labs(title = "Average Performance Index By Adoption Status",x = "Date", y = "Performance Index (Z-Score)", color = "Ever Adopted") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", color = "#9CADCE", size = .5) 

# Now I will generate a table of summary statistics for my paper.
# First create df for pre and post 2013 to compare.
df_pre2013 <- merged_clean %>%
  filter(date < "2013-01-01") %>%
  select(date, ever_adopted_factor, employment_t, wage_bill_t, rev_per_employee, revgrowth, firm_id)
df_post2013 <- merged_clean %>%
  filter(date > "2013-01-01") %>%
  select(date, ever_adopted_factor, employment_t, wage_bill_t, rev_per_employee, revgrowth, firm_id)

# Create pre 2013 table.
tbl_pre2013 <- df_pre2013 %>%
  select(ever_adopted_factor, employment_t, wage_bill_t, rev_per_employee) %>%
  tbl_summary(
    by = ever_adopted_factor,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      employment_t = "Number Employed",
      wage_bill_t = "Labor Cost",
      rev_per_employee = "Revenue per Employee"
    )
  ) 
tbl_pre2013

# Formatted with GT:
tbl_pre2013_gt <- tbl_pre2013 %>%
  as_gt() %>%
  tab_header(
    title = "Pre-2013 Firm-Level Summary Statistics"
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()
  ) %>%
  tab_options(
    table.border.top.color = "#7EC4CF",
    table.border.bottom.color = "#7EC4CF",
    heading.background.color = "#D1CFE2"
  )
tbl_pre2013_gt

# Create post 2013 table.
tbl_post2013 <- df_post2013 %>%
  select(ever_adopted_factor, employment_t, wage_bill_t, rev_per_employee) %>%
  tbl_summary(
    by = ever_adopted_factor,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    label = list(
      employment_t = "Number Employed",
      wage_bill_t = "Labor Cost",
      rev_per_employee = "Revenue per Employee"
    ))
# Formatted with GT:
tbl_post2013_gt <- tbl_post2013 %>%
  as_gt() %>%
  tab_header(
    title = "Post-2013 Firm-Level Summary Statistics"
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()
  ) %>%
  tab_options(
    table.border.top.color = "#7EC4CF",
    table.border.bottom.color = "#7EC4CF",
    heading.background.color = "#D1CFE2"
  )
tbl_post2013_gt

# I think that's about it! 













