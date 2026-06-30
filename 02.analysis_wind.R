# =========================================================
#   Model Specification & Policy Effects Calculations 
# =========================================================

set.seed(1230) 

ltitle1="Results_WindDrivers_Analysis.txt"

# Prepare Data

data_test <- read.csv("WinddepDrivers_dataset.csv")
data_test <- as.data.table(data_test)

#correlation matrix
#useful to assess the dependent - independent variables relationships
correlation_matrix<-c(cor(data_test$lwind.additions, data_test$lltir, use="complete.obs"),
                      cor(data_test$lwind.additions, data_test$lwind.costs, use = "complete.obs"))
print(correlation_matrix)


#Analysis:
#wind 

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 Wind deployment DRIVERS Europe - ANALYSIS                        # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = ltitle1
)

# Analysis


cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 Wind DRIVERS Europe - ANALYSIS                     # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = ltitle1
)

sample<-Europe_iso 
dat_test <- filter(data_test, country %in% sample, year>=1990) 


# Print Sample Header
cat(
  paste0(
    "############################## \n",
    "#  SAMPLE = ", length(sample), " \n",
    "############################## \n",
    "\n \n "),
  file = ltitle1,
  append = T
)

for(p.value in c(.05, .02, .01)){
  
  # Break analysis:
  is <- isatpanel(
    data = dat_test,
    formula = "lwind.additions ~ lltir  + lwind.costs", 
    index = c("country", "year"),
    effect = "individual",
    iis = T,
    tis = T,
    fesis = T, 
    t.pval=p.value,
    plot= T
  )
  
  # Print analysis results
  cat(
    paste0(
      " \n ###########################", 
      " \n # p-value: ", p.value,
      " \n \n "), 
    file = ltitle1, 
    append = T)
  
  sink(ltitle1, append=T)
  print(is)
  sink()
  
  cat(" \n \n \n \n \n", 
      file = ltitle1, 
      append = T)
}


#robust standard errors 
robust_is<-robust_isatpanel(is, HAC = T)
print(robust_is)

#extract results
final_data<-is$finaldata
est_coefficients<-is[["isatpanel.result"]][["mean.results"]]
coef_names<-as.data.frame(row.names(is[["isatpanel.result"]][["mean.results"]]))


#export to excel
openxlsx::write.xlsx(final_data, "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/estimated_data_additions_model_wind.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="estimated_data", startCol="A", startRow=1)


openxlsx::write.xlsx(cbind(coef_names,est_coefficients), "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/estimated_coefficients_additions_model_wind.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="estimated_coefs", startCol="A", startRow=1)

openxlsx::write.xlsx(dat_test, "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/model_data_additions_model_wind.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="model_data", startCol="A", startRow=1)

#Calculate policy contributions
final_data <- read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/estimated_data_additions_model_wind.xlsx")
coefficients <- read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/estimated_coefficients_additions_model_wind.xlsx")

# Rename coefficient columns for consistency
colnames(coefficients) <- c("variable", "coefficient", "st.error", "t.stat", "p.value")

# Extract relevant columns (excluding 'id', 'time', 'y', 'predicted_log_y')
policy_columns <- setdiff(colnames(final_data), c("id", "time", "y", "predicted_log_y"))

# Ensure policy columns and coefficients are numeric
final_data[policy_columns] <- lapply(final_data[policy_columns], as.numeric)
coefficients$coefficient <- as.numeric(coefficients$coefficient)

# Create a mapping of country fixed effects
fixed_effects <- coefficients %>%
  filter(grepl("^id", variable)) %>%
  mutate(id = substr(variable, 3, 5)) %>%
  select(id, fixed_effect = coefficient)

# Merge the fixed effects into final_data by 'id'
final_data <- final_data %>%
  left_join(fixed_effects, by = "id") %>%
  mutate(predicted_log_y = rowSums(
    across(all_of(policy_columns), 
           ~ . * coefficients$coefficient[match(cur_column(), coefficients$variable)], 
           .names = "term_{.col}"),
    na.rm = TRUE
  ) + fixed_effect)  # Add country-specific fixed effect

# Initialize storage for policy effects
policy_effects <- final_data %>% select(id, time)

# Loop through each policy to calculate its effect
for (policy in policy_columns) {
  coef_value <- coefficients$coefficient[match(policy, coefficients$variable)]
  if (is.na(coef_value)) coef_value <- 0  # Prevent NA issues
  
  # Create the counterfactual for each policy
  final_data <- final_data %>%
    mutate(!!paste0("counterfactual_", policy) := predicted_log_y - (.data[[policy]] * coef_value))
  
  # Store the policy effect for this policy using exponentiation first
  policy_effects[[paste0("effect_", policy)]] <- exp(final_data$predicted_log_y+ 0.5 * mean((is1[["isatpanel.result"]][["residuals"]])^2)) - 
    exp(final_data[[paste0("counterfactual_", policy)]]+ 0.5 * mean((is1[["isatpanel.result"]][["residuals"]])^2))
}

# Calculate benchmark_mw
final_data <- final_data %>%
  mutate(benchmark_mw = exp(predicted_log_y + 0.5 * mean((is1[["isatpanel.result"]][["residuals"]])^2)) - 1 - abs(min.wind.additions))

# Pivot the policy effects to a longer format
policy_effects_mw <- policy_effects %>%
  pivot_longer(-c(id, time), names_to = "policy", values_to = "effect_mw") %>% 
  filter(effect_mw != 0)  # Remove zero effects

# Join benchmark_mw to policy_effects_mw
policy_effects_mw <- policy_effects_mw %>%
  left_join(final_data %>% select(id, time, benchmark_mw), by = c("id", "time"))

# Calculate policy contributions as a share of the benchmark model's MW
policy_effects_share <- policy_effects_mw %>%
  mutate(share = ifelse(!is.na(effect_mw) & !is.na(benchmark_mw), 
                        effect_mw / benchmark_mw, 
                        NA_real_)) %>%
  mutate(share = ifelse(effect_mw < 0, -abs(share), abs(share))) %>%
  filter(!is.na(share) & share != 0)  # Remove zero effects

# Compute cumulative policy effects
policy_effects_cumulative <- policy_effects_mw %>%
  filter(!is.na(effect_mw)) %>%
  group_by(id, policy) %>%
  arrange(time) %>%
  mutate(cumulative_effect_mw = cumsum(effect_mw)) %>%
  ungroup()

# Exclude 'lwind.costs' from policy effect data table
policy_effects_mw <- policy_effects_mw %>%
  filter(policy != "effect_lwind.costs")
policy_effects_share <- policy_effects_share %>%
  filter(policy != "effect_lwind.costs")
policy_effects_cumulative <- policy_effects_cumulative %>%
  filter(policy != "effect_lwind.costs")

#Create data tables that compare policy effect with cumulative installed capacity
# Ensure wind.cap is numeric
dat_test <- filter(data_test, country %in% sample, year>=1991) 
dat_test$wind.cap <- as.numeric(dat_test$wind.cap)

# Merge wind capacity data with policy effects
final_data <- final_data %>%
  left_join(dat_test %>% rename(id = country, time = year), by = c("id", "time"))

capacity_data<- final_data %>%
  group_by(id) %>%
  mutate(estimated_cumulative_wind_cap_MW = cumsum(benchmark_mw)) 


# Identify countries with only installation costs and interest rates effects
excluded_countries <- policy_effects_cumulative %>%
  group_by(id) %>%
  summarise(has_other_effects = any(!policy %in% c("effect_lwind.costs", "effect_lltir"), na.rm = TRUE)) %>%
  filter(!has_other_effects) %>%
  pull(id)

# Filter out these countries
policy_effects_cumulative <- policy_effects_cumulative %>%
  filter(!id %in% excluded_countries, !policy %in% c("effect_lwind.costs", "effect_lltir"))
final_data <- final_data %>%
  filter(!id %in% excluded_countries)

#filter out countries with only negative policy effects
countries_with_only_negative_effects <- policy_effects_cumulative %>%
  group_by(id) %>%
  summarise(only_negative = all(cumulative_effect_mw < 0, na.rm = TRUE)) %>%
  filter(only_negative) %>%
  pull(id)

policy_effects_cumulative <- policy_effects_cumulative %>%
  filter(!id %in% countries_with_only_negative_effects)

final_data <- final_data %>%
  filter(!id %in% countries_with_only_negative_effects)

#identify negative policy effects
negative_effects<- policy_effects_cumulative %>%
  group_by(policy) %>%
  summarise(negative_policy_effects = any(cumulative_effect_mw <0, na.rm = TRUE)) %>%
  filter(negative_policy_effects) %>%
  pull(policy)

# Filter out these policies
policy_effects_cumulative <- policy_effects_cumulative %>%
  filter(!policy %in% negative_effects)
final_data <- final_data %>%
  filter(!id %in% excluded_countries)

final_data<- final_data %>%
  group_by(id) %>%
  mutate(estimated_cumulative_wind_cap_MW = cumsum(benchmark_mw))

#identify idiosyncratic effects
#(labeled "iis" in the model specification)
idiosyncratic_effects <- coefficients %>%
  filter(grepl("^(iis)", variable ))%>%
  select(variable, coefficient) %>%
  mutate(variable = paste0("effect_", variable)) %>% 
  mutate(id = gsub("iis)([A-Z]+)\\.\\d+$", "\\2", variable)) %>% 
  pull(variable)

#filter out these effects
policy_effects_cumulative <- policy_effects_cumulative %>%
  filter(!policy %in% idiosyncratic_effects)
final_data <- final_data %>%
  filter(!id %in% excluded_countries)

# Identify the first year where policy effect is non-zero for each country (id)
first_effect_year <- policy_effects_cumulative %>%
  filter(cumulative_effect_mw != 0) %>%
  group_by(id) %>%
  summarise(first_year = min(time), .groups = "drop")  # Get the earliest year for each 'id'

# Identify the first year where multiple policy effects appear for each country
first_multi_effect_year <- policy_effects_cumulative %>%
  group_by(id, time) %>%
  summarise(n_policies = n(), .groups = "drop") %>%  # Count number of policies in each year
  filter(n_policies > 1) %>%  # Keep only years with multiple policy effects
  group_by(id) %>%
  summarise(multi_year = min(time), .groups = "drop")  # Get the earliest year per country

negative_effects <- coefficients %>%
  filter(grepl("^(tis|fesis)", variable) & coefficient < 0) %>%
  select(variable, coefficient) %>%
  mutate(id = gsub("^(tis|fesis)([A-Z]+)\\.\\d+$", "\\2", variable),
         year = as.numeric(gsub("^.*\\.(\\d{4})$", "\\1", variable))) %>% 
  select(c(3,4)) %>% 
  filter(id %in% first_effect_year$id)  # Keep only countries in first_effect_year

#export data to excel 
#estimated policy effects
openxlsx::write.xlsx(policy_effects_cumulative, "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/policy_effects_wind_additions_model.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="policy_contributions_cum", startCol="A", startRow=1)

#raw and estimated data for deployment contributions for all countries 
#estimated cumulative installed capacity in last column of the sheet
openxlsx::write.xlsx(capacity_data, "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/estimated_cumulative_capacity_wind.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="estimated_capacity", startCol="A", startRow=1)

#raw and estimated data for deployment contributions for countries with policy effects only
#estimated cumulative installed capacity in last column of the sheet
openxlsx::write.xlsx(final_data, "R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/policy_contributions_wind_additions_model.xlsx", 
                     asTable = FALSE, Overwrite=TRUE, sheetName="policy_contributions", startCol="A", startRow=1)

