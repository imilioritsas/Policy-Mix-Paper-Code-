#### data assembly and detection #####

#countries with interest rate data available from 2000 
#& excluding MT and CY (mostly rooftop; CYP more than half and MLT almost all is rooftop PV)
Europe<-c("Austria","Belgium", "Denmark","Germany", "Spain", "France", "Greece", 
          "Italy", "Netherlands","Portugal", "Sweden", "United Kingdom",
          "Bulgaria", "Czechia", "Hungary", "Poland",
          "Slovenia", "Finland", "Lithuania", "Estonia")
Europe_iso<-c("AUT", "BEL", "DNK","DEU","ESP","FRA", "GRC","ITA", "NLD", "PRT", "SWE", "GBR",
              "BGR", "CZE","HUN", "POL", "SVN", "FIN", "LTU", "EST")


###### import data #####

#long term nominal interest rates - data from AMECO database from the EC
ltir <- read_csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/long_term_interest_rates.csv") %>%
  # Select relevant columns and convert them to character type
  select(Country, matches("^\\d{4}$")) %>%
  mutate(across(matches("^\\d{4}$"), as.character)) %>%
  mutate(
    Country= case_when(
      TRUE ~ Country            # Keep other codes as they are
    )
  ) %>% 
  # Pivot longer to wider format
  pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "year", values_to = "long_term_int_rates") %>%
  # Convert the 'year' column to numeric and handle missing values
  mutate(year = as.numeric(year),
         long_term_int_rates = as.numeric(long_term_int_rates)) %>% 
  na.omit() %>% 
  filter(Country %in% Europe) %>%
  arrange(Country, year) 

#convert country names to codes
ltir$country<-countrycode(ltir$Country, "country.name", "iso3c") 
#rearrange columns
ltir<-ltir %>% 
  relocate(country, .before = year) %>% 
  select(-c(Country)) %>% 
  arrange(country, year) 

pltir <- pdata.frame(ltir, index = c("country", "year"))

#plot detection
ggplot(pltir, aes(x=year, y=long_term_int_rates, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Long term interest rates by country", 
       x = "Year", y = "Long term interest rates") +
  theme_minimal()

ggplot(pltir, aes(x=year, y=log(long_term_int_rates), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Long term interest rates by country", 
       x = "Year", y = "log(long term interest rates)") +
  theme_minimal() # issue with 0 values; need adjustment - see below

# gdp per capita; data from WB #
gdp_pc<-read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/GDPpc_const_WB.csv") %>% 
  filter(Country.Code %in% Europe_iso) %>% 
  select(c(2, 5:39)) %>% 
  pivot_longer(2:36, names_to = "year") %>%
  transmute(country=Country.Code, year=as.numeric(str_sub(year, 2, 5)),
            gdp_per_capita=as.numeric(value)) %>% 
  na.omit()


pgdp_pc <- pdata.frame(gdp_pc, index = c("country", "year"))

#plot detection
ggplot(pgdp_pc, aes(x=year, y=gdp_per_capita, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "GDP per capita by country", 
       x = "Year", y = "Constant 2015 US$") +
  theme_minimal()

ggplot(pgdp_pc, aes(x=year, y=log(gdp_per_capita), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "GDP per capita by country", 
       x = "Year", y = "log(Constant 2015 US$)") +
  theme_minimal()

# solar installation costs data start 2010
solar_costs<-read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/solar_installation_costs_global.xlsx") %>% 
  select(c(1,3)) %>%
  transmute(year=as.numeric(Year),
            solar_costs=as.numeric(Weighted_average))

#plot detection
ggplot(solar_costs, aes(x=year, y=solar_costs)) +
  geom_line() +
  labs(title = "Global solar installation costs", 
       x = "Year", y = "$/KW") +
  theme_minimal()

#plot detection
ggplot(solar_costs, aes(x=year, y=log(solar_costs))) +
  geom_line() +
  labs(title = "Global solar installation costs", 
       x = "Year", y = "log($/KW)") +
  theme_minimal()

#import US solar installation costs data
# Load US Residential Solar  Data
residential_solar_costs <- read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/National_Residential_Installed Prices (2000-2023).csv") %>% 
  select(c(3,4)) %>%
  transmute(year = as.numeric(Year),
            res_solar_costs = as.numeric(Median) * 1000)  # Convert to $/kW

# Load Small Non-Residential Solar CAPEX Data
small_non_residential_solar_costs <- read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/National_Small Non-Residential_Installed Prices (2000-2023).csv") %>% 
  select(c(3,4)) %>%
  transmute(year = as.numeric(Year),
            small_non_res_solar_costs = as.numeric(Median) * 1000)

# Load Large Non-Residential Solar CAPEX Data
large_non_residential_solar_costs <- read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/National_Large Non-Residential_Installed Prices (2000-2023).csv") %>% 
  select(c(3,4)) %>%
  transmute(year = as.numeric(Year),
            large_non_res_solar_costs = as.numeric(Median) * 1000)

# Merge all US installation cost data
Solar_costs_US <- full_join(residential_solar_costs, small_non_residential_solar_costs, by = "year") %>%
  full_join(large_non_residential_solar_costs, by = "year")


# solar installation cost data start 2010
solar_costs<-read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/solar_installation_costs_global.xlsx") %>% 
  select(c(1,3)) %>%
  transmute(year=as.numeric(Year),
            solar_costs=as.numeric(Weighted_average))

# Merge US and IRENA installation costs data
solar_costs_all <- full_join(Solar_costs_US, solar_costs, by = "year")

plot_solar_costs_all <- ggplot() +
  
  geom_line(data = solar_costs_all, 
            aes(x = year, y = res_solar_costs, color = "Residential Solar installation costs"), 
            size = 1) +
  
  geom_line(data = solar_costs_all,
            aes(x =  year, y = small_non_res_solar_costs, color = "Small Non-Residential Solar Installation Costs"), 
            size = 1) +
  
  geom_line(data = solar_costs_all, 
            aes(x = year, y = large_non_res_solar_costs, color = "Large Non-Residential Solar Installation Costs"), 
            size = 1) +
  
  geom_line(data = solar_costs_all, 
            aes(x = year, y = solar_costs, color = "Global Solar Installation Costs"), 
            size = 1) +
  
  # Manually define legend colors
  scale_color_manual(name = "US Solar Installation Costs", 
                     values = c("Residential Solar Installation Costs" = "black", 
                                "Small Non-Residential Solar Installation Costs" = "blue",
                                "Large Non-Residential Solar Installation Costs" = "grey",
                                "Global Solar Installation Costs" = "red")) +
  
  # Add title and axis labels
  labs(title = "",
       x = "",
       y = "Installation cost (2023 US$ / kW)") +
  
  # Improve theme for readability
  theme_minimal()+
  theme(text=element_text(family="sans", size=18)) +
  guides(fill = guide_legend(reverse = TRUE))

print(plot_solar_costs_all) 

# compute the average ratio of US residential to global installation costs from 2010 
#to rescale the US costs pre -2010; this is to get more valid predicted values 

# Step 1: Compute the Rescaling Ratio (Post-2010)
rescaling_ratio <- solar_costs_all %>%
  filter(year >= 2010, !is.na(solar_costs), !is.na(res_solar_costs)) %>%
  summarize(avg_ratio = mean(solar_costs / res_solar_costs, na.rm = TRUE)) %>%
  pull(avg_ratio)

# Step 2: Apply the Rescaling Factor to the Entire US Residential Installation Costs Series
solar_costs_all <- solar_costs_all %>%
  mutate(res_solar_costs_adjusted = res_solar_costs * rescaling_ratio)  # Adjust ALL years

# Step 3: Plot Original vs Adjusted Installation Costs for Verification
plot_solar_costs_US <- ggplot() +
  geom_line(data = solar_costs_all, aes(x = year, y = res_solar_costs, color = "US Residential Solar Installation Costs"), size = 1) +
  geom_line(data = solar_costs_all, aes(x = year, y = res_solar_costs_adjusted, color = "Adjusted Global Solar Installation Costs"), linetype = "dashed", size = 1) +
  geom_line(data = solar_costs_all, aes(x = year, y = solar_costs, color = "Global Solar Installation Costs"), size = 1) +
  
  scale_color_manual(name = "Installation Costs Comparison", 
                     values = c("US Residential Solar Installation Costs" = "black", 
                                "Adjusted Global Solar Installation Costs" = "red",
                                "Global Solar Installation Costs" = "blue")) +
  
  labs(#title = "Comparison of US Residential and Global Solar Total Installation Costs",
    x = "",
    y = "Installation costs (2023 US$ / kW)") +
  
  theme_minimal()+
  theme(text=element_text(family="sans", size=18)) +
  guides(fill = guide_legend(reverse = TRUE))

print(plot_solar_costs_US)

#save the correlation of the global capex and the proxy we use
#for the overlapping years
solar_costs_correlations_matrix<- c(cor(solar_costs_all$solar_costs, solar_costs_all$res_solar_costs,
                                        use="complete.obs"),
                                    cor(solar_costs_all$solar_costs, solar_costs_all$small_non_res_solar_costs,
                                        use="complete.obs"),
                                    cor(solar_costs_all$solar_costs, solar_costs_all$large_non_res_solar_costs,
                                        use="complete.obs"))

print(solar_costs_correlations_matrix)


#electricity demand - data from OWID taken from Ember & Energy Institute
power_demand<-read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/electricity-demand.csv") %>% 
  filter(Code %in% Europe_iso) %>%
  select(c(2:4)) %>%
  transmute(country=Code, year=as.numeric(Year),
            power_demand=as.numeric(Electricity.demand...TWh))

ppower_demand<-pdata.frame(power_demand, index = c("country", "year"))

#plot detection
ggplot(ppower_demand, aes(x=year, y=power_demand, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Electricity demand by Country", 
       x = "Year", y = "TWh") +
  theme_minimal()

ggplot(ppower_demand, aes(x=year, y=log(power_demand), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Electricity demand by Country", 
       x = "Year", y = "log(TWh)") +
  theme_minimal()

#share of electricity coming from fossil fuels - data from OWID taken from Ember (2025); Energy Institute - Statistical Review of World Energy (2025)
ff_share<-read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/share-electricity-fossil-fuels.csv") %>% 
  filter(Code %in% Europe_iso) %>%
  select(c(2:4)) %>%
  transmute(country=Code, year=as.numeric(Year),
            fossil_fuel_share=as.numeric(Fossil.fuels.....electricity))

pff_share<-pdata.frame(ff_share, index = c("country", "year"))

#plot detection
ggplot(pff_share, aes(x=year, y=fossil_fuel_share, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Electricity from fossil fuels share by Country", 
       x = "Year", y = "%") +
  theme_minimal()


#solar capacity 
#use Eurostat data; start from 2000
solar_capacity<-read_xlsx("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/solar_installed_capacity_Eurostat.xlsx") %>% 
  filter(!row_number() %in% c(1:8,10, 54:56)) %>% 
  pivot_longer(cols = 2:35, names_to = "TIME", values_to = "solar.cap") 

year<-solar_capacity$solar.cap[1:34]
year<-as.numeric(year)
solar_capacity<-cbind(solar_capacity, year) 
solar_capacity$TIME<-solar_capacity$year 
solar_capacity<-solar_capacity[-c(1:34),-4]
colnames(solar_capacity)<-c("country", "year", "solar.cap")
solar_capacity$solar.cap<-as.numeric(solar_capacity$solar.cap)

solar_capacity$country<-countrycode(solar_capacity$country, "country.name", "iso3c") 
solar_capacity<-solar_capacity[complete.cases(solar_capacity),] 

solar_capacity<-solar_capacity %>% 
  filter(country %in% Europe_iso) 

#import UK solar data from IRENA (2020-2023)
# Read the "Country" sheet
irena_data <- read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/IRENA_Stats_extract_2025 H2.xlsx", 
                         sheet = "Country")

# Filter for UK Solar PV (on- and off-grid)
uk_solar <- irena_data %>%
  filter(
    str_detect(`Country`, regex("United Kingdom", ignore_case = TRUE)) |
      str_detect(`ISO3 code`, regex("^GBR$", ignore_case = TRUE))
  ) %>%
  filter(
    str_detect(`Technology`, regex("Solar photovoltaic", ignore_case = TRUE)) &
      str_detect(`Producer Type`, regex("On-grid electricity|Off-grid electricity", ignore_case = TRUE))
  )

#select only relevant columns
uk_solar<-uk_solar %>% 
  select (c("ISO3 code","Year", "Electricity Installed Capacity (MW)" ))

#Merging starts here

# Make sure both datasets are numeric and comparable
solar_capacity$year <- as.numeric(solar_capacity$year)
uk_solar <- uk_solar %>% 
  rename("country"="ISO3 code",
         "year" = "Year",
         "solar.cap"="Electricity Installed Capacity (MW)") 

uk_solar$year<- as.integer(uk_solar$year)
uk_solar$solar.cap <- as.numeric(uk_solar$solar.cap)

# Filter UK data
uk_euro_solar <- solar_capacity %>%
  filter(country == "GBR") %>%
  arrange(year)

# Take Eurostat up to 2019
uk_eurostat_solar <- uk_euro_solar %>% filter(year <= 2019)

# Calculate growth rates from IRENA
uk_solar <- uk_solar %>%
  arrange(year) %>%
  mutate(growth = solar.cap / shift(solar.cap)) 

# Splice starting from Eurostat 2019 level
last_euro_value <- tail(uk_eurostat_solar$solar.cap, 1)
uk_spliced_solar <- uk_solar %>%
  filter(year >= 2020 & year<2024) %>%
  mutate(
    solar_spliced = NA_real_
  )

# Apply growth-rate splicing
for (i in 1:nrow(uk_spliced_solar)) {
  if (i == 1) {
    uk_spliced_solar$solar_spliced[i] <- last_euro_value * uk_solar$growth[uk_solar$year == 2020]
  } else {
    uk_spliced_solar$solar_spliced[i] <- uk_spliced_solar$solar_spliced[i-1] * uk_solar$growth[uk_solar$year == uk_spliced_solar$year[i]]
  }
}

# Combine Eurostat and spliced IRENA data
uk_solar_final <- uk_eurostat_solar %>%
  select(country, year, solar.cap) %>%
  rename(solar_spliced = solar.cap) %>%
  bind_rows(
    uk_spliced_solar %>%
      transmute(country = "GBR", year, solar_spliced)
  ) %>%
  arrange(year)

# Prepare UK final series with matching column names
uk_solar_final <- uk_solar_final %>%
  rename(solar.cap = solar_spliced)

# Replace UK series in full Eurostat panel
solar_capacity_smooth <- solar_capacity %>%
  filter(country != "GBR") %>%
  bind_rows(uk_solar_final)

#rename smoothed dataset to match the initial one
solar_capacity<-solar_capacity_smooth

psolar_capacity<-pdata.frame(solar_capacity,  index = c("country", "year"))

#plot detection
ggplot(psolar_capacity, aes(x=year, y=solar.cap, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Solar cumulative installed capacity", 
       x = "Year", y = "MW") +
  theme_minimal()

ggplot(psolar_capacity, aes(x=year, y=log(solar.cap), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Solar cumulative installed capacity", 
       x = "Year", y = "log(MW)") +
  theme_minimal()


psolar_capacity<-pdata.frame(solar_capacity,  index = c("country", "year"))

#plot detection
ggplot(psolar_capacity, aes(x=year, y=solar.cap, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Solar cumulative installed capacity", 
       x = "Year", y = "MW") +
  theme_minimal()

ggplot(psolar_capacity, aes(x=year, y=log(solar.cap), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Solar cumulative installed capacity", 
       x = "Year", y = "log(MW)") +
  theme_minimal()


#merge data
data_test<-merge(ltir, solar_costs_all) %>% 
  merge(power_demand) %>% 
  merge(ff_share) %>% 
  merge(gdp_pc) %>% 
  merge(solar_capacity) %>% 
  arrange(country, year)  


#### Level Variables
data_test <- as.data.table(data_test)
data_test[, L1.solar.cap:=c(NA, solar.cap[-.N]), by="country"]

####  difs 
data_test[, dL1.solar.cap:= solar.cap-L1.solar.cap, by="country"]


#adjust the dataset to align with the log rules for variables with negative values
min.solar.additions<-min(data_test$dL1.solar.cap, na.rm = T)
min.long.term.int.rates<-min(data_test$long_term_int_rates, na.rm = T)
data_test[ ,solar.additions.adj:=dL1.solar.cap+abs(min.solar.additions) +1, by="country"]
data_test[ ,long.term.int.rates.adj:=long_term_int_rates+abs(min.long.term.int.rates) +1, by="country"]

#use log solar additions as per Polzin et al., (2015)
data_test[, lsolar.additions:= log(solar.additions.adj), by="country"]
data_test[, lltir:= log(long.term.int.rates.adj), by="country"]
data_test[, lsolar.costs.adj:= log(res_solar_costs_adjusted), by="country"]
data_test[, lgdp.pc:= log(gdp_per_capita), by="country"]
data_test[, lpower.demand:=log(power_demand), by="country"]

#### Output
write.csv(data_test, "SolardepDrivers_dataset_robustness_checks_excl_prices.csv", row.names = F)

### clean working directory
rm(ltir, pltir, solar_capacity,psolar_capacity,solar_costs_all, small_non_residential_solar_costs,
   large_non_residential_solar_costs, residential_solar_costs, Solar_costs_US, pgdp_pc,gdp_pc, plot_solar_costs_all,
   plot_solar_costs_US, solar_costs, uk_solar, uk_euro_solar, uk_eurostat_solar, uk_solar_final, uk_spliced_solar,
   solar_capacity_smooth, irena_data, power_demand, ppower_demand, ff_share, pff_share)

# =========================================================
#                  Model Specification 
# =========================================================

set.seed(1230) 

ltitle1="Results_SolarDrivers_Analysis_robustness_checks_excl_prices.txt"

# Prepare Data

data_test <- read.csv("SolardepDrivers_dataset_robustness_checks_excl_prices.csv")
data_test <- as.data.table(data_test)

#correlation matrix
#useful to assess the dependent - independent variables relationships
correlation_matrix<-c(cor(data_test$lsolar.additions, data_test$lltir, use="complete.obs"),
                      cor(data_test$lsolar.additions, data_test$lsolar.costs.adj, use="complete.obs"),
                      cor(data_test$lsolar.additions, data_test$lgdp.pc, use="complete.obs"),
                      cor(data_test$lsolar.additions, data_test$lpower.demand, use="complete.obs"),
                      cor(data_test$lsolar.additions, data_test$fossil_fuel_share, use="complete.obs"))
print(correlation_matrix)

#Analysis:
#solar

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 Solar deployment DRIVERS Europe - ANALYSIS                        # \n",
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
    "#                 Solar DRIVERS Europe - ANALYSIS                     # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = ltitle1
)


# Prepare sample and data
sample<-Europe_iso 
dat_test <- filter(data_test, country %in% sample, year>=2000) 


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
    formula = "lsolar.additions ~ lltir + lgdp.pc + lsolar.costs.adj + lpower.demand + fossil_fuel_share", 
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