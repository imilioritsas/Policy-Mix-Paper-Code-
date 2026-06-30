#### data assembly and detection #####

#countries 
#reduced sample using countries with interest rate data available from 1990
#plus POL (1999-2023) & LTU (2001-2023)
Europe<-c("Austria","Belgium", "Denmark","Germany", "Spain","Finland", "France", "Greece", 
          "Italy","Ireland", "Netherlands","Portugal", "Sweden", "United Kingdom",
          "Lithuania", "Poland")
Europe_iso<-c("AUT", "BEL","DNK","DEU","ESP","FIN", "FRA", "GRC","ITA","IRL", "NLD", "PRT", "SWE", "GBR",
              "LTU", "POL")
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

#global wind installation costs - data from IRENA
wind_costs<-read.csv("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/wind_installation_costs_global_2023.csv") %>% 
  select(c(1,3)) %>%
  transmute(year=as.numeric(Year),
            onshore_wind_costs=as.numeric(Weighted.average))

#plot detection
ggplot(wind_costs, aes(x=year, y=onshore_wind_costs)) +
  geom_line() +
  labs(title = "Global onshore wind installation costs", 
       x = "Year", y = "$/KW") +
  theme_minimal()

#plot detection
ggplot(wind_costs, aes(x=year, y=log(onshore_wind_costs))) +
  geom_line() +
  labs(title = "Global onshore wind installation costs", 
       x = "Year", y = "log($/KW)") +
  theme_minimal()

#restrict the installation costs sample to match with the other time series
wind_costs_test <- filter(wind_costs,  year>=1990) 

ggplot(wind_costs_test, aes(x=year, y=onshore_wind_costs)) +
  geom_line() +
  labs(title = "Global onshore wind installation costs", 
       x = "Year", y = "$/KW") +
  theme_minimal()

ggplot(wind_costs_test, aes(x=year, y=log(onshore_wind_costs))) +
  geom_line() +
  labs(title = "Global onshore wind installation costs", 
       x = "Year", y = "log($/KW)") +
  theme_minimal()

#wind capacity 
#use Eurostat data; start from 1990
wind_capacity<-read_xlsx("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/wind_installed_capacity_Eurostat.xlsx") %>% 
  filter(!row_number() %in% c(1:8,10, 54:56)) %>% 
  pivot_longer(cols = 2:35, names_to = "TIME", values_to = "wind.cap") 

year<-wind_capacity$wind.cap[1:34]
year<-as.numeric(year)
wind_capacity<-cbind(wind_capacity, year) 
wind_capacity$TIME<-wind_capacity$year 
wind_capacity<-wind_capacity[-c(1:34),-4]
colnames(wind_capacity)<-c("country", "year", "wind.cap")
wind_capacity$wind.cap<-as.numeric(wind_capacity$wind.cap)

wind_capacity$country<-countrycode(wind_capacity$country, "country.name", "iso3c") 
wind_capacity<-wind_capacity[complete.cases(wind_capacity),] 

wind_capacity<-wind_capacity %>% 
  filter(country %in% Europe_iso) 

#import Uk solar data from IRENA (2020-2023)
# Read the "Country" sheet
irena_data <- read_excel("R:/SFBWISO/RE Deployment/3. To Submit (AIP)/02. Data & Code/0.Data/IRENA_Stats_extract_2025 H2.xlsx", 
                         sheet = "Country") 


# Filter for UK wind data (onshore + offshore)
uk_wind <- irena_data %>%
  filter(
    str_detect(`Country`, regex("United Kingdom", ignore_case = TRUE)) |
      str_detect(`ISO3 code`, regex("^GBR$", ignore_case = TRUE))
  ) %>%
  filter(
    # Match any rows related to wind power in either the Technology or Sub-technology column
    str_detect(`Group Technology`, regex("Wind energy", ignore_case = TRUE)) &
      str_detect(`Technology`, regex("Wind", ignore_case = TRUE)) &
      str_detect(`Sub-Technology`, regex("Wind", ignore_case = TRUE)) &
      str_detect(`Producer Type`, regex("On-grid electricity|Off-grid electricity", ignore_case = TRUE))
  )


#select only relevant columns
uk_wind<-uk_wind %>% 
  select (c("ISO3 code","Year", "Electricity Installed Capacity (MW)" ))

# Aggregate onshore and offshore (sum by year)
uk_wind <- uk_wind %>%
  group_by(`ISO3 code`, Year) %>%
  summarise(
    Installed_Capacity_MW = sum(`Electricity Installed Capacity (MW)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(Year))

#Merging starts here
# Make sure both datasets are numeric and comparable
wind_capacity$year <- as.numeric(wind_capacity$year)
uk_wind <- uk_wind %>% 
  rename("year" = "Year",
         "wind.cap"="Installed_Capacity_MW") 

uk_wind$year<- as.numeric(uk_wind$year)
uk_wind$wind.cap <- as.numeric(uk_wind$wind.cap)

# Filter UK data
uk_euro_wind <- wind_capacity %>%
  filter(country == "GBR") %>%
  arrange(year)

# Take Eurostat up to 2019
uk_eurostat_wind <- uk_euro_wind %>% filter(year <= 2019)

# Calculate growth rates from IRENA
uk_wind <- uk_wind %>%
  arrange(year) %>%
  mutate(growth = wind.cap / shift(wind.cap)) 

# Splice starting from Eurostat 2019 level
last_euro_value <- tail(uk_eurostat_wind$wind.cap, 1)
uk_spliced_wind <- uk_wind %>%
  filter(year >= 2020 & year<2024) %>%
  mutate(
    wind_spliced = NA_real_
  )

# Apply growth-rate splicing
for (i in 1:nrow(uk_spliced_wind)) {
  if (i == 1) {
    uk_spliced_wind$wind_spliced[i] <- last_euro_value * uk_wind$growth[uk_wind$year == 2020]
  } else {
    uk_spliced_wind$wind_spliced[i] <- uk_spliced_wind$wind_spliced[i-1] * uk_wind$growth[uk_wind$year == uk_spliced_wind$year[i]]
  }
}

# Combine Eurostat and spliced IRENA data
uk_wind_final <- uk_eurostat_wind %>%
  select(country, year, wind.cap) %>%
  rename(wind_spliced = wind.cap) %>%
  bind_rows(
    uk_spliced_wind %>%
      transmute(country = "GBR", year, wind_spliced)
  ) %>%
  arrange(year)

# Prepare UK final series with matching column names
uk_wind_final <- uk_wind_final %>%
  rename(wind.cap = wind_spliced)

# Replace UK series in full Eurostat panel
wind_capacity_smooth <- wind_capacity %>%
  filter(country != "GBR") %>%
  bind_rows(uk_wind_final)

#rename smoothed dataset to match the initial one
wind_capacity<-wind_capacity_smooth


pwind_capacity<-pdata.frame(wind_capacity,  index = c("country", "year"))

#plot detection
ggplot(pwind_capacity, aes(x=year, y=wind.cap, group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Wind cumulative installed capacity", 
       x = "Year", y = "MW") +
  theme_minimal()

ggplot(pwind_capacity, aes(x=year, y=log(wind.cap), group=country, color=country)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Wind cumulative installed capacity", 
       x = "Year", y = "log(MW)") +
  theme_minimal()


#merge data
data_test<-merge(ltir, wind_costs_test) %>% 
  merge(wind_capacity) %>% 
  arrange(country, year)  


#### Level Variables
data_test <- as.data.table(data_test)
data_test[, L1.wind.cap:=c(NA, wind.cap[-.N]), by="country"]

####  difs 
data_test[, dL1.wind.cap:= wind.cap-L1.wind.cap, by="country"]


#adjust the dataset to align with the log rules for variables with negative values
min.wind.additions<-min(data_test$dL1.wind.cap, na.rm = T)
min.long.term.int.rates<-min(data_test$long_term_int_rates, na.rm = T)
data_test[ ,wind.additions.adj:=dL1.wind.cap+abs(min.wind.additions) +1, by="country"]
data_test[ ,long.term.int.rates.adj:=long_term_int_rates+abs(min.long.term.int.rates) +1, by="country"]

#use log wind additions as per Polzin et al., (2015)
data_test[, lwind.additions:= log(wind.additions.adj), by="country"]
data_test[, lltir:= log(long.term.int.rates.adj), by="country"]
data_test[, lwind.costs:= log(onshore_wind_costs), by="country"]


#### Output
write.csv(data_test, "WinddepDrivers_dataset.csv", row.names = F)

### clean working directory
rm(wind_capacity,pwind_capacity,year, wind_costs, wind_costs_test, ltir, pltir,
   irena_data, uk_euro_wind, uk_eurostat_wind, uk_spliced_wind, uk_wind,
   uk_wind_final, wind_capacity_smooth)






