# =========================================================
#   Policy & Non-policy Effects Over Time - Wind (Stacked Area)
# =========================================================
#For running this script, we use the manually created 
#"policy_analysis_wind.xlsx" Excel file;
#we do this to ensure that each individual policy´s characteristics & effects
#are aligned with our general research design on policy effectiveness

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(extrafont)


# File path
file_path <- "/policy_analysis_wind.xlsx" #use own directory here

# Read data
data <- read_excel(file_path, sheet = "effect_quant_by_period")

# Select relevant columns
plot_data <- data %>%
  select(
    time = "time",
    policy_effect = "policy effects and effect duration",
    policy_MW = "error-adjusted total policy effect in MW",
    nonpolicy_MW = "error-adjusted non-policy & unidentified policy effect in MW"
  ) %>%
  mutate(
    policy_GW = policy_MW / 1000,
    nonpolicy_GW = nonpolicy_MW / 1000
  )

# Create policy and non-policy datasets
policy_df <- plot_data %>%
  filter(!is.na(policy_effect)) %>%
  filter(!policy_effect=="-") %>% 
  group_by(time, policy_effect) %>%
  summarise(total_GW = sum(policy_GW, na.rm = TRUE), .groups = "drop")

nonpolicy_df <- plot_data %>%
  group_by(time) %>%
  summarise(total_GW = sum(nonpolicy_GW, na.rm = TRUE), .groups = "drop") %>%
  mutate(policy_effect = "Non-policy effect")

# Combine both datasets
combined_df <- bind_rows(policy_df, nonpolicy_df)
combined_df$time <- as.numeric(combined_df$time)

#Fill missing (time × policy_effect) combinations for smooth areas
combined_df_complete <- combined_df %>%
  complete(time, policy_effect, fill = list(total_GW = 0))

# Determine order: earliest effects first, Non-policy at the bottom
order_df <- combined_df_complete %>%
  group_by(policy_effect) %>%
  summarise(first_year = min(time[total_GW > 0], na.rm = TRUE)) %>%
  arrange(first_year)

# Put Non-policy effect at the top (Inf instead of -Inf)
order_df$order <- ifelse(order_df$policy_effect == "Non-policy effect",
                         Inf, order_df$first_year)

# Reorder the factor by appearance order
combined_df_complete$policy_effect <- factor(
  combined_df_complete$policy_effect,
  levels = order_df$policy_effect[order(order_df$order)]
)

#yearly changes graph creation process
yearly_df <- combined_df_complete %>%
  arrange(policy_effect, time) %>%
  group_by(policy_effect) %>%
  mutate(yearly_GW = total_GW - shift(total_GW)) %>%
  ungroup()

yearly_df <- yearly_df %>%
  mutate(yearly_GW = ifelse(yearly_GW < 0, 0, yearly_GW))

########### instrument clustering graphs ############

# Select relevant columns
plot_data_clusters <- data %>%
  select(
    time = "time",
    policy_effect = "policy_group_name",
    policy_MW = "error-adjusted total policy effect in MW",
    nonpolicy_MW = "error-adjusted non-policy & unidentified policy effect in MW"
  ) %>%
  mutate(
    policy_GW = policy_MW / 1000,
    nonpolicy_GW = nonpolicy_MW / 1000
  )

# Create policy and non-policy datasets
policy_df_clusters <- plot_data_clusters %>%
  filter(!is.na(policy_effect)) %>%
  filter(!policy_effect=="-") %>% 
  group_by(time, policy_effect) %>%
  summarise(total_GW = sum(policy_GW, na.rm = TRUE), .groups = "drop")

nonpolicy_df_clusters <- plot_data_clusters %>%
  group_by(time) %>%
  summarise(total_GW = sum(nonpolicy_GW, na.rm = TRUE), .groups = "drop") %>%
  mutate(policy_effect = "Non-policy effect")

# Combine both datasets
combined_df_clusters <- bind_rows(policy_df_clusters, nonpolicy_df_clusters)
combined_df_clusters$time <- as.numeric(combined_df_clusters$time)

# Fill missing (time × policy_effect) combinations for smooth areas
combined_df_complete_clusters <- combined_df_clusters %>%
  complete(time, policy_effect, fill = list(total_GW = 0))

# Determine order: earliest effects first
order_df_clusters <- combined_df_complete_clusters %>%
  group_by(policy_effect) %>%
  summarise(first_year = min(time[total_GW > 0], na.rm = TRUE)) %>%
  arrange(first_year)

# Put Non-policy effect at the bottom (Inf instead of -Inf)
order_df_clusters$order <- ifelse(order_df_clusters$policy_effect == "Non-policy & unidentified policy effect",
                                  Inf, order_df_clusters$first_year)

# Reorder the factor by appearance order
combined_df_complete_clusters$policy_effect <- factor(
  combined_df_complete_clusters$policy_effect,
  levels = order_df_clusters$policy_effect[order(order_df_clusters$order)]
)

# Plot

# Explicitly assign colors to each policy_effect level
custom_colors <- c(
  "FiT/FiP-based" = "#08306B",                   
  "Other single instrument" = "#253494",    
  "Policy mix" = "#C6DBEF",
  "Tax-based" =  "#2171B5",   
  "Tender-based" = "#6BAED6",
  "TGC-based" = "#41B6C4",
  "Non-policy effect" = "grey"  
)

# Ensure policy_effect is a factor with the correct levels
combined_df_complete_clusters$policy_effect <- factor(
  combined_df_complete_clusters$policy_effect,
  levels = c(
    "FiT/FiP-based",
    "Other single instrument",
    "Policy mix",
    "Tax-based",
    "Tender-based",
    "TGC-based",
    "Non-policy effect"
  )
)

yearly_df_clusters <- combined_df_complete_clusters %>%
  arrange(policy_effect, time) %>%
  group_by(policy_effect) %>%
  mutate(yearly_GW = total_GW - shift(total_GW)) %>%
  ungroup()

yearly_df_clusters <- yearly_df_clusters %>%
  mutate(yearly_GW = ifelse(yearly_GW < 0, 0, yearly_GW))

# Plot yearly changes
ggplot(yearly_df_clusters, aes(x = time, y = yearly_GW, fill = policy_effect)) +
  geom_area(alpha = 0.9, color = "white", size = 0.2) +
  scale_fill_manual(values = custom_colors) +
  labs(
    x = "",
    y = "Installed capacity (GW)",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  theme(text=element_text(family="sans", size=18)) 

#save the data needed for the Figure (Figure 3b)
write.csv(yearly_df_clusters, "yearly_effects_wind.csv", row.names = F)
