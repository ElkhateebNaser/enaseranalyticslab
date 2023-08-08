# 20230806 - Project Tracker Analysis ----

# Load necessary libraries ----

# Check if the 'pacman' package is available; if not, install it
if (!requireNamespace("pacman")) {
  install.packages("pacman")
}

# Load required packages using the 'pacman' package
pacman::p_load(pacman, tidyverse, lubridate, DT)

# Import Data ----

# Read project tracker data from an Excel file
df <- readxl::read_xlsx("fit_out_tracker_data.xlsx")

# Prepare the data ----

# Check the structure of the imported data
glimpse(df)

# Convert specific columns to categorical factors for better analysis
df <- df %>%
  mutate(
    city = as.factor(city),
    country = as.factor(country),
    status = as.factor(status),
    design_manager = as.factor(design_manager),
    delivery_manager = as.factor(delivery_manager)
  )

# Display the updated data structure
glimpse(df)

# Explore the summary statistics for various columns
df_summary <- summary(df)

# Display summary statistics for each column
print(df_summary)

# Team Workload Analysis ----

# Calculate the workload for design managers
design_manager_workload <- df %>%
  filter(status != "completed") %>%
  group_by(design_manager) %>%
  summarise(Active_Projects = n()) %>%
  mutate(Percentage = Active_Projects / sum(Active_Projects) * 100)

# Display the workload for design managers
cat("Workload analysis for Design Managers:")
print(design_manager_workload)

# Plot a bar chart for design manager workload
ggplot(design_manager_workload, aes(x = design_manager, y = Active_Projects, fill = design_manager)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Active_Projects, " (", sprintf("%.1f%%", Percentage), ")")),
            vjust = -0.5, size = 3) +
  labs(title = "Design Manager Workload Analysis",
       x = "Design Manager", y = "Number of Active Projects") +
  theme_minimal()

# Calculate the workload for delivery managers
delivery_manager_workload <- df %>%
  filter(status != "completed") %>%
  group_by(delivery_manager) %>%
  summarise(Active_Projects = n()) %>%
  mutate(Percentage = Active_Projects / sum(Active_Projects) * 100)

# Display the workload for delivery managers
cat("Workload analysis for Delivery Managers:")
print(delivery_manager_workload)

# Plot a bar chart for delivery manager workload
ggplot(delivery_manager_workload, aes(x = delivery_manager, y = Active_Projects, fill = delivery_manager)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Active_Projects, " (", sprintf("%.1f%%", Percentage), ")")),
            vjust = -0.5, size = 3) +
  labs(title = "Delivery Manager Workload Analysis",
       x = "Delivery Manager", y = "Number of Active Projects") +
  theme_minimal()

# Analyze team workload relationships ----

# Calculate the number of active projects for each design and delivery manager pair
team_workload_analysis <- df %>%
  filter(status != "completed") %>% 
  group_by(design_manager, delivery_manager) %>%
  summarise(Active_Projects = n(), .groups = "drop") 

# Create a heatmap to visualize the team workload analysis
ggplot(team_workload_analysis, aes(design_manager, delivery_manager, fill = Active_Projects)) +
  geom_tile() +
  scale_fill_viridis_c() + 
  labs(title = "Team Workload Analysis",
       x = "Design Manager", y = "Delivery Manager", fill = "Number of Active Projects") +
  theme_minimal()

# Design Managers Workload Distribution by Country

# Calculate the average workload per country for design managers
design_manager_location_workload <- df %>%
  filter(status != "completed") %>%
  group_by(design_manager, country) %>%
  summarise(Active_Projects = n(), .groups = "drop") 

# Create a heatmap to visualize the workload distribution by location for design managers
ggplot(design_manager_location_workload, aes(x = country, y = design_manager, fill = Active_Projects)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Design Manager Workload Distribution by Location",
       x = "Country", y = "Design Manager", fill = "Number of Active Projects") +
  theme_minimal()

# delivery Managers Workload Distribution by Country

# Calculate the average workload per country for delivery managers
delivery_manager_location_workload <- df %>%
  filter(status != "completed") %>%
  group_by(delivery_manager, country) %>%
  summarise(Active_Projects = n(), .groups = "drop") 

# Create a heatmap to visualize the workload distribution by location for delivery managers
ggplot(delivery_manager_location_workload, aes(x = country, y = delivery_manager, fill = Active_Projects)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "delivery Manager Workload Distribution by Location",
       x = "Country", y = "delivery Manager", fill = "Number of Active Projects") +
  theme_minimal()
