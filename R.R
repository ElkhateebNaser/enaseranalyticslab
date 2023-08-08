# 20230806 - Project Tracker ----

# the data is created from scratch for a memic for project tracker
# A project is two main stages; design,and delivery

# the purpose of this prject is to
# 1- do descriptive analysis:
# understand what is hapening
# show the KPIs
# show the delivery size
# 2- track the team allocation
# test the team workload

# Import Data ----

# setup
if (!requireNamespace("pacman")) {
  install.packages("pacman")
}

pacman::p_load(pacman, tidyverse, lubridate, DT)

# import the data
df <- readxl::read_xlsx("fit_out_tracker_data.xlsx")

# Prepare the data ----
# check data
glimpse(df)

# Rows: 100
# Columns: 15
# $ id                 <chr> "P-26130", "P-763…
# $ assigned_date      <dttm> 2021-03-18, 2020…
# $ project_name       <chr> "project - 4870",…
# $ country            <chr> "Argentina", "Arg…
# $ city               <chr> "Buenos Aires", "…
# $ standard_program   <dbl> 120, 120, 120, 12…
# $ status             <chr> "completed", "com…
# $ kick_off           <dttm> 2021-03-21, 2020…
# $ design_start       <dttm> 2021-03-22, 2020…
# $ design_completed   <dttm> 2021-04-28, 2020…
# $ delivery_start     <dttm> 2021-04-29, 2020…
# $ delivery_completed <dttm> 2021-09-02, 2020…
# $ handover           <dttm> 2021-09-08, 2020…
# $ design_manager     <chr> "Michael Brown", …
# $ delivery_manager   <chr> "Emily Johnson", …


# city, country, status, design_manager, and
# delivery_manager should be categorical
df <- df %>%
  mutate(
    city = as.factor(city),
    country = as.factor(country),
    status = as.factor(status),
    design_manager = as.factor(design_manager),
    delivery_manager = as.factor(delivery_manager)
  )

glimpse(df)

# Rows: 100
# Columns: 15
# $ id                 <chr> "P-26130", "P-76358"…
# $ assigned_date      <dttm> 2021-03-18, 2020-06…
# $ project_name       <chr> "project - 4870", "p…
# $ country            <fct> Argentina, Argentina…
# $ city               <fct> Buenos Aires, Buenos…
# $ standard_program   <dbl> 120, 120, 120, 120, …
# $ status             <fct> completed, completed…
# $ kick_off           <dttm> 2021-03-21, 2020-06…
# $ design_start       <dttm> 2021-03-22, 2020-06…
# $ design_completed   <dttm> 2021-04-28, 2020-07…
# $ delivery_start     <dttm> 2021-04-29, 2020-07…
# $ delivery_completed <dttm> 2021-09-02, 2020-11…
# $ handover           <dttm> 2021-09-08, 2020-11…
# $ design_manager     <fct> Michael Brown, Jane …
# $ delivery_manager   <fct> Emily Johnson, Emily…

# see summary
df %>% summary

# id
# Length:100
# Class :character
# Mode  :character
#
#
#
#
# assigned_date
# Min.   :2020-01-06 00:00:00
# 1st Qu.:2020-11-12 12:00:00
# Median :2021-09-13 12:00:00
# Mean   :2021-09-11 18:43:12
# 3rd Qu.:2022-07-01 18:00:00
# Max.   :2023-07-25 00:00:00
#
# project_name               country
# Length:100         China       :10
# Class :character   France      : 9
# Mode  :character   India       : 9
# Mexico      : 9
# Australia   : 8
# South Africa: 8
# (Other)     :47
# city    standard_program
# Sydney     : 8   Min.   : 90.0
# Madrid     : 5   1st Qu.:120.0
# São Paulo  : 5   Median :120.0
# Guadalajara: 4   Mean   :117.9
# Guangzhou  : 4   3rd Qu.:120.0
# Lyon       : 4   Max.   :120.0
# (Other)    :70
# status      kick_off
# assigned : 1   Min.   :2020-01-09 00:00:00.00
# completed:89   1st Qu.:2020-11-07 00:00:00.00
# delivery : 3   Median :2021-09-17 00:00:00.00
# design   : 6   Mean   :2021-09-07 17:27:16.35
# kick off : 1   3rd Qu.:2022-06-29 00:00:00.00
# Max.   :2023-07-21 00:00:00.00
# NA's   :1
#   design_start
#  Min.   :2020-01-11 00:00:00.00
#  1st Qu.:2020-10-30 18:00:00.00
#  Median :2021-08-30 12:00:00.00
#  Mean   :2021-09-01 18:22:02.45
#  3rd Qu.:2022-06-23 18:00:00.00
#  Max.   :2023-07-23 00:00:00.00
#  NA's   :2
# design_completed
# Min.   :2020-02-14 00:00:00.00
# 1st Qu.:2020-11-28 00:00:00.00
# Median :2021-08-18 00:00:00.00
# Mean   :2021-09-07 23:29:01.94
# 3rd Qu.:2022-06-17 00:00:00.00
# Max.   :2023-09-05 00:00:00.00
# NA's   :7
#  delivery_start
#  Min.   :2020-02-15 00:00:00.00
#  1st Qu.:2020-11-29 00:00:00.00
#  Median :2021-08-21 00:00:00.00
#  Mean   :2021-09-09 12:54:11.60
#  3rd Qu.:2022-06-17 00:00:00.00
#  Max.   :2023-09-05 00:00:00.00
#  NA's   :7
# delivery_completed
# Min.   :2020-06-28 00:00:00
# 1st Qu.:2021-03-11 06:00:00
# Median :2021-11-29 00:00:00
# Mean   :2021-12-11 09:04:00
# 3rd Qu.:2022-08-23 00:00:00
# Max.   :2023-09-25 00:00:00
# NA's   :10
#     handover
#  Min.   :2020-07-04 00:00:00
#  1st Qu.:2021-03-20 12:00:00
#  Median :2021-12-07 00:00:00
#  Mean   :2021-12-18 23:12:00
#  3rd Qu.:2022-08-31 06:00:00
#  Max.   :2023-10-04 00:00:00
#  NA's   :10
# design_manager      delivery_manager
# Jane Smith   :31     Andrew Wilson:33
# John Doe     :35     David Lee    :36
# Michael Brown:34     Emily Johnson:31

# KPIs requirement ----
# 1- workload. how many active projects by design manager
wl <- df %>%
  filter(status != "completed") %>%
  group_by(design_manager) %>%
  summarise(N = n()) %>%
  ungroup()

# add percentage
wl <- wl %>%
  mutate(percentage = N / sum(N) * 100)

cat("The number of projects for each Design Manager is")
wl

# plot active wl
ggplot(wl, aes(x = design_manager, y = N, fill = design_manager)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(N, " (", sprintf("%.1f%%", percentage), ")")),
            vjust = -0.5, size = 3) +
  labs(title = "Design Manager Counts and Percentages",
       x = "Design Manager", y = "Count") +
  theme_minimal()

# how many active project by delivery manager
wl <- df %>%
  filter(status != "completed") %>%
  group_by(delivery_manager) %>%
  summarise(N = n()) %>%
  ungroup()

# add percentage
wl <- wl %>%
  mutate(percentage = N / sum(N) * 100)

cat("The number of projects for each Design Manager is")
wl

# plot active wl
ggplot(wl, aes(x = delivery_manager, y = N, fill = delivery_manager)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(N, " (", sprintf("%.1f%%", percentage), ")")),
            vjust = -0.5, size = 3) +
  labs(title = "Delivery Manager Counts and Percentages",
       x = "Delivery Manager", y = "Count") +
  theme_minimal()

# Who the most active (design-delivery)
wl_teams <- df %>%
  filter(status != "completed") %>% 
  group_by(design_manager, delivery_manager) %>%
  summarise(N = n(), .groups = "drop") 

# draw heatmap plot
ggplot(wl_teams, aes(design_manager, delivery_manager, fill = N)) +
  geom_tile() +
  scale_fill_viridis_c() + 
  labs(title = "Number of Projects by Project Team", x = "Design Manager", y = "Delivery Manager", fill = "Projects Count") +
  theme_minimal()


