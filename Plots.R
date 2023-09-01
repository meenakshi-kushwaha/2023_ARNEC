
# Load packages -----------------------------------------------------------
pacman::p_load(
  tidyverse,
  rio,
  here,
  plotly
)

# read data ---------------------------------------------------------------
# this downloaded using the GBD data tool by selecting the 
# ARNEC countries individually
# Missed adding North Korea data, so adding that row seperately later
gbd_data <- import(here("data", "IHME_GBD_data_2019.csv" ))


# clean data --------------------------------------------------------------

gbd_rate <- gbd_data %>% 
  filter(metric_name == "Rate") %>% 
  select(location_id, location_name, val) %>% 
  # change country names so match with the world map dataset
  mutate(location_name = 
           case_when(location_name == "Taiwan (Province of China)" ~ "Taiwan",
                     location_name == "Republic of Korea" ~ "South Korea",
                     location_name == "Viet Nam" ~ "Vietnam",
                     location_name == "Lao People's Democratic Republic" ~ "Laos",
                     location_name == "Brunei Darussalam" ~ "Brunei",
                     location_name == "Micronesia (Federated States of)" ~"Micronesia", 
                     TRUE ~ location_name),
  ) 

# add data for North Korea
gbd_rate <- gbd_rate %>% 
  add_row(location_name = "North Korea", val = 32.92)

# isolate countries in dataset
country_names <- gbd_rate %>% 
  pull(location_name) 

# isolate countries within the map
## according to country list above
asia_pacific <- map_data("world", region = country_names)



# join mortality data
asia_pac_mort <- asia_pacific %>% 
  left_join(gbd_rate, join_by(region == location_name))

excluded <- gbd_rate %>% 
  anti_join(asia_pac_mort, join_by(location_name == region))

# create mortality groups - continous to categorical
asia_pac_mort <- asia_pac_mort %>% 
  mutate(val_cat = cut(val, breaks = c(0, 25, 50, 100, 200, 300),
      labels = c("0 to <25", "25 to <50", "50 to <100", "100 to <200", "More than 200")))

# under-5 mortality map ---------------------------------------------------


# categories map - diff color scale
map_4 <- asia_pac_mort %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = val_cat)) +
  geom_polygon(colour = "white") +
  scale_fill_brewer(type = "seq", palette = "RdPu") +
  coord_fixed(xlim = c(60, 170), ylim = c(-45, 54)) +
  labs(fill = "Deaths per 100,000",
       title = "Childhood deaths attributable to air pollution",
       subtitle = "Children under 5 years of age",
       caption = "Data source: GBD 2019") +
  
  theme_minimal(base_size = 13) +
  
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.position = c(0.1, 0.25))

map_4
ggsave(width = 8, height = 10, path = here("plots"), filename = "map5.jpg")


# test code ---------------------------------------------------------------

test <- map_data("world")

ggplot(asia_pacific, aes(x = long, y = lat, group = group)) +
  geom_polygon()
