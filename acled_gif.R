# this gif is crazy to run
# i have no idea how to optimise this
# don't run this anymore -- use the one below 
anim_acled <- acled %>%
  filter(year == 2021) %>% 
  mutate(month = floor_date(event_date, "month")) %>% 
  mutate(month_year = map(month, ~ seq.Date(as.Date(.), as.Date("2021/12/01"), by = "month"))) %>% 
  unnest(month_year) %>% 
  mutate(month = format_ISO8601(month_year, precision = "ym")) %>%
  filter(inter_type != "sole protester action" & event_type != "Strategic developments") %>% 
  mutate(inter_type = fct_lump(inter_type, 6)) %>%
  select(admin3_pcode, data_id, event_type, longitude, latitude, inter_type, month, fatalities) %>% 
  left_join(townships %>%  select(admin1_pcode, admin3_pcode), by = "admin3_pcode") %>% 
  full_join(pcode1_shape, by = "admin1_pcode") %>%
  filter(!is.na(inter_type)) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1, alpha = 0) + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, colour = inter_type, size = fatalities)) +
  scale_colour_manual(values = c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#66A61E", "#E6AB02", "#666666", "#A6761D")) +
  theme_void() + 
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme(legend.text = element_text(size = 11)) + 
  transition_manual(month) +
  labs(title = "Conflict events in { current_frame }",
       subtitle = "Conflict events included are battles, explosions, riots and violence against civilians\nShowing 6 most common interaction types",
       caption = "Data source: ACLED (www.acleddata.com)",
       colour = "interaction type")

# this one works 
anim_acled <- acled %>%
  filter(year == 2021) %>% 
  mutate(month = floor_date(event_date, "month")) %>% 
  mutate(month = map(month, ~ seq.Date(as.Date(.), as.Date("2021/12/01"), by = "month"))) %>% 
  unnest(month) %>% 
  mutate(month = format_ISO8601(month, precision = "ym")) %>%
  filter(inter_type != "sole protester action") %>% 
  mutate(inter_type = fct_lump(inter_type, 6, other_level = "other")) %>% 
  select(admin3_pcode, data_id, event_type, longitude, latitude, inter_type, month, fatalities) %>% 
  left_join(townships %>%  select(admin1_pcode, admin3_pcode), by = "admin3_pcode") %>% 
  ggplot() + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, colour = inter_type, size = fatalities)) +
  scale_colour_manual(values = c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#E6AB02", "#00AFBB", "#666666", "#A6761D", "#66A61E")) +
  theme_void() + 
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1)) +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.2)) + 
  transition_manual(month) +
  labs(title = "Conflict events in { current_frame }",
       subtitle = "Showing 6 most common interaction types\nPeaceful protests have been excluded",
       caption = "Data source: Armed Conflict Location & Event Data Project (ACLED); acleddata.com",
       colour = "interaction type")

animate(anim_acled, width = 1748, height = 2480, res = 150, duration = 22)

anim_save("acled4.gif")
