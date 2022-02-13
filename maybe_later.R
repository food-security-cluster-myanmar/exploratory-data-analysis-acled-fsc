# this works but is not cumulative
acled %>%
  filter(year == 2021) %>%
  mutate(month = format_ISO8601(event_date, precision = "ym")) %>% 
  unnest(month) %>% 
  filter(inter_type != "sole protester action" & event_type != "Strategic developments") %>% 
  mutate(inter_type = fct_lump(inter_type, 6)) %>%
  full_join(pcode3_shape, by = "admin3_pcode") %>% 
  filter(!is.na(inter_type)) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1, alpha = 0) + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, colour = inter_type, size = fatalities)) +
  scale_colour_manual(values = c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#66A61E", "#E6AB02", "#666666", "#A6761D")) +
  theme_void() + 
  transition_manual(month) +
  labs(title = "Conflict events in { current_frame }",
       subtitle =  "Conflict events included are battles, explosions, riots and violence against civilians",
       caption = "Data source: ACLED (www.acleddata.com)")


acled %>%
  filter(year == 2021) %>%
  mutate(month = format_ISO8601(event_date, precision = "ym")) %>% 
  unnest(month) %>% 
  filter(inter_type != "sole protester action" & event_type != "Strategic developments") %>% 
  mutate(inter_type = fct_lump(inter_type, 6)) %>%
  left_join(townships %>%  select(admin1_pcode, admin3_pcode), by = "admin3_pcode") %>% 
  full_join(pcode1_shape, by = "admin1_pcode") %>%
  filter(!is.na(inter_type)) %>% 
  sample_n(1000) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1, alpha = 0) + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, colour = inter_type, size = fatalities)) +
  scale_colour_manual(values = c("#D95F02", "#7570B3", "#1B9E77", "#E7298A", "#66A61E", "#E6AB02", "#666666", "#A6761D")) +
  theme_void() + 
  transition_states(month, state_length = 0) +
  shadow_mark(past = TRUE, alpha = 1) +
  labs(title = "Conflict events in { current_frame }",
       subtitle =  "Conflict events included are battles, explosions, riots and violence against civilians",
       caption = "Data source: ACLED (www.acleddata.com)")

uganda_gif <- uganda %>% 
  filter(!is.na(install_year)) %>% 
  mutate(install_year = pmax(1990, install_year)) %>% 
  mutate(year = map(install_year, ~seq(., 2021))) %>% 
  unnest(year) %>% 
  ggplot(aes(x = long, y = lat)) +
  borders("world", regions = "Uganda") +
  geom_point(size = 0.1, alpha = 0.25) +
  theme_void() +
  scale_colour_discrete(guide = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  transition_manual(year) + 
  labs(title = "Water sources in Uganda in: { current_frame }")

acled %>%
  filter(year == 2021) %>%
  mutate(month = my(event_date)) %>% 
  unnest(month) %>% 
  filter(inter_type != "sole protester action" & event_type != "Strategic developments") %>% 
  mutate(inter_type = fct_lump(inter_type, 6)) 

acled %>%  select(event_date)



acled %>% 
  filter(year == 2021) %>% 
  group_by(admin3_pcode, event_type) %>%  
  summarise(count = n()) %>%
  pivot_wider(names_from = event_type, values_from = count) %>% 
  rowwise() %>% 
  mutate(total_events = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  left_join(acled %>% 
              filter(year == 2021) %>%  
              group_by(admin3_pcode) %>% 
              summarise(fatalities = sum(fatalities, na.rm = TRUE)), by = "admin3_pcode") %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = Battles), size = 0.1) + 
  scale_fill_gradient(trans = "reverse") + 
  theme_void() +
  labs(title = "Battles per township 2021") +
  
  acled %>% 
  filter(year == 2021) %>% 
  group_by(admin3_pcode, event_type) %>%  
  summarise(count = n()) %>%
  pivot_wider(names_from = event_type, values_from = count) %>% 
  rowwise() %>% 
  mutate(total_events = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  left_join(acled %>% 
              filter(year == 2021) %>%  
              group_by(admin3_pcode) %>% 
              summarise(fatalities = sum(fatalities, na.rm = TRUE)), by = "admin3_pcode") %>% 
  na_if(0) %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = fatalities), size = 0.1) + 
  scale_fill_gradient(trans = "reverse") + 
  theme_void() + 
  labs(title = "Fatalities per township 2021")

acled %>% 
  filter(year == 2021) %>% 
  group_by(admin3_pcode, event_type) %>%  
  summarise(count = n(),
            longitude = median(longitude),
            latitude = median(latitude)) %>%
  pivot_wider(names_from = event_type, values_from = count) %>% 
  rowwise() %>% 
  mutate(total_events = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  left_join(acled %>% 
              filter(year == 2021) %>%  
              group_by(admin3_pcode) %>% 
              summarise(fatalities = sum(fatalities, na.rm = TRUE)), by = "admin3_pcode") %>% 
  full_join(fsc %>%
              filter(unique_beneficiaries == "Yes") %>%
              sum_ben(admin3_pcode)) %>%  
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = beneficiaries), size = 0.1) + 
  geom_point(aes(x = longitude, y = latitude)) +
  scale_fill_viridis_c(option = "mako", direction = -1, trans = "log10") + 
  theme_void() +
  labs(title = "Battles per township 2021")

fsc %>%  
  filter(unique_beneficiaries == "Yes") %>% 
  sum_ben(admin3_pcode)

# I think this is the correct one
acled %>% 
  filter(year == 2021 & event_type %out% c("Protests", "Strategic developments")) %>%
  select()
full_join(fsc %>%
            group_by(admin3_pcode) %>% 
            summarise(partners = n_distinct(implementing_partners)), by = "admin3_pcode") %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = partners), size = 0.1) + 
  geom_point(aes(x = longitude, y = latitude), size = 0.5, alpha = 0.3, colour = "red") +
  scale_fill_viridis_c(option = "mako", direction = -1) + 
  theme_void() +
  labs(title = "Distribution of food security partners and conflict events in 2021",
       subtitle =  "Conflict events included are battles, explosions, riots and violence against civilians",
       caption = "Data sources: ACLED (www.acleddata.com) and Food Security Cluster Myanmar")

ggplotly(conflict_map)

glimpse(acled)

acled %>% 
  filter(year == 2021) %>% 
  rowwise() %>% 
  mutate(actors = sum(!is.na(c_across(c(actor1, assoc_actor_1, actor2, assoc_actor_2))))) %>% 
  ungroup() %>% 
  select(longitude, latitude, admin1, admin3, admin3_pcode, event_type, sub_event_type, actors, fatalities) %>% 
  right_join(pcode3_shape) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1, alpha = 0) + 
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) +
  geom_point(aes(x = longitude, y = latitude, 
                 text = paste0(admin3, ",", "\n",
                               admin1, "\n",
                               "event type: ", event_type, "\n",
                               "sub_event_type: ", sub_event_type, "\n",
                               "actors: ", actors, "\n",
                               "fatalities: ", fatalities)), size = 0.5, alpha = 0.4, colour = "red") +
  theme_void() + 
  labs(title = "Conflict events in 2021",
       subtitle =  "Conflict events included are battles, explosions, riots and violence against civilians",
       caption = "Data source: ACLED (www.acleddata.com)")

# do not run this
# ggplotly(conflict_map, tooltip = c("text")) %>%
# layout(showlegend = FALSE, legend = list(font = list(size = 6))) %>% 
# # plotly::style(hoveron = "fill") %>% # this does make all tooltips appear, but catching the edges is difficult sometimes
# layout(title = list(text = paste0("Conflict events in 2021",
# "<br>",
# "<sup>",
# "mouse over for details; click and drag to select and zoom","</sup>")))

