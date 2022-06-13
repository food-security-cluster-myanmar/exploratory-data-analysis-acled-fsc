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


actors %>%
  filter(year == 2021) %>% 
  group_by(actor_simple, event_type, inter1) %>% 
  summarise(count = n(),
            fatalities = sum(fatalities)) %>% 
  group_by(event_type) %>% 
  mutate(pc_fatalities = fatalities / sum(fatalities)) %>% 
  ggplot(aes(x = fatalities, y = inter1, fill = inter1)) +
  geom_col() +
  scale_x_continuous(labels = comma_format(accuracy = 1)) +
  scale_fill_viridis_d() +
  # scale_fill_brewer(palette = "Dark2", direction = -1) + 
  facet_wrap(~ event_type, scales = "free_x") + 
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Number of fatalities associated with actor type", 
       y = "Type of actor", 
       title = "Number of fatalities associated with actor type",
       subtitle = "Faceted by type of conflict event",
       caption = "Data source: Armed Conflict Location & Event Data Project (ACLED); acleddata.com")


acled %>% filter(str_detect(admin1, "Shan") | admin1 %in% c("Kayah", "Kayin", "Mon", "Tanintharyi")) %>% 
  filter(inter_type != "sole protester action" & year == 2021) %>% 
  mutate(inter_type = fct_lump(inter_type, 6, other_level = "other")) %>%
  full_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(size = 0.1, alpha = 0) +
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) + 
  geom_point(aes(x = longitude, y = latitude, size = fatalities, colour = inter_type)) +
  theme_void() + 
  labs(title = "Conflict events by township 2021",
       subtitle = "Peaceful protests have been excluded",
       caption = "Data source: ACLED; acleddata.com") +
  theme(plot.caption=element_text(hjust = 0.2))

conflict_score %>% 
  ggplot(aes(x = score_i, y = score_env)) + 
  geom_point() +
  geom_smooth(method = "lm")

conflict_score %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = score_i), size = 0.1) +
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) + 
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  theme_void() +
  labs(title = "Score_i") +
  theme(plot.caption=element_text(hjust = 0.2)) +
  
  conflict_score %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = score_env), size = 0.1) +
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) + 
  scale_fill_viridis_c(option = "plasma", direction = -1) + 
  theme_void() +
  labs(title = "Score_env") +
  theme(plot.caption=element_text(hjust = 0.2))


fsc %>%
  group_by(admin3_pcode) %>%
  summarise(partners = n_distinct(implementing_partners), by = "admin3_pcode") %>%
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = partners), size = 0.1) +
  geom_sf(data = pcode1_shape, size = 0.5, colour = "black", alpha = 0) + 
  scale_fill_viridis_c(option = "mako", direction = -1, breaks = c(1, 3, 6, 9, 12)) + 
  theme_void() +
  labs(title = "Distribution of food security partners in 2021",
       caption = "Data source: Food Security Cluster Myanmar",
       fill = "number of\npartners") +
  theme(plot.caption=element_text(hjust = 0.2)) 

# table-sr-events-fatalities-sown-area
# replaced by treemap
acled %>% filter(year == 2021) %>%
  filter(sub_event_type != "Peaceful protest") %>% 
  # filter(event_type == "Explosions/Remote violence") %>%
  group_by(admin3_pcode) %>% 
  summarise(events = n(),
            fatalities = sum(fatalities, na.rm = TRUE)) %>% 
  full_join(vulmmr %>%
              select(state_region_pcode, admin3_pcode = township_pcode,
                     contains("mali"), contains("sown_area"), 
                     all_harvested_net_margin_usd, sown_area_of_paddy_acres) %>%
              mutate(pc_paddy = sown_area_of_paddy_acres / all_area_sowed_mali), by = "admin3_pcode") %>%
  left_join(townships %>% select(state_name, admin3_pcode), by = "admin3_pcode") %>%
  filter(!is.na(state_name)) %>% 
  group_by(state_name) %>% 
  summarise(area_sown = sum(all_area_sowed_mali, na.rm = TRUE),
            events = n(), 
            fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop") %>% 
  adorn_percentages("col") %>%   
  mutate(state_name = factor(state_name,
                             levels = c("Sagaing", "Magway", "Chin", "Mandalay", "Kachin", "Kayin", "Kayah", "Shan (North)", "Yangon",
                                        "Mon", "Shan (South)", "Bago (East)", "Tanintharyi", "Ayeyarwady", "Bago (West)", "Nay Pyi Taw",
                                        "Rakhine", "Shan (East)")),
         state_name = fct_rev(state_name)) %>% 
  pivot_longer(cols = c(area_sown, events, fatalities), names_to = "type", values_to = "value") %>% 
  filter(state_name != "Nay Pyi Taw") %>% 
  ggplot(aes(x = state_name, y = value, fill = type)) +
  geom_col(position = "dodge") +  
  scale_fill_manual(values = c("#39b600", "#9590ff", "#f8766d")) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.7, hjust = 0.7)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) + 
  labs(x = "",
       y = "Percentage of total",
       fill = "",
       title = "Conflict events, conflict fatalities and area sown",
       subtitle = "Values show a state's percentage of the total",
       caption = "Data source: ACLED, acleddata.com (2022); Ministry of Agriculture and Irrigation (2015)")

# replaced with new table on actor pair fatalities
actors %>% 
  filter(year == 2021) %>% 
  group_by(actor_simple) %>% 
  summarise(fatalities = sum(fatalities),
            events = n()) %>% 
  mutate(actor_simple = ifelse(fatalities >= 100, actor_simple, "Other")) %>% 
  group_by(actor_simple) %>% 
  summarise(fatalities = sum(fatalities),
            events = sum(events)) %>%
  left_join(actors %>% select(actor_simple, inter1) %>% distinct(), by = "actor_simple") %>% 
  mutate(actor_simple = str_remove(actor_simple, "\\:.*"),
         fatalities_per_event = round(fatalities / events, digits = 2)) %>% 
  select(actor = actor_simple, actor_type = inter1, fatalities, events, fatalities_per_event) %>% 
  arrange(desc(fatalities)) %>% 
  kable(caption = "Top 2021 actors in terms of fatalities; actors associated with less than 100 fatalities have been lumped together as 'Other'",
        format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped") %>% 
  footnote(general = "Data source: Armed Conflict Location & Event Data Project (ACLED); acleddata.com", 
           general_title = "")

