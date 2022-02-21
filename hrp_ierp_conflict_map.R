pin <- read_csv("pin.csv")

# muatating the gap map column
gaps <- pin %>% 
  left_join(fsc %>%sum_ben(admin3_pcode), by = "admin3_pcode") %>% 
  mutate(pc_reached = beneficiaries / target_2021 * 100,
         hrp_version = ifelse(target_2021 == 0 & beneficiaries > 0, "non_hrp", hrp_version),
         hrp_version = case_when(target_2021 > 0 & beneficiaries == 0 ~ "not_reached",
                                 target_2021 > 0 & is.na(beneficiaries) ~ "not_reached",
                                 TRUE ~ hrp_version)) 

fsc %>%  
  left_join(pin %>% 
              filter(target_2021 > 0) %>%  
              select(target_2021, admin3_pcode), by = "admin3_pcode") %>% 
  filter(unique_beneficiaries == "Yes") %>% 
  group_by(admin3_pcode, hrp_ierp) %>% 
  summarise(beneficiaries = sum(beneficiaries),
            u_ben = sum(u_ben),
            target_2021 = mean(target_2021)) %>%  
  pivot_wider(names_from = hrp_ierp, values_from = beneficiaries) %>% 
  mutate(hrp_version = case_when(hrp > 0 ~ "HRP", 
                                 ierp > 0 ~ "IERP",
                                 non_hrp > 0 ~ "NON_HRP",
                                 TRUE ~ target_2021)) %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = hrp_version), size = 0.1) + 
  geom_sf(data = pcode1_shape, alpha = 0, colour = "black", size = 0.5) +
  geom_point(aes(x = longitude, y = latitude), size = 0.5, alpha = 0.4, colour = "red") + 
  scale_fill_viridis_d() + 
  theme_bw() +
  labs(title = "Distribution of 2021 conflict events across HRP, IERP and non-HRP townships", 
       subtitle = "Red dots are conflict events; peaceful protests have been excluded",
       caption = "Data sources: Armed Conflict Location & Event Data Project (ACLED); acleddata.com and Food Security Cluster Myanmar",
       fill = "HRP\nversion",
       colour = "", size = "") + 
  theme(plot.caption = element_text(hjust = 0.5))

fsc %>%  
  filter(unique_beneficiaries == "Yes") %>% 
  sum_ben(admin3_pcode) %>% 
  full_join(read_excel("Gap Analysis_Summary Data _2021 (1).xlsx", 
                       sheet = "Sheet1") %>% 
              clean_names() %>% 
              select(admin3_pcode = tsp_pcode, response_plan), by = "admin3_pcode") %>%  
  mutate(response_plan = ifelse(is.na(beneficiaries), "Not reached", response_plan)) %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = response_plan), size = 0.1) + 
  geom_sf(data = pcode1_shape, alpha = 0, colour = "black", size = 0.5) +
  geom_point(data = acled %>% filter(year == 2021), aes(x = longitude, y = latitude), size = 0.3, alpha = 0.4, colour = "red") + 
  scale_fill_viridis_d() + 
  theme_bw() +
  labs(title = "Distribution of 2021 conflict events across HRP, IERP and non-HRP townships", 
       subtitle = "Red dots are conflict events; peaceful protests have been excluded",
       caption = "Data sources: Armed Conflict Location & Event Data Project (ACLED); acleddata.com and Food Security Cluster Myanmar",
       fill = "HRP\nversion",
       colour = "", size = "") + 
  theme(plot.caption = element_text(hjust = 0.5))
 

# hrp_ierp_conflict map 
hrp_ierp_conflict_map <- acled %>% 
  filter(year == 2021 & sub_event_type != "Peaceful protests") %>% 
  full_join(gaps %>% 
              select(admin3_pcode, hrp_version), by = "admin3_pcode") %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = hrp_version), size = 0.1) + 
  geom_sf(data = pcode1_shape, alpha = 0, colour = "black", size = 0.5) +
  geom_point(aes(x = longitude, y = latitude), size = 0.5, alpha = 0.4, colour = "red") + 
  scale_fill_viridis_d() + 
  theme_bw() +
  labs(title = "Distribution of 2021 conflict events across HRP, IERP and non-HRP townships", 
       subtitle = "Red dots are conflict events; peaceful protests have been excluded",
       caption = "Data sources: Armed Conflict Location & Event Data Project (ACLED); acleddata.com and Food Security Cluster Myanmar",
       fill = "HRP\nversion",
       colour = "", size = "") + 
  theme(plot.caption = element_text(hjust = 0.5))

ggsave("hrp_ierp_conflict_map.png", hrp_ierp_conflict_map, height = 11, width = 9, units = "in", dpi = 300, device = "png")

