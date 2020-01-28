

arrests <- fread("./raw_data/NYPD_Arrests_Data__Historic_.csv") %>% 
  filter(LAW_CAT_CD == "F",
         substring(ARREST_DATE, nchar(ARREST_DATE) - 3) == "2017") %>%
  group_by(ARREST_PRECINCT) %>% 
  tally() %>% 
  rename(arrests = n)
  

ps <- readOGR("./raw_data/shapefiles/Police Precincts", "geo_export_3c2a739c-9600-4c5a-b7e1-a9b2010ac598")
ps <- spTransform(ps, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
ps@data$id <- rownames(ps@data)
dists <- fortify(ps)

dists <- left_join(dists, ps@data)

dists <- left_join(dists, arrests, by = c("precinct" = "ARREST_PRECINCT"))

city_map <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank()) +
  geom_polygon(data = dists, aes(x = long, y = lat, group = group, fill = arrests)) +
  geom_path(data = dists, aes(x = long, y = lat, group = group), color = "black") +
  coord_map() +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient2(low = "#ffeda0", mid = "#feb24c", high = "red", limits = c(0, 2000),
                       oob = squish, name = "Felony Arrests in 2017",
                       labels = comma,
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 10))

saveRDS(city_map, "./temp/arrest_map.rds")

ggsave(city_map, file = "./temp/arrest_map.png")