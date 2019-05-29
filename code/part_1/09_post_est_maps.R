
###### maps
tracts <- readRDS("./temp/tract_pre_match.rds")
block_groups <- readRDS("./temp/block_group_pre_match.rds")

tract_shp <- readOGR("./raw_data/shapefiles/nyct2010_19a", "nyct2010")
tract_shp <- spTransform(tract_shp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
tract_shp@data$id <- rownames(tract_shp@data)
temp <- fortify(tract_shp)
tract_shp <- inner_join(temp, tract_shp@data, by = "id")
rm(temp)

codes <- data.frame("BoroCode" = as.character(c(1:5)), "cc" = c("061", "005", "047", "081", "085"))

tract_shp <- left_join(tract_shp, codes, by = "BoroCode")

tract_shp$GEOID <- with(tract_shp, paste0("36", cc, CT2010))

tract_shp <- left_join(tract_shp, tracts, by = "GEOID")

tract_shp$decrease <- -0.022 * tract_shp$lost_voters * tract_shp$nh_black



bg_shp <- readOGR("./raw_data/shapefiles/tl_2018_36_bg", "tl_2018_36_bg")
bg_shp <- spTransform(bg_shp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
bg_shp@data$id <- rownames(bg_shp@data)
temp <- fortify(bg_shp)
bg_shp <- inner_join(temp, bg_shp@data, by = "id")
rm(temp)

bg_shp <- inner_join(bg_shp, block_groups, by = "GEOID")

load("./temp/bg_model_reg_ols.rdata")
coef <- bg_model2[["coefficients"]][["lost_voters_black"]]

bg_shp$decrease <- bg_shp$lost_voters * coef * bg_shp$nh_black

## bbs 
bbs <- readOGR("./raw_data/shapefiles/Borough Boundaries", "geo_export_14dc9d2c-e65a-48c5-ac4d-1bcd90c6758d")
bbs <- spTransform(bbs, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
bbs <- fortify(bbs)

dec_map <- ggplot() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.key=element_blank(),
        legend.key.width = unit(1.5, "cm"),
        text = element_text(family = "LM Roman 10")) +
  geom_polygon(data = tract_shp, aes(x = long, y = lat, group = group), fill = "#bfbfbf") +
  geom_polygon(data = bg_shp, aes(x = long, y = lat, group = group, fill = decrease)) +
  geom_path(data = bbs, aes(x = long, y = lat, group = group), color = "black") +
  coord_map() +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradient(high = "#bfbfbf", low = "red", labels = percent_format(accuracy = 1), limits = c(-0.05, 0), oob = squish) +
  guides(fill = guide_colorbar(title.hjust = .5, title = NULL))

saveRDS(dec_map, "./temp/dec_block_map.rds")