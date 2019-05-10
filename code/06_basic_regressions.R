### run regressions not using matching - this allows us to measure intensity


block_groups <- readRDS("./temp/block_group_pre_match.rds")

block_groups$lv2 <- block_groups$lost_voters^2

bg_model <- lm(to ~ lost_voters + median_income + nh_black + nh_white + some_college + median_age + share_dem, data = block_groups)
bg_model2 <- lm(to ~ lost_voters + lv2 + median_income + nh_black + nh_white + some_college + median_age + share_dem, data = block_groups)
bg_model2 <- lm(to ~ arrests + median_income + nh_black + nh_white + some_college + median_age + share_dem, data = block_groups)

tracts <- readRDS("./temp/tract_pre_match.rds")
tracts$lv2 <- tracts$lost_voters^2
tract_model <- lm(to ~ lost_voters + lv2 + median_income + nh_black + nh_white + some_college + median_age + share_dem, data = tracts)
tract_model2 <- lm(to ~ arrests + median_income + nh_black + nh_white + some_college + median_age + share_dem, data = tracts)