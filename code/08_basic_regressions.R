### run regressions not using matching - this allows us to measure intensity

block_groups <- readRDS("./temp/block_group_pre_match.rds")

block_groups$median_income <- block_groups$median_income / 1000

block_groups$lost_voters_black <- block_groups$lost_voters * block_groups$nh_black

bg_model  <- lm(to ~ lost_voters +                    median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups)
saveRDS(bg_model, "./temp/bg_model_reg_nosquare.rds")
bg_model2 <- lm(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups)
saveRDS(bg_model2, "./temp/bg_model_reg_square.rds")

tracts <- readRDS("./temp/tract_pre_match.rds")
tracts$median_income <- tracts$median_income / 1000
tracts$lost_voters_black <- tracts$lost_voters * tracts$nh_black

tract_model  <- lm(to ~ lost_voters +                     median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts)
saveRDS(tract_model, "./temp/tract_model_reg_nosquare.rds")
tract_model2 <- lm(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts)
saveRDS(tract_model2, "./temp/tract_model_reg_square.rds")
