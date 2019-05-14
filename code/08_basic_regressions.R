### run regressions not using matching - this allows us to measure intensity

block_groups <- readRDS("./temp/block_group_pre_match.rds")

block_groups$lv2 <- block_groups$lost_voters^2

bg_model  <- lm(to ~ lost_voters +       median_income + nh_black + nh_white + some_college + median_age + share_dem + share_non_citizen + share_winner, data = block_groups)
saveRDS(bg_model, "./temp/bg_model_reg_nosquare.rds")
bg_model2 <- lm(to ~ lost_voters + lv2 + median_income + nh_black + nh_white + some_college + median_age + share_dem + share_non_citizen + share_winner, data = block_groups)
saveRDS(bg_model2, "./temp/bg_model_reg_square.rds")

tracts <- readRDS("./temp/tract_pre_match.rds")
tracts$lv2 <- tracts$lost_voters^2

tract_model  <- lm(to ~ lost_voters +       median_income + nh_black + nh_white + some_college + median_age + share_dem + share_non_citizen + share_winner, data = tracts)
saveRDS(tract_model, "./temp/tract_model_reg_nosquare.rds")
tract_model2 <- lm(to ~ lost_voters + lv2 + median_income + nh_black + nh_white + some_college + median_age + share_dem + share_non_citizen + share_winner, data = tracts)
saveRDS(tract_model2, "./temp/tract_model_reg_square.rds")
