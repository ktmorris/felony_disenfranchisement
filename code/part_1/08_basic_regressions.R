### run regressions not using matching - this allows us to measure intensity

block_groups <- readRDS("./temp/block_group_pre_match.rds")

block_groups$median_income <- block_groups$median_income / 1000

block_groups$lost_voters_black <- block_groups$lost_voters * block_groups$nh_black
block_groups$boro <- substring(block_groups$GEOID, 1, 5)

bg_model  <- lm(to ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups, weights = vap)
bg_model_ses  <- data.frame(summary(lm_robust(to ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups, weights = vap, clusters = district))$coefficients)[, 2]

bg_model2  <- lm(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups, weights = vap)
bg_model2_ses  <- data.frame(summary(lm_robust(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = block_groups, weights = vap, clusters = district))$coefficients)[, 2]
save(bg_model, bg_model_ses, bg_model2, bg_model2_ses, file = "./temp/bg_model_reg_ols.rdata")

#######
tracts <- readRDS("./temp/tract_pre_match.rds")
tracts$median_income <- tracts$median_income / 1000
tracts$lost_voters_black <- tracts$lost_voters * tracts$nh_black
tracts$boro <- substring(tracts$GEOID, 1, 5)

tract_model  <- lm(to ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts, weights = vap)
tract_model_ses  <- data.frame(summary(lm_robust(to ~ lost_voters + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts, weights = vap, clusters = district))$coefficients)[, 2]

tract_model2 <- lm(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts, weights = vap)
tract_model2_ses <- data.frame(summary(lm_robust(to ~ lost_voters + lost_voters_black + median_income + latino + nh_black + nh_white + some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, data = tracts, weights = vap, clusters = district))$coefficients)[, 2]
save(tract_model, tract_model_ses, tract_model2, tract_model2_ses, file = "./temp/tract_model_reg_ols.rdata")