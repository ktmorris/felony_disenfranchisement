### post stats

block_groups <- readRDS("./temp/block_group_pre_match.rds")

count_gt_90_black <- sum(block_groups$nh_black > 0.38)
saveRDS(count_gt_90_black, "./temp/count_gt_90_black")

vap_gt_90_black <- mean(filter(block_groups, nh_black > 0.38)$cvap)
saveRDS(vap_gt_90_black, "./temp/vap_gt_90_black")

count_lt_10_black <- sum(block_groups$nh_black > 0.38)
saveRDS(count_gt_90_black, "./temp/count_gt_90_black")

vap_lt_10_black <- mean(filter(block_groups, nh_black < 0.16)$cvap)
saveRDS(vap_lt_10_black, "./temp/vap_lt_10_black")

### to stats

to_bgs <- block_groups %>% 
  filter(nh_black >= quantile(block_groups$nh_black, 0.8) |
           nh_black <= quantile(block_groups$nh_black, 0.2)) %>% 
  mutate(b = nh_black >= quantile(block_groups$nh_black, 0.8))

model_a <- lm(to ~ b +
                median_income + some_college + median_age + 
                reg_rate + share_dem + share_non_citizen + 
                share_winner, weights = cvap,
              data = to_bgs)

model_b <- data.frame(summary(lm_robust(to ~ b * treat,
                                        data = to_bgs, clusters = district, se_type = "stata"))$coefficients)[, 2]