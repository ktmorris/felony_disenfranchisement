### run regressions not using matching - this allows us to measure intensity

block_groups <- readRDS("./temp/block_group_pre_match.rds")

block_groups$median_income <- block_groups$median_income / 1000

block_groups$lost_voters_black <- block_groups$lost_voters * block_groups$nh_black
block_groups$lost_voters_latino <- block_groups$lost_voters * block_groups$latino
block_groups$boro <- substring(block_groups$GEOID, 1, 5)

bg_model  <- lm(to ~ lost_voters +
                  median_income + latino + nh_black + nh_white +
                  some_college + median_age + reg_rate + share_dem +
                  share_non_citizen + share_winner,
                data = block_groups)

bg_model_ses  <- data.frame(
  summary(lm_robust(to ~ lost_voters +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem +
                      share_non_citizen + share_winner,
                    data = block_groups, clusters = district, se_type = "stata"))$coefficients)[, 2]

bg_model2  <- lm(to ~ lost_voters + lost_voters_black +
                   median_income + latino + nh_black + nh_white +
                   some_college + median_age + reg_rate + share_dem +
                   share_non_citizen + share_winner,
                 data = block_groups)

bg_model2_ses  <- data.frame(
  summary(lm_robust(to ~ lost_voters + lost_voters_black +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem +
                      share_non_citizen + share_winner,
                    data = block_groups, clusters = district, se_type = "stata"))$coefficients)[, 2]

save(bg_model, bg_model_ses, bg_model2, bg_model2_ses, file = "./temp/bg_model_reg_ols.rdata")


bg1 <- lm_robust(to ~ lost_voters + nh_black +
                   median_income + latino + nh_white +
                   some_college + median_age + reg_rate + share_dem +
                   share_non_citizen + share_winner,
                 data = block_groups, clusters = district, se_type = "stata")

bg2 <- lm_robust(to ~ lost_voters * nh_black +
                   median_income + latino + nh_white +
                   some_college + median_age + reg_rate + share_dem +
                   share_non_citizen + share_winner,
                 data = block_groups, clusters = district, se_type = "stata")

start <- plot_summs(bg1, bg2,
                    coefs = c("Lost Voter Count" = "lost_voters",
                              "(Lost Voters)×(% Black)" = "lost_voters:nh_black"))


start + scale_x_continuous(labels = percent_format(accuracy = 0.1)) + theme_bc() +
  theme(axis.text = element_text(size = 23),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title = element_text(size = 23),
        text = element_text(size = 15),
        plot.title = element_text(size = 23)) +
  ggtitle("Effect of Lost Voters on Neighborhood Turnout")


ggsave("./temp/coef_plot3.png", width = 11, height = 7.25, units = "in")

start <- plot_summs(bg1, bg2,
                    coefs = c("Lost Voter Count" = "lost_voters",
                              "(Lost Voters)×(% Black)" = "lost_voters:nh_black",
                              "% Non-Hispanic Black" = "nh_black",
                              "% Latino" = "latino",
                              "% Non-Hispanic White" = "nh_white",
                              "Median Income (Thousands)" = "median_income",
                              "Median Age" = "median_age",
                              "% With Some College" = "some_college",
                              "% Non-Citizens" = "share_non_citizen",
                              "Registration Rate" = "reg_rate",
                              "% Democrats" = "share_dem",
                              "% Won by Council Rep." = "share_winner"))


start + scale_x_continuous(labels = percent_format(accuracy = 0.1)) + theme_bc() +
  theme(axis.text = element_text(size = 23),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title = element_text(size = 23),
        text = element_text(size = 15),
        plot.title = element_text(size = 23)) +
  ggtitle("Effect of Lost Voters on Neighborhood Turnout")


ggsave("./temp/coef_plot3b.png", width = 11, height = 7.25, units = "in")


#######
tracts <- readRDS("./temp/tract_pre_match.rds")
tracts$median_income <- tracts$median_income / 1000
tracts$lost_voters_black <- tracts$lost_voters * tracts$nh_black
tracts$boro <- substring(tracts$GEOID, 1, 5)

tract_model  <- lm(to ~ lost_voters +
                     median_income + latino + nh_black + nh_white +
                     some_college + median_age + reg_rate + share_dem +
                     share_non_citizen + share_winner,
                   data = tracts)

tract_model_ses  <- data.frame(
  summary(lm_robust(to ~ lost_voters +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem +
                      share_non_citizen + share_winner,
                    data = tracts, clusters = district, se_type = "stata"))$coefficients)[, 2]

tract_model2  <- lm(to ~ lost_voters + lost_voters_black +
                     median_income + latino + nh_black + nh_white +
                     some_college + median_age + reg_rate + share_dem +
                     share_non_citizen + share_winner,
                   data = tracts)

tract_model2_ses  <- data.frame(
  summary(lm_robust(to ~ lost_voters + lost_voters_black +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem +
                      share_non_citizen + share_winner,
                    data = tracts, clusters = district, se_type = "stata"))$coefficients)[, 2]


save(tract_model, tract_model_ses, tract_model2, tract_model2_ses, file = "./temp/tract_model_reg_ols.rdata")

#####
