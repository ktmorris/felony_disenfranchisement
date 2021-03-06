## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- F

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  setwd("/scratch/km3815/felony_disenfranchisement")
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}

for(geo in c("block_group", "tract")){

  units <- readRDS(paste0("./temp/", geo, "_pre_match.rds"))

  match_data <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college, median_age, reg_rate, share_dem, share_non_citizen, share_winner)
  
  # genout <- GenMatch(Tr = units$treat, X = match_data,
  #                    M = 3, replace = T, pop.size = 1000, cluster = cl)
  # saveRDS(genout, paste0("./temp/genout_", geo, ".rds"))
  
  genout <- readRDS(paste0("./temp/genout_", geo, ".rds"))
  
  treat <- units$treat
  
  X <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college,
                  median_age, reg_rate, share_dem, share_non_citizen, share_winner)
  
  match_count <- ifelse(geo == "tract", 10, 30)

  mout <- Match(Tr = treat, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = match_count)
  summary(mout)

  save(mout, file = paste0("./temp/mout_", geo, ".RData"))
  
  load(paste0("./temp/mout_", geo, ".RData"))
  
  
  matches <- data.frame("treated" = mout[["index.treated"]],
                        "control" = mout[["index.control"]],
                        "weight" = mout[["weights"]])
  
  units <- units %>% 
    mutate(id = row_number())
  
  treat_row <- units %>% 
    filter(treat) %>% 
    select(id, GEOID)
  
  untreat_row <- units %>% 
    filter(!treat) %>% 
    select(id, control_GEOID = GEOID)
  
  matches <- left_join(matches, treat_row, by = c("treated" = "id"))
  matches <- left_join(matches, untreat_row, by = c("control" = "id"))
  
  matches <- dplyr::select(matches, -treated, -control)
  
  matches_v <- bind_rows(matches,
                         data.frame(GEOID = unique(matches$GEOID),
                                    control_GEOID = unique(matches$GEOID),
                                    weight = 1))
  
  reg <- left_join(matches_v, units, by = c("control_GEOID" = "GEOID"))
  reg$boro <- substring(reg$GEOID, 1, 5)
  
  reg_output <- lm(to ~ treat, data = reg, weights = weight)
  
  reg_output_ses <- data.frame(summary(lm_robust(to ~ treat, data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  reg_output_pval <- data.frame(summary(lm_robust(to ~ treat, data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 4]
  
  reg_output2 <- lm(to ~ treat +
                      median_income + latino + nh_black + nh_white + some_college +
                      median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                    data = reg, weights = weight)
  
  reg_output2_ses <- data.frame(summary(lm_robust(to ~ treat +
                                                    median_income + latino + nh_black + nh_white + some_college +
                                                  median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                                                  data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  
  save(reg_output, reg_output_ses, reg_output_pval,
       reg_output2, reg_output2_ses, file = paste0("./temp/match_reg_", geo, ".rdata"))
  
  reg_output_nhblack <- lm(to ~ treat + treat * nh_black, data = reg, weights = weight)
  reg_output_nhblack_ses <- data.frame(summary(lm_robust(to ~ treat + treat * nh_black, data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  reg_output_nhblack_pval <- data.frame(summary(lm_robust(to ~ treat + treat * nh_black, data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 4]
  
  reg_output2_nhblack <- lm(to ~ treat * nh_black +
                              median_income + latino + nh_white + some_college +
                              median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                            data = reg, weights = weight)
  reg_output2_nhblack_ses <- data.frame(summary(lm_robust(to ~ treat * nh_black +
                                                            median_income + latino + nh_white + some_college +
                                                            median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                                                          data = reg, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  
  save(reg_output_nhblack, reg_output_nhblack_ses, reg_output_nhblack_pval,
       reg_output2_nhblack, reg_output2_nhblack_ses, file = paste0("./temp/match_reg_", geo, "_nhb.rdata"))
  
  ###########
  load(paste0("./temp/mout_", geo, ".RData"))
  order <- fread("./raw_data/misc/var_orders.csv")
  
  balance <- MatchBalance(treat ~ median_income + latino + nh_black + nh_white +
                            some_college + median_age + reg_rate + share_dem + share_non_citizen + share_winner, match.out = mout,
                          data = units)
  TrMean <- c()
  PreMean <- c()
  PreQQmed <- c()
  PreQQmean <- c()
  PreQQmax <- c()
  PostMean <- c()
  PostQQmed <- c()
  PostQQmean <- c()
  PostQQmax <- c()
  
  for(i in c(1:length(balance$BeforeMatching))){
    TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
    PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
    PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
    PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
    PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
    
    PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
    PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
    PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
    PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
  }
  
  varnames <- c("median_income", "latino", "nh_black", "nh_white", "some_college", "median_age", "reg_rate", "share_dem", "share_non_citizen", "share_winner")
  
  
  df <- data.frame("TrMean" = TrMean,
                   "TrMean2" = TrMean,
                   "PreMean" = PreMean,
                   "PreQQmed" = PreQQmed,
                   "PreQQmean" = PreQQmean,
                   "PreQQmax" = PreQQmax,
                   "PostMean" = PostMean,
                   "PostQQmed" = PostQQmed,
                   "PostQQmean" = PostQQmean,
                   "PostQQmax" = PostQQmax,
                   "names" = varnames) %>%
    mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
           change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
           change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
           change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
    mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>%
    mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2)))
  
  df <- full_join(df, order, by = c("names" = "variable")) %>%
    arrange(order) %>%
    select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
    filter(!is.na(TrMean))
  
  colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")
  
  saveRDS(df, paste0("./temp/match_table_", geo, ".rds"))

  #####################
  if(geo == "block_group"){
    reg_output <- lm_robust(to ~ treat +
                                  median_income + latino + nh_black + nh_white + some_college +
                                  median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                                data = reg, weights = weight, cluster = GEOID, se_type = "stata")
    
    reg_output2 <- lm_robust(to ~ treat,
                            data = reg, weights = weight, cluster = GEOID, se_type = "stata")
    
    
    start <- plot_summs(reg_output, reg_output2, model.names = c("With Controls", "Without Controls"),
                        coefs = c("Treated (Lost Voter)" = "treatTRUE"))
    
  
    start + scale_x_continuous(labels = percent_format(accuracy = 0.1)) + theme_bc() +
      theme(axis.text = element_text(size = 23),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 23),
            text = element_text(size = 15),
            plot.title = element_text(size = 23)) +
      ggtitle("Effect of Lost Voters on Neighborhood Turnout")
    
    
    ggsave("./temp/coef_plot1.png", width = 11, height = 7.25, units = "in")
    
    reg_output <- lm_robust(to ~ treat * nh_black +
                              median_income + latino + nh_white + some_college +
                              median_age + reg_rate + share_dem + share_non_citizen + share_winner,
                            data = reg, weights = weight, cluster = GEOID, se_type = "stata")
    
    reg_output2 <- lm_robust(to ~ treat * nh_black,
                             data = reg, weights = weight, cluster = GEOID, se_type = "stata")
    
    
    start <- plot_summs(reg_output, reg_output2, model.names = c("With Controls", "Without Controls"),
                        coefs = c("Treated (Lost Voter)" = "treatTRUE",
                                  "(Treated)×(% Black)" = "treatTRUE:nh_black"))
    
    
    start + scale_x_continuous(labels = percent_format(accuracy = 0.1)) + theme_bc() +
      theme(axis.text = element_text(size = 23),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            axis.title = element_text(size = 23),
            text = element_text(size = 15),
            plot.title = element_text(size = 23)) +
      ggtitle("Effect of Lost Voters on Neighborhood Turnout")
    
    
    ggsave("./temp/coef_plot2.png", width = 11, height = 7.25, units = "in")
    }
  }


