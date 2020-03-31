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

for(geo in c("block_group")){
  
  units <- readRDS(paste0("./temp/", geo, "_pre_match.rds"))
  units2 <- readRDS(paste0("./temp/", geo, "_pre_match_16.rds")) %>% 
    filter(lost_voters == 0)
  
  units <- filter(units, GEOID %in% units2$GEOID)
  
  units <- inner_join(
    select(units, GEOID, treat),
    select(units2, -treat)
  )
 rm(units2)
  
  
  match_data <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college,
                  median_age, reg_rate, share_dem, share_non_citizen, sf)
  
  # genout <- GenMatch(Tr = units$treat, X = match_data,
  #                    M = 3, replace = T, pop.size = 1000, cluster = cl)
  # 
  # saveRDS(genout, paste0("./temp/genout_", geo, "_did.rds"))
  
  genout <- readRDS(paste0("./temp/genout_", geo, "_did.rds"))
  
  treat <- units$treat
  
  X <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college,
                  median_age, reg_rate, share_dem, share_non_citizen, sf)
  
  match_count <- ifelse(geo == "tract", 10, 1)

  mout <- Match(Tr = treat, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = match_count)
  summary(mout)

  save(mout, file = paste0("./temp/mout_", geo, "_did.RData"))
  
  load(paste0("./temp/mout_", geo, "_did.RData"))
  
  
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
  saveRDS(matches_v, paste0("./temp/matches_did_", geo, ".rds"))
  
  
  ###########
  load(paste0("./temp/mout_", geo, "_did.RData"))
  order <- fread("./raw_data/misc/var_orders.csv")
  
  balance <- MatchBalance(treat ~ median_income + latino + nh_black + nh_white +
                            some_college + median_age + reg_rate + share_dem + share_non_citizen + sf, match.out = mout,
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
  
  varnames <- c("median_income", "latino",
                "nh_black", "nh_white", "some_college",
                "median_age", "reg_rate", "share_dem", "share_non_citizen", "sf")
  
  
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
  
  saveRDS(df, paste0("./temp/match_table_", geo, "_did.rds"))
  
  ########
  
  matches <- readRDS(paste0("./temp/matches_did_", geo, ".rds"))
  
  bgs_17 <- inner_join(matches, 
                       readRDS(paste0("./temp/", geo, "_pre_match.rds")) %>% 
                         mutate(year = 2017,
                                district = districtcd) %>% 
                         select(-treat),
                       by = c("control_GEOID" = "GEOID"))
  
  ds <- bgs_17 %>% 
    select(control_GEOID, district = districtcd) %>% 
    distinct()
  
  bgs_16 <- inner_join(matches, 
                       readRDS(paste0("./temp/", geo, "_pre_match_16.rds")) %>% 
                         mutate(year = 2016) %>% 
                         select(-treat, -district),
                       by = c("control_GEOID" = "GEOID"))
  
  bgs_16 <- left_join(bgs_16, ds)
  
  bgs <- bind_rows(bgs_16, bgs_17) %>% 
    mutate(treat = control_GEOID == GEOID)
  
  reg_output <- lm(to ~ I(year == 2017) * treat + as.factor(district), data = bgs, weights = weight)
  
  reg_output_ses <- data.frame(summary(lm_robust(to ~ I(year == 2017) * treat + as.factor(district),
                                                 data = bgs, weights = weight, cluster = GEOID,
                                                 se_type = "stata"))$coefficients)[, 2]
  
  reg_output2 <- lm(to ~ I(year == 2017) * treat + sf +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem + share_non_citizen +
                      as.factor(district),
                    data = bgs, weights = weight)
  
  reg_output2_ses <- data.frame(summary(lm_robust(to ~ I(year == 2017) * treat + sf +
                                                    median_income + latino + nh_black + nh_white +
                                                    some_college + median_age + reg_rate + share_dem + share_non_citizen +
                                                    as.factor(district),
                                                  data = bgs, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  
  reg_output_pval <- data.frame(summary(lm_robust(to ~ I(year == 2017) * treat,
                                                  data = bgs, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 4]
  
  reg_output3 <- lm(to ~ I(year == 2017) * treat + sf +
                      median_income + latino + nh_black + nh_white +
                      some_college + median_age + reg_rate + share_dem + share_non_citizen +
                      as.factor(district) + control_GEOID,
                    data = bgs, weights = weight)
  
  reg_output3_ses <- data.frame(summary(lm_robust(to ~ I(year == 2017) * treat + sf +
                                                    median_income + latino + nh_black + nh_white +
                                                    some_college + median_age + reg_rate + share_dem + share_non_citizen +
                                                    as.factor(district) + control_GEOID,
                                                  data = bgs, weights = weight, cluster = GEOID, se_type = "stata"))$coefficients)[, 2]
  
  save(reg_output, reg_output_ses, reg_output_pval,
       reg_output2, reg_output2_ses,
       reg_output3, reg_output3_ses, file = paste0("./temp/match_reg_", geo, "_did.rdata"))
  
}

bgs_17 <- readRDS(paste0("./temp/block_group_pre_match.rds")) %>% 
  filter(lost_voters > 0)

bgs_16 <- readRDS(paste0("./temp/block_group_pre_match_16.rds")) %>% 
  filter(lost_voters == 0)

bgs_17$treat <- !(bgs_17$GEOID %in% bgs_16$GEOID)

t.test(bgs_17$median_income ~ bgs_17$treat)
t.test(bgs_17$nh_white ~ bgs_17$treat)
t.test(bgs_17$share_dem ~ bgs_17$treat)
t.test(bgs_17$nh_black ~ bgs_17$treat)


# t1 <- readRDS(paste0("./temp/block_group_pre_match_16.rds")) %>% 
#   select(lost_voters_16 = lost_voters, GEOID)
# 
# t2 <- readRDS(paste0("./temp/block_group_pre_match.rds")) %>%
#   select(GEOID, lost_voters_17 = lost_voters)
# 
# t3 <- inner_join(t1, t2) %>% 
#   filter(lost_voters_17 >= lost_voters_16) %>% 
#   mutate(treat = lost_voters_17 > lost_voters_16)
# 
# 
# t1 <- readRDS(paste0("./temp/block_group_pre_match_16.rds")) %>% 
#   filter(GEOID %in% t3$GEOID) %>% 
#   mutate(year = 2016) %>% 
#   select(-treat)
# 
# t2 <- readRDS(paste0("./temp/block_group_pre_match.rds")) %>%
#   filter(GEOID %in% t3$GEOID) %>% 
#   mutate(year = 2017) %>% 
#   select(-treat)
# 
# 
# full <- left_join(bind_rows(t1, t2),
#                   t3) %>% 
#   mutate(l = lost_voters * (year == 2016)) %>% 
#   group_by(GEOID) %>% 
#   mutate(lost_16 = max(l))
# 
# 
# summary(lm_robust(to ~ I(year == 2017) * treat + lost_16 +
#              median_income + latino + nh_black + nh_white +
#              some_college + median_age + reg_rate + share_dem + share_non_citizen, data = full, se_type = "stata"))