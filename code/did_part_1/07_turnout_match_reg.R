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
  rm(units2)
  

  match_data <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college, median_age, reg_rate, share_dem, share_non_citizen, share_winner)
  
  # genout <- GenMatch(Tr = units$treat, X = match_data,
  #                    M = 3, replace = T, pop.size = 1000, cluster = cl)
  # saveRDS(genout, paste0("./temp/genout_", geo, "_did.rds"))
  
  genout <- readRDS(paste0("./temp/genout_", geo, "_did.rds"))
  
  treat <- units$treat
  
  X <- units %>% 
    dplyr::select(median_income, latino, nh_black, nh_white, some_college,
                  median_age, reg_rate, share_dem, share_non_citizen, share_winner)
  
  match_count <- ifelse(geo == "tract", 10, 30)

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

}


bgs_16 <- readRDS("./temp/block_group_pre_match_16.rds") %>% 
  mutate(year = 2016)
bgs_17 <- readRDS("./temp/block_group_pre_match.rds") %>% 
  mutate(year = 2017)

matches <- readRDS("./temp/matches_did_block_group.rds")

bgs_16 <- inner_join(bgs_16, matches, by = c("GEOID" = "control_GEOID"))
bgs_17 <- inner_join(bgs_17, matches, by = c("GEOID" = "control_GEOID"))

bgs <- bind_rows(bgs_16, bgs_17) %>% 
  mutate(treat = GEOID == GEOID.y)

summary(lm(to ~ I(year == 2017) * treat, bgs, weight = weight))
