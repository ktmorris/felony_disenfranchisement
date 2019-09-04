

### precinct preferences

eds <- dbGetQuery(db, "select nys_id, voter_status, assembly_district, election_district
                  from nys_roll_0418 where county_code in (24, 41, 31, 3, 43)")

eds <- eds %>%
  mutate(a = as.integer(voter_status == "ACTIVE"),
         b = as.integer(voter_status == "INACTIVE"),
         c = as.integer(voter_status == "PREREG"))

eds <- setorder(eds, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
eds <- eds[!duplicated(eds$nys_id),]
eds <- select(eds, -a, -b, -c, -voter_status)

nyc <- readRDS("./temp/nyc.rds")

nyc$lost_voter <- nyc$nys_id %in% readRDS("./temp/ids_of_lost_voters.rds")$nys_id

nyc <- filter(nyc, match %in% c("Zip8", "Zip9"))

nyc <- left_join(nyc, eds, by = "nys_id")

ps <- nyc %>% 
  group_by(election_district, assembly_district) %>% 
  summarize(lost = sum(lost_voter))

nyc <- filter(nyc, voter_status != "PURGED")



####
load("./temp/census_data_nyc_bgs_tracts.RData")

nyc <- left_join(nyc, geos[[2]], by = c("bg" = "GEOID"))

rm(geos)

precincts <- nyc %>% 
  group_by(election_district, assembly_district) %>% 
  summarize(median_income = mean(median_income, na.rm = T),
            share_dem = mean(political_party == "DEM" & !is.na(political_party), na.rm = T),
            age = mean(2017 - floor(dob / 10000), na.rm = T),
            some_college = mean(some_college, na.rm = T),
            w = mean(nh_white, na.rm = T),
            b = mean(nh_black, na.rm = T),
            l = mean(latino, na.rm = T),
            share_non_citizen = mean(share_non_citizen, na.rm = T))

precincts <- left_join(precincts, ps)

#### election results

results <- fread("./raw_data/results_2017.csv")
colnames(results) <- make.names(colnames(results))

results <- results %>% 
  filter(grepl("[(]", Unit.Name)) %>% 
  mutate(deb = grepl("bill de blasio", tolower(Unit.Name)) * Tally) %>% 
  group_by(AD, ED) %>% 
  summarize(share_deb = sum(deb) / sum(Tally))


concon <- fread("./raw_data/concon.csv")
colnames(concon) <- make.names(colnames(concon))

concon <- concon %>% 
  filter(Unit.Name %in% c("Yes", "No")) %>% 
  mutate(yes = (Unit.Name == "Yes") * Tally) %>% 
  group_by(AD, ED) %>% 
  summarize(share_yes = sum(yes) / sum(Tally))

aoc <- fread("./raw_data/prim14.csv")
colnames(aoc) <- make.names(colnames(aoc))


aoc <- aoc %>% 
  filter(V21 %in% c("Alexandria Ocasio-Cortez", "Joseph Crowley", "Scattered")) %>% 
  mutate(aoc = grepl("Ocasio-Cortez", V21) * V22) %>% 
  group_by(AD = V12, ED = V13) %>% 
  summarize(share_aoc = sum(aoc) / sum(V22))


ab <- fread("./raw_data/prim9.csv")
colnames(ab) <- make.names(colnames(ab))

ab <- ab %>% 
  filter(V21 %in% c("Adem Bunkeddeko", "Yvette D. Clarke", "Scattered")) %>% 
  mutate(ab = grepl("Bunkeddeko", V21) * V22) %>% 
  group_by(AD = V12, ED = V13) %>% 
  summarize(share_ab = sum(ab) / sum(V22))
  

#####

precincts <- left_join(precincts, results, by = c("assembly_district" = "AD",
                                                  "election_district" = "ED"))

precincts <- left_join(precincts, concon, by = c("assembly_district" = "AD",
                                                  "election_district" = "ED"))

precincts <- left_join(precincts, aoc, by = c("assembly_district" = "AD",
                                                 "election_district" = "ED"))

precincts <- left_join(precincts, ab, by = c("assembly_district" = "AD",
                                              "election_district" = "ED"))




aoc_1 <- lm(share_yes ~ I(lost > 0), data = precincts)
ab_1 <- lm(share_ab ~ I(lost > 0), data = precincts)

aoc_2 <- lm(share_aoc ~ lost + median_income + share_dem + age + some_college + w + b + l, data = precincts)
ab_2 <- predict(lm(share_ab ~ lost + median_income + share_dem + age + some_college + w + b + l, data = precincts))



####

ninth <- precincts %>% 
  filter(!is.na(share_ab))

ninth$p <- predict(lm(share_ab ~ lost + median_income + share_dem + age + some_college + w + b + l, data = ninth))


###summary(lm(share_ab ~ median_income + share_dem + age + some_college + w + b + l, data = ninth))

#### match

source("./code/misc/AutoCluster4.R")
cl <- NCPUS(detectCores() - 1)

units <- precincts %>% 
  ungroup() %>% 
  mutate(treat = lost > 0) %>% 
  select(share_deb, median_income, l, b, w, some_college, age, share_dem, share_non_citizen, treat,
         assembly_district, election_district)

units <- units[complete.cases(units), ]

match_data <- units %>% 
  ungroup() %>% 
  dplyr::select(median_income, l, b, w, some_college, age, share_dem, share_non_citizen)

genout <- GenMatch(Tr = units$treat, X = match_data,
                   M = 3, replace = T, pop.size = 1000, cluster = cl)
saveRDS(genout, "./temp/genout_ny9.rds")

genout <- readRDS(paste0("./temp/genout_ny9.rds"))

treat <- units$treat

X <- units %>%
  ungroup() %>% 
  dplyr::select(median_income, l, b, w, some_college,
                age, share_dem, share_non_citizen)

match_count <- 30

mout <- Match(Tr = treat, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = match_count)
summary(mout)

save(mout, file = "./temp/mout_ny9.RData")

load("./temp/mout_ny9.RData")


matches <- data.frame("treated" = mout[["index.treated"]],
                      "control" = mout[["index.control"]],
                      "weight" = mout[["weights"]])

units <- units %>% 
  ungroup() %>% 
  mutate(id = row_number(),
         p = paste0(election_district, assembly_district))

treat_row <- units %>% 
  filter(treat) %>% 
  select(id, p)

untreat_row <- units %>% 
  filter(!treat) %>% 
  select(id, control_p = p)

matches <- left_join(matches, treat_row, by = c("treated" = "id"))
matches <- left_join(matches, untreat_row, by = c("control" = "id"))

matches <- dplyr::select(matches, -treated, -control)

matches_v <- bind_rows(matches,
                       data.frame(p = unique(matches$p),
                                  control_p = unique(matches$p),
                                  weight = 1))

reg <- left_join(matches_v, units, by = c("control_p" = "p"))

reg_output <- lm(share_deb ~ treat, data = reg, weights = weight)

summary(reg_output)

#####
balance <- MatchBalance(treat ~ median_income + l + b + w +
                          some_college + age + share_dem + share_non_citizen, match.out = mout,
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

varnames <- c("median_income", "latino", "nh_black", "nh_white", "some_college", "median_age","share_dem", "share_non_citizen")


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