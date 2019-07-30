## this is the treated group
nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


assigned_treat <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date <= "2018-10-12",
         parole_status_date >= "2018-05-16")

demos1 <- assigned_treat %>% 
  mutate(male = sex == "MALE",
         black = race == "BLACK",
         white = race == "WHITE",
         latino = race ==  "HISPANIC",
         age = as.numeric((as.Date("2018-11-06") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         restored = ifelse(is.na(restored), F, restored))

demos <- demos1 %>% 
  group_by(restored) %>% 
  summarize_at(vars(male, black, white, latino, age, parole_time, v2016), mean, na.rm = T) %>% 
  mutate_at(vars(male, black, white, latino, v2016), ~ percent(., accuracy = 0.1)) %>% 
  mutate_at(vars(age, parole_time), ~ as.character(number(., accuracy = 0.1)))

demos <- as.data.frame(t(demos))
demos$varnames <- rownames(demos)
demos <- select(demos, varnames, not_restored = V1, restored = V2) %>% 
  filter(varnames != "restored")

rename_v <- data.frame("varnames" = c("male", "black", "white", "latino", "v2016", "age", "parole_time"),
                       "names" = c("Percent Male", "Percent NH-Black", "Percent NH-White", "Percent Latino",
                                   "Percent Voted in 2016", "Age (Years)", "Time Spent on Parole (Years)"))

demos <- left_join(demos, rename_v)



pvals <- unlist(lapply(demos$varnames, function(x){
  t.test(demos1[, x] ~ demos1$restored)$p.value < 0.01
}))

demos$pvals <- pvals

demos <- demos %>% 
  mutate(names = ifelse(pvals, paste0(names, "*"), as.character(names))) %>% 
  select(names, restored, not_restored)

colnames(demos) <- c("Variable", "Rights Restored", "Rights Not Restored")

saveRDS(demos, "./temp/demos_restorees.rds")