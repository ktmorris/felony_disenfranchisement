#### individual level regressions

## this is the treated group
nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2017-01-01",
         parole_status_date <= "2018-10-12",
         !is.na(dob_parole)) %>% 
  mutate(age = as.numeric((as.Date("2018-11-06") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2018 = ifelse(is.na(v2018), 0, v2018),
         finished_post = parole_status_date >= "2018-05-18",
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008))


demos1 <- parolees %>% 
  mutate(male = sex == "MALE",
         black = race == "BLACK",
         white = race == "WHITE",
         latino = race ==  "HISPANIC",
         age = as.numeric((as.Date("2018-11-06") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         restored = ifelse(is.na(restored), F, restored))

demos <- demos1 %>% 
  group_by(finished_post) %>% 
  summarize_at(vars(male, black, white, latino, age, parole_time, v2016), mean, na.rm = T) %>% 
  mutate_at(vars(male, black, white, latino, v2016), ~ percent(., accuracy = 0.1)) %>% 
  mutate_at(vars(age, parole_time), ~ as.character(number(., accuracy = 0.1)))

demos <- as.data.frame(t(demos))
demos$varnames <- rownames(demos)
demos <- select(demos, varnames, finished_before = V1, finished_after = V2) %>% 
  filter(varnames != "finished_post")

rename_v <- data.frame("varnames" = c("male", "black", "white", "latino", "v2016", "age", "parole_time"),
                       "names" = c("Percent Male", "Percent NH-Black", "Percent NH-White", "Percent Latino",
                                   "Percent Voted in 2016", "Age (Years)", "Time Spent on Parole (Years)"))

demos <- left_join(demos, rename_v)



pvals <- unlist(lapply(demos$varnames, function(x){
  t.test(demos1[, x] ~ demos1$finished_post)$p.value
}))

demos$pvals <- pvals

demos <- demos %>% 
  mutate(pvals = round(pvals, digits = 3)) %>% 
  select(names, finished_before, finished_after, pvals)

colnames(demos) <- c("Variable", "Control", "Intend-to-Treat", "p-value")

saveRDS(demos, "./temp/demos_rd_time.rds")