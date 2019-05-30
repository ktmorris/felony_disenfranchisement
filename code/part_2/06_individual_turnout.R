#### individual level regressions

nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2012-01-01",
         parole_status_date <= "2018-10-12",
         !is.na(dob_parole)) %>% 
  mutate(age = as.numeric((as.Date("2018-11-06") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2018 = ifelse(is.na(v2018), 0, v2018),
         finished_post = parole_status_date >= "2018-05-21",
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2)

model1 <- glm(v2018 ~ finished_post + days_since_done + days2,
              family = "binomial", data = parolees)

model2 <- glm(v2018 ~ finished_post + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2018 ~ finished_post + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + counts + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_18.rdata")


### 2016

nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2010-01-01",
         parole_status_date <= "2016-10-14",
         !is.na(dob_parole)) %>% 
  mutate(age = as.numeric((as.Date("2016-11-08") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         finished_post = parole_status_date >= "2016-05-21",
         days_since_done = as.numeric(as.Date("2016-11-08") - parole_status_date),
         days2 = days_since_done^2)

model1 <- glm(v2016 ~ finished_post + days_since_done + days2,
              family = "binomial", data = parolees)

model2 <- glm(v2016 ~ finished_post + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2016 ~ finished_post + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + counts + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_16.rdata")