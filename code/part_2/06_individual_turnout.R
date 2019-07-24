#### individual level regressions

## this is the treated group
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
         finished_post = parole_status_date >= "2018-05-16",
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016))

model1 <- glm(v2018 ~ restored + days_since_done + days2,
              family = "binomial", data = parolees)

model2 <- glm(v2018 ~ restored + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2018 ~ restored + days_since_done + days2 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_18.rdata")

## intend-to-treat models

model1 <- glm(v2018 ~ finished_post,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

model2 <- glm(v2018 ~ finished_post + v2016 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

model3 <- glm(v2018 ~ finished_post + v2016 +
                as.factor(county) + as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

save(model1, model2, model3, file = "./temp/individual_turnout_18_itt.rdata")


#### IV approach

date_to_model_notime <- glm(v2018 ~ v2016 +
                       as.factor(county) + as.factor(race) + as.factor(sex) + age +
                       felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                     family = "binomial",
                     data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-04-18"))

date_to_model_time <- glm(v2018 ~ days_since_done + days2 + v2016 +
                              as.factor(county) + as.factor(race) + as.factor(sex) + age +
                              felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                            family = "binomial",
                            data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-04-18"))

anova(date_to_model_notime, date_to_model_time, test = "Chisq")


save(date_to_model_notime, date_to_model_time, file = "./temp/short_term_to.rdata")


##### RUN THE IV MODELS IN STATA BECAUSE MARGINAL EFFECTS ARE SO MUCH EASIER
##### THIS CREATES A TEX FILE THAT NEEDS TO BE MANUALLY CHANGED TO MATCH FORMATTING
parolees$restored2 <- 1 * parolees$restored
parolees$finished_post2 <- 1 * parolees$finished_post
parolees$male = 1 * (parolees$sex == "MALE")
parolees$nyc = (parolees$county %in% c("KINGS", "NEW YORK", "QUEENS", "BRONX", "RICHMOND"))*1

fwrite(filter(parolees, year(parole_status_date) >= 2017), "./temp/iv_data.csv")

stata("

insheet using \"temp/iv_data.csv\", clear

foreach var in race sex county{
	encode `var', gen(x)
	drop `var'
	rename x `var'
}

label var restored2 \"D(Rights Restored)\"
label var v2016 \"D(Voted in 2016)\"
label var male \"D(Male)\"
label var age \"Age (Years)\"
label var parole_time \"Time Spent on Parole (Years)\"

* simple 2sls regression
ivregress 2sls v2018 ///
	(restored2 = finished_post2)
	
outreg2 using temp/iv_tables, replace lab dec(4) adjr2
	
* add in demographics
ivregress 2sls v2018 v2016 i.race male age i.county ///
	(restored2 = finished_post2)
	
outreg2 using temp/iv_tables, append drop(i.race i.county felony*) lab ///
	addtext(Race / Ethnicity FE, X, County FE, X) dec(4) adjr2

* add in sentence info
ivregress 2sls v2018 v2016 i.race male age i.county felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2)
	
outreg2 using temp/iv_tables, append drop(i.race i.county felony*) lab ///
	addtext(Race / Ethnicity FE, X, County FE, X, Felony Class FE, X) dec(4) adjr2

* ivprobit with all of the covariates
ivprobit v2018 v2016 i.race male age nyc felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2)
*predict marginal effects
margins, dydx(*) predict(pr) post
	
outreg2 using temp/iv_tables, append drop(i.race nyc felony*) lab ///
	addtext(Race / Ethnicity FE, X, NYC FE, X, Felony Class FE, X) dec(4)	

*bivariate probit model with all covariates
biprobit (v2018 = restored2 v2016 i.race male age i.county felony_a felony_b ///
	felony_c felony_d felony_e parole_time) ///
	(restored2 = finished_post2 v2016 i.race male age i.county felony_a felony_b ///
	felony_c felony_d felony_e parole_time)
* predict marginal effects
margins, dydx(*) predict(pmarg1) post

outreg2 using temp/iv_tables, tex(frag) drop(i.race i.county felony*) lab ///
	addtext(Race / Ethnicity FE, X, County FE, X, Felony Class FE, X) dec(4)
	
")


######################
### 2016

nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din") %>% 
  select(-history, -year, -election_type) %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2015-01-01",
         parole_status_date <= "2016-10-14",
         !is.na(dob_parole)) %>% 
  mutate(age = as.numeric((as.Date("2016-11-08") - dob_parole) / 365.25),
         parole_time = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         finished_post = parole_status_date >= "2016-05-16",
         days_since_done = as.numeric(as.Date("2016-11-08") - parole_status_date),
         days2 = days_since_done^2)

model1 <- glm(v2016 ~ finished_post,
              family = "binomial", data = parolees)

model2 <- glm(v2016 ~ finished_post +
                as.factor(county) + as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2016 ~ finished_post +
                as.factor(county) + as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_16.rdata")
