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
         finished_post = parole_status_date >= "2018-05-22",
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008))

model1 <- glm(v2018 ~ restored + days_since_done + days2,
              family = "binomial", data = parolees)

model2 <- glm(v2018 ~ restored + days_since_done + days2 +
                as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2018 ~ restored + days_since_done + days2 +
                as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_18.rdata")

## intend-to-treat models

model1 <- glm(v2018 ~ finished_post,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

model2 <- glm(v2018 ~ finished_post + 
                as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

model3 <- glm(v2018 ~ finished_post + 
                as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = filter(parolees, year(parole_status_date) >= 2017))

save(model1, model2, model3, file = "./temp/individual_turnout_18_itt.rdata")


#### IV approach

date_to_model_notime <- glm(v2018 ~ 
                       as.factor(race) + as.factor(sex) + age +
                       felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                     family = "binomial",
                     data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-04-18"))

date_to_model_time <- glm(v2018 ~ days_since_done + days2 + 
                              as.factor(race) + as.factor(sex) + age +
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

gen months_past = ceil((date(parole_status_date, \"YMD\") - date(\"2018-5-18\", \"YMD\")) / 30.5) * restored2

label var restored2 \"D(Rights Restored)\"
label var v2016 \"D(Voted in 2016)\"
label var male \"D(Male)\"
label var age \"Age (Years)\"
label var parole_time \"Years on Parole\"
label var months_past \"Months Restored\"

* simple 2sls regression
ivregress 2sls v2018 ///
	(restored2 = finished_post2), vce(robust)
	
outreg2 using temp/iv_tables, replace lab dec(4) adjr2

* add in sentence info
ivregress 2sls v2018 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
	
outreg2 using temp/iv_tables, append drop(i.race felony*) lab ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4) adjr2
	

* ivprobit with all of the covariates
ivprobit v2018 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
	
local chisq `e(chi2)'

*predict marginal effects
margins, dydx(*) predict(pr) post
	
outreg2 using temp/iv_tables, append drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)

*bivariate probit model no covariates
biprobit (v2018 = restored2) ///
	(restored2 = finished_post2), vce(robust)
	
local chisq `e(chi2)'
local rho `e(rho)'
* predict marginal effects
margins, dydx(*) predict(pmarg1) post

outreg2 using temp/iv_tables, append drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') dec(4)
	

*bivariate probit model with all covariates
biprobit (v2018 = restored2 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time) ///
	(restored2 = finished_post2 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time), vce(robust)
local chisq `e(chi2)'
local rho `e(rho)'	

* predict marginal effects
margins, dydx(*) predict(pmarg1) post

outreg2 using temp/iv_tables, append tex(frag) drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)
	
")

iv3 <- ivreg(v2018 ~ restored + 
               age +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
             data = filter(parolees, year(parole_status_date) >= 2017, (sex != "MALE" | race != "WHITE")))

save(iv3, file = "./temp/iv_individual_turnout_18.rdata")

### variable treatment effect
stata("

capture erase \"temp/iv_tables_months.tex\"
capture erase \"temp/iv_tables_months.txt\"

insheet using \"temp/iv_data.csv\", clear

foreach var in race sex county{
	encode `var', gen(x)
	drop `var'
	rename x `var'
}

gen months_past_tr = ceil((date(parole_status_date, \"YMD\") - date(\"2018-5-17\", \"YMD\")) / 30.5) * restored2

gen months_past = ceil((date(parole_status_date, \"YMD\") - date(\"2018-5-17\", \"YMD\")) / 30.5) * finished_post2

label var restored2 \"D(Rights Restored)\"
label var v2016 \"D(Voted in 2016)\"
label var male \"D(Male)\"
label var age \"Age (Years)\"
label var parole_time \"Years on Parole\"
label var months_past_tr \"Months Restored\"


* add in sentence info
ivregress 2sls v2018 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (months_past_tr = months_past), vce(robust)
	
outreg2 using temp/iv_tables_months, replace drop(i.race felony*) lab ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4) adjr2


* ivprobit with all of the covariates
ivprobit v2018 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (months_past_tr = months_past), vce(robust)
	
local chisq `e(chi2)'

*predict marginal effects
margins, dydx(*) predict(pr) post
	
outreg2 using temp/iv_tables_months, append tex(frag) drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)

")

## rerun for nonwhite

stata("

insheet using \"temp/iv_data.csv\", clear

capture erase \"temp/iv_tables_nw.tex\"
capture erase \"temp/iv_tables_nw.txt\"

foreach var in sex county{
	encode `var', gen(x)
	drop `var'
	rename x `var'
}

label var restored2 \"D(Rights Restored)\"
label var v2016 \"D(Voted in 2016)\"
label var male \"D(Male)\"
label var age \"Age (Years)\"
label var parole_time \"Years on Parole\"
preserve

foreach race in WHITE NONWHITE BLACK{
  restore, preserve
  di \"STEP A\"
  if \"`race'\" == \"WHITE\" | \"`race'\" == \"BLACK\"{
    keep if race == \"`race'\"
  }
  else{
    keep if race != \"WHITE\"
  }
  
  encode race, gen(x)
  drop race
  rename x race
  
  * 2sls w/ covariates
  ivregress 2sls v2018 male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
  	
  outreg2 using temp/iv_tables_nw, append tex(frag) drop(felony*) lab ///
  	addtext(Felony Class FE, X) dec(4) adjr2
}

foreach race in WHITE NONWHITE BLACK{
  restore, preserve
  
  if \"`race'\" == \"WHITE\" | \"`race'\" == \"BLACK\"{
      keep if race == \"`race'\"
    }
    else{
      keep if race != \"WHITE\"
    }
  *bivariate probit model with all covariates
  biprobit (v2018 = restored2 male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time) ///
  	(restored2 = finished_post2 male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time), vce(robust)
  local chisq `e(chi2)'
  local rho `e(rho)'
  
  * predict marginal effects
  margins, dydx(*) predict(pmarg1) post
  
  outreg2 using temp/iv_tables_nw, append tex(frag) drop(felony*) lab ///
    adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
  	addtext(Felony Class FE, X) dec(4)
}
")
### now for men and women
stata("

insheet using \"temp/iv_data.csv\", clear

capture erase \"temp/iv_tables_mf.tex\"
capture erase \"temp/iv_tables_mf.txt\"

foreach var in sex county race{
	encode `var', gen(x)
	drop `var'
	rename x `var'
}

label var restored2 \"D(Rights Restored)\"
label var v2016 \"D(Voted in 2016)\"
label var male \"D(Male)\"
label var age \"Age (Years)\"
label var parole_time \"Years on Parole\"
preserve

foreach sex in MALE FEMALE{
  restore, preserve
  di \"STEP A\"
  keep if sex == \"`sex'\"
  
  encode sex, gen(x)
  drop sex
  rename x sex
  
  * 2sls w/ covariates
  ivregress 2sls v2018 race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
  	
  outreg2 using temp/iv_tables_mf, append tex(frag) drop(felony*) lab ///
  	addtext(Felony Class FE, X) dec(4) adjr2
}

foreach sex MALE FEMALE{
  restore, preserve
  
  keep if sex ==  \"`sex'\" 
  
  *bivariate probit model with all covariates
  biprobit (v2018 = restored2 race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time) ///
  	(restored2 = finished_post2 race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time), vce(robust)
  local chisq `e(chi2)'
  local rho `e(rho)'
  
  * predict marginal effects
  margins, dydx(*) predict(pmarg1) post
  
  outreg2 using temp/iv_tables_mf, append tex(frag) drop(felony*) lab ///
    adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
  	addtext(Felony Class FE, X) dec(4)
}
")

source("./code/misc/clean_tex_tables.R")

summary(glm(v2018 ~ finished_post + 
               as.factor(race) + as.factor(sex) + age +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time ,
             data = filter(parolees, year(parole_status_date) >= 2017, race != "WHITE"), family = "binomial"))


##### black turnout prior to executive order

black_to1 <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               !finished_post), family = "binomial")

black_to2 <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               race != "HISPANIC",
                               !finished_post), family = "binomial")

black_to1b <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               finished_post), family = "binomial")

black_to2b <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               race != "HISPANIC",
                               finished_post), family = "binomial")

black_to3 <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               restored), family = "binomial")

black_to4 <- glm(v2018 ~ 
                   I(race == "BLACK") + as.factor(sex) + age + 
                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                 data = filter(parolees,
                               year(parole_status_date) >= 2017,
                               race != "HISPANIC",
                               restored), family = "binomial")

save(black_to1, black_to2, black_to1b, black_to2b, black_to3, black_to4, file = "./temp/black_to.rdata")

### turnout for folks on parole after reg deadline

## this is the treated group
nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees_ed <- left_join(parolees, nys_roll, by = "din") %>% 
  filter((parole_status == "DISCHARGED" & parole_status_date > "2018-10-12") |
           parole_status == "ACTIVE",
         release_date_parole < "2018-10-12",
         restored) %>% 
  mutate(v2018 = ifelse(is.na(v2018), 0, v2018))

new_eligible_18 <- nrow(parolees_ed)
new_votes_18 <- sum(parolees_ed$v2018, na.rm = T)

saveRDS(new_eligible_18, "./temp/new_eligible_18.rds")
saveRDS(new_votes_18, "./temp/new_votes_18.rds")

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
         finished_post = parole_status_date >= "2016-05-18",
         days_since_done = as.numeric(as.Date("2016-11-08") - parole_status_date),
         days2 = days_since_done^2)

model1 <- glm(v2016 ~ finished_post,
              family = "binomial", data = parolees)

model2 <- glm(v2016 ~ finished_post +
                as.factor(race) + as.factor(sex) + age,
              family = "binomial", data = parolees)

model3 <- glm(v2016 ~ finished_post +
                as.factor(race) + as.factor(sex) + age +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
              family = "binomial", data = parolees)

save(model1, model2, model3, file = "./temp/individual_turnout_16.rdata")
