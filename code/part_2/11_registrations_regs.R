
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
         finished_post = parole_status_date >= "2018-05-18",
         days_bef = as.Date("2018-10-12") - parole_status_date,
         days2 = as.integer(days_bef)^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008),
         registered = voter_status %in% c("ACTIVE", "INACTIVE"))




model1 <- lm(registered ~ finished_post + as.factor(race) +
               as.factor(sex) + age + v2008 +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef,
             data = filter(parolees, year(parole_status_date) >= 2017),
             cluster = "finished_post")

m1_ses <- data.table(summary(lm.cluster(registered ~ finished_post + as.factor(race) +
                                          as.factor(sex) + age + v2008 +
                                          felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef,
                                        data = filter(parolees, year(parole_status_date) >= 2017),
                                        cluster = "finished_post"))[,2])

parolees$pb <- (parolees$race == "BLACK") * (parolees$finished_post == T)

model2 <- lm(registered ~ finished_post + as.factor(race) +
               as.factor(sex) + age + v2008 +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef + pb,
             data = filter(parolees, year(parole_status_date) >= 2017),
             cluster = "finished_post")

m2_ses <- data.table(summary(lm.cluster(registered ~ finished_post + as.factor(race) +
                                          as.factor(sex) + age + v2008 +
                                          felony_a + felony_b + felony_c + felony_d + felony_e + parole_time +
                                          days_bef + pb,
                                        data = filter(parolees, year(parole_status_date) >= 2017),
                                        cluster = "finished_post"))[,2])

save(model1, m1_ses, model2, m2_ses, file = "./temp/app_reg.rdata")















parolees <- fread("./temp/iv_data.csv")

stata("

capture erase \"temp/iv_tables_reg.tex\"
capture erase \"temp/iv_tables_reg.txt\"

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
ivregress 2sls registered ///
	(restored2 = finished_post2), vce(robust)
	
outreg2 using temp/iv_tables_reg, replace lab dec(4) adjr2

* add in sentence info
ivregress 2sls registered i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
	
outreg2 using temp/iv_tables_reg, append drop(i.race felony*) lab ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4) adjr2
	

* ivprobit with all of the covariates
ivprobit registered i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
	
local chisq `e(chi2)'

*predict marginal effects
margins, dydx(*) predict(pr) post
	
outreg2 using temp/iv_tables_reg, append drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)

*bivariate probit model no covariates
biprobit (registered = restored2) ///
	(restored2 = finished_post2), vce(robust)
	
local chisq `e(chi2)'
local rho `e(rho)'
* predict marginal effects
margins, dydx(*) predict(pmarg1) post

outreg2 using temp/iv_tables_reg, append drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') dec(4)
	

*bivariate probit model with all covariates
biprobit (registered = restored2 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time) ///
	(restored2 = finished_post2 i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time), vce(robust)
local chisq `e(chi2)'
local rho `e(rho)'	

* predict marginal effects
margins, dydx(*) predict(pmarg1) post

outreg2 using temp/iv_tables_reg, append tex(frag) drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)
	
")

iv3 <- ivreg(registered ~ restored + 
               age +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
             data = filter(parolees, year(parole_status_date) >= 2017, (sex != "MALE" | race != "WHITE")))

save(iv3, file = "./temp/iv_individual_turnout_18_reg.rdata")

### variable treatment effect
stata("

capture erase \"temp/iv_tables_months_reg.tex\"
capture erase \"temp/iv_tables_months_reg.txt\"

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
ivregress 2sls registered i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (months_past_tr = months_past), vce(robust)
	
outreg2 using temp/iv_tables_months_reg, replace drop(i.race felony*) lab ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4) adjr2


* ivprobit with all of the covariates
ivprobit registered i.race male age felony_a felony_b ///
	felony_c felony_d felony_e parole_time (months_past_tr = months_past), vce(robust)
	
local chisq `e(chi2)'

*predict marginal effects
margins, dydx(*) predict(pr) post
	
outreg2 using temp/iv_tables_months_reg, append tex(frag) drop(i.race felony*) lab ///
  adds(\"Wald $\\chi^2$\", `chisq') ///
	addtext(Race / Ethnicity FE, X, Felony Class FE, X) dec(4)

")

## rerun for nonwhite

stata("

insheet using \"temp/iv_data.csv\", clear

capture erase \"temp/iv_tables_nw_reg.tex\"
capture erase \"temp/iv_tables_nw_reg.txt\"

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
  ivregress 2sls registered male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
  	
  outreg2 using temp/iv_tables_nw_reg, append tex(frag) drop(felony*) lab ///
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
  biprobit (registered = restored2 male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time) ///
  	(restored2 = finished_post2 male age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time), vce(robust)
  local chisq `e(chi2)'
  local rho `e(rho)'
  
  * predict marginal effects
  margins, dydx(*) predict(pmarg1) post
  
  outreg2 using temp/iv_tables_nw_reg, append tex(frag) drop(felony*) lab ///
    adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
  	addtext(Felony Class FE, X) dec(4)
}
")
### now for men and women
stata("

insheet using \"temp/iv_data.csv\", clear

capture erase \"temp/iv_tables_mf_reg.tex\"
capture erase \"temp/iv_tables_mf_reg.txt\"

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
  ivregress 2sls registered race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time (restored2 = finished_post2), vce(robust)
  	
  outreg2 using temp/iv_tables_mf_reg, append tex(frag) drop(felony*) lab ///
  	addtext(Felony Class FE, X) dec(4) adjr2
}

foreach sex MALE FEMALE{
  restore, preserve
  
  keep if sex ==  \"`sex'\" 
  
  *bivariate probit model with all covariates
  biprobit (registered = restored2 race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time) ///
  	(restored2 = finished_post2 race age felony_a felony_b ///
  	felony_c felony_d felony_e parole_time), vce(robust)
  local chisq `e(chi2)'
  local rho `e(rho)'
  
  * predict marginal effects
  margins, dydx(*) predict(pmarg1) post
  
  outreg2 using temp/iv_tables_mf_reg, append tex(frag) drop(felony*) lab ///
    adds(\"Wald $\\chi^2$\", `chisq', \"$\\rho$\", `rho') ///
  	addtext(Felony Class FE, X) dec(4)
}
")

#source("./code/misc/clean_tex_tables.R")
