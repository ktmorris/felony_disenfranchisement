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
         registered = (voter_status %in% c("ACTIVE", "INACTIVE")) & (registration_date <= 20181012))
###########################################
pool <- filter(parolees, year(parole_status_date) >= 2017) %>% 
  mutate(b = race == "BLACK")

g <- t.test(v2018 ~ b, filter(pool, !finished_post))
h <- t.test(v2018 ~ b, filter(pool, finished_post))

i <- t.test(registered ~ b, filter(pool, !finished_post))
j <- t.test(registered ~ b, filter(pool, finished_post))

k <- pool %>% 
  group_by(b, finished_post) %>% 
  summarize(Turnout = percent(mean(v2018), accuracy = .01),
            Registration = percent(mean(registered), accuracy = .01)) %>% 
  pivot_longer(cols = c(Turnout, Registration)) %>% 
  pivot_wider(id_cols = c(name, finished_post), values_from = c(value), names_from = "b") %>% 
  mutate(finished_post = ifelse(finished_post, "ITT", "Control")) %>% 
  arrange(desc(name), finished_post)

k$p <- round(c(g$p.value, h$p.value, i$p.value, j$p.value), digits = 3)

colnames(k) <- c("Statistic", "Group", "Non-Black", "Black", "p-value")
saveRDS(k, "./temp/means_diff_black.rds")

####################

how_many_restored <- sum(parolees$restored)
saveRDS(how_many_restored, "./temp/how_many_restored.rds")


## intend-to-treat models
parolees$pb <- (parolees$race == "BLACK") * (parolees$finished_post == T)

rd <- filter(parolees, year(parole_status_date) >= 2017)

f1 <- v2018 ~ finished_post + as.factor(race) +
  as.factor(sex) + age + v2008 +
  felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef

f2 <- v2018 ~ finished_post + as.factor(race) +
  as.factor(sex) + age + v2008 +
  felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef + pb

f3 <- registered ~ finished_post + as.factor(race) +
  as.factor(sex) + age + v2008 +
  felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef

f4 <- registered ~ finished_post + as.factor(race) +
  as.factor(sex) + age + v2008 +
  felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef + pb

models <- lapply(c(f1, f2, f3, f4), function(f){
  m <- lm(f, data = rd)
})

ses_cl <- list(
  summary(lm.cluster(formula = f1, data = rd, cluster = rd$finished_post))[ , 2],
  summary(lm.cluster(formula = f2, data = rd, cluster = rd$finished_post))[ , 2],
  summary(lm.cluster(formula = f3, data = rd, cluster = rd$finished_post))[ , 2],
  summary(lm.cluster(formula = f4, data = rd, cluster = rd$finished_post))[ , 2]
)

save(models, ses_cl, file = "./temp/individual_turnout_18_itt.rdata")


#### 
date_to_model_time <- lm(v2018 ~ days_bef + days2 + v2008 +
                           as.factor(race) + as.factor(sex) + age +
                           felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                         data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-05-18"))

date_ses <- data.table(lm_robust(v2018 ~ days_bef + days2 + v2008 +
                                   as.factor(race) + as.factor(sex) + age +
                                   felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                                 data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-05-18"))$std.error)

date_reg_model_time <- lm(registered ~ days_bef + days2 + v2008 +
                            as.factor(race) + as.factor(sex) + age +
                            felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                          data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-05-18"))

date_reg_ses <- data.table(lm_robust(registered ~ days_bef + days2 + v2008 +
                                       as.factor(race) + as.factor(sex) + age +
                                       felony_a + felony_b + felony_c + felony_d + felony_e + parole_time,
                                     data = filter(parolees, year(parole_status_date) >= 2017, parole_status_date < "2018-05-18"))$std.error)

save(date_to_model_time, date_ses, date_reg_model_time, date_reg_ses, file = "./temp/short_term_to.rdata")


##### RUN THE IV MODELS IN STATA BECAUSE MARGINAL EFFECTS ARE SO MUCH EASIER
##### THIS CREATES A TEX FILE THAT NEEDS TO BE MANUALLY CHANGED TO MATCH FORMATTING
parolees$restored2 <- 1 * parolees$restored
parolees$finished_post2 <- 1 * parolees$finished_post
parolees$male = 1 * (parolees$sex == "MALE")
parolees$nyc = (parolees$county %in% c("KINGS", "NEW YORK", "QUEENS", "BRONX", "RICHMOND"))*1
parolees$registered <- parolees$registered * 1

fwrite(filter(parolees, year(parole_status_date) >= 2017), "./temp/iv_data.csv")

parolees$rw <- with(parolees, (race == "BLACK") * restored == T)
parolees$pw <- with(parolees, (race == "BLACK") * finished_post == T)

iv3 <- ivreg(v2018 ~ restored + as.factor(race) +
               as.factor(sex) + age + v2008 +
               felony_a + felony_b + felony_c + felony_d + felony_e +
               parole_time + days_bef | . -restored + finished_post,
             data = filter(parolees, year(parole_status_date) >= 2017))

iv3_ses <- cluster.robust.se(iv3, filter(parolees, year(parole_status_date) >= 2017)$finished_post)[,2]

iv4<- ivreg(v2018 ~ restored + rw + as.factor(race) +
              as.factor(sex) + age + v2008 +
              felony_a + felony_b + felony_c + felony_d + felony_e +
              parole_time + days_bef | . -restored - rw + finished_post + pw,
            data = filter(parolees, year(parole_status_date) >= 2017))

iv4_ses <- cluster.robust.se(iv4, filter(parolees, year(parole_status_date) >= 2017)$finished_post)[,2]

iv3b <- ivreg(registered ~ restored + as.factor(race) +
                as.factor(sex) + age + v2008 +
                felony_a + felony_b + felony_c + felony_d + felony_e +
                parole_time + days_bef | . -restored + finished_post,
              data = filter(parolees, year(parole_status_date) >= 2017))

iv3_sesb <- cluster.robust.se(iv3b, filter(parolees, year(parole_status_date) >= 2017)$finished_post)[,2]

iv4b <- ivreg(registered ~ restored + rw + as.factor(race) +
                as.factor(sex) + age + v2008 +
                felony_a + felony_b + felony_c + felony_d + felony_e +
                parole_time + days_bef | . -restored - rw + finished_post + pw,
              data = filter(parolees, year(parole_status_date) >= 2017))

iv4_sesb <- cluster.robust.se(iv4b, filter(parolees, year(parole_status_date) >= 2017)$finished_post)[,2]


save(iv3, iv4, iv3b, iv4b,
     iv3_ses, iv4_ses, iv3_sesb, iv4_sesb, file = "./temp/iv_individual_turnout_18.rdata")

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
         v2018 = ifelse(is.na(v2018), 0, v2018),
         finished_post = parole_status_date >= "2016-05-18",
         days_since_done = as.numeric(as.Date("2016-11-08") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008),
         registered = voter_status %in% c("ACTIVE", "INACTIVE"),
         days_bef = as.Date("2016-10-14") - parole_status_date)

model4 <- lm(v2016 ~ finished_post +
                as.factor(race) + as.factor(sex) + age + v2008 +
                felony_a + felony_b + felony_c + felony_d + felony_e + parole_time +
               days_bef,
              data = parolees)

m4_ses <- summary(lm.cluster(formula = v2016 ~ finished_post +
                               as.factor(race) + as.factor(sex) + age + v2008 +
                               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time +
                               days_bef, data = parolees, cluster = parolees$finished_post))[ , 2]

save(model4, m4_ses, file = "./temp/individual_turnout_16.rdata")
