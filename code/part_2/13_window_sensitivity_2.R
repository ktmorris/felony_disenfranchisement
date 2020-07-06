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
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008),
         registered = voter_status %in% c("ACTIVE", "INACTIVE"),
         days_bef = as.Date("2018-10-12") - parole_status_date) %>% 
  mutate(age_16 = as.numeric((as.Date("2016-11-08") - dob_parole) / 365.25),
         parole_time_16 = as.numeric((parole_status_date - release_date_parole) / 365.25),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         finished_post_16 = parole_status_date >= "2016-05-18",
         days_since_done_16 = as.numeric(as.Date("2016-11-08") - parole_status_date),
         days2_16 = days_since_done^2,
         days_bef_16 = as.Date("2016-10-12") - parole_status_date)


hold_month <- as.Date("2018-05-18")

cints <- rbindlist(lapply(c(1:16), function(m){
  print(m)
  start <- if(month(hold_month) - m <= 0){
    make_date(year = 2017, month = month(hold_month) - m + 12, day = day(hold_month))
  }else{
    make_date(year = 2018, month = month(hold_month) - m, day = day(hold_month))
  }
  
  mebe <- if(month(hold_month) + m > 12){
    make_date(year = 2019, month = month(hold_month) + m - 12, day = day(hold_month))
  }else{
    make_date(year = 2018, month = month(hold_month) + m, day = day(hold_month))
  }
  
  end <- min(as.Date("2018-10-12"), mebe)
  
  mod <- lm.cluster(v2018 ~ finished_post + 
              as.factor(race) + as.factor(sex) + age +
              felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + v2008 +
                days_bef,
            data = filter(parolees, parole_status_date >= start,
                          parole_status_date <= end),
            cluster = "finished_post")
  
  v2018 ~ finished_post * I(race == "BLACK") + as.factor(race) +
    as.factor(sex) + age + v2008 +
    felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef
  
  ci <- confint(mod)
                              
  j <- data.table(months = m,
                  obs = nrow(filter(parolees, parole_status_date >= start,
                                    parole_status_date <= end)),
                  estimate = mod[["lm_res"]][["coefficients"]][["finished_postTRUE"]],
                  lower = ci[2,1],
                  upper = ci[2,2],
                  type = "2018")
  
  start <- make_date(year(start) - 2, month(start), day(start))
  end <- make_date(year(end) - 2, month(end), day(end))
  
  mod <- lm.cluster(v2016 ~ finished_post_16 + 
                      as.factor(race) + as.factor(sex) + age_16 +
                      felony_a + felony_b + felony_c + felony_d + felony_e + parole_time_16 + v2008 +
                      days_bef_16,
                    data = filter(parolees, parole_status_date >= start,
                                  parole_status_date <= end),
                    cluster = "finished_post_16")
  
  ci <- confint(mod)
  
  k <- data.table(months = m,
                  obs = nrow(filter(parolees, parole_status_date >= start,
                                    parole_status_date <= end)),
                  estimate = mod[["lm_res"]][["coefficients"]][["finished_post_16TRUE"]],
                  lower = ci[2,1],
                  upper = ci[2,2],
                  type = "2016")

  
  return(bind_rows(j, k))
  }))


cints$type <- factor(cints$type, levels = c("2018", "2016"))

coeff <- 1000000

p1 <- ggplot(filter(cints)) +
  facet_grid(rows = "type") +
  geom_col(aes(x = months, y = obs / 1000000),
            fill = "gray60", alpha = 0.5) +
  geom_errorbar(aes(x = months,
                    ymin = lower, ymax = upper), size = 0.2) +
  geom_line(aes(x = months, y = estimate, linetype = type), size = 0.2) + 
  geom_point(aes(x = months, y = estimate, shape = type), size = 1.5) + 
  theme_bw() +
  labs(y = "Estimated Treatment Effect",
       x = "Months Around Cutpoint",
       caption = "Notes: 95% confidence bars shown.
Regressions include covariates displayed in Table 4.
Number of observations in each regression shown on right axis.",
       shape = "Year",
       linetype = "Year") +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~ . * 1000000, name = "Number of Observations",
                                         labels = scales::comma,
                                         breaks = seq(0, 20000, 10000))) +
  theme(text = element_text(family = "LM Roman 10"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)

saveRDS(p1, "./temp/reestimates.rds")

####################################################################################

hold_month <- as.Date("2018-05-18")

cints <- rbindlist(lapply(c(1:16), function(m){
  print(m)
  start <- if(month(hold_month) - m <= 0){
    make_date(year = 2017, month = month(hold_month) - m + 12, day = day(hold_month))
  }else{
    make_date(year = 2018, month = month(hold_month) - m, day = day(hold_month))
  }
  
  mebe <- if(month(hold_month) + m > 12){
    make_date(year = 2019, month = month(hold_month) + m - 12, day = day(hold_month))
  }else{
    make_date(year = 2018, month = month(hold_month) + m, day = day(hold_month))
  }
  
  end <- min(as.Date("2018-10-12"), mebe)
  
  mod1 <- lm.cluster(v2018 ~ finished_post * I(race == "BLACK") + 
                       as.factor(race) + as.factor(sex) + age + v2008 +
                       felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + 
                       days_bef,
                     data = filter(parolees, parole_status_date >= start,
                                   parole_status_date <= end),
                     cluster = "finished_post")
  
  lm(v2018 ~ finished_post * I(race == "BLACK") +
       as.factor(race) + as.factor(sex) + age + v2008 +
       felony_a + felony_b + felony_c + felony_d + felony_e + parole_time + days_bef,
     data = filter(parolees, year(parole_status_date) >= 2017),
     cluster = "finished_post")
  
  
  
  ci <- confint(mod1)
  
  j <- data.table(months = m,
                  obs = nrow(filter(parolees, parole_status_date >= start,
                                    parole_status_date <= end)),
                  estimate = mod1[["lm_res"]][["coefficients"]][["finished_postTRUE"]],
                  lower = ci[2, 1],
                  upper = ci[2, 2],
                  type = "Overall")
  
  k <- data.table(months = m,
                  obs = nrow(filter(parolees, parole_status_date >= start,
                                    parole_status_date <= end)),
                  estimate = mod1[["lm_res"]][["coefficients"]][["finished_postTRUE:I(race == \"BLACK\")TRUE"]],
                  lower = ci[nrow(ci), 1],
                  upper = ci[nrow(ci), 2],
                  type = "White Add")
  
 
  return(bind_rows(j, k))
}))

coeff <- 1000000

ggplot(filter(cints)) +
  geom_col(aes(x = months, y = obs / coeff / 2),
           fill = "gray60", alpha = 0.5) +
  geom_errorbar(aes(x = months,
                    ymin = lower, ymax = upper), size = 0.2) +
  geom_line(aes(x = months, y = estimate, linetype = type), size = 0.2) + 
  geom_point(aes(x = months, y = estimate, shape = type), size = 1.5) + 
  theme_bw() +
  labs(y = "Estimated Treatment Effect",
       x = "Months Around Cutpoint",
       caption = "Notes: 95% confidence bars shown.
Regressions include covariates displayed in Table 4.
Number of observations in each regression shown on right axis.",
       shape = "Race",
       linetype = "Race") +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~ . * coeff, name = "Number of Observations",
                                         labels = scales::comma,
                                         breaks = seq(0, 20000, 10000))) +
  theme(text = element_text(family = "LM Roman 10"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", size = 1) +
  ggtitle("Turnout in 2018 Election")
