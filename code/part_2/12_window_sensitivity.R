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
         days_since_done = as.numeric(as.Date("2018-11-06") - parole_status_date),
         days2 = days_since_done^2,
         restored = ifelse(is.na(restored), F, restored),
         days_since_m21 = parole_status_date - as.Date("2018-05-19"),
         v2016 = ifelse(is.na(v2016), 0, v2016),
         v2008 = ifelse(is.na(v2008), 0, v2008),
         registered = voter_status %in% c("ACTIVE", "INACTIVE"),
         male = sex == "MALE")


iv1 <- as.data.frame(confint_robust(ivreg(v2018 ~ restored + male + race +
               age +
               felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
             data = filter(parolees, year(parole_status_date) >= 2017)), level = 0.95,
             HC_type = "HC1")) %>% 
  add_rownames(var = "coef") %>% 
  filter(coef == "restoredTRUE") %>% 
  rename(lb = `2.5 %`,
         ub = `97.5 %`) %>% 
  mutate(coef = "Rights Restored",
         estimate = (lb + ub) / 2,
         window = "1/1/2017 - 10/12/2018")

iv2 <- as.data.frame(confint_robust(ivreg(v2018 ~ restored + male + race +
                                     age +
                                     felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
                                   data = filter(parolees, parole_status_date >= as.Date("2017-06-01"))), level = 0.95,
                                   HC_type = "HC1")) %>% 
  add_rownames(var = "coef") %>% 
  filter(coef == "restoredTRUE") %>% 
  rename(lb = `2.5 %`,
         ub = `97.5 %`) %>% 
  mutate(coef = "Rights Restored",
         estimate = (lb + ub) / 2,
         window = "6/1/2017 - 10/12/2018")

iv3 <- as.data.frame(confint_robust(ivreg(v2018 ~ restored + male + race +
                                     age +
                                     felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
                                   data = filter(parolees, parole_status_date >= as.Date("2018-01-01"))), level = 0.95,
                                   HC_type = "HC1")) %>% 
  add_rownames(var = "coef") %>% 
  filter(coef == "restoredTRUE") %>% 
  rename(lb = `2.5 %`,
         ub = `97.5 %`) %>% 
  mutate(coef = "Rights Restored",
         estimate = (lb + ub) / 2,
         window = "1/1/2018 - 10/12/2018")

iv4 <- as.data.frame(confint_robust(ivreg(v2018 ~ restored + male + race +
                                     age +
                                     felony_a + felony_b + felony_c + felony_d + felony_e + parole_time | . -restored + finished_post,
                                   data = filter(parolees, parole_status_date >= as.Date("2018-02-18"),
                                                 parole_status_date <= as.Date("2018-08-18"))), level = 0.95,
                                   HC_type = "HC1")) %>% 
  add_rownames(var = "coef") %>% 
  filter(coef == "restoredTRUE") %>% 
  rename(lb = `2.5 %`,
         ub = `97.5 %`) %>% 
  mutate(coef = "Rights Restored",
         estimate = (lb + ub) / 2,
         window = "2/18/2018 - 8/18/2018")

estimates <- bind_rows(iv1, iv2, iv3, iv4)
estimates$window <- factor(estimates$window, levels = c("1/1/2017 - 10/12/2018",
                                                        "6/1/2017 - 10/12/2018",
                                                        "1/1/2018 - 10/12/2018",
                                                        "2/18/2018 - 8/18/2018"))

oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[1:4]


p <- ggplot(data = estimates) + theme_bw()

p <- p + ggstance::geom_pointrangeh(aes(y = coef, x = estimate, 
                                        xmin = lb, xmax = ub, color = window, 
                                        shape = window), position = ggstance::position_dodgev(height = -.5), 
                                    fill = "white", fatten = 3, size = 0.8, show.legend = T)

p <- p + geom_vline(xintercept = 0,
                    linetype = 2, 
                    size = 0.25) +
  scale_color_manual(values = c("#000000", "#2b6f39", "#d38fc5", "navy"), name = "Window") + 
  scale_shape_manual(values = shapes, name = "Window") + theme() + 
  drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                             axis.text.y = element_text(size = 10),
                             panel.grid.major.x = element_line(linetype = "solid"),
                             text = element_text(family = "LM Roman 10"),
                             legend.pos = "right") + 
  scale_x_continuous(labels = percent) +
  xlab("Effect on Turnout (Percentage Point)")

saveRDS(p, "./temp/window_change_plot.rds")
