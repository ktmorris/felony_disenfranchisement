# 
# elects <- fread("./raw_data/misc/elects.csv")
# 
# ############# READ IN NYS VOTER FILES #############
# db <- dbConnect(SQLite(), "D:/rolls.db")
# nys_roll <- dbGetQuery(db, "select history, voter_status, nys_id, political_party, registration_date from nys_roll_0319")
# 
# nys_roll <- nys_roll[nys_roll$nys_id %in% readRDS("./temp/din_nys_parolees.rds")$nys_id, ]
# 
# nys_roll <- left_join(nys_roll, readRDS("./temp/din_nys_parolees.rds"), by = "nys_id")
# 
# nys_roll <- cSplit(nys_roll, "history", sep = ";", direction = "long", type.convert = F)
# 
# nys_roll <- left_join(nys_roll, elects, by = "history")
# 
# 
# nys_roll <- nys_roll %>%
#   group_by(nys_id) %>%
#   mutate(year = ifelse(is.na(year), 0, year),
#          v2018 = max(year == 2018 & election_type == "general"),
#          v2016 = max(year == 2016 & election_type == "general"),
#          v2008 = max(year == 2008 & election_type == "general")) %>%
#   filter(row_number() == 1)
# 
# saveRDS(nys_roll, "./temp/parolee_to_18.rds")

nys_roll <- readRDS("./temp/parolee_to_18.rds")
parolees <- readRDS("./temp/parolees_with_restoration.rds")


parolees <- left_join(parolees, nys_roll, by = "din")
parolees$v2018 <- ifelse(is.na(parolees$v2018), 0, parolees$v2018)
parolees$v2016 <- ifelse(is.na(parolees$v2016), 0, parolees$v2016)
parolees$reg <- parolees$voter_status == "ACTIVE" & !is.na(parolees$voter_status)

low_level <- parolees %>% 
  filter(parole_status == "DISCHARGED") %>% 
  mutate(parole_status_date = as.Date(parole_status_date, "%m/%d/%Y"),
         month_done = make_date(year = year(parole_status_date), month(parole_status_date), day = 1),
         b = race == "BLACK") %>% 
  group_by(month_done) %>% 
  summarize(to16 = mean(v2016),
            to18 = mean(v2018),
            count = n(),
            share_reg = mean(reg))


to16_chart <- ggplot(filter(low_level, year(month_done) >= 2010, month_done < "2016-11-01"),
                     aes(x = month_done, y = to16, weight = count)) + geom_line() +
  geom_smooth(data = filter(low_level, year(month_done) >= 2010, month_done < "2016-06-01"),
              formula = y ~ x + I(x^2), method = lm, fullrange = T,
              color = "black",
              linetype = "longdash") +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  theme_bw() +
  geom_vline(xintercept = as.Date("2016-05-18"), color = "black", linetype = "dashed") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2016-05-18"), y = 0.08, label = "May 18, 2016",
           hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to16_chart, "./temp/to16_chart.rds")


to18_chart <- ggplot(filter(low_level, year(month_done) >= 2012, month_done < "2018-11-01"),
                     aes(x = month_done, y = to18, weight = count)) + geom_line() +
  geom_smooth(data = filter(low_level, year(month_done) >= 2012, month_done < "2018-06-01"),
              formula = y ~ x + I(x^2), method = lm, fullrange = T,
              color = "black",
              linetype = "longdash") +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  theme_bw() +
  geom_vline(xintercept = as.Date("2018-05-18"), color = "black", linetype = "dashed") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2018-05-18"), y = 0.06, label = "May 18, 2018",
           hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to18_chart, "./temp/to18_chart.rds")

to18_chart_recent <- ggplot(filter(low_level, year(month_done) >= 2017, month_done < "2018-11-01"), aes(x = month_done, y = to18, weight = count)) + geom_line() +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  theme_bw() +
  geom_vline(xintercept = as.Date("2018-05-18"), color = "red") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2018-05-18"), y = 0.045, label = "May 18, 2018", hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to18_chart_recent, "./temp/to18_chart_recent.rds")


### turnout stats

to_16_10 <- parolees %>% 
  filter(parole_status == "DISCHARGED",
         year(as.Date(parole_status_date, "%m/%d/%Y")) == 2010) %>% 
  summarize(mean(v2016)) %>% 
  pull()
saveRDS(to_16_10, "./temp/turnout_in_16_finished_10.rds")
to_16_15 <- parolees %>% 
  filter(parole_status == "DISCHARGED",
         year(as.Date(parole_status_date, "%m/%d/%Y")) == 2015) %>% 
  summarize(mean(v2016)) %>% 
  pull()
saveRDS(to_16_15, "./temp/turnout_in_16_finished_15.rds")

### restoration by day
tt <- parolees %>% 
  filter(parole_status == "DISCHARGED",
         !is.na(restored)) %>% 
  mutate(restored = ifelse(is.na(restored), F, restored)) %>% 
  group_by(parole_status_date) %>% 
  summarize(restoration_rate = mean(restored),
            count_restored = sum(restored))

not_found <- parolees %>% 
  filter(parole_status == "DISCHARGED",
         parole_status_date >= "2018-04-18") %>% 
  summarize(not_found = mean(is.na(restored)))

restoration_plot <- ggplot(filter(tt, parole_status_date >= "2018-04-15", parole_status_date < "2018-06-16"), aes(x = parole_status_date, y = restoration_rate)) + geom_line() +
  theme_minimal() + labs(x = "Date Discharged From Parole", y = "Share Formally Restored") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme(text = element_text(family = "LM Roman 10"))

saveRDS(restoration_plot, "./temp/restoration_plot.rds")

count_early <- sum(filter(tt, parole_status_date < "2018-05-21")$count_restored)

saveRDS(count_early, "./temp/count_restored_before_may21.rds")

##############################
dd <- parolees %>% 
  filter(parole_status == "DISCHARGED") %>% 
  mutate(parole_status_date = as.Date(parole_status_date, "%m/%d/%Y"),
         b = race == "BLACK") %>% 
  rename(day_done = parole_status_date) %>% 
  mutate(year = ifelse(year(day_done) %in% c(2015, 2016), 2016, 2018),
         turnout = ifelse(year == 2016, v2016, v2018),
         days = ifelse(year == 2016, day_done - as.Date("2016-05-20"), day_done - as.Date("2018-05-18")),
         group = paste0(year, days >= 0),
         age = as.numeric((as.Date("2016-11-08") - dob_parole) / 365.25),
         parole_time = as.numeric((day_done - release_date_parole) / 365.25),
         white = race == "WHITE",
         asian = race == "ASIAN/PACIFIC",
         black = race == "BLACK",
         latino = race == "HISPANIC",
         other_race = race == "OTHER",
         unknown_race = race == "UNKNOWN",
         treated = days >= 0)

h <- filter(dd, year == 2016) %>% 
  arrange(days)
h$rm <- rollmean(h$turnout, 30, fill = NA, align = "right")
i <- filter(dd, year == 2018) %>% 
  arrange(days)
i$rm <- rollmean(i$turnout, 30, fill = NA, align = "right")

dd <- bind_rows(h, i)

m1 <- lm(turnout ~ as.factor(year) * treated + white +
           as.factor(sex) + age +
           felony_a + felony_b + felony_c + felony_d + felony_e + parole_time, filter(dd, (day_done >= "2017-12-01" & day_done <= "2018-10-12")))
m2 <- lm(turnout ~ as.factor(year) * treated * white +
           as.factor(sex) + age +
           felony_a + felony_b + felony_c + felony_d + felony_e + parole_time, filter(dd, days >= -999))

stargazer(m1, m2,
          type = "text",
          omit = c("felony_", "as.factor(race)WHITE"),
          covariate.labels = c("2018", "Treated", "White",
                               "Male", "Age", "Time on Parole",
                               "2018 * Treated", "2018 * White", "Treated * White",
                               "2018 * Treated * White"),
          se = list(coef(summary(m1, cluster = c("group")))[, 2],
                    coef(summary(m2, cluster = c("group")))[, 2]))

################################
dd2 <- parolees %>% 
  filter(parole_status == "DISCHARGED") %>% 
  mutate(parole_status_date = as.Date(parole_status_date, "%m/%d/%Y"),
         b = race == "BLACK",
         day_done = make_date(year(parole_status_date), month(parole_status_date), 1)) %>% 
  group_by(day_done) %>% 
  mutate(n = n()) %>% 
  summarize_at(vars(v2016, v2018, n), mean) %>% 
  filter((day_done >= "2015-01-01" & day_done <= "2016-10-14") |
           (day_done >= "2017-01-01" & day_done <= "2018-10-12")) %>% 
  mutate(year = ifelse(year(day_done) %in% c(2015, 2016), 2016, 2018),
         turnout = ifelse(year == 2016, v2016, v2018),
         days = ifelse(year == 2016, day_done - as.Date("2016-05-20"), day_done - as.Date("2018-05-18"))) %>% 
  group_by(year) %>% 
  mutate(month = row_number() - 16)

h <- filter(dd2, year == 2016) %>% 
  arrange(days)
h$rm <- rollmean(h$turnout, 30, fill = NA, align = "right")
i <- filter(dd2, year == 2018) %>% 
  arrange(days)
i$rm <- rollmean(i$turnout, 30, fill = NA, align = "right")

dd2 <- bind_rows(h, i)


ggplot(filter(dd2, month >= -11), aes(x = month, y = turnout, linetype = as.factor(year))) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Discharge Date from Parole", y = "Turnout",
       linetype = "Year",
       caption = "Notes: \"0\" denotes April in both years. Turnout in 2016 shown for 2016 group, and 2018 turnout for 2018 group.") +
  theme_bw() +
  geom_vline(xintercept = 0, color = "red") +
  theme(text = element_text(family = "LM Roman 10"),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5)) + 
  scale_linetype_manual(values = c("longdash", "solid")) +
  ggtitle("Turnout Rate by Discharge Month")


m1b <- lm(turnout ~ as.factor(year) * I(days >= 0) + poly(days, 2), filter(dd2, days >= -999), weights = n)
summary(m1b)
