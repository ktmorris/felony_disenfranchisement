# 
# elects <- fread("./raw_data/misc/elects.csv")
# 
# ############# READ IN NYS VOTER FILES #############
# db <- dbConnect(SQLite(), "D:/rolls.db")
# nys_roll <- dbGetQuery(db, "select history, voter_status, nys_id from nys_roll_0319")
# 
# nys_roll <- nys_roll[nys_roll$nys_id %in% readRDS("./temp/din_nys_parolees.rds")$nys_id, ]
# 
# nys_roll <- nys_roll %>%
#   mutate(a = as.integer(voter_status == "ACTIVE"),
#          b = as.integer(voter_status == "INACTIVE"),
#          c = as.integer(voter_status == "PREREG"))
# 
# nys_roll <- setorder(nys_roll, nys_id, -a, -b, -c) ## KEEP ONE RECORD FOR EVERY VOTER
# nys_roll <- nys_roll[!duplicated(nys_roll$nys_id),]
# nys_roll <- select(nys_roll, -a, -b, -c)
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
#          v2016 = max(year == 2016 & election_type == "general")) %>%
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
         month_done = make_date(year = year(parole_status_date), month(parole_status_date), day = 1)) %>% 
  group_by(month_done) %>% 
  summarize(to16 = mean(v2016),
            to18 = mean(v2018),
            count = n(),
            share_reg = mean(reg))


to16_chart <- ggplot(filter(low_level, year(month_done) >= 2010, month_done < "2016-11-01"), aes(x = month_done, y = to16, weight = count)) + geom_line() +
  geom_smooth(data = filter(low_level, year(month_done) >= 2010, month_done < "2016-05-01"), formula = y ~ x + I(x^2), method = lm, fullrange = T) +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  geom_vline(xintercept = as.Date("2016-04-18"), color = "red") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2016-04-18"), y = 0.08, label = "April 18, 2016", hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to16_chart, "./temp/to16_chart.rds")


to16_chart_recent <- ggplot(filter(low_level, year(month_done) >= 2015, month_done < "2016-11-01"), aes(x = month_done, y = to16, weight = count)) + geom_line() +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  geom_vline(xintercept = as.Date("2016-05-21"), color = "red") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2016-05-21"), y = 0.08, label = "May 21, 2016", hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to16_chart_recent, "./temp/to16_chart.rds")


to18_chart <- ggplot(filter(low_level, year(month_done) >= 2012, month_done < "2018-11-01"), aes(x = month_done, y = to18, weight = count)) + geom_line() +
  geom_smooth(data = filter(low_level, year(month_done) >= 2012, month_done < "2018-05-01"), formula = y ~ x + I(x^2), method = lm, fullrange = T) +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  geom_vline(xintercept = as.Date("2018-04-18"), color = "red") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2018-04-18"), y = 0.06, label = "April 18, 2018", hjust = 1.05, family = "LM Roman 10") +
  theme(text = element_text(family = "LM Roman 10")) + expand_limits(y = c(0, .09))

saveRDS(to18_chart, "./temp/to18_chart.rds")

to18_chart_recent <- ggplot(filter(low_level, year(month_done) >= 2017, month_done < "2018-11-01"), aes(x = month_done, y = to18, weight = count)) + geom_line() +
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Month Discharged From Parole", y = "Turnout") +
  geom_vline(xintercept = as.Date("2018-04-18"), color = "red") +
  scale_x_date(labels = date_format("%b-%Y")) +
  annotate(geom = "text", x = as.Date("2018-04-18"), y = 0.045, label = "April 18, 2018", hjust = 1.05, family = "LM Roman 10") +
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