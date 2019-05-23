#######################################################################################################################################
# READ IN DOCCS DATA

names <- fread("./raw_data/doccs_data/parolee_column_names.csv", header = F)$V1

parolees <- read_fwf(
  file ="./raw_data/doccs_data/parolee_data/Parolee.011419.txt",
  fwf_widths(c(1, 9, 3, 10, 3, 7, 3, 30, 3, 6, 3, 10, 3, 15, 3,
               19, 3, 11, 3, 1, 3, 22, 3, 10, 3, 15, 2))
)

colnames(parolees) = names
rm(names)

parolees <- parolees %>% 
  filter(status == "ACTIVE" | 
           (status == "DISCHARGED" & status_date >= "2018-01-01")) %>% 
  select(din)


fwrite(parolees, "./temp/parolees.csv")