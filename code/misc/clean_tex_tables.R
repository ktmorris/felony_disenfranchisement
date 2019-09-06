tab <- read.delim("./temp/iv_tables.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_tab_setup.csv")) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean.tex", quote = F, col.names = F,
            row.names = F)

######

tab <- read.delim("./temp/iv_tables_nw.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_tab_setup_race.csv")) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean_race.tex", quote = F, col.names = F,
            row.names = F)


######

tab <- read.delim("./temp/iv_tables_months.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_months_tab_setup.csv")) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean_months.tex", quote = F, col.names = F,
            row.names = F)