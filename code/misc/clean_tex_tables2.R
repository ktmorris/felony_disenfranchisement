tab <- read.delim("./temp/iv_tables_reg.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_tab_setup.csv") %>% 
                   mutate(new = gsub("Rights Restoration and Turnout",
                                     "Rights Restoration and Registration", new),
                          new = gsub("Dependent Variable: Cast Ballot in 2018 Election",
                                     "Dependent Variable: Registered to Vote", new))) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean_reg.tex", quote = F, col.names = F,
            row.names = F)

######

tab <- read.delim("./temp/iv_tables_nw_reg.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_tab_setup_race.csv") %>% 
                   mutate(new = gsub("Rights Restoration and Turnout",
                                     "Rights Restoration and Registration", new),
                          new = gsub("Dependent Variable: Cast Ballot in 2018 Election",
                                     "Dependent Variable: Registered to Vote", new))) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean_race_reg.tex", quote = F, col.names = F,
            row.names = F)


######

tab <- read.delim("./temp/iv_tables_months_reg.tex", header = F) %>% 
  mutate(row = row_number())

tab <- full_join(tab,
                 fread("./temp/iv_months_tab_setup.csv") %>% 
                   mutate(new = gsub("Rights Restoration and Turnout",
                                     "Rights Restoration and Registration", new),
                          new = gsub("Dependent Variable: Cast Ballot in 2018 Election",
                                     "Dependent Variable: Registered to Vote", new))) %>% 
  filter(remove != 1) %>% 
  mutate(V1 = gsub(" \\\\hline", "", as.character(V1))) %>% 
  arrange(sort) %>% 
  mutate(V1 = ifelse(is.na(V1), new, as.character(V1)),
         V1 = gsub("R-squared", "R$^2$", V1),
         V1 = gsub("Dependent Variable: ", "", V1)) %>% 
  dplyr::select(V1)

write.table(tab, "./temp/iv_clean_months_reg.tex", quote = F, col.names = F,
            row.names = F)