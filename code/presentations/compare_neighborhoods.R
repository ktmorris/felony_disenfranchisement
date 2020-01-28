theme_bc <- function(){theme(plot.title = element_text(family = "Roboto Black", size = 23,
                                            color = "black", hjust = 0.5),
                  axis.title = element_text(family = "Roboto", 
                                            color = "#525353", size = 23),
                  axis.line = element_line(colour = "black"), 
                  text = element_text(family = "Roboto", color = "#525353"), 
                  plot.caption = element_text(family = "Roboto", size = 8), 
                  panel.border = element_blank(), panel.grid.major = element_blank(), 
                  panel.background = element_rect(fill = "#dddddb"), plot.background = element_rect(fill = "#dddddb"), 
                  panel.grid.minor = element_blank(), legend.key = element_blank(), 
                  legend.background = element_rect(color = "black", fill = "#dddddb"), 
                  plot.subtitle = element_text(hjust = 0.5), 
                  plot.margin = margin(10, 10, 10, 10),
                  axis.text = element_text(family = "Roboto", color = "#525353", size = 23))}



table <- readRDS("./temp/match_table_block_group.rds")

colnames(table) <- make.unique(make.names(colnames(table)))

table <- select(table, X, Treated, Untreated = Control, Control = Control.1)

table <- melt(table, id.vars = "X", value.vars = c("Treated", "Untreated", "Control"))

table$variable <- factor(table$variable, levels = c("Untreated", "Treated", "Control"))

table$value <- as.numeric(gsub(",", "", table$value))

ggplot(filter(table, X == "% Non-Hispanic White"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc +
  scale_y_continuous(labels = percent) + labs(y = "Share Non-Hispanic White", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Neighborhood Share Non-Hispanic White")

ggsave("./temp/perc_white_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, X == "% Non-Hispanic Black"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc +
  scale_y_continuous(labels = percent) + labs(y = "Share Non-Hispanic Black", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Neighborhood Share Non-Hispanic Black")

ggsave("./temp/perc_black_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, X == "% Latino"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc +
  scale_y_continuous(labels = percent) + labs(y = "Share Latinx", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Neighborhood Share Latinx")

ggsave("./temp/perc_latino_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, X == "Median Income"), aes(x = variable, y = value/1000, label = dollar(value, accuracy = 1))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc +
  scale_y_continuous(labels = dollar) + labs(y = "Neighborhood Median Income (Thousands)", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Neighborhood Median Income")

ggsave("./temp/income_spsa.png", width = 11, height = 7.25, units = "in")