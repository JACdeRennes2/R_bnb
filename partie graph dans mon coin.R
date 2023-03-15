

ggplot(airbnb_data, aes(x = period, y = log(realSum), fill = period)) +
  geom_boxplot() +
  facet_wrap(~ pays, ncol = 2) +
  scale_fill_manual(values = c("weekdays" = "lightblue", "weekends" = "pink")) +
  labs(title = "Prix semaine et weekend par pays",
       x = "Période",
       y = "log(Prix (en euros))") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  coord_flip()
