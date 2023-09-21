#stipa forb soil moisture exploration

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)http://127.0.0.1:33097/graphics/plot_zoom_png?width=1229&height=712
  sd(x2)/sqrt(length(x2))
}

theme_set(theme_classic())

SF <- soilmoisture_tot %>%
  filter(date < "2022-05-31") %>%
  group_by(logger, block, treatment, dowy, date) %>%
  summarize(mean.vwc = mean(vwc, na.rm = T), se.vwc = calcSE(vwc))


ggplot(SF, aes(x=dowy, y=mean.vwc, fill = logger, color = logger)) +
  geom_ribbon(aes(ymin = mean.vwc - se.vwc, ymax = mean.vwc + se.vwc), alpha = 0.5) +
  geom_vline(xintercept = 76, linetype = "dashed") + ## day shelters were deployed
  scale_fill_manual(values = c("#ca562c", "#008080", "#de8a5a", "#70a494", "#edbb8a", "#b4c8a8")) +
  scale_color_manual(values = c("#ca562c", "#008080", "#de8a5a", "#70a494", "#edbb8a", "#b4c8a8")) +
  xlab("Day of Water Year") +
  ylab("Mean VWC")

ggsave("data_cleaning/soil_moisture/meanVWC_bylogger_ribbon.png", height = 3.5, width = 8)

ggplot(SF, aes(x=dowy, y=mean.vwc, fill = logger, color = logger)) +
  geom_line() +
  geom_vline(xintercept = 76, linetype = "dashed") + ## day shelters were deployed
  scale_fill_manual(values = c("#ca562c", "#008080", "#de8a5a", "#70a494", "#edbb8a", "#b4c8a8")) +
  scale_color_manual(values = c("#ca562c", "#008080", "#de8a5a", "#70a494", "#edbb8a", "#b4c8a8")) +
  xlab("Day of Water Year") +
  ylab("Mean VWC")

ggsave("data_cleaning/soil_moisture/meanVWC_bylogger_line.png", height = 3.5, width = 8)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

