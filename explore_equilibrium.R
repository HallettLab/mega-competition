


mica <- residents_wet %>%
  select(MICA) %>%
  filter(MICA < 20000)

ggplot(mica, aes(x=MICA)) +
  geom_histogram()

min(mica$MICA)
max(mica$MICA)
sort(mica$MICA)
