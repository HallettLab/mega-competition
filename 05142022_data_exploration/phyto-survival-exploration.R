library(tidyverse)


phyto_dat <- read.csv("20220508_MASTER_Collections.csv")

drought <- c(1, 3, 4, 6, 12, 14)
ambient <- c(5, 7, 8, 15, 16)

phyto_dat_clean <- phyto_dat %>%
  filter(phyto.n.indiv != -9999) %>% ## filtering skipped individuals
  filter(!is.na(phyto.n.indiv))  %>% ## filtering individuals that have not been censused yet
  filter(phyto != "VIVI", phyto != "ERBO", phyto != "TWIL-U", phyto != "THIR-U", phyto != "skip", phyto != "empty") %>%
  mutate(Treatment = ifelse(block %in% drought, "D", "A"))

trtcheck <- phyto_dat_clean %>%
  select(Treatment, block) %>%
  group_by(block) %>%
  summarise(unique(Treatment))
## looks good

## check number of phytos 
phyto_counts <- phyto_dat_clean %>%
  group_by(phyto, phyto.n.indiv) %>% 
  summarize(count=n())

## all species survival histogram
ggplot(phyto_dat_clean, aes(x=phyto.n.indiv)) +
  geom_histogram() +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count")

## native forbs
native <- c("ACAM", "AMME", "CLPU", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "TWIL-I")

## filter to include only native forbs
native_phytos <- phyto_dat_clean %>%
  filter(phyto %in% native)

ggplot(native_phytos, aes(x=phyto.n.indiv)) +
  geom_histogram() +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count")

ggplot(native_phytos, aes(x=phyto.n.indiv, fill = Treatment)) +
  geom_histogram(bins = 10, binwidth = 1, color = "black") +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count") +
  scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw()

ggsave("native-forb-phytos.png", height = 3.5, width = 6)

## introduced forbs
introduced_forbs <- c("ANAR", "BRNI", "CESO", "THIR-I")
intro_forbs_phytos <- phyto_dat_clean %>%
  filter(phyto %in% introduced_forbs)

ggplot(intro_forbs_phytos, aes(x=phyto.n.indiv)) +
  geom_histogram() +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count")

ggplot(intro_forbs_phytos, aes(x=phyto.n.indiv, fill = Treatment)) +
  geom_histogram(bins = 10, binwidth = 1, color = "black") +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count") +
  scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw()
ggsave("intro-forb-phytos.png", height = 3.5, width = 6)

## introduced grasses
introduced_grass <- c("AVBA", "BRHO", "TACA", "LOMU")
intro_grass_phytos <- phyto_dat_clean %>%
  filter(phyto %in% introduced_grass)

ggplot(intro_grass_phytos, aes(x=phyto.n.indiv)) +
  geom_histogram() +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count")

ggplot(intro_grass_phytos, aes(x=phyto.n.indiv, fill = Treatment)) +
  geom_histogram(bins = 10, binwidth = 1, color = "black") +
  facet_wrap(~phyto) +
  xlab("Number of Phytometers") + ylab("Count") +
  scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw()

ggsave("intro-grass-phytos.png", height = 3.5, width = 6)


## quantify survival of each species
surv_block <- phyto_dat_clean %>%
  mutate(survival = ifelse(phyto.n.indiv == 0, 0, 1)) %>%
  group_by(phyto, block) %>%
  summarise(perc_surv = sum(survival)/n(), Treatment = unique(Treatment)) #%>%

surv_block$block <- as.factor(surv_block$block)

surv_block <- surv_block %>%
  mutate(block = fct_relevel(block, "1", "3", "4", "6", "12", "14", "5", "7", "8", "15", "16"))

ggplot(surv_block, aes(x=block, y=perc_surv, fill = Treatment)) +
  geom_bar(position='dodge', stat='identity', color = "black")+
  ylab("Percent Survival") + xlab("Block") +
  #scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw() +
  facet_wrap(~phyto) +
  theme(text = element_text(size = 8)) +
  scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw()

ggsave("surv-block-trt.png", height = 4, width = 7)

surv_trt <- phyto_dat_clean %>%
  mutate(survival = ifelse(phyto.n.indiv == 0, 0, 1)) %>%
  group_by(phyto, Treatment) %>%
  summarise(perc_surv = sum(survival)/n()) 
  
ggplot(surv_trt, aes(x=phyto, y=perc_surv, fill = Treatment)) +
  geom_bar(position='dodge', stat='identity', color = "black")+
  ylab("Percent Survival") + xlab("Species") +
  scale_fill_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, hjust = 0.52))

ggsave("surv-trt.png", height = 4, width = 7)


surv_overall <- phyto_dat_clean %>%
  mutate(survival = ifelse(phyto.n.indiv == 0, 0, 1)) %>%
  group_by(phyto) %>%
  summarise(perc_surv = sum(survival)/n()) 

ggplot(surv_overall, aes(x=phyto, y=perc_surv)) +
  geom_bar(position='dodge', stat='identity', color = "black")+
  ylab("Percent Survival") + xlab("Species") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 45, hjust = 0.52))

ggsave("surv-overall.png", height = 4, width = 7)



## calculate num background individuals by each phyto - really to see how many are missing bg indiv