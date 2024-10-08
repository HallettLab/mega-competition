## Explore Community properties & attributes

## number of invasive species & community weighted mean traits
# Origin vs CWM ####
## invasive ####
h = ggplot(sp6sum, aes(y=cwm.height, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM Height") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle("6 Species ")

s = ggplot(sp6sum, aes(y=cwm.sla, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM SLA") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

l = ggplot(sp6sum, aes(y=cwm.ldmc, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM LDMC") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

c = ggplot(sp6sum, aes(y=cwm.crsl, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM CRSL") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

r = ggplot(sp6sum, aes(y=cwm.rmf, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM RMF") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

p = ggplot(sp6sum, aes(y=cwm.pf, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM PF") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

d = ggplot(sp6sum, aes(y=cwm.d, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM Diameter") +
  xlab("Num Invasive Sp") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

## native ####
hn = ggplot(sp6sum, aes(y=cwm.height, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

sn = ggplot(sp6sum, aes(y=cwm.sla, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ln = ggplot(sp6sum, aes(y=cwm.ldmc, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

cn = ggplot(sp6sum, aes(y=cwm.crsl, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

rn = ggplot(sp6sum, aes(y=cwm.rmf, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

pn = ggplot(sp6sum, aes(y=cwm.pf, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

dn = ggplot(sp6sum, aes(y=cwm.d, x=as.factor(num.nat))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab("Num Native Sp") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

## legume ####
hl = ggplot(sp6sum, aes(y=cwm.height, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

sl = ggplot(sp6sum, aes(y=cwm.sla, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ll = ggplot(sp6sum, aes(y=cwm.ldmc, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

cl = ggplot(sp6sum, aes(y=cwm.crsl, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

rl = ggplot(sp6sum, aes(y=cwm.rmf, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

pl = ggplot(sp6sum, aes(y=cwm.pf, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

dl = ggplot(sp6sum, aes(y=cwm.d, x=as.factor(num.legume))) +
  geom_jitter(alpha = 0.15) +
  ylab(" ") +
  xlab("Num Legume Sp") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

## plot ####
ggarrange(h, hn, hl, s, sn, sl, l, ln, ll, c, cn, cl, r, rn, rl, p, pn, pl, d, dn, dl, nrow = 7, ncol = 3)

ggsave(paste0(fig_loc, "origin_cwm.png"), width = 10, height = 14)

# Fdiv * Num Inv vs Ndiff/Fdiff ####
fnin = ggplot(sp6sum, aes(x=fdiv, y=mean_niche, color = as.factor(num.inv))) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle("6 Species") +
  labs(color = "Num Invasives") +
  facet_wrap(~rainfall)

fnif = ggplot(sp6sum, aes(x=fdiv, y=mean_fitness, color = as.factor(num.inv))) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Fitness Differences") +
  xlab("Functional Diversity") +
  #ggtitle(" ") +
  labs(color = "Num Invasives") +
  facet_wrap(~rainfall)

ggarrange(fnin, fnif, ncol = 1, nrow = 2, common.legend = T, labels = "AUTO", legend = "right")

ggsave(paste0(fig_loc, "numinv_fdiv.png"), width = 6, height = 6)

# CWM * Num Inv vs. Ndiff ####
ggplot(sp6sum, aes(x=cwm.height, y=mean_niche, color = as.factor(num.inv))) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Niche Differences") +
  xlab("CWM Height") +
  ggtitle("6 Species") +
  labs(color = "Rainfall") +
  facet_wrap(~rainfall)

ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_niche, color = as.factor(num.inv))) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Niche Differences") +
  xlab("CWM LDMC") +
  ggtitle("6 Species") +
  labs(color = "Rainfall") +
  facet_wrap(~rainfall)

ggplot(sp6sum, aes(x=cwm.sla, y=mean_niche, color = as.factor(num.inv))) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Niche Differences") +
  xlab("CWM SLA") +
  ggtitle("6 Species") +
  labs(color = "Rainfall") +
  facet_wrap(~rainfall)
