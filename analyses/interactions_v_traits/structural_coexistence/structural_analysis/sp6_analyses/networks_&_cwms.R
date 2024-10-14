
#library(GGally)

library(tidyverse)

ggplot(sp6sum, aes(x=cwm.sla, y=mean_dom)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Dominance") +
  xlab("CWM SLA")

ggplot(sp6sum, aes(x=cwm.height, y=mean_dom)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Dominance") +
  xlab("CWM Height")


ggplot(sp6sum, aes(x=cwm.sla, y=mean_asym)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Asymmetry") +
  xlab("CWM SLA")

ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_asym)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Asymmetry") +
  xlab("CWM LDMC")

ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_skew)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Asymmetry") +
  xlab("CWM LDMC")

ggplot(sp6sum, aes(x=cwm.sla, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM SLA")

ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM LDMC")

ggplot(sp6sum, aes(x=cwm.height, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM Height")

ggplot(sp6sum, aes(x=cwm.crsl, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM CRSL")

ggplot(sp6sum, aes(x=cwm.rmf, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM RMF")

ggplot(sp6sum, aes(x=cwm.pf, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM PF")

ggplot(sp6sum, aes(x=cwm.d, y=mean_mod)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Mean Modularity") +
  xlab("CWM Diameter")


ggpairs(sp6sum, columns = c(4:10))


