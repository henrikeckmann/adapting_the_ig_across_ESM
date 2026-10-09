rm(list = ls())

library(psych)
library(irr)
library(blandr)
library(ggplot2)
library(yardstick)

df_avg_ig_long <- read.csv("")
# excluding outlier
df_avg_ig_long <- df_avg_ig_long[which(df_avg_ig_long$enmo_long < -1.22),]

ggplot(df_avg_ig_long, aes(x=enmo_long, y=ig_long)) + 
  geom_point() +
  # xlim(-5, -1) +
  # ylim(-5, -1) +
  facet_grid(vars(metric))
  
df_avg_ig_short <- read.csv("")
# excluding outlier
df_avg_ig_short <- df_avg_ig_short[which(df_avg_ig_short$enmo_avg_ig < -1.22),]

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=enmo_scaled_avg_ig)) +
  ggtitle("enmo scaled")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=mad_standard_avg_ig)) +
  ggtitle("mad standard")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=mad_modelled_avg_ig)) +
  ggtitle("mad modelled")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=mims_avg_ig)) +
  ggtitle("mims")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=counts_avg_ig)) +
  ggtitle("counts")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=mad_naive_avg_ig)) +
  ggtitle("mad naive")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=mims_naive_avg_ig)) +
  ggtitle("mims naive")

ggplot(df_avg_ig_short) + 
  geom_point(aes(x=enmo_avg_ig, y=counts_naive_avg_ig)) +
  ggtitle("counts naive")

#correlations
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$enmo_scaled_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_standard_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_modelled_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_naive_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_naive_avg_ig, method = "pearson")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_naive_avg_ig, method = "pearson")

cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$enmo_scaled_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_standard_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_modelled_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_naive_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_naive_avg_ig, method = "spearman")
cor(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_naive_avg_ig, method = "spearman")

# ICC
mad_st_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_standard_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
mad_mod_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_modelled_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
mims_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
counts_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
mad_na_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mad_naive_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
mims_na_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$mims_naive_avg_ig),
    model = "twoway", type = "consistency", unit = "single")
counts_na_icc<- icc(data.frame(df_avg_ig_short$enmo_avg_ig, df_avg_ig_short$counts_naive_avg_ig),
    model = "twoway", type = "consistency", unit = "single")


#BA-stuff
enmo_sc_ba <- blandr.statistics(df_avg_ig_short$enmo_scaled_avg_ig, df_avg_ig_short$enmo_avg_ig)
blandr.draw(df_avg_ig_short$enmo_scaled_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)

mad_st_ba <- blandr.statistics(df_avg_ig_short$mad_standard_avg_ig, df_avg_ig_short$enmo_avg_ig)
blandr.draw(df_avg_ig_short$mad_standard_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)

mad_mod_ba <- blandr.statistics(df_avg_ig_short$mad_modelled_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_mad_mod <- blandr.draw(df_avg_ig_short$mad_modelled_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_mad_mod <- bap_mad_mod + coord_cartesian(ylim=c(-1.8,0.01))
bap_mad_mod

ggsave(
  "bap_mad_mod.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

mims_ba <- blandr.statistics(df_avg_ig_short$mims_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_mims <- blandr.draw(df_avg_ig_short$mims_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_mims <- bap_mims + coord_cartesian(ylim=c(-1.8,0.01))
bap_mims

ggsave(
  "bap_mims.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

counts_ba <- blandr.statistics(df_avg_ig_short$counts_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_counts <- blandr.draw(df_avg_ig_short$counts_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_counts <- bap_counts + coord_cartesian(ylim=c(-1.8,0.01))
bap_counts

ggsave(
  "bap_counts.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

mad_naive_ba <- blandr.statistics(df_avg_ig_short$mad_naive_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_mad_naive <- blandr.draw(df_avg_ig_short$mad_naive_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_mad_naive <- bap_mad_naive + coord_cartesian(ylim=c(-0.01,1.8))
bap_mad_naive

ggsave(
  "bap_mad_naive.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

mims_naive_ba <- blandr.statistics(df_avg_ig_short$mims_naive_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_mims_naive <- blandr.draw(df_avg_ig_short$mims_naive_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_mims_naive <- bap_mims_naive + coord_cartesian(ylim=c(-0.01,1.8))
bap_mims_naive

ggsave(
  "bap_mims_naive.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)

counts_naive_ba <- blandr.statistics(df_avg_ig_short$counts_naive_avg_ig, df_avg_ig_short$enmo_avg_ig)
bap_counts_naive <- blandr.draw(df_avg_ig_short$counts_naive_avg_ig, df_avg_ig_short$enmo_avg_ig, ciDisplay = FALSE, ciShading = FALSE)
bap_counts_naive <- bap_counts_naive + coord_cartesian(ylim=c(-0.01,1.8))
bap_counts_naive

ggsave(
  "bap_counts_naive.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  scale = 1,
  width = 3.8,
  height = 3.75,
  units = "in",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
  create.dir = FALSE,
)


median(df_avg_ig_short$enmo_avg_ig)
quantile(df_avg_ig_short$enmo_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$mad_modelled_avg_ig)
quantile(df_avg_ig_short$mad_modelled_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$mad_naive_avg_ig)
quantile(df_avg_ig_short$mad_naive_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$mims_avg_ig)
quantile(df_avg_ig_short$mims_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$mims_naive_avg_ig)
quantile(df_avg_ig_short$mims_naive_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$counts_avg_ig)
quantile(df_avg_ig_short$counts_avg_ig, prob=c(.25,.5,.75), type=1)
median(df_avg_ig_short$counts_naive_avg_ig)
quantile(df_avg_ig_short$counts_naive_avg_ig, prob=c(.25,.5,.75), type=1)


# MPE
mpe(df_avg_ig_short, enmo_avg_ig, mad_modelled_avg_ig)
mpe(df_avg_ig_short, enmo_avg_ig, mims_avg_ig)
mpe(df_avg_ig_short, enmo_avg_ig, counts_avg_ig)
mpe(df_avg_ig_short, enmo_avg_ig, mad_naive_avg_ig)
mpe(df_avg_ig_short, enmo_avg_ig, mims_naive_avg_ig)
mpe(df_avg_ig_short, enmo_avg_ig, counts_naive_avg_ig)


