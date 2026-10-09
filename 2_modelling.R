rm(list = ls())

library(rms)
library(ggplot2)
library(viridis)

palette <- viridis(6)

#read in .csv
df <- read.csv("")
df <- df[which(df$nonwear == 0 & df$clipping == 0),]
# df <- df[which(df$enmo >= 1500),]

# some functions seem to present problems when feeding a df to datadist
# works better with individual vectors
enmo <- df$enmo
mad <- df$mad
mims <- df$mims
counts <- df$counts

ddist <- datadist(enmo, mad, mims, counts) # create datadist with vectors
options(datadist = "ddist") # tell rms to use ddist

# MAD
print("mad")
# fit models
mad_fit_lm <- lm(mad ~ 0 + enmo) # no intercept linreg
mad_fit_0 <- ols(mad ~ enmo) # create fit ols object
mad_fit_3 <- ols(mad ~ rcs(enmo, c(1, 4000^(1/2), 4000))) # create fit ols object
mad_fit_4 <- ols(mad ~ rcs(enmo, c(1, 4000^(1/3), 4000^(2/3), 4000))) # create fit ols object
mad_fit_5 <- ols(mad ~ rcs(enmo, c(1, 4000^(1/4), 4000^(2/4), 4000^(3/4), 4000))) # create fit ols object
mad_fit_6 <- ols(mad ~ rcs(enmo, c(1, 4000^(1/5), 4000^(2/5), 4000^(3/5), 4000^(4/5), 4000))) # create fit ols object
mad_fit_7 <- ols(mad ~ rcs(enmo, c(1, 4000^(1/6), 4000^(2/6), 4000^(3/6), 4000^(4/6), 4000^(5/6), 4000))) # create fit ols object

# evaluation
mad_aics <- c(AIC(mad_fit_0), AIC(mad_fit_3), AIC(mad_fit_4), AIC(mad_fit_5), AIC(mad_fit_6), AIC(mad_fit_7))
mad_bics <- c(BIC(mad_fit_0), BIC(mad_fit_3), BIC(mad_fit_4), BIC(mad_fit_5), BIC(mad_fit_6), BIC(mad_fit_7))
mad_rsq <- c(mad_fit_0$stats["R2"], mad_fit_3$stats["R2"], mad_fit_4$stats["R2"], mad_fit_5$stats["R2"], mad_fit_6$stats["R2"], mad_fit_7$stats["R2"]) 

#plotting
print("plotting")

df$mad_fitted_0 <- mad_fit_0$fitted.values
df$mad_fitted_3 <- mad_fit_3$fitted.values
df$mad_fitted_4 <- mad_fit_4$fitted.values
df$mad_fitted_5 <- mad_fit_5$fitted.values
df$mad_fitted_6 <- mad_fit_6$fitted.values
df$mad_fitted_7 <- mad_fit_7$fitted.values

ggplot(df) + 
  geom_point(aes(x=enmo, y=mad), alpha=1/10) +
  geom_line(aes(x=enmo, y=mad_fitted_0), color=palette[1]) +
  geom_line(aes(x=enmo, y=mad_fitted_3), color=palette[2]) +
  geom_line(aes(x=enmo, y=mad_fitted_4), color=palette[3]) +
  geom_line(aes(x=enmo, y=mad_fitted_5), color=palette[4]) +
  geom_line(aes(x=enmo, y=mad_fitted_6), color=palette[5]) +
  geom_line(aes(x=enmo, y=mad_fitted_7), color=palette[6]) +
  ggtitle("MAD")

print("saving plot")
ggsave(
  "mad_models.png",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
)

ggplot(df[seq(1, 3881880, by = 10),]) + 
  geom_point(aes(x=enmo,y=mad), alpha=1) +
  geom_line(
    aes(x=enmo, y=mad_fitted_7),
    color="grey",
    linewidth = 1
    ) +
  theme_minimal() +
  ggtitle("MAD")

ggsave(
  "mad_modelfit.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 3,
  height = 3,
  units = "in",
  limitsize = TRUE,
)

# MIMS
print("mims")

# fit models
mims_fit_lm <- lm(mims ~ 0 + enmo) # no intercept linreg
mims_fit_0 <- ols(mims ~ enmo) # create fit ols object
mims_fit_3 <- ols(mims ~ rcs(enmo, c(1, 4000^(1/2), 4000))) # create fit ols object
mims_fit_4 <- ols(mims ~ rcs(enmo, c(1, 4000^(1/3), 4000^(2/3), 4000))) # create fit ols object
mims_fit_5 <- ols(mims ~ rcs(enmo, c(1, 4000^(1/4), 4000^(2/4), 4000^(3/4), 4000))) # create fit ols object
mims_fit_6 <- ols(mims ~ rcs(enmo, c(1, 4000^(1/5), 4000^(2/5), 4000^(3/5), 4000^(4/5), 4000))) # create fit ols object
mims_fit_7 <- ols(mims ~ rcs(enmo, c(1, 4000^(1/6), 4000^(2/6), 4000^(3/6), 4000^(4/6), 4000^(5/6), 4000))) # create fit ols object

# evaluation
mims_aics <- c(AIC(mims_fit_0), AIC(mims_fit_3), AIC(mims_fit_4), AIC(mims_fit_5), AIC(mims_fit_6), AIC(mims_fit_7))
mims_bics <- c(BIC(mims_fit_0), BIC(mims_fit_3), BIC(mims_fit_4), BIC(mims_fit_5), BIC(mims_fit_6), BIC(mims_fit_7))
mims_rsq <- c(mims_fit_0$stats["R2"], mims_fit_3$stats["R2"], mims_fit_4$stats["R2"], mims_fit_5$stats["R2"], mims_fit_6$stats["R2"], mims_fit_7$stats["R2"]) 

#plotting
print("plotting")

df$mims_fitted_0 <- mims_fit_0$fitted.values
df$mims_fitted_3 <- mims_fit_3$fitted.values
df$mims_fitted_4 <- mims_fit_4$fitted.values
df$mims_fitted_5 <- mims_fit_5$fitted.values
df$mims_fitted_6 <- mims_fit_6$fitted.values
df$mims_fitted_7 <- mims_fit_7$fitted.values

ggplot(df) + 
  geom_point(aes(x=enmo, y=mims), alpha=1/10) +
  geom_line(aes(x=enmo, y=mims_fitted_0), color=palette[1]) +
  geom_line(aes(x=enmo, y=mims_fitted_3), color=palette[2]) +
  geom_line(aes(x=enmo, y=mims_fitted_4), color=palette[3]) +
  geom_line(aes(x=enmo, y=mims_fitted_5), color=palette[4]) +
  geom_line(aes(x=enmo, y=mims_fitted_6), color=palette[5]) +
  geom_line(aes(x=enmo, y=mims_fitted_7), color=palette[6]) +
  ggtitle("MIMS")

print("saving plot")
ggsave(
  "mims_models.png",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
)

# have to scale mims to get plot to align with others
# then fix in post

df$mims = df$mims*100
df$mims_fitted_7 = df$mims_fitted_7*100

ggplot(df[seq(1, 3881880, by = 10),]) + 
  geom_point(aes(x=enmo,y=mims), alpha=1) +
  geom_line(
    aes(x=enmo, y=mims_fitted_7),
    color="grey",
    linewidth = 1
    ) +
  theme_minimal() +
  ggtitle("MIMS")

ggsave(
  "mims_modelfit.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 3,
  height = 3,
  units = "in",
  limitsize = TRUE,
)

# COUNTS
print("counts")
# fit models
counts_fit_lm <- lm(counts ~ 0 + enmo) # no intercept linreg
counts_fit_0 <- ols(counts ~ enmo) # create fit ols object
counts_fit_3 <- ols(counts ~ rcs(enmo, c(1, 4000^(1/2), 4000))) # create fit ols object
counts_fit_4 <- ols(counts ~ rcs(enmo, c(1, 4000^(1/3), 4000^(2/3), 4000))) # create fit ols object
counts_fit_5 <- ols(counts ~ rcs(enmo, c(1, 4000^(1/4), 4000^(2/4), 4000^(3/4), 4000))) # create fit ols object
counts_fit_6 <- ols(counts ~ rcs(enmo, c(1, 4000^(1/5), 4000^(2/5), 4000^(3/5), 4000^(4/5), 4000))) # create fit ols object
counts_fit_7 <- ols(counts ~ rcs(enmo, c(1, 4000^(1/6), 4000^(2/6), 4000^(3/6), 4000^(4/6), 4000^(5/6), 4000))) # create fit ols object

# evaluation
counts_aics <- c(AIC(counts_fit_0), AIC(counts_fit_3), AIC(counts_fit_4), AIC(counts_fit_5), AIC(counts_fit_6), AIC(counts_fit_7))
counts_bics <- c(BIC(counts_fit_0), BIC(counts_fit_3), BIC(counts_fit_4), BIC(counts_fit_5), BIC(counts_fit_6), BIC(counts_fit_7))
counts_rsq <- c(counts_fit_0$stats["R2"], counts_fit_3$stats["R2"], counts_fit_4$stats["R2"], counts_fit_5$stats["R2"], counts_fit_6$stats["R2"], counts_fit_7$stats["R2"]) 

#plotting
print("plotting")

df$counts_fitted_0 <- counts_fit_0$fitted.values
df$counts_fitted_3 <- counts_fit_3$fitted.values
df$counts_fitted_4 <- counts_fit_4$fitted.values
df$counts_fitted_5 <- counts_fit_5$fitted.values
df$counts_fitted_6 <- counts_fit_6$fitted.values
df$counts_fitted_7 <- counts_fit_7$fitted.values

ggplot(df) + 
  geom_point(aes(x=enmo, y=counts), alpha=1/10) +
  geom_line(aes(x=enmo, y=counts_fitted_0), color=palette[1]) +
  geom_line(aes(x=enmo, y=counts_fitted_3), color=palette[2]) +
  geom_line(aes(x=enmo, y=counts_fitted_4), color=palette[3]) +
  geom_line(aes(x=enmo, y=counts_fitted_5), color=palette[4]) +
  geom_line(aes(x=enmo, y=counts_fitted_6), color=palette[5]) +
  geom_line(aes(x=enmo, y=counts_fitted_7), color=palette[6]) +
  ggtitle("COUNTS")

print("saving plot")
ggsave(
  "counts_models.png",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 2000,
  height = 2000,
  units = "px",
  dpi = 300,
  limitsize = TRUE,
)

ggplot(df[seq(1, 3881880, by = 10),]) + 
  geom_point(aes(x=enmo,y=counts), alpha=1) +
  geom_line(
    aes(x=enmo, y=counts_fitted_6),
    color="grey",
    linewidth = 1
  ) +
  theme_minimal() +
  ggtitle("Counts")

ggsave(
  "counts_modelfit.svg",
  plot = last_plot(),
  device = NULL,
  path = "",
  width = 3,
  height = 3,
  units = "in",
  limitsize = TRUE,
)

print("abics")
abics <- data.frame(mad_aics, mad_bics, mad_rsq, mims_aics, mims_bics, mims_rsq, counts_aics, counts_bics, counts_rsq)
abics

write.csv(abics, "")

# based on BIC, best models are mad_fit_7, mims_fit_7 and counts_fit_6
# predict bins
mad_p <- Predict(mad_fit_7, enmo = seq(0,4000,by=25))
mims_p <- Predict(mims_fit_7, enmo = seq(0, 4000, by=25))
counts_p <- Predict(counts_fit_6, enmo = seq(0, 4000, by=25))

# BINS
# GGIR expects mg bins for g data. mad bins here are already in mg, but need to create
# milli-MIMS bins, and milli-counts bins
# coercing first value to zero, and double the modelled value for enmo=4000 for top value
enmo_bins <- c(seq(0, 4000, by = 25), 8000)
mad_bins <- c(0, mad_p$yhat[-1], tail(mad_p$yhat, 1)*2)
mims_bins <- c(0, mims_p$yhat[-1]*1000, tail(mims_p$yhat*1000, 1)*2)
counts_bins <- c(0, counts_p$yhat[-1]*1000, tail(counts_p$yhat*1000, 1)*2) 

#do naive bins
cdf <- ecdf(df$enmo)

mad_sf <- quantile(df$mad, cdf(4000), names = FALSE) / 4000
mims_sf <- quantile(df$mims, cdf(4000), names = FALSE) / 4000 * 1000
counts_sf <- quantile(df$counts, cdf(4000), names = FALSE) / 4000 * 1000

#TEST
# mad_sf <- quantile(df$mad, cdf(2000), names = FALSE) / 2000
# mims_sf <- quantile(df$mims, cdf(2000), names = FALSE) / 2000 * 1000
# counts_sf <- quantile(df$counts, cdf(2000), names = FALSE) / 2000 * 1000
# 
# mad_sf <- quantile(df$mad, cdf(25), names = FALSE) / 25
# mims_sf <- quantile(df$mims, cdf(25), names = FALSE) / 25 * 1000
# counts_sf <- quantile(df$counts, cdf(25), names = FALSE) / 25 * 1000

mad_naive_bins <- enmo_bins * mad_sf
mims_naive_bins <- enmo_bins * mims_sf
counts_naive_bins <- enmo_bins * counts_sf

bins <- data.frame(enmo_bins, mad_bins, mims_bins, counts_bins, mad_naive_bins, mims_naive_bins, counts_naive_bins)

# write csv
#write.csv(bins, "")
