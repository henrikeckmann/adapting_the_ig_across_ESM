rm(list = ls())

library(psych)
library(irr)
library(ggplot2)

df <- read.csv("")

#correlations
mad_spearman <- cor(df$enmo_bins, df$mad_bins, method = "spearman")
mims_spearman <- cor(df$enmo_bins, df$mims_bins, method = "spearman")
counts_spearman <- cor(df$enmo_bins, df$counts_bins, method = "spearman")
mad_naive_spearman <- cor(df$enmo_bins, df$mad_naive_bins, method = "spearman")
mims_naive_spearman <- cor(df$enmo_bins, df$mims_naive_bins, method = "spearman")
counts_naive_spearman <- cor(df$enmo_bins, df$counts_naive_bins, method = "spearman")

# Kappa
mad_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$mad_bins), levels=1:161)
mims_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$mims_bins), levels=1:161)
counts_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$counts_bins), levels=1:161)
mad_naive_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$mad_naive_bins), levels=1:161)
mims_naive_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$mims_naive_bins), levels=1:161)
counts_naive_kappa <- cohen.kappa(data.frame(df$enmo_bins, df$counts_naive_bins), levels=1:161)

metrics <- c("mad","mims", "counts", "mad_naive", "mims_naive", "counts_naive")
kappas <- c(
  mad_kappa$weighted.kappa,
  mims_kappa$weighted.kappa,
  counts_kappa$weighted.kappa,
  mad_naive_kappa$weighted.kappa,
  mims_naive_kappa$weighted.kappa,
  counts_naive_kappa$weighted.kappa
)
out_df <- data.frame(metrics, kappas)
output <- ""
write.csv(out_df, output)
