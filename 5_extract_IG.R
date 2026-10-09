rm(list = ls())
library(GGIR)

bins <- read.csv("")

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$mims_bins,
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$counts_bins,
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$mad_bins,
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = 1,
  epochvalues2csv = TRUE,
)

# mad with standard bins
GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = 1,
  epochvalues2csv = TRUE,
)

# enmo scaled *100
GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = c(seq(0, 4000, by = 25), 8000)*100,
  epochvalues2csv = TRUE,
)

# naive bins
GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$mims_naive_bins,
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$counts_naive_bins,
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = bins$mad_naive_bins,
  epochvalues2csv = TRUE,
)

# naive bins rounded

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = c(seq(0, 173*160, by=173), 173*160*2),
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = c(seq(0, 34000*160, by=34000), 34000*160*2),
  epochvalues2csv = TRUE,
)

GGIR(
  mode = c(1,2),
  datadir = "",
  outputdir = "",
  dataFormat = "ukbiobank_csv",
  iglevels = c(seq(0, 15*160, by=15), 15*160*2),
  epochvalues2csv = TRUE,
)
