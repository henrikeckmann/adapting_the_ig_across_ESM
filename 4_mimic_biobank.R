rm(list = ls())
datadir <- ""
outputdir <- ""

filenames = list.files(
  path=datadir, 
  pattern=NULL, 
  all.files=FALSE, 
  full.names=FALSE,
)

for (filename in filenames){
  df <- read.csv(paste(datadir, filename, sep = ""))
  
  imputed <- df$nonwear
  imputed[which(df$clipping == 1)] <- 1
  
  col1 <- paste(
    "acceleration (mg) - ",
    df$time[1],
    " - ",
    tail(df$time,1),
    " - sampleRate = 5 seconds",
    sep = ""
  )
  
  col2 <- "imputed"
  
  # GGIR expects mg and converts to g, so need to convert mims and counts to 
  # "milli-mims" and "milli-counts" here, like with the bins
  mims = data.frame(df$MIMS_UNIT*1000, imputed)
  colnames(mims) = c(col1, col2)
  write.csv(mims, paste(outputdir, "mims/", "mims_", filename, ".csv", sep = ""), row.names = FALSE)
  
  counts = data.frame(df$counts*1000, imputed)
  colnames(counts) = c(col1, col2)
  write.csv(counts, paste(outputdir, "counts/", "counts_", filename, ".csv", sep = ""), row.names = FALSE)
  
  enmo = data.frame(df$enmo, imputed)
  colnames(enmo) = c(col1, col2)
  write.csv(enmo, paste(outputdir, "enmo/", "enmo_", filename, ".csv", sep = ""), row.names = FALSE)
  
  mad = data.frame(df$mad, imputed)
  colnames(mad) = c(col1, col2)
  write.csv(mad, paste(outputdir, "mad/", "mad_", filename, ".csv", sep = ""), row.names = FALSE)
}

for (filename in filenames){
  df <- read.csv(paste(datadir, filename, sep = ""))
  
  imputed <- df$nonwear
  imputed[which(df$clipping == 1)] <- 1
  
  col1 <- paste(
    "acceleration (mg) - ",
    df$time[1],
    " - ",
    tail(df$time,1),
    " - sampleRate = 5 seconds",
    sep = ""
  )
  
  col2 <- "imputed"
  
  enmo = data.frame(df$enmo*100, imputed)
  colnames(enmo) = c(col1, col2)
  write.csv(enmo, paste(outputdir, "scaled_enmo/", "scaled_enmo_", filename, ".csv", sep = ""), row.names = FALSE)
}
