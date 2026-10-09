# would be good to convert to function, and build functionality to 
# create directories on call

rm(list = ls())

library(GGIRread)
library(GGIR)
library(MIMSunit)
library(dplyr)

epochsize = 5 # ikke fra ggir
windowsizes = c(epochsize, 900, 3600)
epoch = paste(epochsize, "sec")

dynamic_range = c(-8, 8) # used by MIMS_UNIT make sure to update per device spec
# NOTE that GGIR seemingly just assumes +-8g, so need to figure out what
# to do if using device with less range

# NOTE lfe_select is FALSE by defualt in GGIR, but Karas used it!!!
lfe_select = FALSE # False by default in GGIR, but true for Karas!!!

datadir = ""

outputdir = ""

filenames = list.files(
  path=datadir, 
  pattern=NULL, 
  all.files=FALSE, 
  full.names=FALSE,
)

# funktioner taget fra GGIR
averagePerEpoch = function(x,sf,epochsize) {
  x2 = cumsum(c(0,x))
  select = seq(1,length(x2),by = sf*epochsize)
  x3 = diff(x2[round(select)]) / abs(diff(round(select)))
}


EuclideanNorm = function(xyz) {
  return(sqrt((xyz[,1]^2) + (xyz[,2]^2) + (xyz[,3]^2)))
}


for (filename in filenames[-48]){
  tic = Sys.time()
  print(paste(tic, "inspect", filename))
  inspect <- g.inspectfile(
    paste(datadir, filename, sep = "")
  )
  
  sf = inspect$sf # samplingsfrekvens fra inspect object 
  
  print(paste(Sys.time(), "calibrate", filename))
  calibrate <- g.calibrate(
    paste(datadir, filename, sep = ""),
    inspectfileobject = inspect,
  )
  
  print(paste(Sys.time(), "ggirread", filename))
  rdata <- readGENEActiv(
    paste(datadir, filename, sep = ""),
  )
  
  data = rdata$data.out
  temperaturecolumn = which(colnames(data) == "temperature") #GGIRread
  
  # CALIBRATE
  # temperaturecolumn = 6
  print(paste(Sys.time(), "apply calibration"))
  meantemp = mean(as.numeric(data[, temperaturecolumn]), na.rm = TRUE)
  yy = as.matrix(cbind(as.numeric(data[,temperaturecolumn]),
                       as.numeric(data[,temperaturecolumn]),
                       as.numeric(data[,temperaturecolumn])))
  data[,2:4] = scale(as.matrix(data[,2:4]), center = -calibrate$offset, scale = 1/calibrate$scale) +
    scale(yy, center = rep(meantemp,3), scale = 1/calibrate$tempoffset)
  rm(yy); gc()
  
  # fix time
  print(paste(Sys.time(), "fix time"))
  starttime = as.POSIXct(rdata$header$StarTime, format = "%Y-%m-%d %H:%M:%S")
  starttime_num = as.numeric(starttime)
  endtime_num = starttime_num + length(rdata$data.out$time) / sf 
  endtime = as.POSIXct(endtime_num)
  time = seq(from = starttime, to = endtime, length.out = length(rdata$data.out$time)+1)
  time = time[-length(time)]
  data$HEADER_TIME_STAMP = time
  rm(rdata); gc()
  
  # MIMS
  print(paste(Sys.time(), "prepare mims"))
  data <- select(data, HEADER_TIME_STAMP, x, y, z)
  
  data <- rename(
    data,
    X = x,
    Y = y,
    Z = z
  )
  
  rm(time); gc()
  
  print(paste(Sys.time(), "calling mims_unit"))
  mims_df <- mims_unit(
    data,
    dynamic_range = dynamic_range,
    epoch = epoch,
  )
  
  data = data[,2:4]
  
  # Filter-free Euclidean norm related metrics:
  print(paste(Sys.time(), "enmo"))
  EN = EuclideanNorm(data)
  ENMO = EN - 1
  ENMO[which(ENMO < 0)] = 0 #turning negative values into zero
  
  
  ENMO = round(averagePerEpoch(x = ENMO, sf, epochsize), 4) * 1000
  
  #truncate mims_df to length of enmo where samples dont line up with epochs
  print(paste(Sys.time(), "truncate mims_df"))
  mims_df = mims_df[1:length(ENMO),]
  
  mims_df$enmo = ENMO
  rm(ENMO); gc()
  
  print(paste(Sys.time(), "mad"))
  MEANS = rep(averagePerEpoch(x = EN, sf, epochsize), each = sf * epochsize)
  EN = EN[1:length(MEANS)]
  MAD = abs(EN - MEANS)
  MAD = round(averagePerEpoch(x = MAD, sf, epochsize), 4) * 1000
  mims_df$mad = MAD
  rm(EN); rm(MAD); rm(MEANS); gc()
  
  print(paste(Sys.time(), "counts"))
  mycounts = actilifecounts::get_counts(
    raw = data,
    sf = sf,
    epoch = epochsize,
    lfe_select = lfe_select,
    verbose = TRUE
  )
  mims_df$counts = round(mycounts[, 4], 4)
  rm(data); rm(mycounts); gc()
  
  # g.getmeta for non-wear, clipping og start
  print(paste(Sys.time(), "calling g.getmeta"))
  meta <- g.getmeta(
    paste(datadir, filename, sep = ""),
    offset = calibrate$offset,
    scale = calibrate$scale,
    tempoffset = calibrate$tempoffset,
    meantempcal = calibrate$meantempcal,
    inspectfileobject = inspect,
    windowsizes = windowsizes,
    nonwear_approach = "2023",
    do.imp = FALSE,
  )
  
  print(paste(Sys.time(), "calling g.weardec"))
  weardec <- g.weardec(
    meta,
    wearthreshold = 2, # number of axes for nonwear
    ws2 = windowsizes[2], # 15 min windows
    nonWearEdgeCorrection = TRUE # adding comma at end of line forces error 
  )
  
  #fjern ekstra tid i starten
  print(paste(Sys.time(), "discarding extra time and adding nw and clipping scores"))
  ggir_starttime = as.POSIXct(meta$metalong$timestamp[1], format = "%Y-%m-%dT%H:%M:%OS")
  mims_df = mims_df[which(mims_df$HEADER_TIME_STAMP >= ggir_starttime),]
  mims_df = mims_df[1:length(meta$metashort$timestamp),] # make sure length of mims_df matches
  mims_df = rename(mims_df, time = HEADER_TIME_STAMP)
  
  nonwear = weardec$r1 + weardec$r3
  mims_df$nonwear = rep(nonwear, each = 15 * 60 / epochsize)
  clipping = weardec$r2
  mims_df$clipping = rep(clipping, each = 15 * 60 / epochsize)
  rm(weardec); gc()
  
  # print til csv
  print(paste(Sys.time(), "prepare to print"))
  cal_out = c()
  cal_out$scalex = calibrate$scale[1]
  cal_out$scaley = calibrate$scale[2]
  cal_out$scalez = calibrate$scale[3]
  cal_out$offsetx = calibrate$offset[1]
  cal_out$offsety = calibrate$offset[2]
  cal_out$offsetz = calibrate$offset[3]
  cal_out$tempoffsetx = calibrate$tempoffset[1]
  cal_out$tempoffsety = calibrate$tempoffset[2]
  cal_out$tempoffsetz = calibrate$tempoffset[3]
  cal_out$calerrorstart = calibrate$cal.error.start
  cal_out$calerrorend = calibrate$cal.error.end
  cal_out$nhoursused = calibrate$nhoursused
  cal_out$QCmessage = calibrate$QCmessage
  cal_out$usetemp = calibrate$use.temp
  inspect$nonwearratio = sum(nonwear) / length(nonwear)
  inspect$clippingratio = sum(clipping) / length(clipping)
  print("writing inspect.csv")
  write.csv(cal_out, paste(outputdir, "calibrate/", filename, ".csv", sep = ""))
  print("writing calibrate.csv")
  write.csv(inspect, paste(outputdir, "inspect/", filename, ".csv", sep = ""))
  print("writing output.csv")
  write.csv(mims_df, paste(outputdir, "csv/", filename, ".csv", sep = ""))
  
  print(paste(Sys.time(), "cleaning up"))
  rm(cal_out); rm(inspect); rm(calibrate); rm(meta); rm(mims_df) 
  rm(endtime); rm(endtime_num); rm(ggir_starttime); rm(meantemp)
  rm(sf); rm(starttime); rm(starttime_num); rm(temperaturecolumn)
  rm(nonwear); rm(clipping)
  gc()
  print(paste(Sys.time(),"done with", filename))
  toc = Sys.time()
  tictoc = toc - tic
  print(tictoc)
  rm(tic); rm(toc); rm(tictoc); gc()
}
