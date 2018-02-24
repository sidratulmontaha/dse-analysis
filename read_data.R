files <- list.files(full.names = TRUE, recursive = TRUE, include.dirs = FALSE, path = "C:\\Users\\Sidratul\\Desktop\\DSE Data\\", pattern="*.csv")

setAs("character","myDate", function(from) as.Date(from, format="%d-%m-%Y") )

dfList <- lapply(files, function(i) {
  print(i)
  df <- read.csv(i, stringsAsFactors = FALSE, strip.white = TRUE, header=FALSE, colClasses = c("character", "myDate", rep("numeric", 5), rep("NULL", 15)))
  return(df)
})

share_prices <- do.call(rbind, dfList)

colnames(share_prices) <- c("symbol", "date", "open", "high", "low", "close", "volume")

rm(dfList, files)
