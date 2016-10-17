

# преобразовать объект xts в dataframe с сохранением индекса даты
XtstoDf <- function(ts, ...){ 
  df <- as.data.frame(ts)
  df$date <- time(ts)
  return(df)
}

#Реализация фильтра Ходрика-Прескотта
hpfilter <- function(x,lambda=1600){
  eye <- diag(length(x))
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x)
  return(result)
}


openXlsx <- function(file, sheet, ...){
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    readWorksheetFromFile(file, sheet) }
  else 
  {excel <-odbcConnectExcel2007(file)
  df <- sqlFetch(excel, sheet)
  close(excel)
  return(df)
  }
}

# Функция для нормирования многомерного xts с временными рядами
# Значение на дату start принимается за 100
Val2Base <- function(y, start='2012-01-01') {
  start <- paste(start, "::")
  base <- as.numeric(coredata(xts::first(y[start])))
  foo <- y
  for (i in 1:length(base)) foo[,i] <- foo[,i]/base[i]*100
  return(foo)
}

xtsMelt <- function(xtsData,metric){
  df <- data.frame(index(xtsData),coredata(xtsData),stringsAsFactors=FALSE)
  df.melt <- melt(df,id.vars=1)
  df.melt <- data.frame(df.melt,rep(metric,NROW(df.melt)))
  #little unnecessary housekeeping
  df.melt <- df.melt[,c(1,2,4,3)]
  colnames(df.melt) <- c("date","indexname","metric","value")
  df.melt$date <- as.Date(df.melt$date)
  #javascript works better when there are no .
  #remove troublesome . using modified method from Stack Overflow
  i <- sapply(df.melt, is.factor)
  df.melt[i] <- lapply(df.melt[i], gsub, pattern="\\.", replacement="")
  
  return(df.melt)
}


# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

readWorksheetFromURL <- function(url, sheet){ 
  tmpFile <- tempfile(fileext = ".xls") 
  download.file(url, tmpFile, mode= 'wb')
  df <- readWorksheetFromFile(tmpFile,sheet)
  
}

