# Функция для построения и сохранения графиков независимо от платформы - Mac OS или Windows 
openGraph = function( width=7 , height=7 , family ="Helvetica", ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    
    quartz( width=width , height=height , family = family, ... ) 
    par(bty = "l")  #  глобальный параметр - граница вокруг графика слева и снизу 
    par(las = 1)   #  глобальный параметр - текст пишется параллельно оси X
    par(mar=c(4, 2, 1, 1 ))  #  глобальный параметр - границы между область графика и остальной частью 
  } else { # Windows OS
    windows(width=width , height=height ,family = family, ... )
    par(bty = "l")  #  глобальный параметр - граница вокруг графика слева и снизу 
    par(las = 1)   #  глобальный параметр - текст пишется параллельно оси X
    par(mar=c(2, 4, 1, 1 ))  #  глобальный параметр - границы между область графика и остальной частью 
    
  }
}
# Функция лля сохранения в файл текущего содержания окна quartz 
# Взято из R.app (базовый R)
quartz.save <- function (file, type = "png", device = dev.cur(), dpi = 300, 
                         ...) 
{
  dev.set(device)
  current.device <- dev.cur()
  nm <- names(current.device)[1]
  if (nm == "null device") 
    stop("no device to print from")
  if (!grDevices:::dev.displaylist()) 
    stop("can only print from a screen device")
  oc <- match.call()
  oc[[1]] <- as.name("dev.copy")
  oc$file <- NULL
  oc$device <- quartz
  oc$type <- type
  if (missing(file)) 
    file <- paste("Rplot", type, sep = ".")
  oc$file <- file
  oc$dpi <- dpi
  din <- dev.size("in")
  w <- din[1]
  h <- din[2]
  if (is.null(oc$width)) 
    oc$width <- if (!is.null(oc$height)) 
      w/h * eval.parent(oc$height)
  else w
  if (is.null(oc$height)) 
    oc$height <- if (!is.null(oc$width)) 
      h/w * eval.parent(oc$width)
  else h
  on.exit(dev.set(current.device))
  dev.off(eval.parent(oc))
}

dev.copy2png <- function(file)
{
  #  dev2bitmap(file = file, res = 150)   
  dev.copy(png, file = file,  bg = "white")
  dev.off()
}
# Функция для сохранения графиков в файл 
saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) { 
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      quartz.save( file=paste(file,".",type,sep="") , type=sptype , ... )      
    } 
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste(file,".",type,sep="") , ... )
    }
  } else { # Windows OS
    file=paste(file,".",type,sep="") # force explicit extension
    savePlot( file=file , type=type , ... )
  }
}

#Функция. Построение сезонного графика по годам 
SeasonalityDaily <- function(ts, years = 2011:2016, yl =''){
  ts.df <- as.data.frame(ts)
  names(ts.df) <- 'value'
  ts.df$date <- time(ts)
  ts.df$year <- format(time(ts), '%Y')
  ts.df$DayOfYear <- as.numeric( format(time(ts), '%j'))
  ts.df$month <- format(time(ts), '%b')
  current.year <- as.numeric(format(Sys.Date(), '%Y'))
  ts.df <- subset(ts.df, year %in% years)
  ts.df$alpha_line = ifelse(ts.df$year == current.year, 1.0, 0.9)
  ts.df$alpha_line<- factor((ts.df$alpha_line))
  
  p <- ggplot(ts.df, aes(x = DayOfYear, y = value, group = year))
  p<- p + geom_line(aes(color = year))+labs(x = '', color = '')+ylab("")+ief.theme+
    scale_colour_manual(values = rev(ief.palette[1:length(years)]))
  p <- p+geom_point(data = subset(ts.df, year == current.year), aes(x = DayOfYear, y = value), color = ief.palette[1], size = 1.5)
  
  
  p<- p +  scale_x_discrete(breaks = round(c(2, (1:11)*30.4375),0), 
                            labels=c("янв","фев","март","апр","май", 'июнь', 'июль', 'авг', 'сент', 'окт', 'ноя', 'дек'))
  p <- p + annotate("text", label = yl, parse = TRUE, x = 40, y = max(na.omit(ts.df$value)), size = 4)+theme(legend.direction = "horizontal", legend.position=c(0.5,0.4))
  return(p)
  
}



my.panel <- function(x, y, ..., pf = parent.frame()) {
  fmt <- "%b-%d-%y" # format for axis labels
  lines(x, y, ...)
  # if bottom panel
  if (with(pf, length(panel.number) == 0 || 
           panel.number %% nr == 0 || panel.number == nser)) { 
    # create ticks at x values and then label every third tick
    axis(side = 1, at = x, labels = FALSE)
    ix <- seq(1, length(x), 3)
    labs <- format(x, fmt)
    axis(side = 1, at = x[ix], labels = labs[ix], tcl = -0.7, cex.axis = 0.7)
  }
}

# Модификация функции chart.TimesSeries из пакета PerformanceAnalytics
# убраны границы вокруг легенды 
chart.TimeSeries.IEF <-
  function (R, auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, 
            type = "l", lty = 1, lwd = 2, main = NULL, ylab = NULL, xlab = "Date", 
            date.format.in = "%Y-%m-%d", date.format = NULL, xlim = NULL, 
            ylim = NULL, element.color = "black", event.lines = NULL, 
            event.labels = NULL, period.areas = NULL, event.color = "darkgray", 
            period.color = "aliceblue", colorset = ief.palette, pch = (1:12), 
            legend.loc = NULL, ylog = FALSE, cex.axis = 0.8, cex.legend = 0.8, 
            cex.lab = 1, cex.labels = 0.8, cex.main = 1, major.ticks = "auto", 
            minor.ticks = FALSE, grid.color = "lightgray", grid.lty = "solid", 
            xaxis.labels = NULL,columnnames = colnames(R), ...) 
  {
    y = checkData(R)
    columns = ncol(y)
    rows = nrow(y)
    #columnnames = colnames(y)
    if (is.null(date.format)) {
      freq = periodicity(y)
      yr_eq <- ifelse(format(index(first(y)), format = "%Y") == 
                        format(index(last(y)), format = "%Y"), TRUE, FALSE)
      switch(freq$scale, seconds = {
        date.format = "%H:%M"
      }, minute = {
        date.format = "%H:%M"
      }, hourly = {
        date.format = "%d %H"
      }, daily = {
        if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
      }, weekly = {
        if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"
      }, monthly = {
        if (yr_eq) date.format = "%b" else date.format = "%b %y"
      }, quarterly = {
        if (yr_eq) date.format = "%b" else date.format = "%b %y"
      }, yearly = {
        date.format = "%Y"
      })
    }
    rownames = as.Date(time(y))
    rownames = format(strptime(rownames, format = date.format.in), 
                      date.format)
    time.scale = periodicity(y)$scale
    ep = axTicksByTime(y, major.ticks, format.labels = date.format)
    logaxis = ""
    if (ylog) {
      logaxis = "y"
    }
    plot.new()
    if (is.null(xlim[1])) 
      xlim = c(1, rows)
    if (is.null(ylim[1])) {
      ylim = as.numeric(range(y, na.rm = TRUE))
    }
    plot.window(xlim, ylim, xaxs = "r", log = logaxis)
    if (is.null(ylab)) {
      if (ylog) 
        ylab = "ln(Value)"
      else ylab = "Value"
    }
    if (ylog) 
      dimensions = 10^par("usr")
    else dimensions = par("usr")
    if (!is.null(period.areas)) {
      period.dat = lapply(period.areas, function(x, y) c(first(index(y[x])), 
                                                         last(index(y[x]))), y = y)
      period.ind = NULL
      for (period in 1:length(period.dat)) {
        if (!is.na(period.dat[[period]][1])) {
          period.ind = list(grep(period.dat[[period]][1], 
                                 index(y)), grep(period.dat[[period]][2], index(y)))
          rect(period.ind[1], dimensions[3], period.ind[2], 
               dimensions[4], col = period.color, border = NA)
        }
      }
    }
    if (auto.grid) {
      #abline(v = ep, col = grid.color, lty = grid.lty)
      grid(NA, NULL, col = grid.color, lty = grid.lty, lwd = 0.5)
    }
    abline(h = 0, col = element.color)
    if (!is.null(event.lines)) {
      event.ind = NULL
      for (event in 1:length(event.lines)) {
        event.ind = c(event.ind, grep(event.lines[event], 
                                      rownames))
      }
      number.event.labels = ((length(event.labels) - length(event.ind) + 
                                1):length(event.labels))
      abline(v = event.ind, col = event.color, lty = 2)
      if (!is.null(event.labels)) {
        text(x = event.ind, y = ylim[2], label = event.labels[number.event.labels], 
             offset = 0.2, pos = 2, cex = cex.labels, srt = 90, 
             col = event.color)
      }
    }
    if (length(lwd) < columns) 
      lwd = rep(lwd, columns)
    if (length(lty) < columns) 
      lty = rep(lty, columns)
    if (length(pch) < columns) 
      pch = rep(pch, columns)
    for (column in columns:1) {
      lines(1:rows, y[, column], col = colorset[column], lwd = lwd[column], 
            pch = pch[column], lty = lty[column], type = type, 
            ...)
    }
    if (xaxis) {
      if (minor.ticks) 
        axis(1, at = 1:NROW(y), labels = FALSE, col = "#BBBBBB")
      label.height = cex.axis * (0.5 + apply(t(names(ep)), 
                                             1, function(X) max(strheight(X, units = "in")/par("cin")[2])))
      if (is.null(xaxis.labels)) 
        xaxis.labels = names(ep)
      else ep = 1:length(xaxis.labels)
      axis(1, at = ep, labels = xaxis.labels, las = 1, lwd = 1, 
           mgp = c(3, label.height, 0), cex.axis = cex.axis)
      title(xlab = xlab, cex = cex.lab)
    }
    if (yaxis) 
      if (yaxis.right) 
        axis(4, cex.axis = cex.axis, col = element.color, 
             ylog = ylog)
    else axis(2, cex.axis = cex.axis, col = element.color, 
              ylog = ylog)
    box(col = element.color)
    if (!is.null(legend.loc)) {
      legend(legend.loc, inset = 0.02, text.col = colorset, 
             col = colorset, cex = cex.legend, border.col = element.color, 
             lty = lty, lwd = 2, bty = "n", bg = "white", legend = columnnames)
    }
    if (is.null(main)) 
      main = columnnames[1]
    title(ylab = ylab, cex = cex.lab)
    title(main = main, cex = cex.main)
  }


RosneftChart <- function(ts, length = 12, date.format = '%d-%b', legend.loc = 'bottomleft', family = 'Calibri',
                         ylab ='', eng = FALSE, width = 8, height = 3.4, d.adj = 0.7, type = 'l',
                         date_lab = TRUE, ...){
  #Russian_Russia.1251
  if (eng) {Sys.setlocale(category='LC_ALL', locale='C')} 
  
  buffer <- xts::last(ts, paste(length, 'months'))
  buffer <- buffer[paste('::', end.date, sep = '')]
  buffer <- na.locf(buffer)
  
  openGraph(width, height, family = family)
  par(bty = "l")  #  глобальный параметр - граница вокруг графика слева и снизу 
  par(las = 1)   #  глобальный параметр - текст пишется параллельно оси X
  par(mar=c(2, 3, 1, 3 ))  #  глобальный параметр - границы между область графика и остальной частью 
  
  chart.TimeSeries.IEF(buffer, date.format = date.format, main = '', type = type,                      
                       colorset = rn.pal, legend.loc = legend.loc, ylab = '', xlab ='')
  
  text(x = 0, y = max(na.omit(buffer)), adj = -0.1,  labels = ylab, cex = 0.7)
  
  d.text<- paste('', format(index(first(buffer)), format = '%d-%b-%Y'), ' - ', 
                 format(index(last(buffer)), format = '%d-%b-%Y'), sep = '')
  
  text(x = round(length(buffer[,1])*d.adj), y = max(na.omit(buffer)),  labels = d.text, cex = 0.7, col = 'grey')
  if(date_lab){
    mtext(side = 4, at =last(buffer), 
          text = round(coredata(last(buffer)),1), 
          col = rn.pal[1:length(names(buffer))], 
          cex = 0.8, adj = 0)
  }
  if (eng) {Sys.setlocale(category='LC_ALL', locale='Russian_Russia.1251')} 
}

