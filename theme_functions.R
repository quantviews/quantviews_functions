################################################################################
# функции для тем графиков 
###############################################################################

library(devtools)
require(extrafont)
#require(quantmod)
#library(ggfortify)

#fonts()

#devtools::install_github('bart6114/artyfarty')
#library(artyfarty)

# p + theme(axis.title.y = element_text(angle = 0, vjust =1))
# p + theme_tufte()
# p + theme_bain()
# 
# p+theme_rosneft()+scale_color_manual(values = rosneft_pal)+theme(axis.title.y = element_text(angle = 0, vjust =1))
# 
# p+theme_hse()+scale_color_manual(values = hse_pal)+theme(axis.title.y = element_text(angle = 0, vjust =1))
# 
# getSymbols('CPIAUCNS',src='FRED')
# plot(CPIAUCNS['2010::2016'])
# autoplot(CPIAUCNS['2010::2016'], ts.colour = hse_pal[1], ts.size = 1)+theme_hse()

par(bty = "l")  #  глобальный параметр - граница вокруг графика слева и снизу 
par(las = 1)   #  глобальный параметр - текст пишется параллельно оси X
#par(mar=c(3, 4, 2, 3 ))  #  глобальный параметр - границы между область графика и остальной частью 

#rn.pal <- c("#e6b228", "#e66c09", "#52555a", "#9ea2a5", "#6885af", "#abd3df", "#fff8ce")
#hse_colours <- c('#3255a4', '#9a3324', '#ff8038', '#55688b', '#4daa50', '#55688b','#ff8038')


rosneft_pal <- c("#FFD200", "#6B6B6B", "#F99D1C", "#6B6B6B", "#F99D1C", "#E2E2E2", "#FCAF17")
hse_pal <- c("#567dbf", "#9a3324", "#595959", "#6B6B6B", "#9063cd", "#cc9900", "#339933")
ief_pal = c("#3266cc", '#db3a10', '#f39b15', '#2f8234', '#990099', '#0099c5', '#999999', '#000000')
gpb_pal <- c('#01467a', '#7fa2bc', '#0169b7', '#e8343e', '#8fd5a6', '#5b6973', '#a6a6a6')

theme_rosneft <- function(grid_lines = "vertical"){
  
  color.background = "#FFFFFF"
  color.grid.major = "#cdcdcd"
  color.text = "#181818"
  color.axis = "#181818"
  color.background.legend = "#FFFFFF"
  
  if(grid_lines == "vertical"){
    grid.major.x = element_blank()
    grid.major.y = element_line()
  } else {
    grid.major.x = element_line()
    grid.major.y = element_blank()
  }
  
  theme_bw(base_size=11, base_family = fonts_selector("Calibri", "Arial", "sans-serif")) +
    theme(
      
      panel.background=element_rect(fill=color.background, color=color.background),
      plot.background=element_rect(fill=color.background, color=color.background),
      panel.border=element_rect(color=color.background),
      
      panel.grid.major=element_line(color=color.grid.major,size=.20),
      panel.grid.major.x=grid.major.x,
      panel.grid.major.y=grid.major.y,
      panel.grid.minor=element_blank(),
      axis.ticks.y=element_line(color=color.axis),
      axis.ticks.x=element_line(color=color.axis),
      axis.line.x = element_line(color=color.axis),
      axis.line.y = element_line(color=color.axis),
      
      
      legend.background = element_rect(fill=color.background.legend),
      legend.key = element_rect(fill=color.background, color=color.background),
      legend.text = element_text(size=rel(.8),color=color.text),
      legend.position = "bottom",
      
      plot.title=element_text(color=color.text, size=rel(1.3), face = "bold"),
      axis.text.x=element_text(size=rel(.9),color=color.text),
      axis.text.y=element_text(size=rel(.9),color=color.text),
      axis.title.x=element_text(size=rel(0.9),color=color.text, vjust=0),
      axis.title.y=element_text(size=rel(0.9),color=color.text, angle = 0, vjust =1),
      
      plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm")
    )
  
}

theme_hse <- function(grid_lines = "vertical"){
  
  color.background = "#FFFFFF"
  color.grid.major = "#d9d9d9"
  color.text = "#181818"
  color.axis = "#181818"
  color.background.legend = "#FFFFFF"
  
  if(grid_lines == "vertical"){
    grid.major.x = element_blank()
    grid.major.y = element_line()
  } else {
    grid.major.x = element_line()
    grid.major.y = element_blank()
  }
  
  theme_bw(base_size=11, base_family = fonts_selector("Arial","sans", "sans-serif")) +
    theme(
      
      panel.background=element_rect(fill=color.background, color=color.background),
      plot.background=element_rect(fill=color.background, color=color.background),
      panel.border=element_rect(color=color.background),
      
      panel.grid.major=element_line(color=color.grid.major,size=.20),
      panel.grid.major.x=grid.major.x,
      panel.grid.major.y=grid.major.y,
      panel.grid.minor=element_blank(),
      axis.ticks.y=element_line(color=color.axis),
      axis.ticks.x=element_line(color=color.axis),
      axis.line.x = element_line(color=color.axis),
      axis.line.y = element_line(color=color.axis),
      
      
      legend.background = element_rect(fill=color.background.legend),
      legend.key = element_rect(fill=color.background, color=color.background),
      legend.text = element_text(size=rel(.8),color=color.text),
      legend.position = "bottom",
      
      plot.title=element_text(color=color.text, size=rel(1.3), face = "bold"),
      axis.text.x=element_text(size=rel(.9),color=color.text),
      axis.text.y=element_text(size=rel(.9),color=color.text),
      axis.title.x=element_text(size=rel(0.9),color=color.text, vjust=0),
      axis.title.y=element_text(size=rel(0.9),color=color.text, angle = 0, vjust =1),
      
      plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm")
    )
  
}
