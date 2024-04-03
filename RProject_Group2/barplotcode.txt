data <- read.table("DatasetNA.txt", header = TRUE)

my_barplot <- function(data,column,color,title,xlab,ylab,xlim,ylim,plot=TRUE){
  if(plot==TRUE){
    plot.new()
  } 
  
  uni = unique(data[column])
  plot(0:ylim, axes = FALSE, type = "n", xlab = xlab, ylab = ylab)
  
  title(title)
  axis(side = 2, c(0, 25, 50, 75, ylim)) #side=2 dikey cizgi
  
  for(x in 1:nrow(uni)-1){  #-1 son indeksi asmasini onlemek icin
    value=0
    
    value <- sum(data[[column]] == uni[[1]][x + 1])
    
    my_rect <- function(xleft, ybottom, xright, ytop, col) {
      polygon(c(xleft, xright, xright, xleft), c(ybottom, ybottom, ytop, ytop), col = col)
    }
    my_rect(x*25, 0, x*25+21, value, col = color) #rectleri hizalar
    
    
    text(27*x+10, -2, uni[[1]][x+1]) #texi hizalar
  }
}

my_barplot(data, 3, "#9933ff", "GENDERS", "x-axis", "y-axis", 0, 100)
my_barplot(data, 2, "#0066cc", "GROUPS", "x-axis", "y-axis", 0, 100)

twice <- function(){  #iki grafik tek ekran
  par(mfcol = c(1, 2), mai = c(0.5, 0.5, 0.5, 0.5))  #1x2 ve margins
  my_barplot(data, 3, "#fff333", "GENDER", "xlib", "ylib", 0, 100, FALSE)
  my_barplot(data, 2, "#00cc33", "GROUPS", "xlib", "ylib", 0, 100, FALSE)
}

twice()

