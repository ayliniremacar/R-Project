data <- read.table("DatasetNA.txt", header = TRUE)
data <- na.omit(data)


par(mfrow=c(1,1))
writehist <- function(data, column, color, title, xlab, ylab, xlim, ylim, plot = TRUE) {
  if (plot == TRUE) {
    plot.new() #teker teker plotta gozukmesi icin
    
  } 
  
  plot(0:ylim, axes = FALSE, type = "n", xlab = xlab, ylab = ylab)
  title(title)
  axis(side = 2, c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, ylim)) #side=2 y eksenine yazar
  
  min_val <- min(data[column])
  max_val <- max(data[column])
  
  first_group <- 0
  second_group <- 0
  third_group <- 0
  fourth_group <- 0
  fifth_group <- 0
  
  for (x in 1:(nrow(data) - 1)) {
    if (data[[column]][x + 1] > min_val & data[[column]][x + 1] <= min_val + (max_val - min_val) / 8) {
      first_group <- first_group + 1
    } else if (data[[column]][x] > min_val + 0.12 & data[[column]][x] <= min_val + (max_val - min_val) / 6) {
      second_group <- second_group + 1
    } else if (data[[column]][x] > min_val + 0.16 & data[[column]][x] <= min_val + (max_val - min_val) / 4) {
      third_group <- third_group + 1
    } else if (data[[column]][x] > min_val + 0.25 & data[[column]][x] <= min_val + (max_val - min_val) / 2) {
      fourth_group <- fourth_group + 1
    } else {
      fifth_group <- fifth_group + 1
    }
  } 
  
  #kendi rect fonksiyonumuz
  my_rect <- function(xleft, ybottom, xright, ytop, col) {
    polygon(c(xleft, xright, xright, xleft), c(ybottom, ybottom, ytop, ytop), col = col)
  }
  
  my_rect(0, first_group, 20, 0, col = color)
  my_rect(20, second_group, 40, 0, col = color)
  my_rect(40, third_group, 60, 0, col = color)
  my_rect(60, fourth_group, 80, 0, col = color)
  my_rect(80, fifth_group, 100, 0, col = color)
  
  
  a <- sprintf("%.2f", min_val)
  text(1, -2, a, srt = 0)
  
  a <- sprintf("%.2f", min_val + (max_val - min_val) / 8) 
  text(20, -2, a, srt = 0) #x ekseninde texti hizalama 
  
  a <- sprintf("%.2f", min_val + (max_val - min_val) / 6)
  text(40, -2, a, srt = 0)
  
  a <- sprintf("%.2f", min_val + (max_val - min_val) / 4)
  text(60, -2, a, srt = 0)
  
  a <- sprintf("%.2f", min_val + (max_val - min_val) / 2)
  text(80, -2, a, srt = 0)
  
  a <- sprintf("%.2f", max_val)
  text(100, -2, a, srt = 0)
}

writehist(data, 4, "#f39999", "Histogram for Var1", "x-axis", "y-axis", 100, 100)
writehist(data, 5, "#18b6f4", "Histogram for Var2", "x-axis", "y-axis", 100, 100)
writehist(data, 6, "#800080", "Histogram for Var3", "x-axis", "y-axis", 100, 100)
writehist(data, 7, "#ffc3a0", "Histogram for Var4", "x-axis", "y-axis", 100, 100)
writehist(data, 8, "#ff7770", "Histogram for Var5", "x-axis", "y-axis", 100, 100)
writehist(data, 9, "#f59999", "Histogram for Var6", "x-axis", "y-axis", 100, 100)
writehist(data, 10, "#ff0000", "Histogram for Var7", "x-axis", "y-axis", 100, 100)
writehist(data, 11, "#33ff33", "Histogram for Var8", "x-axis", "y-axis", 100, 100)

multiple <- function() {
  par(mfcol = c(2, 4), mai = c(0.3, 0.3, 0.3, 0.3), cex = 0.5) #ekrani boler ve kenar bosluklari
  writehist(data, 4, "#33ffcc", "Histogram for Var1", "x", "y", 100, 100, FALSE)
  writehist(data, 5, "#00ffff", "Histogram for Var2", "x", "y", 100, 100, FALSE)
  writehist(data, 6, "#00cccc", "Histogram for Var3", "x", "y", 100, 100, FALSE)
  writehist(data, 7, "#006666", "Histogram for Var4", "x", "y", 100, 100, FALSE)
  writehist(data, 8, "#3366cc", "Histogram for Var5", "x", "y", 100, 100, FALSE)
  writehist(data, 9, "#0000ff", "Histogram for Var6", "x", "y", 100, 100, FALSE)
  writehist(data, 10, "#000066", "Histogram for Var7", "x", "y", 100, 100, FALSE)
  writehist(data, 11, "#77cc67", "Histogram for Var8", "x", "y", 100, 100, FALSE)
}
multiple()

