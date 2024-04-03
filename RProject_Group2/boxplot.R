data <- read.table("DatasetNA.txt", header = TRUE)

data <- na.omit(data) #NAlar?? ihmal etmek icin

#kendi boxplot fonksiyonumuz
draw_boxplot <- function(x, main, ylab) {
  
  stats <- fivenum(x)
  lower_hinge <- stats[2]
  upper_hinge <- stats[4]
  
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = main,
       xlim = c(0.5, 1.5), ylim = range(x))
  
  #box cizer
  segments(0.8, lower_hinge, 1.2, lower_hinge)
  segments(0.8, upper_hinge, 1.2, upper_hinge)
  segments(0.8, lower_hinge, 0.8, upper_hinge)
  segments(1.2, lower_hinge, 1.2, upper_hinge)
  draw_rect(0.8, lower_hinge, 1.2, upper_hinge, col = "lightblue", border = "black")
  
  
  #medyan cizer
  median <- stats[3]
  segments(0.8, median, 1.2, median, col = "red")
  
  #whisker cizer
  segments(1, stats[1], 1, stats[2])
  segments(1, stats[5], 1, stats[4])
  
  outliers <- which(x < stats[1] | x > stats[5])
  
  points(rep(1, length(outliers)), x[outliers], pch = 19)
  
  #eksenleri ayarlar
  axis(1, at = 1, labels = ylab)
  axis(2)
  box()
}
 
#kendi rect fonksiyonumuz
draw_rect <- function(x1, y1, x2, y2, col, border) {
  polygon(c(x1, x2, x2, x1), c(y1, y1, y2, y2), col = col, border = border)
}

variables <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")

#ekran?? bolme
par(mfrow = c(2, 4)) 


for (variable in variables) {
  
  #erkekler icin boxplot
  male_data <- subset(data, Gender == "Male")[, variable]
  draw_boxplot(male_data, paste(variable, "by Gender: Male"), variable)
  
  #kad??nlar icin boxplot
  female_data <- subset(data, Gender == "Female")[, variable]
  draw_boxplot(female_data, paste(variable, "by Gender: Female"), variable)
  
  #1den 5e kadar olan grafikler plot ekraninda bir onceki sayfada
}


variables <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")


for (variable in variables) {
  par(mfrow = c(1, 2)) 
  
  male_data <- data[data$Gender == "Male", variable]
  draw_boxplot(male_data, paste(variable, "by Gender: Male"), variable)
  
  
  female_data <- data[data$Gender == "Female", variable]
  draw_boxplot(female_data, paste(variable, "by Gender: Female"), variable)
  
  readline(prompt = "Press RUN to continue!") 
  
  dev.off()
}


