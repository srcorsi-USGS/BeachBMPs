

test_plot <- function(filenm){
  file_to_save <- filenm #"./30_explore_data/figures/test_plot.pdf"
  pdf(file_to_save)
  x <- 1:10
  
    y <- 21:30
    plot(x,y,ylab="testing",main = "test plot")
  
  dev.off()
}

