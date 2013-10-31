sigmf <- function(x, a, c){
 #Do the herpa derpa addapted from: http://www.mathworks.de/de/help/fuzzy/sigmf.html 
  
  sigmf <- 1 / (1+ exp(a * (x - c)))
  
  
}