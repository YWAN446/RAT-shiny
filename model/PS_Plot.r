PS_Plot<-function(main,num,ln_conc){
  par(pin=c(3.3,4.4))
  n<-round(num)
  c=round(ln_conc*10,digits=0)
  if (c>100) c=100;
  if (c<1) c=1;
  
  #current_dir<-getwd();
  #setwd("~/stat/RAT-shiny/pic");
  
  library(png)
  
  img.white <- readPNG("./pic/person.png")
  img.red <- readPNG("./pic/red_person.png")
  img.green <- readPNG("./pic/green_person.png")
  #img.green <- readPNG("./pic/grey_person.png")
  
  brightness <- function(hex) {
    v <- col2rgb(hex)
    sqrt(0.299 * v[1]^2 + 0.587 * v[2]^2 + 0.114 * v[3]^2) /255
  }
  
  img_to_colorramp <- function(img, ramp) {
    cv <- as.vector(img)
    b <- sapply(cv, brightness)
    g <- ramp(b)
    a <- substr(cv, 8,9)     # get alpha values
    ga <- paste0(g, a)       # add alpha values to new colors
    img.grey <- matrix(ga, nrow(img), ncol(img), byrow=TRUE)  
  }
  
  ramp <- function(colors) 
    function(x) rgb(colorRamp(colors)(x), maxColorValue = 255)
  
  ramp1 <- colorRamp(c("white","red1"))
  img.red <- as.raster(img.red)
  colorramp <- ramp((rgb(ramp1(seq(0, 1, length = 101)), max = 255)[c+1]))
  
  img.red<-img_to_colorramp(img.red,colorramp)
  plot(1:2,type="n",xlim=c(0,3.68),ylim=c(0,7.27),xaxt="n",yaxt="n",xlab="",ylab="",
       main=main, sub=paste("\nPercent Exposed =",n,"%","\nLog10 Dose=",signif(ln_conc,digits=3)),bty='n',cex.main=1.2)
  if (n<0 | n>100) print("Warning: Percent Exposed should between 0 and 100%!");
  if (n==0) {
    for (j in 0:99){
      rasterImage(img.green, 0.25+0.228*(j%%10)+0.1*(j%%10), 7.27-0.25-0.587*(j%/%10+1)-0.1*(j%/%10), 0.25+0.228*(j%%10+1)+0.1*(j%%10), 7.27-0.25-0.587*(j%/%10)-0.1*(j%/%10), interpolate=FALSE)
    }  
  }
  if (n==100) {
    for (i in 0:(n-1)) {
      rasterImage(img.red, 0.25+0.228*(i%%10)+0.1*(i%%10), 7.27-0.25-0.587*(i%/%10+1)-0.1*(i%/%10), 0.25+0.228*(i%%10+1)+0.1*(i%%10), 7.27-0.25-0.587*(i%/%10)-0.1*(i%/%10), interpolate=FALSE)
    }
  }
  if (n>0 & n<100){
    for (i in 0:(n-1)) {
      rasterImage(img.red, 0.25+0.228*(i%%10)+0.1*(i%%10), 7.27-0.25-0.587*(i%/%10+1)-0.1*(i%/%10), 0.25+0.228*(i%%10+1)+0.1*(i%%10), 7.27-0.25-0.587*(i%/%10)-0.1*(i%/%10), interpolate=FALSE)
    }
    for (j in n:99){
      rasterImage(img.green, 0.25+0.228*(j%%10)+0.1*(j%%10), 7.27-0.25-0.587*(j%/%10+1)-0.1*(j%/%10), 0.25+0.228*(j%%10+1)+0.1*(j%%10), 7.27-0.25-0.587*(j%/%10)-0.1*(j%/%10), interpolate=FALSE)
    }
  }
  #setwd(current_dir);
}
