################################################### Load Image
library(imager)
img <- load.image(file.choose())

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}

################################################### Set up stuff
 target <- 1-img[,,1,1]
  
 cent <- c(round(dim(target)[1]/2,0),round(dim(target)[2]/2,0)) 
 rad<-round(dim(target)[1]/2,0)-1
 
################################################### Plot 
plot(img)
points(cent[1],cent[2],col="red",pch=20)

2*pi*rad
N<-99
angle.inc <- 2 * pi/N
angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
xv<-yv<-c() 
for(i in 1:N){
   xv[i] <- round(cos(angles[i]) * rad + cent[1],0)
   yv[i] <- round(sin(angles[i]) * rad + cent[2],0)  
} 
 points(xv,yv,pch=20,col="red")
 
################################################### Start search
Samps<-375
thresh<-0.2
decrement<-0.0
current <- rep(NA,Samps+1)
current[1] <- 1
shifty<-0.0001

for(z in 1:Samps){
score<-rep(NA,N)
for(j in c(1:N)[-current[z]]){ 
dx <- xv[current[z]] - xv[j]
dy <- yv[current[z]] - yv[j]

tx<-xv[current[z]]:xv[j]
ty = round(yv[current[z]] + dy * (tx - xv[current[z]]) / dx,0)

weight<-rep(NA,length(tx))
for(k in 1:length(tx)){
weight[k]<-target[tx[k],ty[k]]
}

score[j]<- mean(weight,na.rm=TRUE) # + mean(ifelse(weight>thresh,1,0),na.rm=TRUE)
 }
 
current[z+1]<-ifelse(runif(1,0,1)>shifty,which(score==max(score,na.rm=T))[sample(1:length(which(score==max(score,na.rm=T))),1)],sample(1:N,1)) 
dx <- xv[current[z]] - xv[current[z+1]]
dy <- yv[current[z]] - yv[current[z+1]]
tx<-xv[current[z]]:xv[current[z+1]]
ty = round(yv[current[z]] + dy * (tx - xv[current[z]]) / dx,0)
for(k in 1:length(tx)){
target[tx[k],ty[k]] <- target[tx[k],ty[k]]*decrement
}
print(z)
#if(z>2)
#segments(xv[current[z-1]],yv[current[z-1]],xv[current[z]],yv[current[z]], col=add.alpha("darkred",0.09) )
}

windows()
plot(xv[current],-yv[current],type="n")
dist<-c()
for(i in 2:Samps){
segments(xv[current[i-1]],-yv[current[i-1]],xv[current[i]],-yv[current[i]], col=add.alpha("black",0.2) )
dist[i] <- sqrt((xv[current[i]]-xv[current[i-1]])^2 + (yv[current[i]]-yv[current[i-1]])^2)
    }
    
print(sum(dist,na.rm=TRUE)/rad)*0.6


    
  
  
  
  
  

 
 
          
