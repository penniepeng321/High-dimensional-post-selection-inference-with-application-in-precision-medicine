#g-measure plots
par(mfrow=c(2,3), mar=c(4.5,4.5,2,0), oma=c(0,0,0,0))
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
##png(file="gms11_1.png",width=450,height=450)

n=200;p=100;s=5
m=min(n-1,p)
bs=c(0.2,0.3,0.4,0.5,0.6#,0.7,0.8
)

B1=read.table('B1.csv',header=T)
B2=read.table('B2.csv',header=T)
B3=read.table('B3.csv',header=T)
B4=read.table('B4.csv',header=T)
B5=read.table('B5.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.2,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
legend("bottomright", 
       c("BIC","LCV","BON","FDR","NPE"), 
       pch=c("I","C","","",""),
       lty=c(2,3,4,5,1),
       lwd=c(1,1,2,2,2),cex=1.5)
##dev.off()

#png(file="gms11_2.png",width=450,height=450)

n=200;p=100;s=10
m=min(n-1,p)
bs=c(0.2,0.3,0.4,0.5,0.6#,0.7,0.8
)
B1=read.table('B11.csv',header=T)
B2=read.table('B21.csv',header=T)
B3=read.table('B31.csv',header=T)
B4=read.table('B41.csv',header=T)
B5=read.table('B51.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.2,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()

#png(file="gms11_3.png",width=450,height=450)
n=200;p=100;s=20
m=min(n-1,p)
bs=c(0.2,0.3,0.4,0.5,0.6#,0.7,0.8
)

B1=read.table('B12.csv',header=T)
B2=read.table('B22.csv',header=T)
B3=read.table('B32.csv',header=T)
B4=read.table('B42.csv',header=T)
B5=read.table('B52.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.2,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)

#dev.off()
#png(file="gms11_4.png",width=450,height=450)

n=200;p=400;s=10
bs=c(0.3,0.4,0.5,0.6,0.7#,0.8,0.9
)
m=min(n-1,p)

B1=read.table('C1.csv',header=T)
B2=read.table('C2.csv',header=T)
B3=read.table('C3.csv',header=T)
B4=read.table('C4.csv',header=T)
B5=read.table('C5.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.6,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
legend("bottomright", 
       c("BIC","LCV","BON","FDR","NPE"), 
       pch=c("I","C","","",""),
       lty=c(2,3,4,5,1),
       lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_5.png",width=450,height=450)
n=200;p=400;s=20
bs=c(0.3,0.4,0.5,0.6,0.7#,0.8,0.9
)
m=min(n-1,p)

B1=read.table('C11.csv',header=T)
B2=read.table('C21.csv',header=T)
B3=read.table('C31.csv',header=T)
B4=read.table('C41.csv',header=T)
B5=read.table('C51.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.6,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_6.png",width=450,height=450)

n=200;p=400;s=40
bs=c(0.3,0.5,0.7,0.9,1.1#,1.3,1.5
)
m=min(n-1,p)

B1=read.table('C12.csv',header=T)
B2=read.table('C22.csv',header=T)
B3=read.table('C32.csv',header=T)
B4=read.table('C42.csv',header=T)
B5=read.table('C52.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.6,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_7.png",width=450,height=450)

n=200;p=2000;s=10
bs=c(0.3,0.4,0.5,0.6,0.7#,0.8,0.9
)
m=min(n-1,p)

B1=read.table('A38.csv',header=T)
B2=read.table('A39.csv',header=T)
B3=read.table('A40.csv',header=T)
B4=read.table('A41.csv',header=T)
B5=read.table('A42.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.3,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
legend("bottomright", 
       c("BIC","LCV","BON","FDR","NPE"), 
       pch=c("I","C","","",""),
       lty=c(2,3,4,5,1),
       lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_8.png",width=450,height=450)

n=200;p=2000;s=20
bs=c(0.3,0.4,0.5,0.6,0.7,0.8#,0.9,1.1
)
m=min(n-1,p)

B1=read.table('A45.csv',header=T)
B2=read.table('A46.csv',header=T)
B3=read.table('A47.csv',header=T)
B4=read.table('A48.csv',header=T)
B5=read.table('A49.csv',header=T)
B6=read.table('A50.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
B6=as.matrix(B6)
Bmat=cbind(B1,B2,B3,B4,B5,B6)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.3,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_9.png",width=450,height=450)

n=200;p=2000;s=40
bs=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.1,1.3,1.5,1.7,1.9)
m=min(n-1,p)

B1=read.table('A52.csv',header=T)
B2=read.table('A53.csv',header=T)
B3=read.table('A54.csv',header=T)
B4=read.table('A55.csv',header=T)
B5=read.table('A56.csv',header=T)
B6=read.table('A57.csv',header=T)
B7=read.table('A58.csv',header=T)
B8=read.table('D1.csv',header=T)
B9=read.table('D2.csv',header=T)
B10=read.table('D3.csv',header=T)
B11=read.table('D4.csv',header=T)
B12=read.table('D5.csv',header=T)


B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
B6=as.matrix(B6)
B7=as.matrix(B7)
B8=as.matrix(B8)
B9=as.matrix(B9)
B10=as.matrix(B10)
B11=as.matrix(B11)
B12=as.matrix(B12)
Bmat=cbind(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.3,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_10.png",width=450,height=450)

n=200;p=10000;s=10
bs=c(0.3,0.4,0.5,0.6,0.7#,0.8,0.9
)
m=min(n-1,p)

B1=read.table('A80.csv',header=T)
B2=read.table('A81.csv',header=T)
B3=read.table('A82.csv',header=T)
B4=read.table('A83.csv',header=T)
B5=read.table('A84.csv',header=T)

B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
Bmat=cbind(B1,B2,B3,B4,B5)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.1,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
legend("bottomright", 
       c("BIC","LCV","BON","FDR","NPE"), 
       pch=c("I","C","","",""),
       lty=c(2,3,4,5,1),
       lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
#png(file="gms11_11.png",width=450,height=450)

n=200;p=10000;s=20
bs=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.1,1.3)
m=min(n-1,p)

B1=read.table('A87.csv',header=T)
B2=read.table('A88.csv',header=T)
B3=read.table('A89.csv',header=T)
B4=read.table('A90.csv',header=T)
B5=read.table('A91.csv',header=T)
B6=read.table('A92.csv',header=T)
B7=read.table('A93.csv',header=T)
B8=read.table('D13.csv',header=T)
B9=read.table('D14.csv',header=T)


B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
B6=as.matrix(B6)
B7=as.matrix(B7)
B8=as.matrix(B8)
B9=as.matrix(B9)
Bmat=cbind(B1,B2,B3,B4,B5,B6,B7,B8,B9)

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.1,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()
###png(file="gms11_12.png",width=450,height=450)

n=200;p=10000;s=40
bs=c(0.3,0.4,0.5,0.6,0.7,0.8, 1.1,1.3,1.5,1.7,1.9
     )
m=min(n-1,p)

B1=read.table('A94.csv',header=T)
B2=read.table('A95.csv',header=T)
B3=read.table('A96.csv',header=T)
B4=read.table('A97.csv',header=T)
B5=read.table('A98.csv',header=T)
B6=read.table('A99.csv',header=T)

B8=read.table('D6.csv',header=T)
B9=read.table('D7.csv',header=T)
B10=read.table('D8.csv',header=T)
B11=read.table('D9.csv',header=T)
B12=read.table('D10.csv',header=T)


B1=as.matrix(B1)
B2=as.matrix(B2)
B3=as.matrix(B3)
B4=as.matrix(B4)
B5=as.matrix(B5)
B6=as.matrix(B6)

B8=as.matrix(B8)
B9=as.matrix(B9)
B10=as.matrix(B10)
B11=as.matrix(B11)
B12=as.matrix(B12)
Bmat=cbind(B1,B2,B3,B4,B5,B6  ,B8,B9,B10,B11,B12
           )

gmesmat=matrix(0,5,length(bs))

for(j in 1:length(bs)){
  B1=Bmat[,(16*(j-1)+1):(16*j)]
  for(k in 1:5){
    t1=B1[(B1[,16]!=0),2*k]/B1[(B1[,16]!=0),16]
    t2=1-(B1[(B1[,16]!=0),(2*k-1)]-B1[(B1[,16]!=0),(2*k)])/(m-B1[(B1[,16]!=0),16])
    gmesmat[k,j]=mean(sqrt(t1*t2))
  }
}

gmes1=gmesmat[1,]
gmes2=gmesmat[2,]
gmes3=gmesmat[3,]
gmes4=gmesmat[4,]
gmes5=gmesmat[5,]

plot(bs,gmes1,pch="I",ylim=c(0.1,1),xlab="
     intensity",ylab='g-measure',main=
       paste("n=",n,"/p=",p,"/s=",s,"/AR"),cex.lab = 1.5, cex.axis = 1.3)
lines(bs,gmes1,lty=2,lwd=1)
points(bs,gmes2,pch="C")
lines(bs,gmes2,lty=3,lwd=1)
points(bs,gmes3,pch="")
lines(bs,gmes3,lty=4,lwd=2)
points(bs,gmes4,pch="")
lines(bs,gmes4,lty=5,lwd=2)
lines(bs,gmes5,lty=1,lwd=2)
# legend("bottomright", 
#        c("BIC","LCV","BON","FDR","NPE"), 
#        pch=c("I","C","","",""),
#        lty=c(2,3,4,5,1),
#        lwd=c(1,1,2,2,2),cex=1.5)
#dev.off()