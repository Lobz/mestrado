distances<-function(x,y,diag.na=TRUE){
    dx<-outer(x,x,"-")
    dy<-outer(y,y,"-")
    sqd<- dx*dx+dy*dy
    if(diag.na){
        diag(sqd)<-NA
    }
    sqrt(sqd)
}

ripleysK<- function(distance,d){
    sum(distance<d,na.rm=TRUE)/nrow(distance)
}

plot.ripleysCurve<-function(distance){
    ds<-seq(0,max(distance,na.rm=TRUE),length.out=100)
    Ks<-sapply(ds,function(x)ripleysK(distance,x))
    plot(Ks~ds,type="l")
    data.frame(d=ds,K=Ks)
}
# microbenchmark(rK1(distance),rK2(distance))
#Unit: milliseconds
          #expr       min        lq     mean    median        uq       max neval
 #rK1(distance) 5384.9868 5737.6233 6151.198 6076.3482 6544.4239 7450.8414   100
 #rK2(distance) 5610.9036 6031.6575 6461.709 6345.8948 6959.6068 7672.6926   100

rK1<-function(distance){
    ds<-seq(max(distance,na.rm=TRUE),0,length.out=100)
    sapply(ds,function(x)ripleysK(distance,x))
}


rK2<-function(distance){
    n<-nrow(distance)
    dist<-na.omit(c(distance))
    ds<-seq(max(dist),0,length.out=100)
    sapply(ds,function(x)sum(dist<x)/n)
}
#microbenchmark(rK1(distance),rK3(distance),times=25)
#Unit: seconds
          #expr      min        lq      mean    median        uq       max neval
 #rK1(distance) 5.539847  5.753845  6.398355  6.742953  6.924375  6.979224    25
 #rK3(distance) 9.452783 10.630319 11.200226 11.589839 11.731328 12.404801    25

rK3<-function(distance){
    n<-nrow(distance)
    dist<-na.omit(c(distance))
    ds<-seq(max(dist),0,length.out=100)
    Ks<-array()
    for(i in 1:length(ds)){
        dist<-dist[dist<ds[i]]
        Ks[i]<-length(dist)
    }
    Ks/n
}

#microbenchmark(rK1(distance),rK4(distance),times=15)
#Unit: seconds
          #expr      min       lq     mean   median       uq      max neval
 #rK1(distance) 6.659809 6.678316 6.707263 6.691988 6.722456 6.786035    15
 #rK4(distance) 4.966289 5.004283 5.117810 5.035102 5.053751 5.916702    15

rK4<-function(distance){
    ds<-seq(0,max(distance,na.rm=TRUE),length.out=100)
    sapply(ds,function(x)sum(distance<x,na.rm=TRUE))/nrow(distance)
}

