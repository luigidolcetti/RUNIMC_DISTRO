rstPlot<-function(x,
                  quantCap = 1,
                  frame = T,
                  tick = T,
                  origin = c(-0.5,-0.5),
                  res = c(1,1),
                  majorT = 10,
                  cex.mjt = 20,
                  minorT = 1,
                  cex.mnt = 40,
                  asp=1){

  if (length(dim(x))==2) xTrans<-quantNorm(x,quantCap)
  if (length(dim(x))==3) {
    xi=dim(x)
    if (length(quantCap)!=xi[3]){
      ###add a warning###
      quantCap<-rep(quantCap[1],xi[3])
    }
    xTrans<-lapply(1:xi[3],function(i){
      quantNorm(x[,,i],quantCap[i])
    })
    if(xi[3]==2) xTrans<-append(xTrans,list(matrix(0,nrow = xi[1],ncol=xi[2])))
    xTrans<-array(unlist(xTrans,recursive = F),dim=c(xi[1:2],3))
  }

  ncl<-ncol(xTrans)
  nrw<-nrow(xTrans)
  x0<-origin[1]
  y0<-origin[2]
  xl<-ncl*res[1]
  yl<-nrw*res[2]
  xm<-x0+xl
  ym<-y0+yl
  if (xl>=yl) zl<-xl else zl<-yl
  xMjTl<-zl/cex.mjt
  yMjTl<-zl/cex.mjt
  xMnTl<-zl/cex.mnt
  yMnTl<-zl/cex.mnt

  par(mai=c(0,0,0,0))

  plot(NA,
       xlim=c(x0-xMjTl,xm+xMjTl),
       ylim=c(y0-yMjTl,ym+yMjTl),
       asp=asp,
       xaxs='i',
       yaxs='i',
       bty='n',
       xaxt='n',
       yaxt='n',
       ann=F)


  rasterImage(xTrans,x0,y0,xm,ym,interpolate = F)

  if (frame) rect(x0,y0,xm,ym)

  if (tick) {
    for (i in seq(ceiling(x0),floor(xm),majorT)){
      lines(matrix(c(i,y0,i,y0-xMjTl),ncol=2,byrow = T),)
      lines(matrix(c(i,ym,i,ym+xMjTl),ncol=2,byrow = T),)
    }
    for (i in seq(ceiling(y0),floor(ym),majorT)){
      lines(matrix(c(x0,i,x0-yMjTl,i),ncol=2,byrow = T))
      lines(matrix(c(xm,i,xm+yMjTl,i),ncol=2,byrow = T))
    }
    for (i in seq(ceiling(x0),floor(xm),minorT)){
      lines(matrix(c(i,y0,i,y0-xMnTl),ncol=2,byrow = T))
      lines(matrix(c(i,ym,i,ym+xMnTl),ncol=2,byrow = T))
    }
    for (i in seq(ceiling(y0),floor(ym),minorT)){
      lines(matrix(c(x0,i,x0-yMnTl,i),ncol=2,byrow = T))
      lines(matrix(c(xm,i,xm+yMnTl,i),ncol=2,byrow = T))
    }
  }
  return(list(x=x,
              quantCap=quantCap,
              majorT=majorT,
              minorT=minorT,
              x0=x0,
              y0=y0,
              xm=xm,
              ym=ym,
              majorT=majorT,
              minorT=minorT,
              xMjTl=xMjTl,
              xMnTl=xMnTl,
              yMjTl=yMjTl,
              yMnTl=yMnTl,
              usr=par('usr'),
              asp=asp))
}

rstLegend_continuous<-function(Rplot=NULL,
                               scaleCol = T,
                               scaleBar = T,
                               chNames=NULL,
                               cex = 1,
                               textC=NULL){

  x0Leader<-Rplot$usr[1]
  xmLeader<-Rplot$usr[2]
  xMjTl<-Rplot$xMjTl

  totaly<-(x0Leader+xmLeader+2*xMjTl)/1000
  sloty<-totaly/12
  par(mar=c(0,0,0,0),cex=cex)

  plot(NA,
       xlim=c(x0Leader,xmLeader),
       ylim=c(0,totaly),
       xaxs='i',
       yaxs='i',
       bty='n',
       xaxt='n',
       yaxt='n',
       ann=F)

  leftMargin<-x0Leader+(xmLeader- x0Leader)/10
  ndiv=255

  if (scaleCol){
    scl<-matrix(seq(0,1,length.out=ndiv),nro=1)
    if (length(dim(Rplot$x))==2){
      text(leftMargin+(xmLeader- x0Leader)/4,
           10.5*sloty,
           chNames,
           adj=c(0.5,0.5))
      rasterImage(scl,
                  leftMargin,
                  9*sloty,
                  leftMargin+(xmLeader- x0Leader)/2
                  ,10*sloty)
      rect(leftMargin,
           9*sloty,
           leftMargin+(xmLeader- x0Leader)/2
           ,10*sloty)

      quanX<-quantile(Rplot$x[Rplot$x>0],Rplot$quantCap)
      adjX<-Rplot$x
      adjX[adjX>quanX]<-quanX
      adjX<-(adjX-min(adjX))/(max(adjX)-min(adjX))
      adjX<-hist(adjX,breaks=ndiv,plot=F)
      adjX$counts<-log(adjX$counts)/max(log(adjX$counts))
      adjX<-matrix(c(leftMargin+ adjX$mids * ((xmLeader- x0Leader)/2),
                     (adjX$counts*sloty)+(9*sloty)),ncol=2)
      lines(adjX,col='blue')

      realXmax<-formatC(max(Rplot$x),digits = 1)
      quanXmax<-formatC(quantile(Rplot$x[Rplot$x>0],Rplot$quantCap),digits = 1)
      text(leftMargin,
           sloty*9.5,
           '0 -',
           adj=c(1,0.5))
      text(leftMargin+(xmLeader- x0Leader)/2,
           sloty*9.5,
           paste0('- ',quanXmax,'%',realXmax),
           adj=c(0,0.5))
    } else {

      dimX<-dim(Rplot$x)
      enlrg<-1/2
      totLengthScl<-(xmLeader- x0Leader)/(4)

      for (i in 1:dimX[3]){
        newScl<-array(rep(matrix(0,
                                 nrow=1,
                                 ncol=ndiv),
                          3), dim=c(1,ndiv,3))
        newScl[,,i]<-scl
        text(leftMargin+totLengthScl*(i-1),
             10.5*sloty,
             chNames[i],
             adj=c(0,0.5))
        rasterImage(newScl,
                    leftMargin+totLengthScl*(i-1),
                    9*sloty,
                    leftMargin+totLengthScl*i-(enlrg*totLengthScl),
                    10*sloty)
        rect(leftMargin+totLengthScl*(i-1),
             9*sloty,
             leftMargin+totLengthScl*i-(enlrg*totLengthScl),
             10*sloty)
        quanX<-quantile(Rplot$x[Rplot$x>0],Rplot$quantCap[i])
        adjX<-Rplot$x
        adjX[adjX>quanX]<-quanX
        adjX<-(adjX-min(adjX))/(max(adjX)-min(adjX))
        adjX<-hist(adjX,breaks=ndiv,plot=F)
        adjX$counts<-scales::modulus_trans(0)$transform(adjX$counts)/max(scales::modulus_trans(0)$transform(adjX$counts))
        adjX<-matrix(c(leftMargin+totLengthScl*(i-1)+ adjX$mids * (totLengthScl-(enlrg*totLengthScl)),
                       (adjX$counts*sloty)+(9*sloty)),ncol=2)
        lines(adjX,col='cyan')
        realXmax<-formatC(max(Rplot$x),digits = 1)
        quanXmax<-formatC(max(quanX),digits = 1)
        text(leftMargin,
             sloty*9.5,
             '0 -',
             adj=c(1,0.5))
        text(leftMargin+totLengthScl*i-(enlrg*totLengthScl),
             sloty*9.5,
             paste0('- ',quanXmax,'%',realXmax),
             adj=c(0,0.5))

      }
    }
  }
  if (scaleBar){
    tempy<-7*sloty
    lengthM<-sloty

    lines(matrix(c(leftMargin,tempy,
                   leftMargin+Rplot$majorT,tempy),
                 ncol=2, byrow = T))
    lines(matrix(c(leftMargin,tempy-lengthM,
                   leftMargin,tempy+lengthM),
                 ncol=2, byrow = T))
    lines(matrix(c(leftMargin+Rplot$majorT,tempy-lengthM,
                   leftMargin+Rplot$majorT,tempy+lengthM),
                 ncol=2, byrow = T))
    for (i in seq(leftMargin,leftMargin+Rplot$majorT,Rplot$minorT)){
      lines(matrix(c(i,tempy-lengthM/2,
                     i,tempy+lengthM/2),
                   ncol=2, byrow = T))
    }

    text(leftMargin+Rplot$majorT,tempy,
         paste0(' ',Rplot$majorT,'/',Rplot$minorT,' Units'),
         adj=c(0,0.5))
  }
  if(!is.null(textC)){
    tempy<-5*sloty
    text(leftMargin,tempy,textC,adj=c(0,1))
  }
}
