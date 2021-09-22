#'Some nice thing
#'
#'
#' @export
quantNorm<-function(fn_dt,
                    fn_trsh=0.997,
                    fn_zeroOff=T){

  xCoords<-ncol(fn_dt)
  yCoords<-nrow(fn_dt)
  if (fn_zeroOff){
    maxVal<- stats::quantile(fn_dt[fn_dt>0],probs = fn_trsh,na.rm=T)} else {
      maxVal<- stats::quantile(fn_dt,probs = fn_trsh,na.rm=T)
    }
  imgOut<-fn_dt
  imgOut[imgOut>maxVal]<-maxVal

  if (any(class(imgOut)=='RasterLayer')){
    imgOut<-(imgOut-raster::minValue(imgOut))/(raster::maxValue(imgOut)-raster::minValue(imgOut))
  } else {
    imgOut<-(imgOut-min(imgOut))/(max(imgOut)-min(imgOut))
  }
  return(imgOut)

}
