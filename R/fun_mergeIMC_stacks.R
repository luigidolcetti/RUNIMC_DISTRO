#' Merge IMC stacks
#'
#' Utility function (should not be used directly) to merge two IMC stacks
#' obviously with reference to the same image (checked via uid).
#' Merge can preserve (update = F) or not all the layers in the case the two
#' stacks contain layers with the same name.
#' @param x IMC_RasterStack
#' @param y IMC_RasterStack
#' @param update, logical, preserve layers with identical name from
#'   both stacks (F)
#' @details All the additional slots (uid, study...) are copied from x
#'   while attributes are copied from y
#' @export
mergeIMC_stacks<-function(x,y,update=T){
  if (x@uid!=y@uid) stop(mError('uids are different'),call. = F)
  xnm<-names(x)
  ynm<-names(y)
  xout<-lapply(setNames(xnm,xnm),function(nm)x[[nm]])
  yout<-lapply(setNames(ynm,ynm),function(nm)y[[nm]])
  if (update){
    out<-raster::stack(append(xout[!(xnm%in%ynm)],yout))
  } else {
    newnm<-ynm
    newnm[(ynm%in%xnm)]<-paste0(ynm[(ynm%in%xnm)],'.new')
    names(yout)<-newnm
    out<-raster::stack(append(xout,yout))
  }
  out<-new('IMC_RasterStack',out)
  out@uid<-x@uid
  out@IMC_text_file<-x@IMC_text_file
  out@study<-x@study
  out@sample<-x@sample
  out@replicate<-x@replicate
  out@ROI<-x@ROI
  out@bioGroup<-x@bioGroup
  out@type<-x@type
  attr(out,'artnTimeStmp')<-attr(y,'artnTimeStmp')
  attr(out,'fileArchive')<-attr(y,'fileArchive')
  return(out)
}
