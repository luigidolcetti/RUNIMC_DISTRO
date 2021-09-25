archiveRDS<-function(x,
                     filepath = NULL,
                     forceSave = F){

  if (is.null(x)) return(list(r=0))
  mdf<-attr(x,'mdtnTimeStmp')
  arc<-attr(x,'artnTimeStmp')
  fpt<-attr(x,'fileArchive')
  sld<-attr(x,'sealed')
  if (is.null(filepath) & is.na(fpt)) stop(mError('specify a file where to save'))
  if (is.na(arc)) arc<-''
  if (mdf==arc & !forceSave) return(list(r=0))
  if (sld) return(list(r=0))
  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'mdtnTimeStmp')<-newTimeStmp
  attr(x,'artnTimeStmp')<-newTimeStmp
  attr(x,'fileArchive')<-filepath
  st<-try(saveRDS(x,filepath))
  if (inherits(st,'try-error')) {
    return(list(r=0))
  } else {
    return(list(r=1,
                mdtnTimeStmp=newTimeStmp,
                artnTimeStmp=newTimeStmp,
                fileArchive=filepath))
  }
}

archiveXML<-function(x,
                     objectType = c('analysis','study'),
                     filepath = NULL,
                     forceSave = F){

  objectType<-match.arg(objectType,objectType,several.ok = F)
  if (is.null(x)) return(list(r=0))
  mdf<-attr(x,'mdtnTimeStmp')
  arc<-attr(x,'artnTimeStmp')
  fpt<-attr(x,'fileArchive')
  sld<-attr(x,'sealed')
  if (is.null(filepath) & is.na(fpt)) stop(mError('specify a file where to save'))
  if (is.na(arc)) arc<-''
  if (mdf==arc & !forceSave) return(list(r=0))
  if (sld) return(list(r=0))
  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'mdtnTimeStmp')<-newTimeStmp
  attr(x,'artnTimeStmp')<-newTimeStmp
  attr(x,'fileArchive')<-filepath
  out<-XMLparseObject(x,objectType)
  st<-try(XML::saveXML(out,filepath))
  if (inherits(st,'try-error')) {
    return(list(r=0))
  } else {
    return(list(r=1,
                mdtnTimeStmp=newTimeStmp,
                artnTimeStmp=newTimeStmp,
                fileArchive=filepath))}
}

