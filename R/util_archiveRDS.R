archiveRDS<-function(x,filepath=NULL,forceSave=F){
  if (is.null(x)) return(0)
  mdf<-attr(x,'mdtnTimeStmp')
  arc<-attr(x,'artnTimeStmp')
  fpt<-attr(x,'fileArchive')
  if (is.null(filepath) & is.na(fpt)) stop(mError('specify a file where to save'))
  if (is.na(arc)) arc<-''
  if (mdf==arc & !forceSave) return(0)
  saveRDS(x,filepath)
  return(1)
}
