#' Stringify object attributes
#'
#' Internal utility function to imake a string of object attributes.
#'
#' @param x an object
#' @return string
strgfObjectAttr<-function(x){
  out<-paste0('Created:\t\t',
  attr(x,'crtnTimeStmp'),'\n',
  'Modified:\t\t',
  attr(x,'mdtnTimeStmp'),'\n',
  'Archived:\t\t',
  attr(x,'artnTimeStmp'),'\n',
  'Archiviation file:\t',
  attr(x,'fileArchive'),'\n')
  return(out)
}
