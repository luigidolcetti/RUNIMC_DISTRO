#' Initialize object attributes
#'
#' Internal utility function to initialize object attributes.
#'
#' @param x an object
#' @return the same object with attributes creation (crtnTimeStmp),
#'   modification (mdtnTimeStmp), archivation (artnTimeStmp), and
#'   the path to the archiviation folder (fileArchive)
initObjectAttr<-function(x){
  crtnTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'crtnTimeStmp')<-crtnTimeStmp
  attr(x,'mdtnTimeStmp')<-crtnTimeStmp
  attr(x,'artnTimeStmp')<-NA
  attr(x,'fileArchive')<-NA
  attr(x,'sealed')<-F
  return(x)
}
