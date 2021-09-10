#' Modify object attributes
#'
#' Internal utility function to modify object attributes.
#'
#' @param x an object
#' @param crtnTimeStmp character as produced by format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#'    representing the creation time.
#' @param mdtnTimeStmp character, modification time
#' @param artnTimeStmp character, archiviation time
#' @param fileArchive character, archiviation path
#' @return the same object with attributes creation (crtnTimeStmp),
#'   modification (mdtnTimeStmp), archivation (artnTimeStmp), and
#'   the path to the archiviation folder (fileArchive)
mdfObjectAttr<-function(x,
                        crtnTimeStmp,
                        mdtnTimeStmp,
                        artnTimeStmp,
                        fileArchive){
  attr(x,'crtnTimeStmp')<-crtnTimeStmp
  attr(x,'mdtnTimeStmp')<-mdtnTimeStmp
  attr(x,'artnTimeStmp')<-artnTimeStmp
  attr(x,'fileArchive')<-fileArchive
  return(x)
}
