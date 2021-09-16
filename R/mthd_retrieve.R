#' Archive
#'
#' Archive is used to save to disk an entire study, an Analysis or a part of it
#'
#' @param x a study (environment) or one of its components
#' @param what which component of a study to archive (not implemented at the moment)
#' @param objectReturn should the function return the object (TRUE) or the file path (FALSE)?
#' @param forceSave each object in a study is time stamped and saving is inhibited if this object has not been modified. forceSave set to TRUE revert this behaviour
#' @param studyTable explicitly provide the study table
#' @return the same object after it has been written to disk or a character string representing the file path
#' @seealso
#' @examples
#' \dontrun{
#' archive(x = MyStudy)
#' }
#' @details archive has specific methods for a study and for its elements
#' @export
#' @docType methods
#' @rdname Archive-methods
setGeneric("retrieve", function(x,...)
  standardGeneric("retrieve"))

setMethod("retrieve","character",function(x,...){

  if (file.exists(x) & dir.exists(x)) pathIsFile<- F
  if (file.exists(x) & !dir.exists(x)) pathIsFile<- T
  if (!file.exists(x) & !dir.exists(x)) pathIsFile<- NA

  if (is.na(pathIsFile)) stop(mError('Cannot open this file or directory'))

  if (!pathIsFile) {

    newCollection<-list.files(x,pattern = '*.stk',include.dirs = F,recursive = F,full.names = T)
    newCollection<-lapply(newCollection,IMCstackOpen)
    uids<-lapply(newCollection,slot,'uid')
    names(newCollection)<-uids
    type<-unique(unlist(lapply(newCollection,slot,'type')))

    if (length(type)>1){
      warning(mWarning('heterogenous types, changing to ANY'))
      type<-'ANY'
    }

    switch (type,

            ANY = newCollection<-new('IMC_RsCollection',newCollection),
            raw = newCollection<-new('IMC_RsCollection',newCollection),
            calc = newCollection<-new('IMC_RsCollection',newCollection),
            class = newCollection<-new('IMC_Classification',newCollection))

    return(newCollection)
  }

  if (pathIsFile){
    ext<-strsplit(x,'.',fixed = T)[[1]][2]

    switch(ext,
           RDS = out<-readRDS(x),
           XML = {
             out<-retrieveXML(x)

           },
           stop(mError('unknown file'),call. = F))

      return(out)

  }

})
