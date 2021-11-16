#' Retrieve
#'
#' Retrieve load from disk a study, an analysis or part of it
#'
#' @param x character, file path to a file or directory
#' @return the object written to the specified file or the raster stacks
#'   contained in the specified folder
#' \dontrun{
#' myStudy <- retrieve(x = ./path/to/myStudy)
#' }
#' @details archive has specific methods for a study and for its elements
#' @export
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
