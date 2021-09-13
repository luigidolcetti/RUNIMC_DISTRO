#'@include classDef_IMC_StudyTable.R
#'@include classDef_IMC_ChannelTable.R
#'@include classDef_IMC_RasterStack.R
#'@include classDef_IMC_ChannelTable.R

IMC_StudyTable<-setClass('IMC_Study',
                         contains = 'environment')

setMethod('initialize','IMC_Study',
          function(.Object,
                   name=NULL,
                   rootFolder=NULL,
                   rawDataFolder=NULL,
                   studyTable = NULL,
                   raster=NULL,
                   whichColumns=NULL,
                   channels=NULL,
                   analysis=NULL,
                   currentAnalysis=NULL,
                   ...) {
            Object <- callNextMethod(.Object, ...)
            Object$name<-name
            Object$rootFolder<-rootFolder
            Object$rawDataFolder<-rawDataFolder
            Object$studyTable<-studyTable
            Object$raster<-raster
            Object$whichColumns<-whichColumns
            Object$channels<-channels
            Object$analysis<-analysis
            Object$currentAnalysis<-currentAnalysis
            Object<-initObjectAttr(Object)

            return(Object)})

setMethod('show','IMC_Study',
          function(object){
            stt<-object$studyTable
            if (is.null(stt)){
              cat('Empty IMC study')
            } else {
              nroi<-nrow(stt)
              cht<-object$channels
              nchl<-length(which(cht$channel!=''))
              nmrkr<-length(which(cht$marker!=''))
              nld<-length(which(cht$loaded))
              att<-strgfObjectAttr(object)
              cat('IMC study \n')
              cat(paste0('with ',nroi,'\t\t ROI \n'))
              cat(paste0('     ',nchl,'\t\t channels, (',nld,' loaded)\n'))
              cat(paste0('     ',nmrkr,'\t\t markers \n'))
              cat(att)
            }
          })


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
setGeneric("archive", function(x,what=NULL,filePathName=NULL,forceSave=F,...)
  standardGeneric("archive"))

setMethod("archive","IMC_Study",function(x,
                                         what=c('Study',
                                                'Analysis',
                                                'StudyTable',
                                                'ChannelTable',
                                                'ClassificationDirectives',
                                                'Classification',
                                                'Classifier',
                                                'ExtractionDirectives',
                                                'TrainingFeatures',
                                                'SegmentationDirectives',
                                                'Segmentation',
                                                'InterpretationMatrix',
                                                'Expres'),filePathName=NULL,forceSave=F,...){
  studyTotal<-c('Study',
                'Analysis',
                'StudyTable')

  analysisTotal<-c('Analysis',
                   'ClassificationDirectives',
                   'Classification',
                   'Classifier',
                   'ExtractionDirectives',
                   'TrainingFeatures',
                   'SegmentationDirectives',
                   'Segmentation',
                   'InterpretationMatrix',
                   'Expres')

  whatElse<-match.arg(what,what,several.ok = T)

  if (any('Study' %in% whatElse)) {
    whatElse<-c(studyTotal,analysisTotal)} else {
      if (any('Analysis' %in% whatElse)){
        whatElse<-analysisTotal
      }
    }

  for (we in whatElse){
    switch(we,
           StudyTable = {
             fpn<-file.path(x$rootFolder,'StudyTable.sttb')
             out<-archiveRDS(x$studyTable,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
               attr(x$studyTable,'artnTimeStmp')<-newTimeStmp
               attr(x$studyTable,'fileArchive')<-fpn
             } else {cat('Study Table not saved')}
           },
           ChannelTable = {
             fpn<-file.path(x$rootFolder,'ChannelTable.chtb')
             out<-archiveRDS(x$channels,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$channels,'mdtnTimeStmp')<-newTimeStmp
               attr(x$channels,'artnTimeStmp')<-newTimeStmp
               attr(x$channels,'fileArchive')<-fpn
             } else {cat('Study Table not saved')}
           }
    )
  }

})
