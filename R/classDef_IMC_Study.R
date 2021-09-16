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
              nchl<-length(which(cht$channel!='\n'))
              nmrkr<-length(which(cht$marker!='\n'))
              nld<-length(which(cht$loaded))
              att<-strgfObjectAttr(object)
              cat('IMC study \n')
              cat(paste0('with ',nroi,'\t\t ROI \n\n'))
              cat(paste0('     ',nchl,'\t\t channels, (',nld,' loaded)\n\n'))
              cat(paste0('     ',nmrkr,'\t\t markers \n\n'))
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
                                         what = c('Study',
                                                  'Analysis',
                                                  'StudyTable',
                                                  'ChannelTable',
                                                  'FilterFrame',
                                                  'ClassificationDirectives',
                                                  'Classification',
                                                  'Classifier',
                                                  'ExtractionDirectives',
                                                  'TrainingFeatures',
                                                  'SegmentationDirectives',
                                                  'Segmentation',
                                                  'InterpretationMatrix',
                                                  'Expres'),
                                         filePathName = NULL,
                                         forceSave = F,
                                         verbose = T,
                                         ...){

  studyTotal<-c('FilterFrame',
                'ClassificationDirectives',
                'Classifier',
                'ExtractionDirectives',
                'TrainingFeatures',
                'SegmentationDirectives',
                'Segmentation',
                'InterpretationMatrix',
                'Expres',
                'Analysis',
                'StudyTable',
                'ChannelTable',
                'Study')

  analysisTotal<-c('FilterFrame',
                   'ClassificationDirectives',
                   'Classifier',
                   'ExtractionDirectives',
                   'TrainingFeatures',
                   'SegmentationDirectives',
                   'Segmentation',
                   'InterpretationMatrix',
                   'Expres',
                   'Analysis')

  whatElse<-match.arg(what,what,several.ok = T)

  if (any('Study' %in% whatElse)) {
    whatElse<-studyTotal} else {
      if (any('Analysis' %in% whatElse)){
        whatElse<-analysisTotal
      }
    }

  for (we in whatElse){
    switch(we,
           StudyTable = {
             fpn<-file.path(x$rootFolder,x$name,'archive','StudyTable.RDS')
             out<-archiveRDS(x$studyTable,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
               attr(x$studyTable,'artnTimeStmp')<-newTimeStmp
               attr(x$studyTable,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Study table archived\n'))
             } else {
               if (verbose) cat(mWarning('Study table not archived\n'))
             }
           },
           ChannelTable = {
             fpn<-file.path(x$rootFolder,x$name,'archive','ChannelTable.RDS')
             out<-archiveRDS(x$channels,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$channels,'mdtnTimeStmp')<-newTimeStmp
               attr(x$channels,'artnTimeStmp')<-newTimeStmp
               attr(x$channels,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Channel table archived\n'))
             } else {
               if (verbose) cat(mWarning('Channel table not archived\n'))
             }
           },
           FilterFrame = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','ClassificationDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$filters,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$filters,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$filters,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$filters,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Filter frame archived\n'))
             } else {
               if (verbose) cat(mWarning('Filter frame not archived\n'))
             }
           },
           ClassificationDirectives = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','ClassificationDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$classificationDirectives,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$classificationDirectives,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$classificationDirectives,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$classificationDirectives,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Classification directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Classification Directives not archived\n'))
             }
           },
           Classifier = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Classifier.RDS')
             out<-archiveRDS(x$currentAnalysis$classifier,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$classifier,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$classifier,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$classifier,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Classifier archived\n'))
             } else {
               if (verbose) cat(mWarning('Classifier not archived\n'))
             }
           },
           ExtractionDirectives = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','ExtractionDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$extractionDirectives,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$extractionDirectives,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$extractionDirectives,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$extractionDirectives,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Classification directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Classification Directives not archived\n'))
             }
           },
           TrainingFeatures = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','TrainingFeatures.RDS')
             out<-archiveRDS(x$currentAnalysis$trainingFeatures,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$trainingFeatures,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$trainingFeatures,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$trainingFeatures,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Classification directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Classification Directives not archived\n'))
             }
           },
           SegmentationDirectives = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','SegmentationDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$segmentationDirectives,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$segmentationDirectives,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$segmentationDirectives,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$segmentationDirectives,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Segmentation directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Segmentation Directives not archived\n'))
             }
           },
           Segmentation = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Segmentation.RDS')
             out<-archiveRDS(x$currentAnalysis$segmentation,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$segmentation,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$segmentation,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$segmentation,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Segmentation archived\n'))
             } else {
               if (verbose) cat(mWarning('Segmentation not archived\n'))
             }
           },
           InterpretationMatrix = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','InterpretationMatrix.RDS')
             out<-archiveRDS(x$currentAnalysis$interpretationMatrix,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$interpretationMatrix,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$interpretationMatrix,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$interpretationMatrix,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Interpretation matrix archived\n'))
             } else {
               if (verbose) cat(mWarning('Interpretation matrix not archived\n'))
             }
           },
           Expres = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Expression.RDS')
             out<-archiveRDS(x$currentAnalysis$exprs,
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis$exprs,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$exprs,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis$exprs,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Expression archived\n'))
             } else {
               if (verbose) cat(mWarning('Expression archived\n'))
             }
           },
           Analysis = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Analysis.XML')
             out<-archiveXML(x$currentAnalysis,
                             'analysis',
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis,'artnTimeStmp')<-newTimeStmp
               attr(x$currentAnalysis,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Analysis archived\n'))
             } else {
               if (verbose) cat(mWarning('Analysis archived\n'))
             }
           },
           Study = {
             fpn<-file.path(x$rootFolder,x$name,'archive','Study.XML')
             out<-archiveXML(x,
                             'study',
                             fpn,
                             forceSave=forceSave)
             if (out==1) {
               newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
               attr(x,'mdtnTimeStmp')<-newTimeStmp
               attr(x,'artnTimeStmp')<-newTimeStmp
               attr(x,'fileArchive')<-fpn
               if (verbose) cat(mMessage('Study archived\n'))
             } else {
               if (verbose) cat(mWarning('Study archived\n'))
             }
           }
    )
  }

})
