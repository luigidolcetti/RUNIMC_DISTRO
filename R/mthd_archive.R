#' Archive
#'
#' Archive is used to save to disk an entire study, an Analysis or a part of it
#'
#' @param x a study (environment) or one of its components
#' @param what which component of a study to archive (not implemented at the moment)
#' @param forceSave each object in a study is time stamped and saving is inhibited if this object has not been modified. forceSave set to TRUE revert this behaviour
#' @return the same object after it has been written to disk or a character string representing the file path
#' @seealso
#' @examples
#' \dontrun{
#' archive(x = MyStudy)
#' }
#' @details archive has specific methods for a study and for its elements
#' @export
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
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$studyTable,ii)<-out[[ii]]
               }
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
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$channels,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Channel table archived\n'))
             } else {
               if (verbose) cat(mWarning('Channel table not archived\n'))
             }
           },
           FilterFrame = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','filters.RDS')
             out<-archiveRDS(x$currentAnalysis$filters,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$filters,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Filter table archived\n'))
             } else {
               if (verbose) cat(mWarning('Filter table not archived\n'))
             }
           },
           ClassificationDirectives = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','ClassificationDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$classificationDirectives,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$classificationDirectives,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Classification directives archived\n'))
             } else {
               if (verbose) cat(mWarning('classification directives not archived\n'))
             }
           },
           Classifier = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Classifier.RDS')
             out<-archiveRDS(x$currentAnalysis$classifier,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$classifier,ii)<-out[[ii]]
               }
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
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$extractionDirectives,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Extraction directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Extraction directives not archived\n'))
             }
           },
           TrainingFeatures = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','TrainingFeatures.RDS')
             out<-archiveRDS(x$currentAnalysis$trainingFeatures,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$trainingFeatures,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Training features table archived\n'))
             } else {
               if (verbose) cat(mWarning('Training features table not archived\n'))
             }
           },
           SegmentationDirectives = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','SegmentationDirectives.RDS')
             out<-archiveRDS(x$currentAnalysis$segmentationDirectives,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$segmentationDirectives,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Segmentation directives archived\n'))
             } else {
               if (verbose) cat(mWarning('Segmentation directives not archived\n'))
             }
           },
           Segmentation = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Segmentation.RDS')
             out<-archiveRDS(x$currentAnalysis$segmentation,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$segmentation,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Segmentation list archived\n'))
             } else {
               if (verbose) cat(mWarning('Segmentation list not archived\n'))
             }
           },
           InterpretationMatrix = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','InterpretationMatrix.RDS')
             out<-archiveRDS(x$currentAnalysis$interpretationMatrix,
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$interpretationMatrix,ii)<-out[[ii]]
               }
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
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis$exprs,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Expression table archived\n'))
             } else {
               if (verbose) cat(mWarning('Expression table not archived\n'))
             }
           },
           Analysis = {
             fpn<-file.path(x$currentAnalysis$folder,'archive','Analysis.XML')
             out<-archiveXML(x$currentAnalysis,
                             'analysis',
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x$currentAnalysis,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Current analisys archived\n'))
             } else {
               if (verbose) cat(mWarning('Current analysis not archived\n'))
             }
           },
           Study = {
             fpn<-file.path(x$rootFolder,x$name,'archive','Study.XML')
             out<-archiveXML(x,
                             'study',
                             fpn,
                             forceSave=forceSave)
             if (out$r==1) {
               for (ii in names(out)[-1]){
                 attr(x,ii)<-out[[ii]]
               }
               if (verbose) cat(mMessage('Study archived\n'))
             } else {
               if (verbose) cat(mWarning('Study archived\n'))
             }
           }
    )
  }

})
