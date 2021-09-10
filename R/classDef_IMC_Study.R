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
