#' Study
#'
#' A study is an environment containing all the objects that the different
#' functions and methods need to work with. In brief a study must contain
#' all the sample images organized in raster stacks, a study table specifying
#' various meta-data concerning the samples, and a further Analysis environment
#' containing the elaboration of the raw data. Usually a study is created via
#' the function [initStudy]. The only reason for initializing an empty study it would
#' be to populate the environment with elements from other studies via the
#' [archive]/[retrieve] system.
#' Note that the show method for a study utilizes mainly the information
#' contained in the study table. If the study table is empty, i.e. the object
#' myStudy$studyTable is NULL, the show method will return 'empty study' even
#' if all the other objects within the study are properly set and functional.
#' @include classDef_IMC_StudyTable.R
#' @include classDef_IMC_ChannelTable.R
#' @include classDef_IMC_RasterStack.R
#' @include classDef_IMC_ChannelTable.R
#' @export
IMC_Study<-setClass('IMC_Study',
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


