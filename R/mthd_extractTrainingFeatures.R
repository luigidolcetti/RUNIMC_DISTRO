#' Extraction of training features
#'
#' Training feature (pixels) are extracted from training data, following
#'   following the directives provided via the function [addExtractionDirectives()].
#'
#' @param x IMC_Study, a study
#' @param ... not implemented
#' @examples
#' \dontrun{
#' addExtractionDirectives(MyStudy,c(0.75,100),'core', append = F)
#' extractTrainingFeatures(MyStudy)
#' }
#' @export
setGeneric("extractTrainingFeatures", function(x,...)
  standardGeneric("extractTrainingFeatures"))

setMethod('extractTrainingFeatures',signature = ('IMC_Study'),
          function(x,...){


            if (is.null(x$currentAnalysis$extractionDirectives)) stop(mError('Before extracting features extraction directives must be specified'))

            newCoverage<-c(unlist(x$currentAnalysis$extractionDirectives$coverage))
            newCoverageLabel<-c(unlist(x$currentAnalysis$extractionDirectives$prefix))

            trainingPolygonPath<-paste(x$currentAnalysis$folder,'training/Polygons',sep='/')

            trainingPolygon<-list.files(trainingPolygonPath,full.names = T)

            if (length(trainingPolygon)==0) stop(mError(paste0('Could not find any polygon file in ',trainingPolygonPath)))

            trainingFeatures<-lapply(trainingPolygon,function(tpl){
              polygonFiles<-sf::st_read(tpl)
              TEMP_UID<-as.character(Reduce('=',polygonFiles$uid))

              rst<-list(raster::stack(x$raster[[TEMP_UID]],x$currentAnalysis$derivedRasters[[TEMP_UID]]))
              names(rst)<-TEMP_UID
              trainingMatrix<-extractFeatures(rst,
                                              polygonFiles,
                                              fn_coverage = newCoverage,
                                              fn_coverage_label = newCoverageLabel)

            })



            TEMP_value<-lapply(trainingFeatures,function(x)x$value)
            TEMP_value<-do.call(rbind.data.frame,c(TEMP_value,stringsAsFactors=F))
            TEMP_value$label<-as.factor(TEMP_value$label)
            TEMP_value$parLabel<-as.factor(TEMP_value$parLabel)
            TEMP_geometry<-lapply(trainingFeatures,function(x)x$geometry)
            TEMP_geometry<-do.call(rbind.data.frame,c(TEMP_geometry,stringsAsFactors=F))

            trainingMatrix<-list(value=TEMP_value,geometry=TEMP_geometry)
            trainingMatrix<-new('IMC_TrainingFeatures',trainingMatrix)
            trainingMatrix<-initObjectAttr(trainingMatrix)
            x$currentAnalysis$trainingFeatures<-trainingMatrix

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
