#' Segmentation directives
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
  setGeneric("addSegmentationDirectives", function(x,method=NULL,methodParameters=NULL,...)
    standardGeneric("addSegmentationDirectives"))

setMethod('addSegmentationDirectives',signature = ('IMC_Study'),
          function(x,method=NULL,methodParameters=NULL,...){


            if (is.null(method)) stop(mError('provide a method'),call. = F)
            if (!any(method %in% names(methodParametersSegmentation))) stop(mError('unknown method'),call. = F)
            if (is.null(methodParameters)) {
              message(mWarning(paste0('no parameters provided for ',method,', default parameters will be added. You can change them manually in *@methodParameters') ))
              methodParameters<-methodParametersSegmentation[[method]]} else {
                namesDefault = names (methodParametersSegmentation[[method]])
                namesProvided = names (methodParameters)
                methodParameters<-append(methodParameters,methodParametersSegmentation[[method]][!(namesDefault %in% namesProvided)])
              }


            newDirectives<-new('IMC_SegmentationDirectives',
                               method=method,
                               methodParameters=methodParameters)

            x$currentAnalysis$segmentationDirectives<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
