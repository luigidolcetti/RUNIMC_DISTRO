#' Distil expression
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
  setGeneric("distilExpression", function(x,method='weightedMean',...)
    standardGeneric("distilExpression"))

setMethod('distilExpression',signature = ('IMC_Study'),
          function(x,method='weightedMean',...){


            if (is.null(x$currentAnalysis$segmentation)) stop(mError('could not find any segmentation'))
            if (is.null(x$raster)) stop(mError('could not find any raster to process for segmentation'))

            condensedPoligonList<-extractPolygons(x$currentAnalysis$segmentation)
            condensedPoligonList<-extractMeanPixel(fn_polygons = condensedPoligonList,
                                                   fn_raster = x$raster)

            # condensedPoligonList<-as(condensedPoligonList,'IMC_ExpressionMatrix')
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            condensedPoligonList<-initObjectAttr(condensedPoligonList)

            x$currentAnalysis$exprs<-condensedPoligonList
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
