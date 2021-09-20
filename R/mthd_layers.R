#' Layers
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("layerNames", function(x,channels,...)
  standardGeneric("layerNames"))


setMethod('layerNames',signature = ('IMC_Study'),
          function(x,channels,...){

            if (!is.null(x$raster)) {
              rl<-lapply(x$raster,names)
              rl<-Reduce('=',rl)
            } else rl<-NULL

            if (!is.null(x$currentAnalysis$derivedRasters)) {
              dr<-lapply(x$currentAnalysis$derivedRasters, names)
              dr<-Reduce('=',dr)
            } else dr<-NULL

            if (!is.null(x$currentAnalysis$classification)){
              cl<-lapply(x$currentAnalysis$classification,names)
              cl<-Reduce('=',cl)
            } else cl<-NULL

            return(list(raw=rl,calc=dr,class=cl))
          })
