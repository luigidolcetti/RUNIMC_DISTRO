#' Interpretation Matrix
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("addInterpretationMatrix", function(x,classifierObject=NULL,undtLabel=NULL,undtID=NULL,...)
  standardGeneric("addInterpretationMatrix"))

setMethod('addInterpretationMatrix',signature = ('IMC_Study'),
          function(x,classifierObject=NULL,undtLabel=NULL,undtID=NULL,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'),call. = F)
            if (is.null(classifierObject)) stop(mError('specify classifier'),call. = F)
            if (is.null(x$currentAnalysis$classifier[[classifierObject]])) stop(mError('cannot find specific the specified classifier object'),call. = F)

            iMat<-guideMatrix(x$currentAnalysis$classifier[[classifierObject]])

            for (i in names(iMat)){
              iMat[[i]]$seed[iMat[[i]]$level==i]<-1
              iMat[[i]]$include[iMat[[i]]$level==i]<-1
              iMat[[i]]$action[iMat[[i]]$level!=i]<-2
            }

            if (!is.null(undtLabel) & !is.null(undtID)){
              iMat<-sapply(names(iMat),function(nms){
                iMat[[nms]]<-rbind.data.frame(iMat[[nms]],data.frame(level=undtLabel,
                                                                     label=undtID,
                                                                     seed=0,
                                                                     include=1,
                                                                     action=0,
                                                                     row.names = undtLabel,
                                                                     stringsAsFactors = F))
              },USE.NAMES = T,simplify = F)
            }


            iMat<-new('IMC_InterpretationMatrix',iMat)
            x$currentAnalysis$interpretationMatrix<-iMat

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
