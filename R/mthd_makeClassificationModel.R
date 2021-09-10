#' Make a classification model
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("makeClassificationModel", function(x,method=NULL,seed=1234, ...)
  standardGeneric("makeClassificationModel"))

setMethod('makeClassificationModel',signature = ('IMC_Study'),
          function(x,method=NULL,seed=1234,...){

            if (is.null(x$currentAnalysis$classificationDirectives)) stop(mError('Before making a classification model directives must be specified'))
            if (length(x$currentAnalysis$classificationDirectives)>1 &
                is.null(method)) stop(mError('specify what method to apply'))
            if (length(x$currentAnalysis$classificationDirectives)==1 &
                is.null(method)) method<-
                x$currentAnalysis$classificationDirectives[[1]]@method

            switch(method,

                   randomForest = {
                     set.seed=seed
                     rVar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$responseVariable
                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     cPrm<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters[!(names(x$currentAnalysis$classificationDirectives[[method]]@methodParameters) %in%c('responseVariable','predictiveFeatures'))]
                     cPrm<-cPrm[unlist(lapply(cPrm,function(x) !is.null(x)))]
                     fFormula<-eval(parse(text=paste0(rVar,'~',paste(pFtr,collapse = '+'))))
                     rFcall<-c(list(formula = fFormula,
                                    data = x$currentAnalysis$trainingFeatures$value),
                               cPrm)
                     rf_classifier <- do.call(randomForest::randomForest,rFcall)

                   },
                   randomOnions = {

                     Lvar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels
                     rVar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$responseVariable
                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     cPrm<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters[!(names(x$currentAnalysis$classificationDirectives[[method]]@methodParameters) %in%c('responseVariable','predictiveFeatures','labels','classificationLyr','prefix'))]
                     cPrm<-cPrm[unlist(lapply(cPrm,function(x) !is.null(x)))]
                     fFormula<-eval(parse(text=paste0(rVar,'~',paste(pFtr,collapse = '+'))))
                     rf_classifier<-sapply(Lvar,function(lbl){
                       cat('Random Forest...:::',lbl,':::\n')
                       rFcall<-c(list(formula = fFormula,
                                      data = x$currentAnalysis$trainingFeatures$value[x$currentAnalysis$trainingFeatures$value$parLabel==lbl,]),
                                 cPrm)
                       rf<-do.call(randomForest::randomForest,rFcall)
                       return(rf)
                     },USE.NAMES = T,simplify = F)
                   },
                   stop(mError('unknown method')))

            if (is.null(x$currentAnalysis$classifier)){
              x$currentAnalysis$classifier<-new('IMC_Classifier',
                                                val = list(rf_classifier))
              names(x$currentAnalysis$classifier)<-method
            } else {
              x$currentAnalysis$classifier[[method]]<-rf_classifier
            }

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$classifier,'mdtnTimeStmp')<-newTimeStmp
          })
