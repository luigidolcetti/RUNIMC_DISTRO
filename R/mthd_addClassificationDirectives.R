#' Classification directives
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
  setGeneric("addClassificationDirectives", function(x,method=NULL,methodParameters=NULL,...)
    standardGeneric("addClassificationDirectives"))


setMethod('addClassificationDirectives',signature = ('IMC_Study'),
          function(x,method=NULL,methodParameters=NULL,...){


            if (is.null(method)) stop(mError('provide a method'),call. = F)
            if (is.null(methodParameters)) message(mWarning(paste0('no parameters for ',method, ' provided. Default will be used') ))

            switch(method,
                   randomForest = {
                     if (is.null(methodParameters)){
                       methodParameters<-methodParametersClassification[[method]]
                       methodParameters$responseVariable = 'label'
                       methodParameters$predictiveFeatures = tf_featureList(x)
                     } else {
                       namesDefault = names (methodParametersClassification[[method]])
                       namesProvided = names (methodParameters)
                       if (!all(namesProvided %in% namesDefault)) {
                         message(mWarning('some of the parameters do not match and will be ignored'))
                         methodParameters<-methodParameters[namesProvided %in% namesDefault]
                       }
                       methodParameters<-append(methodParameters,methodParametersClassification[[method]][!(namesDefault %in% namesProvided)])
                     }
                     # methodParameters<-methodParameters[unlist(lapply(methodParameters,function(x) !is.null(x)))]
                   },
                   randomOnions = {
                     if (is.null(methodParameters)){
                       methodParameters<-methodParametersClassification[[method]]
                       methodParameters$responseVariable = 'DFC'
                       methodParameters$predictiveFeatures = tf_featureList(x)
                       methodParameters$labels = tf_labelList(x)
                     } else {
                       namesDefault = names (methodParametersClassification[[method]])
                       namesProvided = names (methodParameters)
                       if (!all(namesProvided %in% namesDefault)) {
                         message(mWarning('some of the parameters do not match and will be ignored'))
                         methodParameters<-methodParameters[namesProvided %in% namesDefault]
                       }
                       methodParameters<-append(methodParameters,methodParametersClassification[[method]][!(namesDefault %in% namesProvided)])
                     }
                     # methodParameters<-methodParameters[unlist(lapply(methodParameters,function(x) !is.null(x)))]
                   },
                   stop(mError('unkwon method'),call. = F))

            newDirectives<-new('IMC_ClassificationDirectives',
                               method=method,
                               methodParameters=methodParameters)


            if (is.null(x$currentAnalysis$classificationDirectives)){
              x$currentAnalysis$classificationDirectives<-new('IMC_ClassificationDirectivesList')
            }

            x$currentAnalysis$classificationDirectives[[method]]<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$classificationDirectives,'mdtnTimeStmp')<-newTimeStmp
          })
