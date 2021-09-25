#' IMC_Classifier
#'
#' Container list for classification and regression models
#' @export
IMC_Classifier<-setClass('IMC_Classifier',
                         contains = 'list')

setMethod('initialize','IMC_Classifier',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            if(!is.null(val)){
              Object@.Data <- val
            }
            Object<-initObjectAttr(Object)
            return(Object)})
