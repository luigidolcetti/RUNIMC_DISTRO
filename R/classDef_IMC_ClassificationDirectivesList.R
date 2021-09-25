#' IMC_ClassificationDirectivesList
#'
#' Container list for IMC_ClassificationDirectives
#' @export
IMC_ClassificationDirectivesList<-setClass('IMC_ClassificationDirectivesList',
                                           contains = 'list')

setMethod('initialize','IMC_ClassificationDirectivesList',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})
