IMC_AnalysisList<-setClass('IMC_AnalysisList',
                           contains = 'list')

setMethod('initialize','IMC_AnalysisList',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)}
)
