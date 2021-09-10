IMC_ClassificationDirectives<-setClass('IMC_ClassificationDirectives',
                                       slots = c(method='character',
                                                 methodParameters='list'))

setMethod('initialize','IMC_ClassificationDirectives',
          function(.Object, method=c(),methodParameters=list(),...) {
            Object <- callNextMethod(.Object, ...)
            Object@method<-method
            Object@methodParameters<-methodParameters
            Object<-initObjectAttr(Object)
            return(Object)})
