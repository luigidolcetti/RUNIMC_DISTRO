IMC_SegmentationDirectives<-setClass('IMC_SegmentationDirectives',
                                     slots = c(method='character',
                                                    methodParameters='list'))

setMethod('initialize','IMC_SegmentationDirectives',
          function(.Object, method=c(),methodParameters=list(),...) {
            Object <- callNextMethod(.Object, ...)
            Object@method<-method
            Object@methodParameters<-methodParameters
            Object<-initObjectAttr(Object)
            return(Object)})
