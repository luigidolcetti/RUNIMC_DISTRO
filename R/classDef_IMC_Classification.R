IMC_Classification<-setClass('IMC_Classification',
                             contains = 'list')

setMethod('initialize','IMC_Classification',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})
