
#'
IMC_RsCollection<-setClass('IMC_RsCollection',
                           contains = 'list')

setMethod('initialize','IMC_RsCollection',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)

            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})
