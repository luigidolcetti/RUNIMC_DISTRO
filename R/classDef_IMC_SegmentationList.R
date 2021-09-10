IMC_SegmentationList<-setClass('IMC_SegmentationList',
                               contains = 'list')

setMethod('initialize','IMC_SegmentationList',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})
