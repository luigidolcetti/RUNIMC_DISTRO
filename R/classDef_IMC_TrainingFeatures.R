IMC_TrainingFeatures<-setClass('IMC_TrainingFeatures',
                               contains = 'list')

setMethod('initialize','IMC_TrainingFeatures',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})
