IMC_InterpretationMatrix<-setClass('IMC_InterpretationMatrix',
                                   contains = 'list')

setMethod('initialize','IMC_InterpretationMatrix',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})
