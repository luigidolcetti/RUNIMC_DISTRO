#'
IMC_ExpressionTable<-setClass('IMC_ExpressionTable',
                              slots = c(names='character',
                                        description='character',
                                        pIDs='character',
                                        exprs = 'list'))

setMethod('initialize','IMC_ExpressionTable',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@names <- character(0)
            Object@description<-character(0)
            Object@pIDs <- character(0)
            Object@exprs <- list()
            Object<-initObjectAttr(Object)
            return(Object)})
