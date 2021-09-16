
#'
IMC_RasterStack<-setClass('IMC_RasterStack',
                          contains = 'RasterStack',
                          slots = c(uid='character',
                                    IMC_text_file='character',
                                    study='character',
                                    sample='character',
                                    replicate='character',
                                    ROI='character',
                                    bioGroup='character',
                                    channels = 'IMC_ChannelTable',
                                    type = 'character'))

setMethod('initialize','IMC_RasterStack',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@uid<-''
            Object@IMC_text_file<-''
            Object@study<-''
            Object@sample<-''
            Object@replicate<-''
            Object@ROI<-''
            Object@bioGroup<-''
            Object@channels<-new('IMC_ChannelTable')
            Object@type<-''
            Object<-initObjectAttr(Object)
            return(Object)})

setMethod('show','IMC_RasterStack',
          function(object){

            att<-strgfObjectAttr(object)
            cat(paste0('IMC RasterStack: \n'))
            cat(paste0('uid:\t\t',object@uid,'\n'))
            cat(paste0('raw:\t\t',object@IMC_text_file,'\n'))
            cat(paste0('study:\t\t',object@study,'\n'))
            cat(paste0('sample:\t\t',object@sample,'\n'))
            cat(paste0('replicate:\t',object@replicate,'\n'))
            cat(paste0('ROI:\t\t',object@ROI,'\n'))
            cat(paste0('bio-group:\t',object@bioGroup,'\n'))
            cat(paste0('layers:\t',paste(names(object),collapse=', '),'\n'))
            cat(paste0('type:\t\t',object@type,'\n'))
            cat(att)
          })
