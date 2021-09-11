
#'
IMC_RsCollection<-setClass('IMC_RsCollection',
                           contains = 'list')

setMethod('initialize','IMC_RsCollection',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)

            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})

setMethod('show','IMC_RsCollection',
          function(object){
            nms<-names(object)
            out<-lapply(object,function(o){
              nc<-raster::ncol(o)
              nr<-raster::nrow(o)
            ex<-raster::extent(o)
            nlyr<-raster::nlayers(o)
                out<-data.frame(
                  uid=o@uid,
                  ncol=nc,
                  nrow=nr,
                  xmin=ex[1],
                  xmax=ex[2],
                  ymin=ex[3],
                  ymax=ex[4],
                  nlyr=nlyr,
                  stringsAsFactors = F)
            })

            out<-do.call(rbind.data.frame,out)
            rownames(out)<-NULL
              att<-strgfObjectAttr(object)
              cat(paste0('IMC RasterStacks collection: \n'))
              print(out)
              cat(att)
          })
