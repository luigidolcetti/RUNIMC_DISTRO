#'
#' @export
setGeneric("IMC_stack", function(x, ...)
  standardGeneric("IMC_stack"))


setMethod('IMC_stack',signature(x='list'),
          function(x,
                   uid,
                   IMC_text_file,
                   study,
                   sample,
                   replicate,
                   ROI,
                   bioGroup,
                   channels,
                   type,
                   ...){

            rstrStk<-raster::stack(x)
            rstrStk<-methods::as(rstrStk,'IMC_RasterStack')
            if (!missing(uid)) {rstrStk@uid<-uid} else {stop('uid missing')}
            if (!missing(IMC_text_file)) {rstrStk@IMC_text_file<-IMC_text_file} else {stop('IMC original file missing')}
            if (!missing(study)) {
              if (!is.null(study)) {rstrStk@study<-study} else {rstrStk@study<-'NO_study'}}
            if (!missing(sample)) {
              if (!is.null(sample)) {rstrStk@sample<-sample} else {rstrStk@sample<-'NO_sample'}}
            if (!missing(ROI)) {
              if (!is.null(ROI)) {rstrStk@ROI<-ROI} else {rstrStk@ROI<-'NO_ROI'}}
            if (!missing(replicate)) {
              if (!is.null(replicate)) {rstrStk@replicate<-replicate} else {rstrStk@replicate<-'NO_replicate'}}
            if (!missing(bioGroup)) {
              if (!is.null(bioGroup)) {rstrStk@bioGroup<-bioGroup} else {rstrStk@bioGroup='NO_bioGroup'}}
            if (!missing(channels)) {rstrStk@channels<-channels}
            if (!missing(type)) {rstrStk@type<-type} else {rstrStk@type<-'ANY'}
            return(rstrStk)
          })

setMethod('IMC_stack',signature(x='IMC_RasterStack'),
          function(x,...){
            rasterStackList<-list(x,...)
            uid<-Reduce('=',lapply(rasterStackList,slot,'uid'))
            IMC_text_file<-Reduce('=',lapply(rasterStackList,slot,'IMC_text_file'))
            study<-Reduce('=',lapply(rasterStackList,slot,'study'))
            sample<-Reduce('=',lapply(rasterStackList,slot,'sample'))
            replicate<-Reduce('=',lapply(rasterStackList,slot,'replicate'))
            ROI<-Reduce('=',lapply(rasterStackList,slot,'ROI'))
            bioGroup<-Reduce('=',lapply(rasterStackList,slot,'bioGroup'))
            channels<-Reduce('=',lapply(rasterStackList,slot,'channels'))
            type<-Reduce('=',lapply(rasterStackList,slot,'type'))
            newStack<-raster::stack(rasterStackList)
            newStack<-as(newStack,'IMC_RasterStack')
            newStack@uid<-uid
            newStack@IMC_text_file<-IMC_text_file
            newStack@study<-study
            newStack@sample<-sample
            newStack@replicate<-replicate
            newStack@ROI<-ROI
            newStack@bioGroup<-bioGroup
            newStack@channels<-channels
            newStack@type<-type
            return(newStack)
          })

