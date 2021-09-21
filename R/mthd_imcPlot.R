#' Plot
#'
#'
#'
#' @param x IMC_study, a study
#' @examples
#' \dontrun{
#'
#' }
#' @export
setGeneric("imcPlot", function(x,uid,channels,
                               xlim=NULL,
                               ylim=NULL,
                               quantCap = 1,
                               frame = T,
                               tick = T,
                               res = c(1,1),
                               majorT = 10,
                               cex.mjt = 20,
                               minorT = 1,
                               cex.mnt = 40,
                               legend=T,
                               legend.position = c('b','t'),
                               cex.legend = 0.8,
                               overlay =NULL,
                               col = NULL,
                               border = 'cyan',
                               ...)
           standardGeneric("imcPlot"))


setMethod('imcPlot',signature = ('IMC_Study'),
          function(x,uid,channels,
                   xlim=NULL,
                   ylim=NULL,
                   quantCap = 1,
                   frame = T,
                   tick = T,
                   res = c(1,1),
                   majorT = 10,
                   cex.mjt = 20,
                   minorT = 1,
                   cex.mnt = 40,
                   legend=T,
                   legend.position = c('b','t'),
                   cex.legend = 0.8,
                   overlay =NULL,
                   col = NULL,
                   border = 'cyan',
                   ...){

            if (missing(channels)) stop(mError('specify channels'),call. = F)
            if (missing(uid)) stop(mError('specify uid'))
            if (length(channels)>3) {
              warning(mWarning('it is possible to show oly three channels, list will be shortened...'))
              channels<-channels[1:3]
            }

            chTb<-channelTable(x)
            lylst<-layerNames(x)

            org<-lapply(channels,function(ch){

              if (any(ch %in% chTb$marker)){
                return(list(org='raw',ch=chTb$RcolumnNames[chTb$marker==ch]))
              }
              if (any(ch %in% lylst$raw)){
                return(list(org='raw',ch=ch))
              }
              if (any(ch %in% lylst$calc)){
                return(list(org='calc',ch=ch))
              }
              if (any(ch %in% lylst$raw)){
                return(list(org='class',ch=ch))
              }

              stop(mError('unknown marker, channel or layer'),call. = F)
            })

            rst<-lapply(org,function(or){

              switch(or$org,

                     raw = return(x$raster[[uid]][[or$ch]]),
                     calc = return(x$currentAnalysis$derivedRasters[[uid]][[or$ch]]),
                     class = return(x$currentAnalysis$classification[[uid]][[or$ch]]))
            })

            rst<-lapply(rst,function(rs){
              ext<-raster::extent(rs)
              res<-raster::res(rs)
              ncl<-raster::ncol(rs)
              nrw<-raster::nrow(rs)

              if (is.null(xlim)) xlim<-c(ext[1:2])
              if (is.null(ylim)) ylim<-c(ext[3:4])

              r0<-ceiling((ext[4]-ylim[2])/res[2])
              if (r0<1) r0<-1
              c0<-ceiling((xlim[1]-ext[1])/res[1])
              if (c0<1) c0<-1
              rm<-ceiling((ext[4]-ylim[1])/res[2])
              if (rm>nrw) rm<-nrw
              cm<-ceiling((xlim[2]-ext[1])/res[1])
              if (cm>ncl) cm<-ncl

              out<-raster::getValuesBlock(rs,r0,rm-r0,c0,cm-c0,'matrix')
              return(list(rst=out,nrow=rm-r0,ncol=cm-c0,origin=c(xlim[1],ylim[1])))
            })

            rrs<-lapply(rst,'[[','rst')
            nrs<-Reduce('=',lapply(rst,'[[','nrow'))
            ncs<-Reduce('=',lapply(rst,'[[','ncol'))
            origin<-Reduce('=',lapply(rst,'[[','origin'))
            rst<-array(unlist(rrs,recursive = F),dim = c(nrs,ncs,length(rrs)))

            if (legend) {

              legend.position<-match.arg(legend.position,c('b','t'),F)
              switch (legend.position,
                      t = layout(matrix(c(2,1),ncol=1),widths = 1,heights = c(1,3)),
                      b = layout(matrix(c(1,2),ncol=1),widths = 1,heights = c(3,1))
              )

            }

            if (!is.null(overlay)){
              overlay<-x$currentAnalysis$exprs@exprs[[overlay]]
            }

            pl<-rstPlot(x = rst,
                        quantCap = quantCap,
                        frame = frame,
                        tick = tick,
                        res = res,
                        majorT = majorT,
                        minorT = minorT,
                        origin = origin,
                        cex.mjt = cex.mjt,
                        cex.mnt = cex.mnt,
                        asp = 1,
                        uid = uid,
                        overlay = overlay,
                        col = col,
                        border = border
            )


            if (legend){


              textOut<-studyTable(x)
              rownames(textOut)<-NULL
              textOutSub<-textOut[textOut$uid==uid,]
              textOutSub<-unlist(lapply(seq_along(colnames(textOut)),
                                        function(i){
                                          paste0(colnames(textOut)[i],': ',textOutSub[i])
                                        }))
              textOut<-paste0(textOutSub[1],'\n',
                              textOutSub[5],'/ ',
                              textOutSub[6],'/ ',
                              textOutSub[7],'\n',
                              textOutSub[8],',')
              rstLegend_continuous(Rplot= pl,
                                   scaleCol = T,
                                   scaleBar = T,
                                   chNames=channels,
                                   cex = cex.legend,
                                   textC=textOut)
            }

          }
)
