#'Some nice thing
#'
#'
#' @export
plotsPoly<-function(fn_rst,
                    fn_rst_red=NULL,
                    fn_rst_green=NULL,
                    fn_rst_blue=NULL,
                    fn_xmin,
                    fn_xmax,
                    fn_ymin,
                    fn_ymax,
                    fn_plcn,
                    fn_plc,
                    fn_plcRange,
                    fn_xaxs,
                    fn_yaxs,
                    fn_xaxt,
                    fn_yaxt,
                    fn_colNA,
                    fn_Bx,fn_By,
                    fn_bgc,
                    fn_geom,
                    fn_geomB,
                    fn_lwd,
                    fn_title){

  if(is.null(fn_rst_red) & is.null(fn_rst_green) & is.null(fn_rst_blue)){

    raster::plot(fn_rst,
                 xlim=c(fn_xmin,
                        fn_xmax),
                 ylim=c(fn_ymin,
                        fn_ymax),
                 col=fn_plc(fn_plcn),
                 breaks=fn_plcRange,
                 asp=1,
                 xaxs=fn_xaxs,
                 yaxs=fn_yaxs,
                 xaxt=fn_xaxt,
                 yaxt=fn_yaxt,
                 bty = 'n',
                 legend=F,
                 colNA=fn_colNA)} else
                 {

                   fn_rst<-raster::crop(fn_rst,raster::extent(fn_xmin,fn_xmax,fn_ymin,fn_ymax))
                   ncl<-raster::ncol(fn_rst)
                   nrw<-raster::nrow(fn_rst)
                   newArray<-array(rep(matrix(0,
                                              nrow=nrw,
                                              ncol=ncl),
                                       3), dim=c(nrw,ncl,3))

                   # if (!is.null(fn_rst_red)) {
                   #   browser()
                   #   TEMP_array<-raster::as.array(fn_rst[[fn_rst_red]])
                   #   TEMP_dim<-dim(TEMP_array)
                   #   if (TEMP_dim[3]>1) TEMP_array<-pmax(TEMP_array)[,,1] else TEMP_array<-TEMP_array[,,1]
                   #   newArray[,,1]<-TEMP_array
                   # }

                   if (!is.null(fn_rst_red)) newArray[,,1]<-apply(raster::as.array(fn_rst[[fn_rst_red]]),c(1,2),max)
                   if (!is.null(fn_rst_green)) newArray[,,2]<-apply(raster::as.array(fn_rst[[fn_rst_green]]),c(1,2),max)
                   if (!is.null(fn_rst_blue)) newArray[,,3]<-apply(raster::as.array(fn_rst[[fn_rst_blue]]),c(1,2),max)


                   plot(NA,
                        xlim=c(fn_xmin,
                               fn_xmax),
                        ylim=c(fn_ymin,
                               fn_ymax),
                        bty = 'n',
                        asp=1)

                   srtExt<-raster::extent(fn_rst)

                   rasterImage(newArray,
                               xleft = srtExt[1],
                               ybottom = srtExt[3],
                               xright = srtExt[2],
                               ytop = srtExt[4],
                               interpolate = F)

                 }

  points(x=fn_Bx,y=fn_By,
         pch=21,
         bg=fn_bgc)
  plot(fn_geom,
       border=fn_geomB,
       lwd=fn_lwd,
       bty ='n',
       add=T)
  title(fn_title,adj=0,line=0.3)
}
