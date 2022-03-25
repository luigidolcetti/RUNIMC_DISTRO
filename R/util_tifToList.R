#'tifToList
#'
#'Utility function to transform single tif images in a stack text file readable
#'with [loadTxtToStack] and [initStudy]. All tif images must have same extent.
#'@param fn_sourceFile character vector containing the full path to each tif file
#'  as it can be obtained for eamle with [list.files] with full.names=T
#'@param fn_origin numeric vector of length two indicating the coordinate of the
#'  origin of the image, by default the origin correspond to the upper left corner
#'  of the image, therefore the image will be flipped vertically.
#'@param fn_direction numeric vector of length two, of which only the sign will
#'  used to determine if the picture extends left (-1) or right (+1),
#'  and up (+1) or down (-1).
#'@param fn_resolution numeric vector of length two setting x and y resolution.
#'@param fn_Zindex numeric scalar, starting number for cell indexing (Z).
#'@param fn_colNames character vector of the same lent as fn_sourceFile, giving
#'  the name of each column. Column names should be compatible with the way they
#'  are suppose to be interpred by [loadTxtToStack] and [initStudy] (eg: CD4-c01,
#'  CD8-c02, DAPI-c03).
#'@param fn_targetFile character scalar, full path to the output file.
#'@param fn_nThread numeric scalar, number of thread to use in [data.table::fread],
#'   if data.table version is <1.14.3 fn_nThread is coerced to 1
#'@param fn_verbose logic scalar
#'@export
tifToList <- function(fn_sourceFile = NULL,
                      fn_origin = c(0,0),
                      fn_direction = c(1,1),
                      fn_resolution = c(1,1),
                      fn_Zindex = 0,
                      fn_columnNames = NULL,
                      fn_targetFile = NULL,
                      fn_nThread = 1,
                      fn_verbose = T){

  if (is.null(fn_sourceFile)) stop(mError("Provide a list of source tiff files"),call. = F)
  if (!all(file.exists(fn_sourceFile))) stop(mError("Unable to find one or more of the source files"),call. = F)
  if (length(fn_origin)!=2 | !is.numeric(fn_origin)) stop(mError("origin must be a numeric vector of length 2"))
  if (length(fn_resolution)!=2 | !is.numeric(fn_resolution)) stop(mError("resolution must be a numeric vector of length 2"))
  if (length(fn_direction)!=2 | !is.numeric(fn_direction)) stop(mError("origin must be a numeric vector of length 2"))
  if (is.null(fn_targetFile)) stop(mError("provide a valid file path to write to"))
  if (is.null(fn_columnNames)) {
    message(mWarning('Column names are not specified and will be automatically generated'))
    fn_columnNames <- paste0('c',formatC(1:length(fn_sourceFile),
                                         width = ceiling(log10(length(fn_sourceFile)))+1,
                                         flag = '0'))
  }
  if (utils::packageVersion('data.table')<"1.14.3") fn_nThread <- 1

  listTiff<-lapply(fn_sourceFile,tiff::readTIFF,native=F,info=T,indexed=T)

  ZTiff<-unlist(unique(lapply(listTiff,length)))
  XTiff<-unlist(unique(lapply(listTiff,ncol)))
  YTiff<-unlist(unique(lapply(listTiff,nrow)))

  if (length(XTiff)!=1) stop(mError("Tiffs have different X extent"))
  if (length(YTiff)!=1) stop(mError("Tiffs have different Y extent"))
  if (length(ZTiff)!=1) stop(mError("Tiffs have different length"))

  XTiff_f<-rep(seq(from = fn_origin[1],
             length.out = XTiff ,
             by = fn_resolution[1]*sign(fn_direction[1])),
             each = YTiff)

  YTiff_f<-rep(seq(from = fn_origin[2],
             length.out = YTiff ,
             by = fn_resolution[2]*sign(fn_direction[2])),
             XTiff)

  ZTiff_f<-(0:(ZTiff-1))+fn_Zindex

  out<-lapply(setNames(1:length(fn_sourceFile),fn_columnNames),function(x){
    as.vector(listTiff[[x]])
  })

  out<-do.call(cbind,out)

  out<-cbind(X=XTiff_f,Y=YTiff_f,Z=ZTiff_f,out)

  data.table::fwrite(x = out,
                     file = fn_targetFile,
                     append = F,
                     quote = F,
                     sep = '\t',
                     row.names = F,
                     nThread = fn_nThread,
                     yaml = F,
                     verbose = F)

  if (fn_verbose) message(mMessage(paste0("file written to ",fn_targetFile)))

}
