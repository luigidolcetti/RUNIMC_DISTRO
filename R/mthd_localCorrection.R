#' Clean up Classification
#'
#' Method used to 'clean' the classification layer. Essentially a wrapper around
#'   [raster::focal()] that use a function that substitute the classification of
#'   a pixel with the most abundant one in the square matrix defined by the parameter
#'   matrixExtent.
#'
#' @param x IMC_Study, the study whom classification is to modify
#' @param labelLayer character, the name of the layer to modify. Must be a discrete
#'   value layer.
#' @param matrixExtent numeric, extent of the square matrix to use in the focal
#'   function, must be an odd number.
#' @param suffix character, suffix to add to labelLayer to form the new classification
#'   layer name.
#' @param paddingLabel character, a class to be used for padding. Must be a class
#'   already present in the classification to modify.
#' @return an IMC_classification object that will overwrite the current analysis
#'   classification
#' @export
setGeneric("localCorrection", function(x,
                                       labelLayer='label',
                                       suffix = '_clean',
                                       matrixExtent = 3,
                                       paddingLabel = 'undetermined',
                                       cl=NA,
                                       ...)
  standardGeneric("localCorrection"))



setMethod('localCorrection',signature = ('IMC_Study'),
          function(x,
                   labelLayer='label',
                   suffix = '_clean',
                   matrixExtent = 3,
                   paddingLabel = 'undetermined',
                   cl=NA,
                   ...){

            chkLayer<-lapply(x$currentAnalysis$classification,names)

            chkLayer<-sapply(chkLayer,function(x){any(labelLayer %in% x)},simplify = T,USE.NAMES = F)

            if (!all(chkLayer)) stop(mError('cannot find specified labelLayer'),call. = F)

            chkRAT<-lapply(x$currentAnalysis$classification,function(x){
              raster::levels(x[[labelLayer]])[1]
            })

            chkRATnull<-sapply(chkRAT,is.null,simplify = T,USE.NAMES = F)

            if (any(chkRATnull)) stop(mError('not all classification have classes'),call. = F)

            chkRATpadding<-sapply(chkRAT,function(x){
              paddingLabel %in% x[[1]][,2]
            },simplify = T,USE.NAMES = F)

            if (!all(chkRATpadding)) stop(mError('some class table do not contain paddingLabel'),call.=F)

            pdv<-unique(sapply(chkRAT,function(x){
              x[[1]][x[[1]][,2]==paddingLabel,1]
            },simplify = T,USE.NAMES = F))

            if (length(pdv)>1) stop(mError('problems with padding value matching more than one ID'),call. = F)

            if ((matrixExtent %% 2)==0) stop(mError('matrixExtent must be a odd number'))


            uids<-st_uids(x)

            oldStk<-as.list(x$currentAnalysis$classification)

            if (!is.na(cl)){

              cl<-parallel::makeCluster(cl)
              parallel::clusterExport(cl,
                                      varlist = c(
                                        'oldStk',
                                        'labelLayer',
                                        'matrixExtent',
                                        'pdv'),
                                      envir = environment())

              newStk<-parallel::parLapply(setNames(uids,uids),function(i){

                rst<-oldStk[[i]][[labelLayer]]
                filePath<-raster::filename(rst)
                fileN<-fs::path_file(filePath)
                fileD<-fs::path_dir(filePath)
                fileE<-sub('.*\\.', '',fileN)
                fileNN<-sub('\\.[^.]*$','',fileN)
                TEMPfile<-file.path(x$currentAnalysis$folder,'Temp',paste0('TEMP_',fileNN,suffix,'.nc'))
                newRst<-raster::focal(x = rst,
                                      w = matrix(1, ncol=matrixExtent,nrow = matrixExtent),
                                      fun = function(x){
                                        tableX<-table(x)
                                        wM<-which.max(tableX)[1]
                                        nV<-as.numeric(names(tableX)[wM])
                                        return(nV)
                                      },
                                      pad=T,
                                      padValue=pdv,
                                      overwrite = T,
                                      progress='text',
                                      filename=TEMPfile)
                newRst<-raster::ratify(newRst)
                levels(newRst)<-raster::levels(rst)
                newName<-paste0(labelLayer,suffix)
                names(newRst)<-newName
                newRst<-raster::writeRaster(newRst,
                                            filename = file.path(fileD,
                                                                 paste0(fileNN,suffix,'.',fileE)),
                                            overwrite=T,
                                            format='raster')
                unlink(TEMPfile)
                oldStk[[i]][[newName]]<-newRst

                fpt<-raster::filename(oldStk[[i]])
                rstrStk<-IMCstackSave(oldStk[[i]],fpt)

                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                attr(rstrStk,'mdtnTimeStmp')<-newTimeStmp
                attr(rstrStk,'artnTimeStmp')<-newTimeStmp
                attr(rstrStk,'fileArchive')<-fpt

                return(rstrStk)
              },cl=cl)

              parallel::stopCluster(cl)
            } else {

              newStk<-lapply(setNames(uids,uids),function(i){

              cat (paste0('cleaning up ',labelLayer,' of ',i,'\n'))
              rst<-oldStk[[i]][[labelLayer]]
              filePath<-raster::filename(rst)
              fileN<-fs::path_file(filePath)
              fileD<-fs::path_dir(filePath)
              fileE<-sub('.*\\.', '',fileN)
              fileNN<-sub('\\.[^.]*$','',fileN)
              TEMPfile<-file.path(x$currentAnalysis$folder,'Temp',paste0('TEMP_',fileNN,suffix,'.nc'))
              newRst<-raster::focal(x = rst,
                                    w = matrix(1, ncol=matrixExtent,nrow = matrixExtent),
                                    fun = function(x){
                                      tableX<-table(x)
                                      wM<-which.max(tableX)[1]
                                      nV<-as.numeric(names(tableX)[wM])
                                      return(nV)
                                    },
                                    pad=T,
                                    padValue=pdv,
                                    overwrite = T,
                                    progress='text',
                                    filename=TEMPfile)
              newRst<-raster::ratify(newRst)
              levels(newRst)<-raster::levels(rst)
              newName<-paste0(labelLayer,suffix)
              names(newRst)<-newName
              newRst<-raster::writeRaster(newRst,
                                          filename = file.path(fileD,
                                                               paste0(fileNN,suffix,'.',fileE)),
                                          overwrite=T,
                                          format='raster')
              unlink(TEMPfile)
              oldStk[[i]][[newName]]<-newRst

              fpt<-raster::filename(oldStk[[i]])
              rstrStk<-IMCstackSave(oldStk[[i]],fpt)

              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

              attr(rstrStk,'mdtnTimeStmp')<-newTimeStmp
              attr(rstrStk,'artnTimeStmp')<-newTimeStmp
              attr(rstrStk,'fileArchive')<-fpt

              return(rstrStk)
            })

            }

            newClassification<-new('IMC_Classification',newStk)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

            attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
            attr(newClassification,'mdtnTimeStmp')<-newTimeStmp
            attr(newClassification,'artnTimeStmp')<-newTimeStmp
            attr(newClassification,'fileArchive')<-attr(x$currentAnalysis$classification,'fileArchive')


            x$currentAnalysis$classification<-newClassification

            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp


          })
