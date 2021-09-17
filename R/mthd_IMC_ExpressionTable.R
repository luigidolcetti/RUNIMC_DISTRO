#' @export
setGeneric("ept_names", function(x,...)
  standardGeneric("ept_names"))

setMethod('ept_names','IMC_Study',function(x,...){
  return(x$currentAnalysis$exprs@names)
  })

setMethod('ept_names','IMC_ExpressionTable',function(x,...){
  return(x@names)
})

#' @export
setGeneric("ept_types", function(x,...)
  standardGeneric("ept_types"))

setMethod('ept_types','IMC_Study',function(x,...){
  nms<-ept_names(x)
  out<-unlist(lapply(setNames(nms,nms),function(nm){
    class(x$currentAnalysis$exprs@exprs[[nm]])[1]}))

  return(out)
  })

setMethod('ept_types','IMC_ExpressionTable',function(x,...){
  nms<-ept_names(x)
  out<-unlist(lapply(setNames(nms,nms),function(nm){
    class(x@exprs[[nm]])[1]}))

  return(out)
})


#' @export
setGeneric("ept_colnames", function(x,...)
  standardGeneric("ept_colnames"))

setMethod('ept_colnames','IMC_Study',function(x,...){
  nms<-ept_names(x)
  out<-lapply(setNames(nms,nms),function(nm){
    colnames(x$currentAnalysis$exprs@exprs[[nm]])})

  return(out)
})

setMethod('ept_types','IMC_ExpressionTable',function(x,...){
  nms<-ept_names(x)
  out<-lapply(setNames(nms,nms),function(nm){
    colnames(x@exprs[[nm]])[1]})

  return(out)
})


#' @export
setGeneric("ept_add_primary", function(x,newTab,name,...)
  standardGeneric("ept_add_primary"))

setMethod('ept_add_primary','IMC_ExpressionTable',function(x,newTab,name,...){

  if (missing(newTab)) stop (mError('specify a table to add'))
  if (missing(name)) stop (mError('specify a name'))

  if (any(newTab$pIDs %in% x@pIDs)) stop(mError('duplicated IDs'))

  if (any(x@names==name)){
    oldpIDs<-x@exprs[[name]]$pIDs
    exclusionID<-unlist(lapply(x@exprs,function(tst){
      oldIDs<-tst$pIDs
      !all(oldIDs %in% oldpIDs)
    }))
    x@exprs<-x@exprs[exclusionID]
    x@pIDs<-x@pIDs[names(x@pIDs!=name)]
  }

  newIDs<-newTab$pIDs
  x@pIDs<-c(x@pIDs,setNames(newIDs,rep(name,length(newIDs))))

  x@exprs[[name]]<-newTab
  x@names<-names(x@exprs)

  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'mdtnTimeStmp')<-newTimeStmp

  return(x)
})

setMethod('ept_add_primary','IMC_Study',function(x,newTab,name,...){

  if (missing(newTab)) stop (mError('specify a table to add'))
  if (missing(name)) stop (mError('specify a name'))

  if (any(newTab$pIDs %in% x$currentAnalysis$exprs@pIDs)) stop(mError('duplicated IDs'))

  if (any(x@names==name)){
    oldpIDs<-x$currentAnalysis$extrs@exprs[[name]]$pIDs
    exclusionID<-unlist(lapply(x$currentAnalysis$exprs@exprs,function(tst){
      oldIDs<-tst$pIDs
      !all(oldIDs %in% oldpIDs)
    }))
    x$currentAnalysis$exprs@exprs<-x$currentAnalysis$exprs@exprs[exclusionID]
    x$currentAnalysis$exprs@pIDs<-x$currentAnalysis$exprs@pIDs[names(x@pIDs!=name)]
  }

  newIDs<-newTab$pIDs
  x$currentAnalysis$exprs@pIDs<-c(x$currentAnalysis$exprs@pIDs,setNames(newIDs,rep(name,length(newIDs))))

  x$currentAnalysis$exprs@exprs[[name]]<-newTab
  x$currentAnalysis$exprs@names<-names(x$currentAnalysis$exprs@exprs)

  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x$currentAnalysis$exprs,'mdtnTimeStmp')<-newTimeStmp
  attr(x,'mdtnTimeStmp')<-newTimeStmp
  attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

})

#' @export
setGeneric("ept_add_secondary", function(x,newTab,name,...)
  standardGeneric("ept_add_secondary"))

setMethod('ept_add_secondary','IMC_ExpressionTable',function(x,newTab,name,...){

  if (missing(newTab)) stop (mError('specify a table to add'))
  if (missing(name)) stop (mError('specify a name'))

  if (!all(newTab$pIDs %in% x@pIDs)) stop(mError('unknown IDs'))

  x@exprs[[name]]<-newTab
  x@names<-names(x@exprs)

  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'mdtnTimeStmp')<-newTimeStmp

  return(x)
})

setMethod('ept_add_secondary','IMC_Study',function(x,newTab,name,...){

  if (missing(newTab)) stop (mError('specify a table to add'))
  if (missing(name)) stop (mError('specify a name'))

  if (!all(newTab$pIDs %in% x$currentAnalysis$exprs@pIDs)) stop(mError('unknown IDs'))

  x$currentAnalysis$exprs@exprs[[name]]<-newTab
  x$currentAnalysis$exprs@names<-names(x$currentAnalysis$exprs@exprs)

  newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x$currentAnalysis$exprs,'mdtnTimeStmp')<-newTimeStmp
  attr(x,'mdtnTimeStmp')<-newTimeStmp
  attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

})

#' @export
setGeneric("ept_extractPixelStatistics", function(x,geometry,raster,name,fun,cl=NA,...)
  standardGeneric("ept_extractPixelStatistics"))

setMethod('ept_extractPixelStatistics','IMC_Study',function(x,
                                                            geometry,
                                                            raster = c('raw',
                                                                       'calc',
                                                                       'class'),
                                                            name,
                                                            fun,
                                                            cl = NA,
                                                            verbose= T,
                                                            ...){
  if (missing(geometry)) stop (mError('give geometry'))
  if (missing(raster)) stop (mError('give raster'))
  if (missing(name)) stop (mError('give name'))
  if (missing(fun)) stop (mError('give function'))

  if (!any(geometry %in% x$currentAnalysis$exprs@names)) stop(mError('unknown geometry'))

  raster<-match.arg(raster,c('raw','calc','class'),several.ok = F)

  switch(raster,
         raw = whichRaster<-quote(x$raster),
         calc = whichRaster<-quote(x$currentAnalysis$derivedRasters),
         class = whichRaster<-quote(x$currentAnalysis$classification))

  newTab<-.extractPixelStatistics(fn_polygons = sf::st_as_sf(x$currentAnalysis$exprs@exprs[[geometry]]),
                                  fn_raster = eval (whichRaster),
                                  fn_function = fun,
                                  verbose=T,
                                  cl=cl,)

  ept_add_secondary(x,newTab,name)

})


#' @export
setGeneric("ept_apply", function(x,name,newName,columns,fun,...)
  standardGeneric("ept_apply"))

setMethod('ept_apply','IMC_Study',function(x,
                                                            name,
                                                            newName,
                                                            columns,
                                                            fun,
                                                            ...){

  if (missing(name)) stop (mError('give name of an origin table'),call. = F)
  if (missing(newName)) stop (mError('give name for results'),call. = F)
  if (missing(columns)) warning (mWarning('column names not specified'),call. = F,noBreaks. = T)
  if (missing(fun)) stop (mError('give function'))

  newDF<-x$currentAnalysis$exprs@exprs[[name]]
  clnms<-colnames(newDF)
  clnmsT<-clnms!='pIDs'

  if (!missing(columns)){
    if (!is.null(columns)){
      if (all(columns %in% clnms)){
        newDFtarget<-newDF[,columns]} else {
          stop(mError("some columns don't match"))
        }} else {
          newDFtarget<-newDF[,clnms[clnmsT]]
        }} else {
          newDFtarget<-newDF[,clnms[clnmsT]]
        }

  newDF[,colnames(newDFtarget)]<-apply(newDFtarget,2,fun)

  ept_add_secondary(x,newDF,newName)

})



#' @export
setGeneric("ept_pull_sf", function(x,geometry,dataFrame,...)
  standardGeneric("ept_pull_sf"))

setMethod('ept_pull_sf','IMC_Study',function(x,
                                             geometry,
                                             dataFrame,
                                             ...){

  if (missing(geometry)) stop(mError('give geometry'),call. = F)
  if (length(geometry)!=1) stop(mError('only 1 geometry is accepted'),call. = F)

  availableNames<-x$currentAnalysis$exprs@names

  if (!geometry %in% availableNames) stop(mError('unknown geometry'),call. = F)
  if (!all(dataFrame %in% availableNames)) stop(mError('some dataframe are not available'),call. = F)

  neoGeo<-x$currentAnalysis$exprs@exprs[[geometry]]

  class_neoGeo<-class(neoGeo)

  if (!all(class_neoGeo %in% c('sf','data.frame'))) stop(mError('geometry is not valid'),call. = F)

  clnmsG<-try(attr(neoGeo,'sf_column'))

  if (inherits(clnmsG,'try-error')) stop(mError('geometry column is not valid'),call. = F)

  for (df in dataFrame){
    ### dplyr::join doesn't work

    pIDs<-neoGeo[,'pIDs',drop=T]
    neoDF<-x$currentAnalysis$exprs@exprs[[df]]
    prIDs<-neoDF$pIDs %in% pIDs
    abIDs<-!(pIDs %in% neoDF$pIDs)
    neoDF<-neoDF[prIDs,]
    extIDs<-pIDs[abIDs]
    extraRows<-as.data.frame(matrix(NA,
                                    nrow=length(extIDs),
                                    ncol=ncol(neoDF),
                                    dimnames = list(NULL,colnames(neoDF))))
    extraRows$pIDs<-extIDs
    neoDF<-rbind.data.frame(neoDF,extraRows,stringsAsFactors = F)
    neoDF<-neoDF[match(pIDs,neoDF$pIDs),-1]
    clnmsDF<-colnames(neoDF)
    clnmsWhich<-clnmsDF!='pIDs'
    clnmsDF[clnmsWhich]<-paste(clnmsDF[clnmsWhich],df,sep = '.')
    colnames(neoDF)<-clnmsDF
    neoGeo<-dplyr::bind_cols(neoGeo,neoDF)
  }

  clnms<-colnames(neoGeo)
  clnmsT<-clnms %in% c('pIDs',clnmsG)
  neoGeo<-neoGeo[,c('pIDs',clnms[!clnmsT],clnmsG)]
  neoGeo<-sf::st_as_sf(neoGeo)
  return(neoGeo)

})




