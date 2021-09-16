#' @export
setGeneric("ept_add_primary", function(x,newTab,name,...)
  standardGeneric("ept_add_primary"))

setMethod('ept_add_primary','IMC_ExpressionTable',function(x,newTab,name,...){

  if (missing(newTab)) stop (mError('specify a table to add'))
  if (missing(name)) stop (mError('specify a name'))

  if (any(newTab$pIDs %in% x@pIDs)) stop(mError('duplicated IDs'))

  browser()
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
