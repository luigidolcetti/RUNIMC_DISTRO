#'Some nice thing
#'
#'
checkDir<-function(parentalFolder,
                   childFolder,
                   verbose=F){

  folderList<-dir(parentalFolder)

  if (!(any (childFolder %in% folderList))){
    dir.create(file.path(parentalFolder,childFolder),showWarnings = F)

    if (verbose) {
      lenPF<-nchar(parentalFolder)
      if (lenPF>13) {
        outPF<-paste0(substr(parentalFolder,1,5),
                      '...',
                      substr(parentalFolder,lenPF-5,lenPF))
      } else {
        lenPF<-parentalFolder
      }

      message(mMessage(paste0('\n',
                              childFolder,
                              ' folder created in ',
                              outPF)))}

    return(file.path(parentalFolder,childFolder))
  }

  if (verbose) {
    message(mWarning(paste0('\n',childFolder, ' folder already exist, nothing to do')))

  }

  return(file.path(parentalFolder,childFolder))
}
