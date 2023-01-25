#' Validate Data
#'
#' Validate data checks that all the *.txt files used as raw input images have
#' the same header line, that channel names are recognized, and images are complete
#' meaning that all columns within an image has the same number and position of
#' rows and vice versa.
#' Parameters are the same as in **[initStudy]**
#' @return if everything is OK, the function returns 0, otherwise some clue to start
#' investigating what and where the problem might be.
#' @export
validateData<-function(fn_rawDataFolder=NULL,
                       fn_pattern = '.txt',
                       fn_x_column = 'X',
                       fn_y_column = 'Y',
                       fn_nonChannelColumns = c('X','Y','Z','Start_push','End_push','Pushes_duration'),
                       fn_emptyChannelPattern = '[0-9]+[A-z]+\\([A-z]+[0-9]+[A-z]+\\)',
                       fn_usedChannelPattern = '[0-9]+[A-z]+-.+\\([A-z]+[0-9]+[A-z]+\\)',
                       fn_channelPattern = '[0-9]+[A-z]+.*\\(([A-z]+[0-9]+[A-z]+)\\)',
                       fn_markerPattern = '[0-9]+[A-z]+-(.+)\\([A-z]+[0-9]+[A-z]+\\)',
                       ...){

  if (is.null(fn_rawDataFolder)) stop(mError('need a folder to inspect'),call. = FALSE)
  if (!file.exists(fn_rawDataFolder)) stop(mError('cannot find this folder'),call. = FALSE)

  fullPathList <- list.files(path = fn_rawDataFolder,
                             pattern = fn_pattern,
                             full.names = TRUE,
                             all.files = FALSE,
                             recursive = FALSE,
                             include.dirs = FALSE,
                             no.. = TRUE)

  fileNamesList <- list.files(path = fn_rawDataFolder,
                              pattern = fn_pattern,
                              full.names = FALSE,
                              all.files = FALSE,
                              recursive = FALSE,
                              include.dirs = FALSE,
                              no.. = TRUE)

  if (is.null(fullPathList) | length(fullPathList) == 0) {
    stop(mError('the selected path contains no files'),call. = FALSE)
  }

  ##### inspect headers #####

  headerList <- lapply(setNames(fullPathList,fileNamesList), function(x) {
    readLines(con = x,
              n = 1)
  })

  headerList <- unlist(headerList)

  headerMatrix <- expand.grid(headerList,headerList,stringsAsFactors = FALSE)
  sampleMatrix <- expand.grid(fileNamesList,fileNamesList,stringsAsFactors = FALSE)
  compareMatrix <- headerMatrix[,1] == headerMatrix[,2]

  if (!all(compareMatrix)) {
    nFiles <- 1:length(fileNamesList)
    nFiles <- formatC(nFiles,digits = floor(log10(nFiles))+1,flag = ' ')
    compareMatrix <- matrix(compareMatrix,
                            ncol = length(fileNamesList),
                            nrow = length(fileNamesList),
                            dimnames = list(fileNamesList,nFiles))
    out <- compareMatrix
    out[compareMatrix] <- 'OK'
    out[!compareMatrix] <- 'NO'
    outMask <- upper.tri(out,FALSE)
    out[!outMask] <- ""
    out <- cbind.data.frame(n=nFiles,
                            out)
    warning(mError('Headers are not equal, inspect the object that was just created'),call. = FALSE)
    return(out)
  }

  headerMaster <- unique(headerList)

  headerMaster <- strsplit(headerMaster,'\t')[[1]]

  if (length(headerMaster)==1) {
    stop(mError('no columns detected, please check that separator is \\t (tab)'),call. = FALSE)
  }

  testUsedChannels <- grepl(fn_usedChannelPattern,headerMaster,perl = TRUE, fixed = FALSE)
  testEmptyChannels <- grepl(fn_emptyChannelPattern,headerMaster,perl = TRUE, fixed = FALSE)

  if (!any(testUsedChannels) & !any(testEmptyChannels)) {
    stop(mError('there might be some problem with regex expression because channel columns cannot be interpreted'),call. = FALSE)
  }

  testX <- grepl(fn_x_column,headerMaster, fixed = TRUE)
  testY <- grepl(fn_y_column,headerMaster, fixed = TRUE)

  if (!any(testX) | !any(testY)) {
    stop(mError('cannot find coordinate columns'),call. = FALSE)
  }

  ##### inspect structure ####
  #####

  headerList <- lapply(setNames(fullPathList,fileNamesList), function(x) {
    bunchOfCoords <- data.table::fread(x,
                             sep = '\t',
                             select = c(fn_x_column,fn_y_column),
                             colClasses = 'numeric',
                             data.table = FALSE)

    colnames(bunchOfCoords) <- c('X','Y')
    Xlead <- aggregate(X~Y,bunchOfCoords,function(x){digest::digest(sort(unique(x)),algo = 'crc32')})
    Xlead <- unique(Xlead[,'X',drop = TRUE])
    Ylead <- aggregate(Y~X,bunchOfCoords,function(x){digest::digest(sort(unique(x)),algo = 'crc32')})
    Ylead <- unique(Ylead[,'Y',drop = TRUE])

    if (length(Xlead)!=1 | length(Ylead)!=1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  headerList <- unlist(headerList,use.names = TRUE)

  if (any(headerList)) {
    warning(mError('one or more images are not complete, inspect the object that was just created'),call. = FALSE)
    out <- data.frame(Matrix = ifelse(headerList,'incomplete','ok'),
                      row.names = names(headerList),
                      stringsAsFactors = FALSE)
    return(out)
  }

  return(0)
}

