#' RUNIMC GUI for annotation
#'
#' Open a GUI for features annotations, setting help=T give some hints on how to
#' proceed during annotation.... but it is mostly just annoying when you alrady
#' know what to do, so it is set to False by default.
#'
#' @export
runIMC <- function(x=NULL,help=F) {


  appDir <- system.file("ShinyRUNIMC", package = "RUNIMCTEMP")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `RUNIMC`.", call. = FALSE)
  }

  if (is.null(x)) stop(mError("specify a study"),call. = F)
  if (is.null(x$currentAnalysis$name)) stop(mError("study does not contain a current analysis"),call. = F)
  if (!dir.exists(file.path(x$currentAnalysis$folder,'training','polygons'))) stop(mError("the current analysis folder tree is currupted"),call. = F)

 shiny::shinyOptions(studyName=x$name,
                     rasterPath=file.path(x$rootFolder,x$name,'rasterStacks'),
                     analysisName=x$currentAnalysis$name,
                     trainingPolygonPath=file.path(x$currentAnalysis$folder,'training','polygons'),
                     help = help)

 shiny::runApp(appDir, display.mode = "normal")

}


