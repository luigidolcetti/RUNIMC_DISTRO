
server <- function(input, output, session) {

  ###### Initialize service env ######
  shinyServiceEnv<-new.env()

  shinyServiceEnv$rstPath<-list.files(system.file("\\ShinyRUNIMC\\demo\\PIC\\rst",
                                                  package = "RUNIMCTEMP"),
                                      pattern = '*.nc',
                                      full.names = T)

  shinyServiceEnv$rstPic<-sapply(shinyServiceEnv$rstPath,raster::raster,simplify = F,USE.NAMES = F)
  names(shinyServiceEnv$rstPic)<-paste0('lyr',1:length(shinyServiceEnv$rstPic))
  shinyServiceEnv$rstStack<-IMC_stack(x = shinyServiceEnv$rstPic,
                                      uid = "demo",
                                      IMC_text_file = "demo",
                                      study = NULL,
                                      sample = NULL,
                                      replicate = NULL,
                                      ROI = NULL,
                                      bioGroup = NULL,
                                      channels = new('IMC_ChannelTable'))

  shinyServiceEnv$nmsRst<-names(shinyServiceEnv$rstStack)
  shinyServiceEnv$plc<-colorRampPalette(c('black','white'))
  shinyServiceEnv$plcn<-255
  shinyServiceEnv$pWidth = 500
  shinyServiceEnv$pHeight<-shinyServiceEnv$pWidth*((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/(shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1]))
  shinyServiceEnv$hotKey<-c('p','d','n','g','shift+v','enter','shift+c','t')
  shinyServiceEnv$NAcol<-'blue'
  shinyServiceEnv$studyName<-shiny::getShinyOption('studyName')
  shinyServiceEnv$analysisName<- shiny::getShinyOption('analysisName')
  shinyServiceEnv$rasterPath<-  shiny::getShinyOption('rasterPath')
  shinyServiceEnv$trainingPolygonPath<- shiny::getShinyOption('trainingPolygonPath')
  shinyServiceEnv$capFlag <- F
  if (shiny::getShinyOption('help')==T) {
    shinyServiceEnv$infoColor <- "color:gold"
    shinyServiceEnv$infoTrigger <- "hover"
  } else {
    shinyServiceEnv$infoColor <- "color:gray"
    shinyServiceEnv$infoTrigger <- "manual"
  }
  options(shiny.maxRequestSize=1000*1024^2)

  ###### Initialize reactive values ######

  PAINTMODE<-shiny::reactiveValues(pntm='n',
                                   ent=F,
                                   del=F,
                                   mark=F,
                                   delV=F)

  VERTEXBUFFER<-shiny::reactiveValues(x=integer(0),
                                      y=integer(0))

  GEOM<-shiny::reactiveValues(buffer=sf::st_sf(sf::st_sfc()),
                              bufferColor='red',
                              dataBase=sf::st_sf(uid=character(0),
                                                 label=character(0),
                                                 color=character(0),
                                                 GEOMETRY=sf::st_sfc(),
                                                 stringsAsFactors = F),
                              toShow=sf::st_sf(label=character(0),
                                               color=character(0),
                                               GEOMETRY=sf::st_sfc(),
                                               stringsAsFactors = F),
                              toBorder=character(0))

  GEOMETRY<-shiny::reactiveValues(index=1,
                                  vertices=list())

  BGCOLOR<-shiny::reactiveValues(bgc='white',
                                 lty=1)

  RSTMARC<-shiny::reactiveValues(x=0,y=0)

  ZBRAKES<-shiny::reactiveVal(rep(0,1,1/(shinyServiceEnv$plcn-1)))

  LBLTBLV <- shiny::reactiveValues(table = data.frame(label=c('NUC','JNC','BKG'),
                                                      note=c('TEST','TEST','TEST'),
                                                      color=c('red','green','cyan'),
                                                      show=c(T,T,T),
                                                      stringsAsFactors = FALSE))

  UPLOADFILENAME<-shiny::reactiveVal("demo....")
  DOWNLOADFILENAME<-shiny::reactiveVal("demo....")
  CURRENTSCHEMA<-shiny::reactiveVal("No schema...")

  UPLOADFILELIST<-shiny::reactiveVal(list.files(
    path = shinyServiceEnv$rasterPath,
    full.names = F,
    recursive = F,
    pattern = '*.txt',
    include.dirs = F
  ))

  # DEFTBLV<-shiny::reactiveValues(table=matrix(c(NA),
  #                                             ncol= 9,
  #                                             nrow=0,
  #                                             dimnames = list(NULL,layers=c('Name','L0','red','green','blue','L1','L2','L3','L4'))
  #                                               ))
  DEFTBLV<-shiny::reactiveValues(table=data.frame(Name=vector('character',0),
                                                  L0=vector('character',0),
                                                  red=vector('character',0),
                                                  green=vector('character',0),
                                                  blue=vector('character',0),
                                                  L1=vector('character',0),
                                                  L2=vector('character',0),
                                                  L3=vector('character',0),
                                                  L4=vector('character',0),
                                                  row.names = NULL,
                                                  stringsAsFactors = F))


  # CURRENTSTACK<-shiny::reactiveVal(shinyServiceEnv$rstStack)

  CURRENTSTACK_TEMP<-shiny::reactiveValues(stack=shinyServiceEnv$rstStack,
                                           caps=sapply(shinyServiceEnv$nmsRst,function(x)0.996,simplify = F,USE.NAMES = T))

  isolate({
    for (i in shinyServiceEnv$nmsRst){
      CURRENTSTACK_TEMP$stack[[i]]<-quantNorm(fn_dt = CURRENTSTACK_TEMP$stack[[i]],
                                              fn_trsh = CURRENTSTACK_TEMP$caps[[i]])
    }
  })


  MESSAGETOTHEPEOPLE<-shiny::reactiveVal("Start draw polygons on the left and navigate on the right portview")

  output$fileUpload<-shiny::renderText('Upload a raster Stack....')


  ###### file Controls####
  output$fileControls <- renderUI({
    shiny::fluidRow(
      shinydashboard::box(
        shiny::renderText(MESSAGETOTHEPEOPLE()),
        width = 12),
      shinydashboard::box(
        shiny::helpText(paste0('Study::: ',shinyServiceEnv$studyName,
                               ' / ',
                               'Analysis:::',shinyServiceEnv$analysisName))
      ),
      shinydashboard::box(

        shiny::selectInput(inputId = "fileRasterList",
                           label = htmltools::h3(span("Available pictures"),span(icon("info-circle"), id = "fileRasterList_info", style = shinyServiceEnv$infoColor)),
                           choices = c("select a raster from the list",UPLOADFILELIST()),
                           selected = NULL,
                           multiple = F),

        shinyBS::bsTooltip("fileRasterList_info", "Choose one sample from the list of samples available in this study",
                           "bottom",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),

        shiny::fileInput(inputId = "fileRaster",
                         label = htmltools::h3(span("Upload a sample from file"),span(icon("info-circle"), id = "fileRaster_info", style = shinyServiceEnv$infoColor)),
                         multiple = FALSE,
                         accept = ".stk"),

        shinyBS::bsTooltip("fileRaster_info", "Browse to the .stk file you want to use for annotation... the file should be relevant to this study, but you may want to have a look to a picture that it is not. You can... just remember not to save any polygon otherwise it is going to rise an error when trying to extract features.",
                           "bottom", options = list(container = "body"),trigger = shinyServiceEnv$infoTrigger),

        shiny::verbatimTextOutput('fileUpload',placeholder = T),

        width = 12),

      shinydashboard::box(
        shiny::fileInput(inputId = "deftblUpload",
                         label = htmltools::h3(span("Upload label definitions"),span(icon("info-circle"), id = "deftblUpload_info", style = shinyServiceEnv$infoColor)),
                         multiple = FALSE,
                         accept = ".sqlite"),

        downloadButton('deftbl','Download label definitions'),

        shinyBS::bsTooltip("deftblUpload_info", "You may already have a table of labels, defining some features you want to upload. Otherwise you can download the current table to a file with the button below",
                           "bottom", options = list(container = "body"),trigger = shinyServiceEnv$infoTrigger),

        width = 6),

      shinydashboard::box(
        shiny::fileInput(inputId = "dldtblUpload",
                         label = htmltools::h3(span("Upload Polygons"),span(icon("info-circle"), id = "dldtblUpload_info", style = shinyServiceEnv$infoColor)),
                         multiple = FALSE,
                         accept = ".sqlite"),

        downloadButton('dldtbl','Download Polygons'),

        shinyBS::bsTooltip("dldtblUpload_info", "Upload polygons already traced for this picture from an .sqlite file...(By the way It would be a very bad idea to mix polygons from different pictures). You can also save the table of polygons to a file using the button below, although it would be better doing it from the button on the next tab that make sure the file endup in the proper folder.",
                           "bottom", options = list(container = "body"),trigger = shinyServiceEnv$infoTrigger),
        width = 6),

      # shinydashboard::box(
      #
      #   width = 6),
      #
      #
      # shinydashboard::box(
      #
      #   width = 6)

    )

  })

  ##### picture Controls ####
  output$pictureControls <- renderUI({
    shiny::fluidRow(
      shinyjs::useShinyjs(),
      keys::useKeys(),
      keys::keysInput("keys", shinyServiceEnv$hotKey),

      shinydashboard::box(
        shiny::renderText(MESSAGETOTHEPEOPLE()),
        width = 12),

      shinydashboard::box(
        shiny::plotOutput("plotG",
                          click='plotG_click',
                          hover =  (NULL)),

        shinyBS::bsPopover(id = "plotG",
                           title = "Main view port",
                           content = "This is the main viewport, on which you can add polygons surrounding cells of interest.<br> (1) Mouse left-click to add a vertex.<br>(2) Enter to close a polygon (at least 3 vertex).<br>(3) Shift-v to dismiss the vertices and start over.<br>(4) Shift-c to delete polygons: place one point inside each polygon to delete",
                           placement = "right",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),


        width = 5
      ),

      shinydashboard::box(
        shiny::plotOutput("plotT",
                          click = "plotT_click",
                          hover = (NULL)),

        shinyBS::bsPopover(id = "plotT",
                           title = "Navigation viewport",
                           content = "Mouse left-click to change the center of the region of interest",
                           placement = "left",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),

        width = 5
      ),

      shinydashboard::box(
        shiny::textOutput(outputId = 'NofV'),
        div(style=('height: 80px;padding: 0px;margin-top: 0px;'),
            shiny::selectInput(inputId = 'cvrLabel',
                               label = htmltools::h5(span('coverage label'),span(icon("info-circle"), id = "labelBox_info", style = shinyServiceEnv$infoColor)),
                               choices = c('Select a label',LBLTBLV$table$label),
                               selected = NULL)),
        shinyBS::bsPopover(id = "labelBox_info",
                           title = "Label selector",
                           content = "Select a lable you want to annotate",
                           placement = "bottom",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),
        div(style=('height: 30px;padding: 0px;margin-top: 0px;'),
            span('Current schema'),span(icon("info-circle"), id = "currentSchema_info", style = shinyServiceEnv$infoColor)),
        shinyBS::bsPopover(id = "currentSchema_info",
                           title = "Current Schema",
                           content = "Refer to the bottom of this page for storing and using channel schemas",
                           placement = "bottom",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),
        # div(style=('height: 70px;padding: 0px;margin-top: 0px;  font-size: 20px;'),
        shiny::textOutput(outputId = "currentSchema"),
        div(style=('height: 80px;padding: 0px;margin-top: 0px;'),
            shiny::sliderInput("lineWidth",
                               htmltools::h5(span('Line width'),span(icon("info-circle"), id = "lineWidth_info", style = shinyServiceEnv$infoColor)),
                               min = 0.5,
                               max = 10,
                               value = c(1),
                               step = 0.5)),
        shinyBS::bsPopover(id = "lineWidth_info",
                           title = "Line width",
                           content = "Change the thickness of perimeter of polygons",
                           placement = "bottom",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),
        div(style=('height: 110px;padding: 0px;margin-top: 0px;'),
            shiny::sliderInput("quantCap",
                               htmltools::h5(span('Contrast'),span(icon("info-circle"), id = "quantCap_info", style = shinyServiceEnv$infoColor)),
                               min = 0,
                               max = 1,
                               value = 0.996,step = 0.001)),
        shinyBS::bsPopover(id = "quantCap_info",
                           title = "Contrast",
                           content = "Improve the contrast cutting the brighter pixels",
                           placement = "bottom",
                           options = list(container = "body"),
                           trigger = shinyServiceEnv$infoTrigger),
        div(style=('height: 30px;padding: 0px;margin-top: 0px;'),
            shiny::actionButton(inputId = 'savePolyOTF',
                                label = 'Seve polygons')),
        width = 2
      ),
      shinydashboard::box(
        shiny::plotOutput("plotP"),
        width = 5
      ),
      shinydashboard::box(
        shiny::sliderInput("sTrls","Thresholds",
                           min = 0,
                           max = 1,
                           value = c(0,1),step = 0.001),
        shiny::sliderInput("xTrls","X position",
                           min = shinyServiceEnv$rstStack[[1]]@extent[1],
                           max = shinyServiceEnv$rstStack[[1]]@extent[2],
                           value = round((shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1])/2)),
        shiny::sliderInput("yTrls","Y position",
                           min = shinyServiceEnv$rstStack[[1]]@extent[3],
                           max = shinyServiceEnv$rstStack[[1]]@extent[4],
                           value = round((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/2)),
        shiny::sliderInput("zTrls","Zoom",
                           min = 1,
                           max = {
                             TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                             if (TEMP<100) {TEMP} else {100}},
                           value = {
                             TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                             if (TEMP<100) {TEMP/2} else {50}}),
        width = 3
      ),
      shinydashboard::box(
        shiny::selectInput("lTrls0","Layer 0",
                           choices = shinyServiceEnv$nmsRst,
                           selected = shinyServiceEnv$nmsRst[[1]]),
        shiny::selectInput("lTrls0_red","Red",
                           choices = shinyServiceEnv$nmsRst,
                           selected = NULL,
                           multiple = T),
        shiny::selectInput("lTrls0_green","Green",
                           choices = shinyServiceEnv$nmsRst,
                           selected = NULL,
                           multiple = T),
        shiny::selectInput("lTrls0_blue","Blue",
                           choices = shinyServiceEnv$nmsRst,
                           selected = NULL,
                           multiple = T),
        width = 2
      ),
      shinydashboard::box(
        shiny::selectInput("lTrls1","Layer 1",
                           choices = shinyServiceEnv$nmsRst,
                           selected = shinyServiceEnv$nmsRst[[1]]),
        shiny::selectInput("lTrls2","Layer 2",
                           choices = shinyServiceEnv$nmsRst,
                           selected = shinyServiceEnv$nmsRst[[1]]),
        shiny::selectInput("lTrls3","Layer 3",
                           choices = shinyServiceEnv$nmsRst,
                           selected = shinyServiceEnv$nmsRst[[1]]),
        shiny::selectInput("lTrls4","Layer 4",
                           choices = shinyServiceEnv$nmsRst,
                           selected = shinyServiceEnv$nmsRst[[1]]),
        width = 2
      ),
      # shinydashboard::box(
      #   shiny::textInput(inputId = 'defName',
      #                    label = 'name',
      #                    value = 'name'),
      #   shiny::actionButton("addDef","Add"),
      #   shiny::actionButton("delDef","Delete"),
      #   shiny::actionButton("appDef","Apply"),
      #   width=2),
      # shinydashboard::box(
      #   shiny::downloadButton('expDef','Export',icon = NULL),
      #   width=1),
      # shinydashboard::box(
      #   shiny::fileInput("impDef", label = NULL,
      #                    multiple = FALSE,
      #                    accept = ".R",width = '10%',buttonLabel = 'Import',placeholder = NULL),
      #   width=1),
      shinydashboard::box(
        htmltools::div(style="display:inline-block",
                       shiny::textInput(inputId = 'defName',
                                        label = NULL,
                                        value = 'name',
                                        width = '300px')),
        shiny::actionButton("addDef","Add"),
        shiny::actionButton("repDef","Replace"),
        shiny::actionButton("delDef","Delete"),
        shiny::actionButton("upDef","MoveUp"),
        shiny::actionButton("downDef","MoveDown"),
        shiny::actionButton("appDef","Apply"),
        shiny::downloadButton('expDef','Export',icon = NULL),

        shiny::fileInput("impDef", label = NULL,
                         multiple = FALSE,
                         accept = ".R",
                         width = '20%',
                         buttonLabel = 'Import',
                         placeholder = NULL),
        div(style="background-color:#ADD8E6;",
            DT::DTOutput('defTbl')),
        width = 12)
    )
  })

  ##### label Controls ####
  output$labelControls <- renderUI({
    shiny::fluidRow(
      shinydashboard::box(
        shiny::renderText(MESSAGETOTHEPEOPLE()),
        width = 12),
      shinydashboard::box(
        shiny::actionButton("addLbl","Add label"),
        shiny::actionButton("delLbl","delete label"),
        shiny::actionButton("tggl","Toggle visibility")),
      shinydashboard::box(
        shiny::actionButton("delPt","delete all plygons keep labels"),
        shiny::actionButton('delLblPt',"delete all labels and polygons")),
      shiny::fluidRow(
        div(style="background-color:#ADD8E6;",
            DT::DTOutput('lblTbl'))
      )
    )
  })

  ##### stats Controls ####
  output$statsControls <- renderUI({
    shinydashboard::tabItem(tabName = 'Stats',

                            shiny::fluidRow(
                              shinydashboard::box(
                                shiny::renderText(MESSAGETOTHEPEOPLE()),
                                width = 12),
                              shinydashboard::box(
                                shiny::actionButton('recalcStats','recalculate stats'),
                                div(style="background-color:#ADD8E6;",
                                    DT::DTOutput('tableT')),
                                width=12
                              ),
                              width=12),
                            shiny::fluidRow(
                              shinydashboard::box(
                                shiny::actionButton('refreshPlots','update plots'),
                                shiny::plotOutput("Stata"),
                                width=12
                              ),
                              width=12)
    )
  })



  # session$onSessionEnded(function(){
  #
  #   if (exists("shinyServiceEnv", mode="environment",where = .GlobalEnv)) rm(shinyServiceEnv,pos = .GlobalEnv)
  #
  # })


  # observers ---------------------------------------------

  shiny::observeEvent(input$quantCap,{

    if (shinyServiceEnv$capFlag){
      CURRENTSTACK_TEMP$caps[[input$lTrls0]]<-input$quantCap

      CURRENTSTACK_TEMP$stack[[input$lTrls0]]<-quantNorm(fn_dt = shinyServiceEnv$rstStack[[input$lTrls0]],
                                                         fn_trsh = CURRENTSTACK_TEMP$caps[[input$lTrls0]])

      # TEMP<-shinyServiceEnv$rstStack
      # for (lyr in shinyServiceEnv$nmsRst){
      #   TEMP[[lyr]]<-quantNorm(TEMP[[lyr]],input$quantCap)
      # }
      #
      # CURRENTSTACK(TEMP)
      #
      # rm(TEMP)
    } else {
      shinyServiceEnv$capFlag <- T
    }

  })

  shiny::observeEvent(input$lTrls0,{

    shiny::updateSliderInput(session,"quantCap",NULL,
                             min = 0,
                             max = 1,
                             value = CURRENTSTACK_TEMP$caps[[input$lTrls0]])
    shinyServiceEnv$capFlag <- F

  })


  shiny::observeEvent(LBLTBLV$table,{
    shiny::updateSelectInput(
      session = session,
      inputId = 'cvrLabel',
      label = 'coverage label',
      choices = c(NA,LBLTBLV$table$label),
      selected = NULL
    )

    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
  })

  shiny::observeEvent(GEOM$dataBase,{
    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
  })

  shiny::observeEvent(input$cvrLabel,{
    GEOM$bufferColor<-LBLTBLV$table$color[LBLTBLV$table$label==input$cvrLabel]
    if(length(GEOM$bufferColor)==0){GEOM$bufferColor='white'}
  })


  shiny::observeEvent(input$delPt,{
    GEOM$dataBase<-sf::st_sf(uid=character(0),
                             label=character(0),
                             color=character(0),
                             GEOMETRY=sf::st_sfc(),
                             stringsAsFactors = F)
  })

  shiny::observeEvent(input$delLblPt,{
    GEOM$dataBase<-sf::st_sf(uid=character(0),
                             label=character(0),
                             color=character(0),
                             GEOMETRY=sf::st_sfc(),
                             stringsAsFactors = F)

    LBLTBLV$table<-data.frame(label=character(0),
                              note=character(0),
                              color=character(0),
                              show=logical(0),
                              stringsAsFactors = FALSE)

  })


  shiny::observeEvent(input$addLbl,{
    LBLTBLV$table<-rbind.data.frame(LBLTBLV$table,
                                    data.frame(label=paste0(LETTERS[sample(1:26,3)],collapse = ""),
                                               note='TEST',
                                               color=colors()[sample(1:657,1)],
                                               show=T,
                                               stringsAsFactors = FALSE),
                                    stringsAsFactors = F)
  })


  shiny::observeEvent(input$delLbl,{
    if (!is.null(input$lblTbl_rows_selected)) {
      GEOM$dataBase<-GEOM$dataBase[!GEOM$dataBase$label==LBLTBLV$table$label[as.numeric(input$lblTbl_rows_selected)],]
      LBLTBLV$table <- LBLTBLV$table[-as.numeric(input$lblTbl_rows_selected),]}
  })

  shiny::observeEvent(input$tggl,{
    if (!is.null(input$lblTbl_rows_selected)) {
      LBLTBLV$table$show[as.numeric(input$lblTbl_rows_selected)] <- !LBLTBLV$table$show[as.numeric(input$lblTbl_rows_selected)]}
  })


  shiny::observeEvent(input$lblTbl_cell_edit,{
    info = input$lblTbl_cell_edit
    i = info$row
    j = info$col
    v = info$value

    if (j==3 && any(v %in% LBLTBLV$table[,3])) {
      shiny::showNotification(ui = 'color in use !!!',closeButton = T,type = 'error',session = session)
      v<-LBLTBLV$table[i, j]}

    if (j==3 && !any(v %in% colors())) {
      shiny::showNotification(ui = 'invalid color !!!',closeButton = T,type = 'error',session = session)
      v<-LBLTBLV$table[i, j]}

    if (j==1 && any(v %in% LBLTBLV$table[,1])) {
      shiny::showNotification(ui = 'label in use !!!',closeButton = T,type = 'error',session = session)
      v<-LBLTBLV$table[i, j]
    }

    if (j==1 && any(grepl(v,LBLTBLV$table[,1]))) {
      whichLabel<-which(grepl(v,LBLTBLV$table[,1]))
      shiny::showNotification(ui = 'invalid label!!!',closeButton = T,type = 'error',session = session)
      v<-LBLTBLV$table[i, j]
    }

    if (j==1 && any(unlist(lapply(LBLTBLV$table[,1],function(x)grepl(x,v))))) {
      whichLabel<-which(grepl(v,LBLTBLV$table[,1]))
      shiny::showNotification(ui = 'invalid label!!!',closeButton = T,type = 'error',session = session)
      v<-LBLTBLV$table[i, j]
    }

    if (j==1){
      labelSplit<-strsplit(v,character(0))[[1]]
      labelLetters<-grepl('^[A-Za-z]+$', labelSplit)
      labelNumbers<-grepl('^[0-9]+$', labelSplit)
      labelLogic<-labelLetters|labelNumbers
      labelLogic<-Reduce('&',labelLogic)
      if (!labelLogic | labelNumbers[1]) {
        shiny::showNotification(ui = 'invalid label !!!',closeButton = T,type = 'error',session = session)
        v<-LBLTBLV$table[i, j]}
    }

    if (nrow(GEOM$dataBase)!=0){
      if (j==3 ) {
        if (nrow(GEOM$dataBase[as.data.frame(GEOM$dataBase$color)==LBLTBLV$table[i,j],])!=0){
          GEOM$dataBase$color[as.data.frame(GEOM$dataBase$color)==LBLTBLV$table[i,j]]<-v
          GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
          GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
        }}
      if (j==1 ) {
        if (nrow(GEOM$dataBase[as.data.frame(GEOM$dataBase$label)==LBLTBLV$table[i,j],])!=0){
          GEOM$dataBase$label[as.data.frame(GEOM$dataBase$label)==LBLTBLV$table[i,j]]<-v
          GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
          GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
        }}
    }
    LBLTBLV$table[i, j]<- v
    DT::reloadData(proxy = proxy)
  })

  shiny::observeEvent(input$keys, {


    if (input$keys=='enter') {PAINTMODE$ent= T}
    if (input$keys=='shift+c') {PAINTMODE$del= T}
    if (input$keys=='shift+v'){PAINTMODE$delV=T}

  })

  shiny::observeEvent(input$sTrls,{
    ZBRAKES(seq(input$sTrls[1],
                input$sTrls[2],
                1/(shinyServiceEnv$plcn-1)))

  })

  #|-Population marker definition table commands =================================

  #..|-AddDef ######################################

  shiny::observeEvent(input$addDef,{

    if (is.null(input$lTrls0_red))iR<-"" else iR<-input$lTrls0_red
    if (is.null(input$lTrls0_green))iG<-"" else iG<-input$lTrls0_green
    if (is.null(input$lTrls0_blue))iB<-"" else iB<-input$lTrls0_blue

    newLine<-data.frame(input$defName,
                        input$lTrls0,
                        I(list(iR)),
                        I(list(iG)),
                        I(list(iB)),
                        input$lTrls1,
                        input$lTrls2,
                        input$lTrls3,
                        input$lTrls4)
    colnames(newLine)<-colnames(DEFTBLV$table)
    DEFTBLV$table<-rbind.data.frame(DEFTBLV$table,newLine)
  })

  #..|-replaceDef ######################################

  shiny::observeEvent(input$repDef,{

    info = input$defTbl_rows_selected
    if (!is.null(info)){
      if (is.null(input$lTrls0_red))iR<-"" else iR<-input$lTrls0_red
      if (is.null(input$lTrls0_green))iG<-"" else iG<-input$lTrls0_green
      if (is.null(input$lTrls0_blue))iB<-"" else iB<-input$lTrls0_blue

      newLine<-data.frame(input$defName,
                          input$lTrls0,
                          I(list(iR)),
                          I(list(iG)),
                          I(list(iB)),
                          input$lTrls1,
                          input$lTrls2,
                          input$lTrls3,
                          input$lTrls4)
      colnames(newLine)<-colnames(DEFTBLV$table)
      DEFTBLV$table[info,]<-newLine[1,]
    }
  })

  #..|-delDef #################
  shiny::observeEvent(input$delDef,{
    info = input$defTbl_rows_selected
    if (!is.null(info)) DEFTBLV$table<-DEFTBLV$table[-info,]
  })

  #..|-appDef ################
  shiny::observeEvent(input$appDef,{
    info = input$defTbl_rows_selected
    if (!is.null(info)) {
      oldLine<-DEFTBLV$table[info,,drop=F]

      shiny::updateTextInput(session,"defName",value = oldLine[1,1])
      shiny::updateSelectInput(session,"lTrls0",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,2])
      shiny::updateSelectInput(session,"lTrls0_red",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,3][[1]])
      shiny::updateSelectInput(session,"lTrls0_green",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,4][[1]])
      shiny::updateSelectInput(session,"lTrls0_blue",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,5][[1]])
      shiny::updateSelectInput(session,"lTrls1",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,6])
      shiny::updateSelectInput(session,"lTrls2",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,7])
      shiny::updateSelectInput(session,"lTrls3",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,8])
      shiny::updateSelectInput(session,"lTrls4",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = oldLine[1,9])
      CURRENTSCHEMA(oldLine[1,1])
    }
  })

  #..|-upDef ################
  shiny::observeEvent(input$upDef,{
    info <- input$defTbl_rows_selected
    nr<- nrow(DEFTBLV$table)
    if (!is.null(info)) {
      if (info>1){
        oldOrder<-1:nr
        newOrder<-oldOrder
        newOrder[info-1]<-info
        newOrder[info]<-oldOrder[info-1]
        DEFTBLV$table<-DEFTBLV$table[newOrder,,drop=F]
      }
    }
  })

  #..|-downDef ################
  shiny::observeEvent(input$downDef,{
    info <- input$defTbl_rows_selected
    nr<- nrow(DEFTBLV$table)
    if (!is.null(info)) {
      if (info<nr){
        oldOrder<-1:nr
        newOrder<-oldOrder
        newOrder[info+1]<-info
        newOrder[info]<-oldOrder[info+1]
        DEFTBLV$table<-DEFTBLV$table[newOrder,,drop=F]
      }
    }
  })

  #..|-Download def################
  output$expDef <- shiny::downloadHandler(
    filename = function(){"filter_def.R"},
    content = function(file) {
      saveRDS(DEFTBLV$table,file = file)
    })

  #..|-Upload def##################
  shiny::observeEvent(input$impDef,{
    DEFTBLV$table<-readRDS(input$impDef$datapath)
  })

  ###### Tables ######

  output$tableT<-DT::renderDT(LBLTBLV$polygons,
                              server = F,
                              editable = F,
                              option=list(bFilter=0,
                                          bInfo=0,
                                          bLengthChange=0,
                                          bAutoWidth=0,
                                          pageLength=nrow(LBLTBLV$polygons)))

  output$lblTbl<-DT::renderDT(LBLTBLV$table,
                              server = T,
                              editable = T,
                              selection = 'single',
                              options = list(bFilter=0,
                                             bInfo=0,
                                             bLengthChange=0,
                                             bAutoWidth=0,
                                             pageLength=nrow(LBLTBLV$table)))
  proxy = DT::dataTableProxy('lblTbl')

  output$defTbl<-DT::renderDT(DEFTBLV$table,
                              server = F,
                              editable = F,
                              selection = 'single',
                              option=list(bFilter=0,
                                          bInfo=0,
                                          bLengthChange=0,
                                          bAutoWidth=0,
                                          pageLength=nrow(DEFTBLV$table)))

  ###### Plots ######

  output$currentSchema<-shiny::renderText(CURRENTSCHEMA())
  output$text_out<-shiny::renderText(UPLOADFILENAME())
  output$plotG<-shiny::renderPlot(

    {
      input$sTrls
      if (is.null(input$lTrls0_red) &
          is.null(input$lTrls0_green) &
          is.null(input$lTrls0_blue)){

        par(oma=c(0,0,0,0),mar=c(2,2,1,1),bty='n',bg='black')
        plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack[[input$lTrls0]],
                  fn_xmin = input$xTrls-input$zTrls,
                  fn_xmax = input$xTrls+input$zTrls,
                  fn_ymin = input$yTrls-input$zTrls,
                  fn_ymax = input$yTrls+input$zTrls,
                  fn_plcn = shinyServiceEnv$plcn,
                  fn_plc = shinyServiceEnv$plc,
                  fn_plcRange = ZBRAKES(),
                  fn_xaxs = 'i',
                  fn_yaxs = 'i',
                  fn_xaxt = 's',
                  fn_yaxt = 's',
                  fn_colNA = shinyServiceEnv$NAcol,
                  fn_Bx = VERTEXBUFFER$x,
                  fn_By = VERTEXBUFFER$y,
                  fn_bgc = GEOM$bufferColor,
                  fn_geom = GEOM$toShow,
                  fn_geomB = GEOM$toBorder,
                  fn_lwd = input$lineWidth,
                  fn_title = input$lTrls0)
      } else {
        par(oma=c(0,0,0,0),mar=c(2,2,1,1),bty='n',bg='black',fg='white')

        plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack,
                  fn_rst_red = input$lTrls0_red,
                  fn_rst_green = input$lTrls0_green,
                  fn_rst_blue = input$lTrls0_blue,
                  fn_xmin = input$xTrls-input$zTrls,
                  fn_xmax = input$xTrls+input$zTrls,
                  fn_ymin = input$yTrls-input$zTrls,
                  fn_ymax = input$yTrls+input$zTrls,
                  fn_plcn = shinyServiceEnv$plcn,
                  fn_plc = shinyServiceEnv$plc,
                  fn_plcRange = ZBRAKES(),
                  fn_xaxs = 'i',
                  fn_yaxs = 'i',
                  fn_xaxt = 's',
                  fn_yaxt = 's',
                  fn_colNA = shinyServiceEnv$NAcol,
                  fn_Bx = VERTEXBUFFER$x,
                  fn_By = VERTEXBUFFER$y,
                  fn_bgc = GEOM$bufferColor,
                  fn_geom = GEOM$toShow,
                  fn_geomB = GEOM$toBorder,
                  fn_lwd = input$lineWidth,
                  fn_title = input$lTrls0)
      }
    })

  ####
  output$plotP<-shiny::renderPlot({
    input$sTrls
    par(oma=c(0,0,0,0),mar=c(0,0,1,0),bty='n',bg='black',fg='white')
    layout.matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
    layout(mat = layout.matrix,
           heights = c(2, 2),
           widths = c(2, 2))

    plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack[[input$lTrls1]],
              fn_xmin = input$xTrls-input$zTrls,
              fn_xmax = input$xTrls+input$zTrls,
              fn_ymin = input$yTrls-input$zTrls,
              fn_ymax = input$yTrls+input$zTrls,
              fn_plcn = shinyServiceEnv$plcn,
              fn_plc = shinyServiceEnv$plc,
              fn_plcRange = ZBRAKES(),
              fn_xaxs = 'i',
              fn_yaxs = 'i',
              fn_xaxt = 'n',
              fn_yaxt = 'n',
              fn_colNA = shinyServiceEnv$NAcol,
              fn_Bx = VERTEXBUFFER$x,
              fn_By = VERTEXBUFFER$y,
              fn_bgc = GEOM$bufferColor,
              fn_geom = GEOM$toShow,
              fn_geomB = GEOM$toBorder,
              fn_lwd = input$lineWidth,
              fn_title = input$lTrls1)
    plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack[[input$lTrls2]],
              fn_xmin = input$xTrls-input$zTrls,
              fn_xmax = input$xTrls+input$zTrls,
              fn_ymin = input$yTrls-input$zTrls,
              fn_ymax = input$yTrls+input$zTrls,
              fn_plcn = shinyServiceEnv$plcn,
              fn_plc = shinyServiceEnv$plc,
              fn_plcRange = ZBRAKES(),
              fn_xaxs = 'i',
              fn_yaxs = 'i',
              fn_xaxt = 'n',
              fn_yaxt = 'n',
              fn_colNA = shinyServiceEnv$NAcol,
              fn_Bx = VERTEXBUFFER$x,
              fn_By = VERTEXBUFFER$y,
              fn_bgc = GEOM$bufferColor,
              fn_geom = GEOM$toShow,
              fn_geomB = GEOM$toBorder,
              fn_lwd = input$lineWidth,
              fn_title = input$lTrls2)
    plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack[[input$lTrls3]],
              fn_xmin = input$xTrls-input$zTrls,
              fn_xmax = input$xTrls+input$zTrls,
              fn_ymin = input$yTrls-input$zTrls,
              fn_ymax = input$yTrls+input$zTrls,
              fn_plcn = shinyServiceEnv$plcn,
              fn_plc = shinyServiceEnv$plc,
              fn_plcRange = ZBRAKES(),
              fn_xaxs = 'i',
              fn_yaxs = 'i',
              fn_xaxt = 'n',
              fn_yaxt = 'n',
              fn_colNA = shinyServiceEnv$NAcol,
              fn_Bx = VERTEXBUFFER$x,
              fn_By = VERTEXBUFFER$y,
              fn_bgc = GEOM$bufferColor,
              fn_geom = GEOM$toShow,
              fn_geomB = GEOM$toBorder,
              fn_lwd = input$lineWidth,
              fn_title = input$lTrls3)
    plotsPoly(fn_rst = CURRENTSTACK_TEMP$stack[[input$lTrls4]],
              fn_xmin = input$xTrls-input$zTrls,
              fn_xmax = input$xTrls+input$zTrls,
              fn_ymin = input$yTrls-input$zTrls,
              fn_ymax = input$yTrls+input$zTrls,
              fn_plcn = shinyServiceEnv$plcn,
              fn_plc = shinyServiceEnv$plc,
              fn_plcRange = ZBRAKES(),
              fn_xaxs = 'i',
              fn_yaxs = 'i',
              fn_xaxt = 'n',
              fn_yaxt = 'n',
              fn_colNA = shinyServiceEnv$NAcol,
              fn_Bx = VERTEXBUFFER$x,
              fn_By = VERTEXBUFFER$y,
              fn_bgc = GEOM$bufferColor,
              fn_geom = GEOM$toShow,
              fn_geomB = GEOM$toBorder,
              fn_lwd = input$lineWidth,
              fn_title = input$lTrls4)
  })

  ###
  output$plotT<-shiny::renderPlot({
    input$sTrls
    par(oma=c(0,0,0,0),mar=c(2,2,1,1),bty='n',bg='black',fg='white')
    raster::plot(CURRENTSTACK_TEMP$stack[[input$lTrls0]],
                 col=shinyServiceEnv$plc(shinyServiceEnv$plcn),
                 breaks= ZBRAKES(),
                 asp=1,
                 xaxs="i",
                 yaxs="i",
                 xaxt="s",
                 yaxt="s",
                 legend=F,
                 colNA=shinyServiceEnv$NAcol)
    plot(GEOM$toShow,
         border=GEOM$toBorder,
         lwd = input$lineWidth,
         add=T)
    title(input$lTrls0,adj=0,line=0.3)
    abline(h=c(input$yTrls-input$zTrls,
               input$yTrls+input$zTrls),
           v=c(input$xTrls-input$zTrls,
               input$xTrls+input$zTrls),
           col=GEOM$bufferColor,lty=1,
           lwd = input$lineWidth)
  })

  shiny::observeEvent(VERTEXBUFFER$x,{
    output$NofV<-shiny::renderText(paste0('Vertex N: ',length(VERTEXBUFFER$x)))
  })


  ###### Click plot ######

  shiny::observeEvent(input$plotG_click,{

    if(input$cvrLabel!='Select a label'){
      VERTEXBUFFER$x<-c(VERTEXBUFFER$x,input$plotG_click$x)
      VERTEXBUFFER$y<-c(VERTEXBUFFER$y,input$plotG_click$y)
    } else {
      MESSAGETOTHEPEOPLE('Select a lable for the catecory you are outlineing')
    }

  })

  shiny::observeEvent(PAINTMODE$ent,{

    if (PAINTMODE$ent==T){

      PAINTMODE$ent<-F

      nVertex<-length(VERTEXBUFFER$x)

      if (nVertex>=3){
        VERTEXBUFFER$x<-c(VERTEXBUFFER$x,VERTEXBUFFER$x[1])
        VERTEXBUFFER$y<-c(VERTEXBUFFER$y,VERTEXBUFFER$y[1])
        Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),ncol=2,byrow = F)
        Vpoly<-sf::st_polygon(list(Vmat))
        if (!sf::st_is_valid(Vpoly)) {
          MESSAGETOTHEPEOPLE('This polygon was invalid... pay particular attention to self crossing')
          VERTEXBUFFER$x=integer(0)
          VERTEXBUFFER$y=integer(0)
        } else{
          Vpoly<-sf::st_sfc(Vpoly)
          lblSelect<-input$cvrLabel
          clSelect<-LBLTBLV$table$color[LBLTBLV$table$label==lblSelect]
          GEOM$buffer<-sf::st_sf(uid=shinyServiceEnv$rstStack@uid,
                                 label=lblSelect,
                                 color=clSelect,
                                 GEOMETRY = sf::st_sfc(Vpoly),
                                 stringsAsFactors = F)
          GEOM$dataBase<-rbind.data.frame(GEOM$dataBase,GEOM$buffer,stringsAsFactors = F)
          VERTEXBUFFER$x=integer(0)
          VERTEXBUFFER$y=integer(0)
        }
      } else {
        MESSAGETOTHEPEOPLE('A polygon has at least three vertex')
      }
    }
  })

  shiny::observeEvent(PAINTMODE$del,{

    if (PAINTMODE$del==T){
      PAINTMODE$del<-F
      Vmat<-matrix(c(VERTEXBUFFER$x,VERTEXBUFFER$y),
                   ncol=2,
                   byrow = F)
      if (nrow(Vmat)==0) {return(0)}
      if (nrow(GEOM$dataBase)==0) {return(0)}
      vpoly<-sf::st_sfc(sf::st_multipoint(Vmat))
      intersection<-which(unlist(lapply(sf::st_intersects(GEOM$dataBase,vpoly),function(x){length(x)==0})))
      GEOM$dataBase<-GEOM$dataBase[intersection,]
      VERTEXBUFFER$x=integer(0)
      VERTEXBUFFER$y=integer(0)
    }
  })

  shiny::observeEvent(PAINTMODE$delV,{
    if (PAINTMODE$delV==T){
      PAINTMODE$delV<-F
      VERTEXBUFFER$x<-integer(0)
      VERTEXBUFFER$y<-integer(0)}
  })


  shiny::observeEvent(input$plotT_click,{

    shiny::updateSliderInput(session,"xTrls",NULL,
                             min = shinyServiceEnv$rstStack[[1]]@extent[1],
                             max = shinyServiceEnv$rstStack[[1]]@extent[2],
                             value = round(input$plotT_click$x))
    shiny::updateSliderInput(session,"yTrls",NULL,
                             min = shinyServiceEnv$rstStack[[1]]@extent[3],
                             max = shinyServiceEnv$rstStack[[1]]@extent[4],
                             value = round(input$plotT_click$y))
  })

  ###### uopload Raster via browser#####
  shiny::observeEvent(input$fileRaster,{

    shinyServiceEnv$rstStack<-IMCstackOpen(input$fileRaster$datapath)

    shinyServiceEnv$nmsRst<-names(shinyServiceEnv$rstStack)

    fnm<-shinyServiceEnv$rstStack@IMC_text_file
    fnm<-strsplit(fnm,'/')
    fnm<-fnm[[1]][length(fnm[[1]])]

    UPLOADFILENAME(paste0('file:',fnm,'\n uid: ',shinyServiceEnv$rstStack@uid))
    DOWNLOADFILENAME(fnm)

    textDetails<-paste0('uid: ',shinyServiceEnv$rstStack@uid,'\n',
                        'file: ',shinyServiceEnv$rstStack@IMC_text_file,'\n',
                        'layers: ',paste(shinyServiceEnv$nmsRst,collapse = ', '))
    output$fileUpload<-shiny::renderText(textDetails)

    rm(fnm)

    CURRENTSTACK_TEMP$stack<-shinyServiceEnv$rstStack
    CURRENTSTACK_TEMP$caps<-sapply(shinyServiceEnv$nmsRst,function(x)0.996,simplify = F,USE.NAMES = T)

    for (i in shinyServiceEnv$nmsRst){
      CURRENTSTACK_TEMP$stack[[i]]<-quantNorm(fn_dt = shinyServiceEnv$rstStack[[i]],
                                              fn_trsh = CURRENTSTACK_TEMP$caps[[i]])
    }

    isolate({

      shiny::updateSliderInput(session,"quantCap",NULL,
                               min = 0,
                               max = 1,
                               value = CURRENTSTACK_TEMP$caps[[shinyServiceEnv$nmsRst[[1]]]])

      shiny::updateSliderInput(session,"xTrls",NULL,
                               min = shinyServiceEnv$rstStack[[1]]@extent[1],
                               max = shinyServiceEnv$rstStack[[1]]@extent[2],
                               value = round((shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1])/2))
      shiny::updateSliderInput(session,"yTrls",NULL,
                               min = shinyServiceEnv$rstStack[[1]]@extent[3],
                               max = shinyServiceEnv$rstStack[[1]]@extent[4],
                               value = round((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/2))
      shiny::updateSliderInput(session,"zTrls",NULL,
                               min = 1,
                               max = {
                                 TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                 if (TEMP<100) {TEMP} else {100}},
                               value = {
                                 TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                 if (TEMP<100) {TEMP/2} else {50}})

      shiny::updateSelectInput(session,"lTrls0",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls0_red",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls0_green",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls0_blue",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls1",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls2",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls3",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls4",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
    })
  })

  ###### uopload Raster via list#####
  shiny::observeEvent(input$fileRasterList,{

    if (input$fileRasterList=='select a raster from the list') return()

    shinyServiceEnv$rstStack<-IMCstackOpen(file.path(
      shinyServiceEnv$rasterPath,input$fileRasterList
    ))

    shinyServiceEnv$nmsRst<-names(shinyServiceEnv$rstStack)

    fnm<-shinyServiceEnv$rstStack@IMC_text_file
    fnm<-strsplit(fnm,'/')
    fnm<-fnm[[1]][length(fnm[[1]])]
    UPLOADFILENAME(paste0('file:',fnm,'\n uid: ',shinyServiceEnv$rstStack@uid))
    DOWNLOADFILENAME(fnm)

    textDetails<-paste0('uid: ',shinyServiceEnv$rstStack@uid,'\n',
                        'file: ',shinyServiceEnv$rstStack@IMC_text_file,'\n',
                        'layers: ',paste(shinyServiceEnv$nmsRst,collapse = ', '))
    output$fileUpload<-shiny::renderText(textDetails)

    rm(fnm)

    CURRENTSTACK_TEMP$stack<-shinyServiceEnv$rstStack
    CURRENTSTACK_TEMP$caps<-sapply(shinyServiceEnv$nmsRst,function(x)0.996,simplify = F,USE.NAMES = T)

    for (i in shinyServiceEnv$nmsRst){
      CURRENTSTACK_TEMP$stack[[i]]<-quantNorm(fn_dt = shinyServiceEnv$rstStack[[i]],
                                              fn_trsh = CURRENTSTACK_TEMP$caps[[i]])
    }


    isolate({

      shiny::updateSliderInput(session,"quantCap",NULL,
                               min = 0,
                               max = 1,
                               value = CURRENTSTACK_TEMP$caps[[shinyServiceEnv$nmsRst[[1]]]])

      shiny::updateSliderInput(session,"xTrls",NULL,
                               min = shinyServiceEnv$rstStack[[1]]@extent[1],
                               max = shinyServiceEnv$rstStack[[1]]@extent[2],
                               value = round((shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1])/2))
      shiny::updateSliderInput(session,"yTrls",NULL,
                               min = shinyServiceEnv$rstStack[[1]]@extent[3],
                               max = shinyServiceEnv$rstStack[[1]]@extent[4],
                               value = round((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/2))
      shiny::updateSliderInput(session,"zTrls",NULL,
                               min = 1,
                               max = {
                                 TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                 if (TEMP<100) {TEMP} else {100}},
                               value = {
                                 TEMP<-min(shinyServiceEnv$rstStack[[1]]@extent[2],shinyServiceEnv$rstStack[[1]]@extent[4])
                                 if (TEMP<100) {TEMP/2} else {50}})

      shiny::updateSelectInput(session,"lTrls0",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls0_red",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls0_green",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls0_blue",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = NULL)
      shiny::updateSelectInput(session,"lTrls1",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls2",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls3",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
      shiny::updateSelectInput(session,"lTrls4",NULL,
                               choices = shinyServiceEnv$nmsRst,
                               selected = shinyServiceEnv$nmsRst[[1]])
    })



    # TEMP<-shinyServiceEnv$rstStack
    # for (lyr in shinyServiceEnv$nmsRst){
    #   TEMP[[lyr]]<-quantNorm(TEMP[[lyr]],0.996)
    # }
    #
    # CURRENTSTACK(TEMP)
    #
    # rm(TEMP)
  })

  ###### uopload Table#####

  shiny::observeEvent(input$dldtblUpload,{
    GEOM$dataBase<-sf::st_read(input$dldtblUpload$datapath,stringsAsFactors = F)
    TEMPloc<-as.data.frame(GEOM$dataBase)[,c('label','color')]
    TEMPloc<-unique(TEMPloc)
    LBLTBLV$table<-data.frame(label=TEMPloc$label,
                              note=rep('TEST',nrow(TEMPloc)),
                              color=TEMPloc$color,
                              show=rep(T,nrow(TEMPloc)),
                              stringsAsFactors = FALSE)
    GEOM$toShow<-GEOM$dataBase$GEOMETRY[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]]
    GEOM$toBorder<-as.character(GEOM$dataBase$color[GEOM$dataBase$label %in% LBLTBLV$table$label[LBLTBLV$table$show==T]])
    # invalidateLater(10000)
  })


  ###### Download table ######



  output$dldtbl <- shiny::downloadHandler(

    filename = function(){


      paste0(shinyServiceEnv$rstStack@IMC_text_file,".sqlite")},
    content = function(file) {

      uid_check<-unique(GEOM$dataBase$uid)
      if (length(uid_check)>1) {
        MESSAGETOTHEPEOPLE('Polygons referring to more than one image has been detected,
                         foreign polygong will be deleted')
        GEOM$dataBase<-GEOM$dataBase[GEOM$dataBase$uid==shinyServiceEnv$rstStack@uid,]
      }
      sf::st_write(GEOM$dataBase,fileTarget,append=F,quite=T,layer_options = c("OVERWRITE=yes"))
    }
  )

  observeEvent(input$savePolyOTF,{

    uid_check<-unique(GEOM$dataBase$uid)
    if (length(uid_check)>1) {
      MESSAGETOTHEPEOPLE('Polygons referring to more than one image has been detected,
                         foreign polygong will be deleted')
      GEOM$dataBase<-GEOM$dataBase[GEOM$dataBase$uid==shinyServiceEnv$rstStack@uid,]
    }
    fileTarget<-file.path(shinyServiceEnv$trainingPolygonPath,
                          paste0(shinyServiceEnv$rstStack@IMC_text_file,".sqlite"))
    sf::st_write(GEOM$dataBase,fileTarget,append=F,quite=T,layer_options = c("OVERWRITE=yes"))
    MESSAGETOTHEPEOPLE('Polygon table saved')
  })

  ###### uopload def Table#####

  shiny::observeEvent(input$deftblUpload,{

    GEOM$dataBase<-sf::st_sf(uid=character(0),
                             label=character(0),
                             color=character(0),
                             GEOMETRY=sf::st_sfc(),
                             stringsAsFactors = F)

    LBLTBLV$table<-data.frame(label=character(0),
                              note=character(0),
                              color=character(0),
                              show=logical(0),
                              stringsAsFactors = FALSE)

    LBLTBLV$table<-utils::read.table(file = input$deftblUpload$datapath,
                                     header = T,
                                     sep = '\t',
                                     as.is = T,
                                     check.names = F,
                                     stringsAsFactors = F)

  })


  ###### Download def table ######



  output$deftbl <- shiny::downloadHandler(

    filename = function(){


      "labels.txt"},
    content = function(file) {
      utils::write.table(LBLTBLV$table,file,append=F,sep = '\t',row.names = F,col.names = T,quote = F)

    }
  )





  ##### recalculate stats #####

  shiny::observeEvent(input$recalcStats,{

    isolate({
      if (length(GEOM$dataBase)>0){
        if(nrow(GEOM$dataBase)>0){
          TEMP_label<-GEOM$dataBase$label
          TEMP_area<-sf::st_area(GEOM$dataBase)
          TEMP_perimeter<-lwgeom::st_perimeter(GEOM$dataBase)
          TEMP_roundness<-4*pi*TEMP_area/(TEMP_perimeter^2)
          TEMP_dataFrame<-data.frame(label=TEMP_label,
                                     area = signif(TEMP_area,4),
                                     perimeter = signif(TEMP_perimeter,4),
                                     roundness = signif(TEMP_roundness,4))
          TEMP_totalArea<-aggregate(area~label,TEMP_dataFrame,sum)
          names(TEMP_totalArea)[2]<-'total area'
          TEMP_medianArea<-aggregate(area~label,TEMP_dataFrame,median)
          names(TEMP_medianArea)[2]<-'median area'
          TEMP_medianPerimeter<-aggregate(perimeter~label,TEMP_dataFrame,median)
          names(TEMP_medianPerimeter)[2]<-'median perimeter'
          TEMP_medianRoundness<-aggregate(roundness~label,TEMP_dataFrame,median)
          names(TEMP_medianRoundness)[2]<-'median roundness'
          TEMP_nPoly<-as.data.frame(table(TEMP_dataFrame$label))
          names(TEMP_nPoly)<-c('label','n poly')
          out_dataFrame<-plyr::join_all(list(TEMP_nPoly,
                                             TEMP_totalArea,
                                             TEMP_medianArea,
                                             TEMP_medianPerimeter,
                                             TEMP_medianRoundness),by='label')
        } else {
          MESSAGETOTHEPEOPLE('The list of polygons is empty')
          out_dataFrame<-data.frame()
        }
      } else {
        MESSAGETOTHEPEOPLE('The list of polygons is empty')
        out_dataFrame<-data.frame()
      }

    })

    LBLTBLV$polygons<-out_dataFrame
  })

  shiny::observeEvent(input$refreshPlots,{

    if (length(GEOM$dataBase)>0){
      if(nrow(GEOM$dataBase)>0){

        TEMP_rst<-list(shinyServiceEnv$rstStack)
        names(TEMP_rst)<-unique(GEOM$dataBase$uid)
        pixelValue<-extractFeatures(TEMP_rst,GEOM$dataBase,fn_coverage = c(1,0),fn_coverage_label = c("X"))
        rm(TEMP_rst)

        pixelValue<-tidyr::pivot_longer(pixelValue$value,names_to='layer',values_to='value',cols=names(shinyServiceEnv$rstStack))

        pall<-cbind(paste0('X_',LBLTBLV$table$label),LBLTBLV$table$color)

        ggp<-ggplot2::ggplot(pixelValue)+
          ggplot2::geom_density(ggplot2::aes(x=value,y=..scaled..,color=label),alpha=0.8,position = 'identity',lwd=1)+
          ggplot2::scale_color_manual(values = pall[,2],breaks = pall[,1])+
          ggplot2::scale_x_continuous(trans = scales::modulus_trans(-1))+
          ggplot2::facet_wrap(facets='layer',scales = "free")+
          ggplot2::theme (panel.background = ggplot2::element_rect(fill='black'),
                          panel.grid = ggplot2::element_blank(),
                          strip.background = ggplot2::element_rect(fill="gray5"),
                          strip.text = ggplot2::element_text(colour="white"),
                          axis.title = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.line = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank(),
                          legend.position = 'bottom',
                          legend.key = ggplot2::element_rect(fill = "black"))

        output$Stata<-shiny::renderPlot(ggp)
      } else {
        MESSAGETOTHEPEOPLE('The list of polygons is empty')

      }
    } else {
      MESSAGETOTHEPEOPLE('The list of polygons is empty')

    }
  })

}
