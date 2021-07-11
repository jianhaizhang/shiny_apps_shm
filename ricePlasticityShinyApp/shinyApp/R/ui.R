library(shiny); library(shinydashboard); library(shinydashboardPlus); library(yaml); library(plotly); library(DT); library(shinyWidgets); library(shinyBS); library(shinyjs)
lis.cfg <- yaml.load_file('config/config.yaml')
tit <- sub('^(title|width):', '', lis.cfg$title)


js <- "function openFullscreen(elem) {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}"

search_ui <- function(id, lab=label) {
  ns <- NS(id) 
  fluidRow(splitLayout(cellWidths=c('1%', '13%', '1%', '85%'), '',
  radioButtons(inputId=ns('sch.mode'), label='Search mode', choices=c('Single', 'Multiple'), selected='Multiple', inline=TRUE), '',
  uiOutput(ns('sch.box'))
  ))
}


data_ui <- function(id, deg=FALSE) {
  ns <- NS(id)
  # if (deg == FALSE) tabPanel("Primary Visualization", value='primary',
  if (deg==FALSE) box(width = 12, title = "Data (replicates aggregated)", closable = FALSE, solidHeader = TRUE, collapsible = TRUE, enable_sidebar = FALSE, status = "primary", enable_dropdown = FALSE,
    tabsetPanel(type="pills", id=NULL, selected="dTabAll",
      tabPanel('Complete', value='dTabAll',
      navbarPage('Parameters:',
      tabPanel("Basic",
      fluidRow(splitLayout(cellWidths=c('1%', '15%', '1%', '15%', '1%', '15%', '1%', '15%'), '',
      actionButton(ns("dat.all.but"), "Confirm selection", style = "margin-top: 24px;"), '',
      selectInput(inputId=ns('log'), label='Log/exp-transform', choices=c("No", 'Log2'="log2", 'Exp2'="exp2"), selected='No'), '', 
      selectInput(inputId=ns('scaleDat'), label='Scale by', choices=c('No'='No', 'Row'='Row', 'Selected'='Selected', 'All'='All'), selected='Row'), '',
      numericInput(ns('page'), label="Page height", value=300, min=50, max=Inf, step=100, width=150)
      )),
      bsTooltip(id=ns('normDat'), title="CNF: calcNormFactors in edgeR. <br/> ESF: estimateSizeFactors in DESeq2. <br/> VST: varianceStabilizingTransformation in DESeq2. <br/> rlog: regularized log in DESeq2.", placement = "top", trigger = "hover"),
      bsTooltip(id=ns('scaleDat'), title="Row: scale each row independently. <br/> Selected: scale across all selected genes as a whole. <br/> All: scale across all genes as a whole.", placement = "top", trigger = "hover"),
      bsTooltip(id=ns('log'), title="No: original values in uploaded data are used. <br/> Log2: transform data to log2-scale. <br/> Exp2: transform data to the power of 2.", placement = "top", trigger = "hover")
      ), # tabPanel
      tabPanel("Filter",
      fluidRow(splitLayout(cellWidths=c('1%', '14%', '1%', '30%', '1%', '24%', '1%', '24%'), '',
      numericInput(inputId=ns("A"), label="Threshold (A) to exceed", value=0), '',
      numericInput(inputId=ns("P"), label="Proportion (P) of samples with values >= A", value=0), '',
      numericInput(inputId=ns("CV1"), label="Min coefficient of variation (CV1)", value=-10^4), '', 
      numericInput(inputId=ns("CV2"), label="Max coefficient of variation (CV2)", value=10^4)
      )), actionButton(inputId=ns('fil.but'), label="Submit"), verbatimTextOutput(ns("fil.par")) 
      ),
      tabPanel("Threshold",
      fluidRow(splitLayout(cellWidths=c('1%', '20%', '1%', '20%', '1%', '10%'), '', 
      textInput(ns("sig.max"), "Signal threshold (max)", '', placeholder=('Default to max signal.'), width=200), '',
      textInput(ns("sig.min"), "Signal threshold (min)", '', placeholder=('Default to min signal.'), width=200), '',
      actionButton(ns("sig.but"), "Confirm", icon=NULL, style = "margin-top: 24px;")
      )), br(), span(uiOutput(ns('msg.sig.thr')), style='color:red')  
      )
      ), # navbarPage 
      fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", dataTableOutput(ns("dtAll")), ""))
     ), # tabPanel('Complete'
      tabPanel('Selected', value='dTabSel',
        fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", dataTableOutput(ns("dtSel")), "")), br()
      ),
      tabPanel('Selected Profile', value='dTabSelProf',
        fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", plotOutput(ns("selProf")), ""))
      )
   ) # tabsetPanel(

  ) else if (deg == TRUE) {
    box(width = 12, title = "Data (with replicates)", closable = FALSE, solidHeader = TRUE, 
      collapsible = TRUE, enable_sidebar = FALSE, status = "primary", enable_dropdown = FALSE,
      fluidRow(splitLayout(cellWidths=c('1%', '14%', '1%', '30%', '1%', '24%', '1%', '24%'), '',
      numericInput(inputId=ns("A"), label="Threshold (A) to exceed", value=0), '',
      numericInput(inputId=ns("P"), label="Proportion (P) of samples with values >= A", value=0), '',
      numericInput(inputId=ns("CV1"), label="Min coefficient of variation (CV1)", value=-10^4), '', 
      numericInput(inputId=ns("CV2"), label="Max coefficient of variation (CV2)", value=10^4)
      )), actionButton(inputId=ns('fil.but'), label="Submit"), verbatimTextOutput(ns("fil.par")),
      fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", dataTableOutput(ns("dtRep")),  ""))
    )
  }
}

upload_ui <- function(id) {
   ns <- NS(id)
   tabPanel(title="Landing Page", value='landing',
      h4(strong("Choose a data set"), style='margin-left:3px;'),
      fluidRow(splitLayout(cellWidths=c('1%', '30%'), '', 
      selectInput(ns("fileIn"), NULL, c('NONE'), 'NONE')
      )), br(),
      h4(strong("Gallery"), style='margin-left:3px;'),
      fluidRow(
        column(6, id='field', style='text-align:center', uiOutput(ns('field'))),
        column(6, id='plate', style='text-align:center', uiOutput(ns('plate')))
      ),
      fluidRow(
        column(6, id='sub', style='text-align:center', uiOutput(ns('sub'))),
        column(6, id='wdwl', style='text-align:center', uiOutput(ns('wdwl')))
      )
   )
  
}

shm_ui <- function(id, data.ui, search.ui) {
  ns <- NS(id)
  tabPanel("Spatial Heatmap", value='shmPanelAll', icon=NULL,
    br(),
    # list(
    # width = ifelse(input$lgdTog %% 2 == 0, 9, 12), 
      # boxPad(color = NULL, title = NULL, solidHeader = FALSE, 
    # Append matrix heatmap, network with SHMs.    
    do.call(tabsetPanel, list(type="pills", id=ns('shmMhNet'), selected="shm1",
    # tabsetPanel(type = "pills", id=NULL, selected="shm1",
  
      tabPanel(title="Image", value='shm1',
      column(12, search.ui, style='z-index:5'),  
      navbarPage('Parameters:',
      tabPanel("Basic",
      fluidRow(splitLayout(cellWidths=c('0.5%', '8%', '0.5%', '8%', '0.5%', '8%', '0.5%', '9%', '0.5%', '11%', '0.5%', '8%', '0.5%', '8%', '0.5%', '8%'), '',  
      actionButton(ns("fs"), "Full screen", onclick = "openFullscreen(document.getElementById('barSHM'))"), '',
      div(title = 'Number of columns for the subplots.',
      dropdownButton(inputId=ns('colDrop'), label='Columns', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width='100%',
        sliderInput(ns("col.n"), "", min=1, max=50, step=1, value=2, width='100%')
      )
      ), '',
      div(title='Data column: by the column order in data matrix.',
      dropdownButton(inputId=ns('disDrop'), label='Display by', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        radioButtons(inputId=ns('genCon'), label='', choices=c("Gene"="gene", "Condition"="con", "Data column"="none"), selected='', inline=FALSE, width='100%')
      )), '',
      dropdownButton(inputId=ns('scaleDrop'), label='Scale plots', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        sliderInput(ns("scale.shm"), "", min=0.1, max=10, step=0.1, value=1, width='100%')
      ), '',
      dropdownButton(inputId=ns('scroDrop'), label='Scrolling height', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        sliderInput(ns("scrollH"), "", min=50, max=10000, step=50, value=450, width='100%')
      ), '',
      dropdownButton(inputId=ns('titDrop'), label='Title size', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        sliderInput(ns("title.size"), "", min=0, max=100, step=0.5, value=12, width='100%')
      ), '',
      dropdownButton(inputId=ns('dropdown'), label='Color key', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
      fluidRow(splitLayout(cellWidths=c('1%', '60%', '35%'), '', textInput(ns("color"), "Color scheme", '', placeholder=paste0('Eg: ', ''), width=200),
      actionButton(ns("col.but"), "Confirm", icon=NULL, style = "margin-top: 24px;"))),
      radioButtons(inputId=ns('cs.v'), label='Color key based on', choices=c("Selected rows", "All rows"), selected='', inline=TRUE)
      ), '', 
      dropdownButton(inputId=ns('togDrop'), label='Horizontal layout', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        sliderInput(ns("togSld"), "Adjust horizontal layout", min=0, max=1, step=0.05, value=0.67, width='100%')
      )
      )), # fluidRow
      # bsPopover(id=ns('genCon'), title="Data column: by the column order in data matrix.", placement = "top", trigger = "hover"),
      textOutput(ns('h.w.c')), textOutput(ns('msg.col')), br()
      ), # tabPanel
      tabPanel("Value legend",
      fluidRow(splitLayout(cellWidths=c('1%', '10%', '1%', '10%', '1%', '10%', '1%', '10%', '1%', '10%'), '', 
      numericInput(inputId=ns('val.lgd.row'), label='Rows', value='', min=1, max=Inf, step=1, width=150), '',
      numericInput(inputId=ns('val.lgd.key'), label='Key size', value='', min=0.0001, max=1, step=0.01, width=150), '',
      numericInput(inputId=ns('val.lgd.text'), label='Text size', value='', min=0.0001, max=Inf, step=1, width=140), '',
      radioButtons(inputId=ns('val.lgd.feat'), label='Include features', choices=c('No', 'Yes'), selected='', inline=TRUE), '',
      actionButton(ns("val.lgd"), "Add/Remove", icon=icon("refresh"), style = "margin-top: 24px;") 
      ))
      ), # tabPanel
      tabPanel("Shape outline",
      splitLayout(cellWidths=c('1%', '15%', '1%', '13%'), '', 
      selectInput(ns('line.color'), label='Line color', choices=c('grey70', 'black', 'red', 'green', 'blue'), selected=''), '', 
      numericInput(inputId=ns('line.size'), label='Line size', value='', min=0.05, max=Inf, step=0.05, width=150) 
      )), # tabPanel
     tabPanel("Download",

#     tags$div(title="Download the spatial heatmaps and legend plot.",
      # h1(strong("Download paramters:"), style = "font-size:20px;"),
      fluidRow(splitLayout(cellWidths=c('0.7%', '15%', '1%', '10%', '1%', '12%', '1%', '12%', '1%', '8%', '1%', '8%'), '',
      radioButtons(inputId=ns('ext'), label='File type', choices=c("jpg", "png", "pdf"), selected='jpg', inline=TRUE), '', 
      numericInput(inputId=ns('res'), label='Resolution (dpi)', value='', min=10, max=Inf, step=10, width=150), '',
      radioButtons(inputId=ns('lgd.incld'), label='Include legend plot', choices=c('Yes', 'No'), selected='', inline=TRUE), '', 
      numericInput(inputId=ns('lgd.size'), label='Legend plot size', value='', min=-1, max=Inf, step=0.1, width=140), '',
      actionButton(ns("dld.but"), "Confirm", icon=icon("refresh"), style = "margin-top: 24px;"), '',
      # downloadButton(ns("dld.shm"), "Download", style = "margin-top: 24px;")
      uiOutput(ns('dldBut'))
      )), # fluidRow
      bsTooltip(id=ns('ext'), title="Select a file type to download.", placement = "bottom", trigger = "hover")

      #fluidRow(splitLayout(cellWidths=c('18%', '1%', '25%', '1%', '18%') 
      # tags$div(title="Alegend plot.",
      # ), '', 
      #)) # fluidRow 
      ) # tabPanel 
     
      #) # navbarMenu
      ), # navbarPage 
 
    verbatimTextOutput(ns('msgSHM')), uiOutput(ns('shm.ui')), data.ui
    ), # tabPanel 

      tabPanel(title='Interactive', value='interTab',
      navbarPage('', id=ns('interNav'),
      tabPanel('Plot', value='interPlot',
        fluidRow(splitLayout(cellWidths=c("1%", "13%", '5%', "80%"), '',
        actionButton(ns("ggly.but"), "Click to show/update", icon=icon("refresh"), style="color:#fff; background-color:#499fe9;border-color:#2e6da4"), '',
        uiOutput(ns('sld.fm'))
        )),
        # The input ids should be unique, so no legend plot parameters are added here.
        fluidRow(splitLayout(cellWidths=c("1%", "7%", "61%", "30%"), "", plotOutput(ns("bar2")), htmlOutput(ns("ggly")), plotOutput(ns("lgd2"))))
      ),
      tabPanel('Parameters',
        fluidRow(splitLayout(cellWidths=c('1.5%', '16%', '1%', '12%', '1%', '12%'), '',
          numericInput(ns('t'), label='Transition time (s)', value=2, min=0.1, max=Inf, step=NA, width=270), '',
          numericInput(ns('scale.ly'), label='Scale plot', value=1, min=0.1, max=Inf, step=0.1, width=170), '',
          downloadButton(ns("dld.anm"), "Download", style = "margin-top: 24px;") 
         )), textOutput(ns('tran'))
      )) # navbarPage
      ),
      tabPanel(title='Video', value='vdoTab',
      navbarPage('', id=ns('vdoNav'),
      tabPanel('Video', value='video',
      actionButton(ns("vdo.but"), "Click to show/update", icon=icon("refresh"), style="color:#fff; background-color:#499fe9;border-color:#2e6da4"),
      fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", uiOutput(ns('video')), ""))
      ),
      tabPanel("Parameters",
      fluidRow(splitLayout(cellWidths=c('1%', '8%', '1%', '10%', '1%', '13%', '1%', '10%', '1%', '8%', '2%', '14%', '1%', '8%'), '',
      numericInput(inputId=ns('vdo.key.row'), label='Key rows', value=2, min=1, max=Inf, step=1, width=270), '',
      numericInput(inputId=ns('vdo.key.size'), label='Key size', value=0.04, min=0.01, max=Inf, step=0.1, width=270), '',
      radioButtons(inputId=ns("vdo.val.lgd"), label="Key value", choices=c("Yes", "No"), selected='No', inline=TRUE), '', 
      radioButtons(inputId=ns("vdo.label"), label="Feature label", choices=c("Yes", "No"), selected='No', inline=TRUE), '',
      numericInput(inputId=ns('vdo.lab.size'), label='Label size', value=2, min=0, max=Inf, step=0.5, width=150), '',
      selectInput(ns("vdo.dim"), label="Fixed dimension", choices=c('1920x1080', '1280x800', '320x568', '1280x1024', '1280x720', '320x480', '480x360', '600x600', '800x600', '640x480'), selected='640x480', width=110), '',
      numericInput(inputId=ns('vdo.bar.width'), label='Color key width', value=0.1, min=0.01, max=0.9, step=0.03, width=270)
      )), # fluidRow

      fluidRow(splitLayout(cellWidths=c('1%', '14%', '1%', '13%'), '', 
      numericInput(inputId=ns('vdo.itvl'), label='Transition time (s)', value=1, min=0.1, max=Inf, step=1, width=270), '',
      numericInput(inputId=ns('vdo.res'), label='Resolution (dpi)', value=400, min=1, max=1000, step=5, width=270)
      )), # fluidRow
      textOutput(ns('tran.vdo')), htmlOutput(ns('ffm')) 
      )
      ) # navbarPage
      ) # tabPanel
      )) # append, do.call

    #  ) # list

  )
}

network_ui <- function(id) {
  ns <- NS(id)
  list(
  tabPanel(id='cluster', "Cluster", value='clus', icon=NULL,
      fluidRow(splitLayout(style='margin-top:3px;margin-bottom:3px', cellWidths=c('1%', '15%', '1%', '15%'), '', 
      dropdownButton(inputId=ns('dpbMea'), label='Similarity/Dissimilarity', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250, 
      radioButtons(inputId=ns('measure'), label="Measure", choices=c('Correlation', 'Distance'), selected='Correlation', inline=TRUE, width='100%'), 
      div(title='Only applicable when "Correlation" is selected.',
      radioButtons(inputId=ns("cor.abs"), label="Absolute correlation", choices=c('No', 'Yes'), selected='No', inline=TRUE, width='100%')
      )
      ), '',
      dropdownButton(inputId=ns('dpbThr'), label='Subset', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=400,
        div(title='Subset the most similar neighbors at a cutoff by top proportion, specific number, or similarity/dissimilairty value.',
        radioButtons(inputId=ns("thr"), label="By", choices=c('Proportion'='p', 'Number'='n', 'Similarity/Dissimilarity'='v'), selected='p', inline=TRUE, width='100%'),
        numericInput(inputId=ns('mhm.v'), label='Cutoff', value=0.2, min=-Inf, max=Inf, step=0.1, width='100%')
        )
      )
      )),
      bsTooltip(id=ns('dpbMea'), title='Measure to subset the nearest neighbors for the target gene (s) in spatial heatmaps.', placement = "top", trigger = "hover"),
      navbarPage('', id=ns('clusNav'),
      tabPanel(strong('Matrix Heatmap (MHM)'), value='mhmPlot', 
      actionButton(ns("mhm.but"), "Click to show/update", icon=icon("refresh"), style="color: #fff; background-color:#499fe9;border-color:#2e6da4"), plotlyOutput(ns("HMly"))
      ),
      tabPanel('Parameter (MHM)', value='mhmPar',
      fluidRow(splitLayout(cellWidths=c('1%', '20%'), '', 
      radioButtons(inputId=ns("mat.scale"), label="Scale by", choices=c("No", "Column", "Row"), selected='No', inline=TRUE)
      # radioButtons(inputId="mhm.but", label="Show plot:", choices=c("Yes", "No"), selected='No', inline=TRUE)
      ))
      ), # tabPanel('Plot', value='mhmPar'
    tabPanel(strong("Interactive Network (NET)"), value='netPlot', icon=NULL, 
      actionButton(ns("cpt.nw"), "Click to show/update", icon=icon("refresh"), style="color: #fff; background-color:#499fe9;border-color:#2e6da4"),
      fluidRow(splitLayout(cellWidths=c("1%", "5%", "92%", "2%"), "", plotOutput(ns("bar.net")), visNetworkOutput(ns("vis")), ""))
    ),
    tabPanel("Parameter (NET)", value='netPar', icon=NULL, 
      #fluidRow( # If column widths are not integers, columns are vertically aligned.
      fluidRow(splitLayout(style='margin-top:3px;margin-bottom:3px', cellWidths=c('0.5%', '9%', '0.5%', '10%', '0.5%', '14%', '0.5%', '15%', '0.5%', '11%'), '',
      dropdownButton(inputId=ns('dpwNetTar'), label='Target gene', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250,
        selectInput(ns("gen.sel"), "", c("None"), selected='None', width='100%')
      ), '',
      dropdownButton(inputId=ns('dpwNetType'), label='Network type', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250, 
      selectInput(inputId=ns("net.type"), label="", choices=c('signed', 'unsigned', 'signed hybrid', 'distance'), selected='signed', width='100%')
      ), '',
      dropdownButton(inputId=ns('dpwModSize'), label='Module size/splitting', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=250, 
        numericInput(ns("min.size"), "Min module size", value=15, min=15, max=5000, width='100%'),
        selectInput(ns("ds"), HTML("Module splitting sensitivity level <br/> (Larger value results in more <br/> modules with smaller sizes)"), 3:2, selected=3, width='100%')
      ), '',
      dropdownButton(inputId=ns('dpwNetAdj'), label='Adjacency/edges', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=300,
        selectInput(ns("adj.in"), HTML("Adjacency threshold <br/> (the smaller, the more edges)"), sort(seq(0, 1, 0.002), decreasing=TRUE), selected=1, width='100%'),
        numericInput(ns("max.edg"), HTML("Maximun edges <br/> (too many edges may crash the app)"), value=50, min=1, max=500, width='100%'), htmlOutput(ns("edge"))
      ), '',
      dropdownButton(inputId=ns('dpwColNet'), label='Color key', circle=FALSE, icon=NULL, status='primary', inline=FALSE, width=300,
      fluidRow(splitLayout(cellWidths=c('1%', '60%', '35%'), '',
      textInput(ns("color.net"), "Color scheme", 'yellow,orange,red', placeholder='Eg: yellow,orange,red', width='100%'),
      actionButton(ns("col.but.net"), "Confirm", icon=icon("refresh"), style='margin-top:24px'))) # fluidRow(splitLayout
      )
      )) # fluidRow(splitLayout
     )
    ) # navbarPage('', id=ns('mhmNav')
  )
  ) # list
}


deg_ui <- function(id) { 
  ns <- NS(id)
  tabPanel('Spatial Enrichment', value='deg',
    data_ui(ns("datDEG"), deg=TRUE),
    box(title='Spatial Enrichment', status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
      navbarPage(NULL, 
        tabPanel(title="Parameters",
          dataTableOutput(ns("dt.vs3")), br(),
          fluidRow(splitLayout(cellWidths=c('1%', '30%', '1%', '30%', '1%', '12%', '1%', '12%'), '',
            uiOutput(ns('ssg.sam')), '', uiOutput(ns('ssg.con')), '', 
            selectInput(ns("sam.con"), "Compare by", c('feature', 'factor', 'feature__factor'), 'feature'), '',
            uiOutput(ns('ssg.tar'))
           )),
          fluidRow(splitLayout(cellWidths=c('1%', '20%', '1%', '20%', '1%', '20%', '1%', '10%', '1%', '10%'), '',
            selectInput(ns('ssg.meth'), label='Select methods', choices=c('edgeR', 'limma', 'DESeq2', 'ROKU', 'distinct'), selected=c('edgeR', 'limma'), multiple=TRUE), '',
            selectInput(ns("edg.lim.nor"), "Normalize-edgeR/limma", c("CNF-TMM", "CNF-TMMwsp", "CNF-RLE", "CNF-upperquartile", "none"), 'CNF-TMM'), '',
            selectInput(ns("rok.dis.nor"), "Normalize-ROKU/distinct", c("CNF-TMM", "CNF-TMMwsp", "CNF-RLE", "CNF-upperquartile", "ESF", "VST", "rlog", "none"), 'CNF-TMM'), '',
            numericInput(ns('ssg.fc'), 'Log2-fold change', 1, min=0, max=Inf, step=1), '',
            numericInput(ns('ssg.fdr'), 'FDR', 0.05, min=0, max=1, step=0.01)
          )),
          actionButton(ns("ssg.update"), "Update", icon=icon("refresh"), style="color: #fff; background-color:purple;border-color: #2e6da4") 
        ), # tabPanel
        tabPanel(title="Results in plots", value='w.meth',
          dataTableOutput(ns("dt.vs1")), br(),
          strong('Over-Expressed'), br(), column(12, column(8, plotOutput(ns("upset1"))), column(4, plotOutput(ns("ovl1")))),
          strong('Under-Expressed'), br(), column(12, column(8, plotOutput(ns("upset2"))), column(4, plotOutput(ns("ovl2")))),
        ),
        tabPanel(title="Link with spatial heatmap", value='dt.deg', 
          dataTableOutput(ns("dt.vs2")), br(),
          column(12, search_ui(ns('deg')), style='z-index:5'),  
          column(12, dataTableOutput(ns("dt.deg"))),
          downloadButton(ns("dld.ssg.tab"), "Download")
        ) 
      ) # navbarPage
    ) # box(title='Spatial Enrich'
  )
}


ui <- function(request) {
  dashboardPage(
 
  # includeCSS("style.css"),
  dashboardHeader(title = NULL, titleWidth = 0),
  dashboardSidebar(collapsed = TRUE, disable = TRUE, width = 0, sidebarMenu() ),
  controlbar = dashboardControlbar(id = "right.bar", collapsed = FALSE, overlay = FALSE, width = 51),
  dashboardBody(
   # tags$head(HTML('<title>spatialHeatmap</title>')),
   useShinyjs(), 
   includeCSS("www/style.css"),
   tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
   
   tags$head(tags$script(src = "javascript.js"), tags$script(HTML(js))),
   includeScript(path = "www/javascript.js"),
   tags$script(src="javascript.js"),
   tags$head(HTML("<script type='text/javascript' src='javascript.js'></script>")),
   HTML('<head>
         <link rel="stylesheet" type="text/css" href="style.css">
         <script type="text/javascript" src="www/javascript.js"></script>
         </head>
   '),

   # Place title on the right of dashboard header.
   tags$script(HTML('
     $(document).ready(function() {
       $("header").find("nav").append(\'<span class="myClass">RiceSpatialHeatmap</span>\');
     })
   ')),
    fluidRow(
 
      # tabsetPanel(type = "pills", id=NULL, selected="primary", data_ui('dat') ),
      # tabsetPanel(type = "pills", id = 'shm.sup', selected = "shm2", shm_ui('shmAll'))
      do.call(tabsetPanel, append(list(type = "pills", id = 'shm.sup', selected="landing", upload_ui('upl'), shm_ui('shmAll', data_ui('dat'), search_ui('sear'))),
        list(tabPanel("About", value='about',
        'Coming soon!'
        ))
    ))

    )
 ) # dashboardBody
)

}
