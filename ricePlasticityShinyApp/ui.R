library(shiny); library(shinydashboard); library(yaml); library(plotly); library(visNetwork); library(DT); library(shinyWidgets)

shinyUI(dashboardPage(
  
  # includeCSS("style.css"),
  dashboardHeader(title=textOutput('spatialHeatmap')$attribs$id, titleWidth=350),

  dashboardSidebar(
  
    sidebarMenu(

      menuItem("Input", icon=icon("list"),
      menuSubItem("View", tabName="hm_net"), #br(),
      fileInput("config", "Optional: upload a config file", accept=".yaml", multiple=FALSE),
      fileInput("tar", "Optional: upload batched data/aSVGs in two tar files", accept=c(".tar"), multiple=TRUE),
      selectInput("fileIn", "Step1: data sets", c('none', 'customData', 'customComputedData'), 'none'),
      fileInput("svgInpath", "Step 2A: upload one aSVG file", accept=".svg", multiple=FALSE),
      fileInput("svgInpath1", "Step 2B: upload multiple aSVG files", accept=".svg", multiple=TRUE),
      fileInput("geneInpath", "Step 3: upload formatted data matrix", accept=c(".txt", ".csv"), multiple=FALSE),
      radioButtons(inputId='dimName', label='Step 4: is column or row gene?', choices=c("None", "Row", "Column"), selected='None', inline=TRUE),
      h4(strong("Custom computed data")),
      fileInput("adj.modInpath", "Upload the adjacency matrix and module definition file", accept=".txt", multiple=TRUE)

      ),
      menuItem("Instruction", tabName="ins", icon=icon("info")),
      menuItem("Acknowledgement", tabName="ack", icon=icon("info")),
      menuItem('Download', tabName='dld', icon=icon('download'))

     )

  ),

  dashboardBody(
 
   tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
   tags$script(src="javascript.js"),
   # tit <- textOutput('spatialHeatmap')$attribs$id,
   # tit <- paste0('<span class="myClass">', tit, '</span>'),
   # To place title on the right of dashboard header.
   tags$script(HTML('
     $(document).ready(function() {
       $("header").find("nav").append(\'<span class="myClass"> </span>\');
     })
    ')),
    tabItems(
      tabItem(tabName="hm_net", 
      box(title="Data Matrix", status="primary", solidHeader=TRUE, collapsible=TRUE, height=NULL, width=12,
      tabBox(title=NULL, id=NULL, height=NULL, width=12, selected="Data", side="right",
      tabPanel("Re-match",
      uiOutput('svg'), 
      actionButton("match", "Update", icon=icon("refresh"), style="color: #fff; background-color:purple;border-color: #2e6da4"),
      verbatimTextOutput('msg.match'), 
      fluidRow(splitLayout(cellWidths=c("1%", "49%", "49%", "1%"), "", uiOutput('sam'), uiOutput('sf'), ""))

      ),
      tabPanel("Data",
      fluidRow(column(1, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
      dropdownButton(inputId='drdn.fil', label='Filter', circle=FALSE, icon=NULL, status='primary',
      fluidRow(splitLayout(cellWidths=c('1%', '15%', '1%', '30%', '1%', '25%', '1%', '25%', '12%', '10%'), '',
      numericInput(inputId="A", label="Value (A) to exceed:", value=0), '',
      numericInput(inputId="P", label="Proportion (P) of samples with values >= A:", value=0), '',
      numericInput(inputId="CV1", label="Min coefficient of variation (CV1):", value=-10^4), '', 
      numericInput(inputId="CV2", label="Max coefficient of variation (CV2):", value=10^4), 
      actionButton(inputId='fil.but', label="Submit"), verbatimTextOutput("fil.par")
      ))
      )),
      column(1, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
      dropdownButton(inputId='drdn.scale', label='Transform', circle=FALSE, icon=NULL, status='primary', width=400,
      fluidRow(splitLayout(cellWidths=c('1%', '40%', '1%', '50%'), '',
      radioButtons(inputId='log', label='Log/exp:', choices=c("No", "log2", "exp2"), selected='No', inline=TRUE), '',
      radioButtons(inputId='scale', label='Scale by:', choices=c('No', 'Row', 'Column'), selected='No', inline=TRUE)
      )))),
      column(10, offset=0, style='padding-left:40px; padding-right:10px; padding-top:0px; padding-bottom:5px', uiOutput('col.order'))
      ),
      fluidRow(splitLayout(cellWidths=c("1%", "43%", "10%"), '', textInput(inputId='search', label='Search:', value='', placeholder='Muliple IDs must only be separated by space or comma.', width='100%'), actionButton('search.but', 'Submit'))),
      fluidRow(splitLayout(cellWidths=c("1%", "98%", "1%"), "", dataTableOutput("dt"), ""))
      )
    )),

      fluidRow(column(10), column(2, radioButtons(inputId='hide.lgd', label="Hide legend:", choices=c('Yes', 'No'), selected='No', inline=TRUE))),
      uiOutput('shm.ui'), uiOutput('lgd.ui')
      #, 
      # 'height=NULL': height is automatic.
#      box(title="Matrix Heatmap", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12, height=NULL, 
#      fluidRow(splitLayout(cellWidths=c('1%', '15%', '1%', '10%', '1%', '20%', '1%', '7%', '1%', '20%', '1%', '10%'), '', 
#      radioButtons(inputId='measure', label="Measure:", choices=c('correlation', 'distance'), selected='correlation', inline=TRUE), '', 
#      radioButtons(inputId="cor.abs", label="Cor.absolute:", choices=c('No', 'Yes'), selected='No', inline=TRUE), '', 
#      radioButtons(inputId="thr", label="Select by:", choices=c('proportion'='p', 'number'='n', 'value'='v'), selected='p', inline=TRUE), '',
#      numericInput(inputId='mhm.v', label='Cutoff: ', value=0.2, min=-Inf, max=Inf, step=NA, width=NULL), '',
#      radioButtons(inputId="mat.scale", label="Scale by:", choices=c("No", "Column", "Row"), selected='No', inline=TRUE), '',
#      # radioButtons(inputId="mhm.but", label="Show plot:", choices=c("Yes", "No"), selected='No', inline=TRUE)
#      actionButton("mhm.but", "Update", icon=icon("refresh"), style="color: #fff; background-color:purple;border-color: #2e6da4")
#      )),
#      plotlyOutput("HMly")), br(),
#      box(title="Interactive Network", status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
#      fluidRow(
#      # If column widths are not integers, columns are vertically aligned.
#      column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      textInput("color.net", "Color scheme:", 'yellow,orange,red', placeholder='Eg: yellow,orange,red', width=150), actionButton("col.but.net", "Go", icon=icon("refresh"))
#      ),
#      column(1, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      selectInput(inputId="net.type", label="Network type:", choices=c('signed', 'unsigned', 'signed hybrid', 'distance'), selected='signed', width=100)
#      ),
#      column(1, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      numericInput("min.size", "Minmum module size:", value=15, min=15, max=5000, width=150)
#      ),
#      column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      tags$span(style="color:brown;font-weight:NULL", selectInput("gen.sel", "Select a target gene:", c("None"), selected='None', width=150))
#      ),
#      column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      selectInput("ds","Module splitting sensitivity level:", 3:2, selected=3, width=170)
#      ),
#      column(1, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      selectInput("adj.in", "Adjacency threshold (the smaller, the more edges):", sort(seq(0, 1, 0.002), decreasing=TRUE), selected=1, width=200)
#      ),
#      column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      tags$span(style="color:brown", numericInput("max.edg", "Maximun edges (too many edges may crash the app):", value=50, min=1, max=500, width=200)),
#      htmlOutput("edge")
#      ),
#      column(1, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
 #     actionButton("cpt.nw", "Update", icon=icon("refresh"), style="color: #fff; background-color:purple;border-color: #2e6da4")
 #     )
 #     ),
 #     fluidRow(splitLayout(cellWidths=c("1%", "6%", "91%", "2%"), "", plotOutput("bar.net"), visNetworkOutput("vis"), ""))
#      ),

# box(title='SSG', status="primary", solidHeader=TRUE, collapsible=TRUE, width=12,
#  column(12, offset=0,
 #   column(4, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      uiOutput('ssg.sam')),
#    column(4, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      uiOutput('ssg.con')),
#    column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      selectInput("sam.con", "Compare by:", c('sample', 'condition', 'sam__con'), 'sample')),
#    column(2, offset=0, style='padding-left:10px; padding-right:0px; padding-top:0px; padding-bottom:5px',
#      selectInput('ssg.meth', label='Methods:', choices=c('edgeR', 'limma', 'DESeq2', 'ROKU', 'distinct'), selected=c('edgeR', 'limma'), multiple=TRUE))
#  ),
#  column(12, offset=0, 
 #   column(2, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
  #    selectInput("edg.lim.nor", "Normalize-edgeR/limma:", c("CNF-TMM", "CNF-TMMwsp", "CNF-RLE", "CNF-upperquartile", "none"), 'CNF-TMM')
   # ),
   # column(2, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
   #   selectInput("rok.dis.nor", "Normalize-ROKU/distinct:", c("CNF-TMM", "CNF-TMMwsp", "CNF-RLE", "CNF-upperquartile", "ESF", "VST", "rlog", "none"), 'CNF-TMM')
    #),
    #column(2, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
    #  numericInput('ssg.fc', 'Log2-fold change:', 1, min=0, max=Inf, step=1)
    #),
    #column(2, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
     # numericInput('ssg.fdr', 'FDR:', 0.05, min=0, max=1, step=0.01)
    #),
    #column(2, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
    #uiOutput('ssg.tar')),
    #column(1, offset=0, style='padding-left:10px; padding-right:10px; padding-top:0px; padding-bottom:5px',
    #actionButton("ssg.update", "Update", icon=icon("refresh"), style="color: #fff; background-color:purple;border-color: #2e6da4")
    #)
 # ),
  #tabBox(title="", width=12, id="ssg", selected='w.meth', side='right',
    # tabPanel(title="Across methods table", dataTableOutput("a.table"), value='a.table'), 
   # tabPanel(title="Table", 
    #  downloadButton("dld.ssg.tab", "Download"), dataTableOutput("w.table"), value='w.table'), 
    # tabPanel(title="Upset Plot Down", plotOutput("upset2"), value='a.meth'), 
    #tabPanel(title="Upset Plot",
     # tags$p(tags$b('Over-Expressed')), column(12, column(8, plotOutput("upset1")), column(4, plotOutput("ovl1"))),
     # tags$p(tags$b('Under-Expressed')), column(12, column(8, plotOutput("upset2")), column(4, plotOutput("ovl2"))), 
      #value='w.meth')
  #)
# )

      ),

      tabItem(tabName="ins", 
      box(title="Summary", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("sum"), width=12),
      box(title="Input", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("input"), width=12),
      box(title="Data Matrix", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("matrix"), width=12), 
      box(title="Spatial Heatmap", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("shm.ins"), width=12),
      box(title="Matrix Heatmap", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("mhm.ins"), width=12),
      box(title="Interactive Network", status="primary", solidHeader=TRUE, collapsible=TRUE, htmlOutput("net.ins"), width=12)
      ),
      tabItem(tabName="ack", htmlOutput("ack")),
      tabItem(tabName="dld",
      box(title="Download", status="primary", solidHeader=TRUE, collapsible=TRUE,
      htmlOutput("dld"), downloadButton("dld.cfg", "Download config file"), 
      downloadButton("dld.sgl", "Download single aSVG/data"), 
      downloadButton("dld.mul", "Download multiple aSVGs/data"),
      downloadButton("dld.st", "Download spatiotemporal aSVG/data"), width=12),
      ) 

      )

    )

))
