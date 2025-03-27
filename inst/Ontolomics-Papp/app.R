###Loading packages, if you can not load package, please install them first.
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(data.table)
library(ggsci)
library(ggplot2)
library(patchwork)
library(dplyr)
library(GOSemSim)
library(openxlsx)
library(plotly)
##Cancer datasets downloaded from CPTAC database
cptacdatanames<-c(#"PDC000109_Prospective.Colon.VU_Colon.Adenocarcinoma_Proteome",
                  "PDC000116_Prospective.Colon.PNNL_Colon.Adenocarcinoma_Proteome",
                  "PDC000120_Prospective.Breast.BI.Proteome_Breast.Invasive.Carcinoma_Proteome",
                  "PDC000125_CPTAC.UCEC.Discovery.Study_Uterine.Corpus.Endometrial.Carcinoma_Proteome",
                  "PDC000127_CPTAC.CCRCC.Discovery.Study_Clear.Cell.Renal.Cell.Carcinoma_Proteome",
                  "PDC000153_CPTAC.LUAD.Discovery.Study_Lung.Adenocarcinoma_Proteome",
                  "PDC000173_TCGA.Breast.Cancer_Breast.Invasive.Carcinoma_Proteome",
                  #"PDC000180_Pediatric.Brain.Cancer.Pilot.Study_Pediatric.AYA.Brain.Tumors_Proteome",
                  "PDC000198_HBV.Related.Hepatocellular.Carcinoma_Hepatocellular.Carcinoma_Proteome",
                  "PDC000219_Academia.Sinica.LUAD100_Lung.Squamous.Cell.Carcinoma_Proteome",
                  "PDC000221_CPTAC.HNSCC.Discovery.Study_Head.and.Neck.Squamous.Cell.Carcinoma_Proteome",
                  "PDC000270_CPTAC.PDA.Discovery.Study_Pancreatic.Ductal.Adenocarcinoma_Proteome",
                  "PDC000360_PTRC.HGSOC.FFPE.Discovery_Ovarian.Serous.Cystadenocarcinoma_Proteome",
                  "PDC000362_PTRC.HGSOC.Frozen.Validation_Ovarian.Serous.Cystadenocarcinoma_Proteome")
##UI for Ontolomics-P, rendering reactive HTML using the Shiny UI library.
ui<-renderUI(
  fluidPage(
    ##Adding the title of the tool
    title="Ontolomics-P",
    shinyjs::useShinyjs(),
    ##adding logo
    fluidRow(div(
      HTML(
        "<div style='text-align:center;margin-top:20px;margin-right:0px'>
          <a href='#' target=''><img src='OntolomicsPti.png' width='200px'>
          </a>
          </div>"
      )
    )),
    ## Creates a list of HTML tags to be included in the Shiny app UI
    tagList(
      # Creates a list of HTML tags to be included in the Shiny app UI.
      tags$head(
        # Specifies elements to be included in the <head> section of the HTML document.

        tags$link(
          # Includes an external CSS file for styling.
          rel = "stylesheet",
          type = "text/css",
          href = "busystyle.css"
        ),
        tags$script(
          # Includes an external JavaScript file for additional functionality.
          type = "text/javascript",
          src = "busy.js"
        ),
        tags$style(
          # Inline CSS for defining the appearance of the loading message.
          type = "text/css",
          "
      #loadmessage {
        position: fixed;
        top: 0px;
        left: 0px;
        width: 100%;
        height: 100%;
        padding: 250px 0px 5px 0px;
        text-align: center;
        font-weight: bold;
        font-size: 100px;
        color: #000000;
        background-color: #D6D9E4;
        opacity: 0.6;
        z-index: 105;
      }
      "
        ),
        tags$script(
          # JavaScript for detecting screen dimensions and resizing events. Sends updates to the Shiny server.
          '
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
      });
      '
        ),
        tags$style(
          # Inline CSS for defining the appearance of a tooltip element (ID: tooltip).
          type = "text/css",
          "
      #tooltip {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        ),
        tags$style(
          # Inline CSS for another tooltip element (ID: tooltip2).
          type = "text/css",
          "
      #tooltip2 {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        ),
        tags$style(
          # Inline CSS for a tooltip element (ID: tooltip3).
          type = "text/css",
          "
      #tooltip3 {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        ),
        tags$style(
          # Inline CSS for a tooltip element (ID: tooltip4).
          type = "text/css",
          "
      #tooltip4 {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        ),
        tags$style(
          # Inline CSS for a tooltip element (ID: tooltip5).
          type = "text/css",
          "
      #tooltip5 {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        ),
        tags$style(
          # Inline CSS for a tooltip element (ID: tooltip6).
          type = "text/css",
          "
      #tooltip6 {
        position: absolute;
        border: 1px solid #333;
        background: #fff;
        padding: 1px;
        color: #333;
        display: block;
        width: 300px;
        z-index: 5;
      }
      "
        )
      )
    ),
    ## Defining a panel that is conditionally displayed based on the specified JavaScript condition
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      ##Welcome page
      tabPanel(
        "Welcome",
        uiOutput("welcomeui"),
        icon = icon("home")
      ),
      ##Page for Step 1: Import Data
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h3(
              "Step 1: Import Data",
              tags$span(
                id = 'span1',
                `data-toggle` = "tooltip",
                title = '
                In this part, users can type in a UniProt ID/gene name/noun or upload their own proteome expression matrix. The example data were obtained in the "" part.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            prettyRadioButtons(
              inputId = "loadseqdatatype",
              label = "",
              thick = TRUE,
              choices = list("Type in a keyword" = 1,"Upload data"=2),
              animation = "pulse",
              status = "info",
              selected = 1,
              inline = TRUE
            ),
            hr(),
            conditionalPanel(
              condition = "input.loadseqdatatype==1",
              textInput("proinputids",h5("1. Please type in a gene name/UniProt ID/noun:"),value="",width = NULL, placeholder = 'e.g. P04217, A1BG or liver')
            ),
            conditionalPanel(
              condition = "input.loadseqdatatype==2",
              radioButtons("uploadpaste",label="",choices = list("A. Upload" = 1,"B. Paste"=2,
                                                                 "C. Load example data"=3),
                           selected = 1,inline = TRUE),
              conditionalPanel(
                condition = "input.uploadpaste==1",
                radioButtons(
                  "metabopathfileType_Input",
                  label = h5("1. File format:"),
                  choices = list(".xlsx"=1,".xls"=2,".csv/txt"=3),
                  selected = 1,
                  inline = TRUE
                ),
                fileInput('metabopathfile1', h5('1.1. Import your data:'),
                          accept=c('text/csv','text/plain','.xlsx','.xls')),
                checkboxInput('metabopathheader', '1.2. Header?', TRUE),
                checkboxInput('metabopathfirstcol', '1.3. First column?', FALSE),
                conditionalPanel(condition = "input.metabopathfileType_Input==1",
                                 numericInput("metabopathxlsxindex",h5("1.4. Sheet index:"),value = 1)),
                conditionalPanel(condition = "input.metabopathfileType_Input==2",
                                 numericInput("metabopathxlsxindex",h5("1.4. Sheet index:"),value = 1)),
                conditionalPanel(condition = "input.metabopathfileType_Input==3",
                                 radioButtons('metabopathsep', h5('1.4. Separator:'),
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t',
                                                BlankSpace=' '),
                                              ','))
              ),
              conditionalPanel(
                condition = "input.uploadpaste==2",
                textAreaInput("metabopath_zhantie",label = h5("1. Paste your data here:"),value="",height ="100px")
              )
            ),
            conditionalPanel(
              condition = "input.loadseqdatatype==3",
              downloadButton("loadseqdatadownload1","1. Download example data from Rat",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
            ),
            tags$hr(style="border-color: grey;"),
            selectInput("origidatatype",h5("2. Data type:"),choices = c("UniProt ID","Gene Name","Noun")),
            bsTooltip("origidatatype",'Here means that what users type in or upload above. "UniProt ID" means users type in a UniProt ID or upload a table with UniProt IDs. "Gene Name" means users type in a gene name or upload a table with gene names. "Noun" means users type in a noun which they want to check, such as liver.',
                      placement = "right",options = list(container = "body")),
            selectInput("wuzhongid",h5("3. Species:"),choices = c("9606-Human","10090-Mouse","10116-Rat"))#,
            #bsTooltip("origidatatype",'.',placement = "right",options = list(container = "body"))
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("Data view:"),
            dataTableOutput("seqrawdata")
          )
        ),
        icon = icon("upload")
      ),
      ##Here shows Ontology-guide Results
      tabPanel(
        "Ontology-guide Results",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 2: Ontology-guide Results",
              tags$span(
                id = 'span2',
                `data-toggle` = "tooltip2",
                title = '
                This step aligns shows the "Ontology-guide" results.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loadseqdatatype==1",
              numericInput("wordnumber",h5("1. Minimum word number:"),value = 3),
              bsTooltip("wordnumber",'Those words below this number will not be shown in the cloud plot.',
                        placement = "right",options = list(container = "body")),
              numericInput("cloudsize",h5("2. Cloud area size:"),value = 18),
              bsTooltip("cloudsize",'The area size of cloud plot.',
                        placement = "right",options = list(container = "body"))
            ),
            conditionalPanel(
              condition = "input.loadseqdatatype==2",
              numericInput("padjustval",h5("1. Adjusted P value:"),value = 1),
              bsTooltip("padjustval",'The threshold of BH-adjusted P value.',
                        placement = "right",options = list(container = "body")),
              numericInput("simplegonum",h5("2. GO ID number based on GeneRatio:"),value = 100),
              bsTooltip("simplegonum",'The number of GO IDs with top GeneRatio value from enrichment analysis results, for example, 100 here means this tool uses the GO IDs with the top 100 GeneRatio values to process next simplifyGO function.',
                        placement = "right",options = list(container = "body")),
              numericInput("barplotnum",h5("3. Object number for barplot:"),value = 20),
              bsTooltip("barplotnum",'The number of objects (e.g. GO terms or topics) are shown in the barplot, for example, 20 here means the top 20 objects based on the GeneRatio values are shown in the barplot.',
                        placement = "right",options = list(container = "body"))
            ),
            tags$hr(style="border-color: grey;"),
            actionButton("btn_results","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            conditionalPanel(
              condition = "input.loadseqdatatype==1",
              radioButtons(
                "resultsxuanze",
                label = h4(""),
                choices = list("A. Original GO functions" = 1,"B. Topics analysis results"=2),
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey;"),
              hidden(
                div(
                  id="resultsxuanze_btn",
                  conditionalPanel(
                    condition = "input.resultsxuanze==1",
                    h4("A.1. Original GO function plots based on the keyword that users type in:"),
                    downloadButton("originalgoplotdl","Download"),
                    plotOutput("originalgoplot",height="800"),
                    h4("A.2. Original GO function table based on the keyword that users type in:"),
                    downloadButton("originalgotabledl","Download"),
                    dataTableOutput("originalgotable"),
                    h4("A.3. Word frequency table of the word clouds in A.1. above:"),
                    downloadButton("wordfreqtabledl","Download"),
                    dataTableOutput("wordfreqtable")
                  ),
                  conditionalPanel(
                    condition = "input.resultsxuanze==2",
                    h4("B.1. The correlation plots between the original GOs and those in each topic:"),
                    downloadButton("topicgoplotdl","Download"),
                    plotOutput("topicgoplot"),
                    h4("B.2. The correlation table between the original GOs and those in each topic:"),
                    downloadButton("topicgotabledl","Download"),
                    dataTableOutput("topicgotable")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.loadseqdatatype==2",
              radioButtons(
                "resuploadxuanze",
                label = h4(""),
                choices = list("A. Classical GO enrichment analysis" = 1,"B. Topics enrichment analysis"=2),
                selected = 1,
                inline = TRUE
              ),
              tags$hr(style="border-color: grey;"),
              hidden(
                div(
                  id="resuploadxuanze_btn",
                  conditionalPanel(
                    condition = "input.resuploadxuanze==1",
                    h4("A.1. Classical GO enrichment plots based on the IDs/Names that users upload:"),
                    downloadButton("originaluploadgoplotdl","Download"),
                    plotOutput("originaluploadgoplot",height="800"),
                    h4("A.2. Classical GO enrichment table based on the IDs/Names that users upload:"),
                    downloadButton("originaluploadgotabledl","Download"),
                    dataTableOutput("originaluploadgotable")
                  ),
                  conditionalPanel(
                    condition = "input.resuploadxuanze==2",
                    h4("B.1. The topics enrichment analysis plots:"),
                    downloadButton("topicuploadgoplotdl","Download"),
                    plotOutput("topicuploadgoplot",height="800"),
                    h4("B.2. The topics enrichment analysis table:"),
                    downloadButton("topicuploadgotabledl","Download"),
                    dataTableOutput("topicuploadgotable")
                  )
                )
              )
            )
          )
        ),
        icon = icon("table")
      ),
      ##Here shows Data-driven Results
      tabPanel(
        "Data-driven Results",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 3: Data-driven Results",
              tags$span(
                id = 'span3',
                `data-toggle` = "tooltip3",
                title = '
                This step shows the "Data-driven" results. Please check https://proteomic.datacommons.cancer.gov/pdc/explore-quantitation-data for the detailed introduction about the quantitation data in each CPTAC proteome database.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            tags$hr(style="border-color: grey;"),
            checkboxInput("datalogif","1. Log or not?",TRUE),
            selectInput("dataplottype",h5("2. Plot type:"),choices = c("Boxplot","Heatmap")),
            checkboxInput("datazscoreif","3. Z-score or not?",TRUE),
            textInput("volcanocol",h5("4. Color for volcano plot:"),value = c("lightslateblue;grey;tomato")),
            #selectInput("studynames",h5("4. Study names:"),multiple = T,
            #            choices = c("PDC000109_Prospective.Colon.VU_Colon.Adenocarcinoma_Proteome"=1,
            #                        "PDC000116_Prospective.Colon.PNNL_Colon.Adenocarcinoma_Proteome"=2,
            #                        "PDC000120_Prospective.Breast.BI.Proteome_Breast.Invasive.Carcinoma_Proteome"=3,
            #                        "PDC000125_CPTAC.UCEC.Discovery.Study_Uterine.Corpus.Endometrial.Carcinoma_Proteome"=4,
            #                        "PDC000127_CPTAC.CCRCC.Discovery.Study_Clear.Cell.Renal.Cell.Carcinoma_Proteome"=5,
            #                        "PDC000153_CPTAC.LUAD.Discovery.Study_Lung.Adenocarcinoma_Proteome"=6,
            #                        "PDC000173_TCGA.Breast.Cancer_Breast.Invasive.Carcinoma_Proteome"=7,
            #                        "PDC000180_Pediatric.Brain.Cancer.Pilot.Study_Pediatric.AYA.Brain.Tumors_Proteome"=8,
            #                        "PDC000198_HBV.Related.Hepatocellular.Carcinoma_Hepatocellular.Carcinoma_Proteome"=9,
            #                        "PDC000219_Academia.Sinica.LUAD100_Lung.Squamous.Cell.Carcinoma_Proteome"=10,
            #                        "PDC000221_CPTAC.HNSCC.Discovery.Study_Head.and.Neck.Squamous.Cell.Carcinoma_Proteome"=11,
            #                        "PDC000270_CPTAC.PDA.Discovery.Study_Pancreatic.Ductal.Adenocarcinoma_Proteome"=12,
            #                        "PDC000360_PTRC.HGSOC.FFPE.Discovery_Ovarian.Serous.Cystadenocarcinoma_Proteome"=13,
            #                        "PDC000362_PTRC.HGSOC.Frozen.Validation_Ovarian.Serous.Cystadenocarcinoma_Proteome"=14)),
            tags$hr(style="border-color: grey;"),
            actionButton("btn_dataresults","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            selectInput("studynames",h5("5. Study names:"),multiple = T,
                        choices = cptacdatanames,width="700px"),
            tags$hr(style="border-color: grey;"),
            radioButtons(
              "resultsxuanze2",
              label = NULL,
              choices = list("A. Figure results" = 1,"B. Table results"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            hidden(
              div(
                id="resultsxuanze2_btn",
                conditionalPanel(
                  condition = "input.resultsxuanze2==1",
                  h4("A.1. The protein(s) that users type in or upload in the Step 1 is/are displayed in a boxplot or heatmap:"),
                  downloadButton("boxheatplotdl","Download"),
                  plotOutput("boxheatplot",height="800"),
                  h4("A.2. The protein(s) that users type in or upload in the Step 1 is/are displayed in the volcano plot:"),
                  downloadButton("volcanoplotdl","Download"),
                  plotlyOutput("volcanoplot",height="800"),
                  h4("A.3. The gene co-expression network analysis based on expression profiles:"),
                  downloadButton("conetworkplotdl","Download"),
                  plotOutput("conetworkplot",height="800")
                ),
                conditionalPanel(
                  condition = "input.resultsxuanze2==2",
                  h4("The expression data of the protein(s) that users type in or upload in the Step 1:"),
                  downloadButton("volcanoplotdatadl","Download"),
                  numericInput("vpdataindex",h5("Which result table users want to show below:"),value = 1),
                  dataTableOutput("volcanoplotdata")
                )
              )
            )
          )
        ),
        icon = icon("table")
      ),
      ##Here shows user manual
      tabPanel(
        "Help",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 4: User Manual",
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                Here shows the detailed step-by-step operations about this software.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            tags$hr(style="border-color: grey;"),
            radioButtons(
              "databasexz",
              label = NULL,
              choices = list("I. Type in a keyword" = 1,"II. Upload data"=2,
                "III. Topics analysis database" = 3,"IV. GO terms used for topics analysis"=4),
              selected = 1,
              inline = F
            )
          ),
          mainPanel(
            width = 9,
            conditionalPanel(
              condition = "input.databasexz==1",
              div(style="text-align:left;margin-top:15px;font-size:150%;",HTML("<b>I.1 Input data preparation: Type in a keyword</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("In the 'Import Data' part (Step 1), users could type in a gene name (e.g. TNFSF10), or a UniProt ID (e.g. P50591), or a noun (e.g. Liver). For instance, when users input 'TNFSF10', which is a gene name, the parameters should be configured as follows:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP1.png',height=500))),
              div(style="text-align:left;margin-top:15px;font-size:150%;",HTML("<b>I.2 Results</b>")),
              div(style="text-align:left;margin-top:15px;font-size:130%;",HTML("<b>I.2.1 Ontology-guide Results</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("Herein, Ontolomics-P built a topics database based on the GO database (please refer to the 'III. Topics analysis database' part), aiding in the retrieval of pertinent protein/gene functional information. It outputs two kinds of results:")
              ),
              div(style="text-align:left;margin-top:15px;font-size:120%;",HTML("<b>I.2.1.A Original GO functions</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("'Original GO functions' means that this tool retrieves the GO functions of the input keyword (e.g. TNFSF10) and plots word clouds of BPs, MFs and CCs respectively. The results include: <br><u>A.1.</u> Original GO function plots based on the keyword that users type in: Here will show the distribution of the number, word clouds of BPs, MFs and CCs for TNFSF10 respectively. <br><u>A.2.</u> Original GO function table based on the keyword that users type in: Here will show the GO functions of TNFSF10 derived from GO database.<br><u>A.3.</u> Word frequency table of the word clouds in A.1. above: Here shows the word frequency for the world clouds (BP, CC, MF) in figure A.1. above.<br>The results are shown as below:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP2.png',height=1500))),
              div(style="text-align:left;margin-top:15px;font-size:120%;",HTML("<b>I.2.1.B Topics analysis results</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("'Topics analysis results' means that this tool processes topics analysis based on the pre-established topics database (please refer to the 'III. Topics analysis database' part). The results include: <br><u>B.1.</u> The correlation plots between the original GOs and those in each topic: This will show the distribution of the semantic similarity between the keyword’s (e.g. TNFSF10) GO functions and each topic derived from BPs, MFs and CCs, respectively. Then it also exhibits the histogram of the 125 semantic similarity values. <br><u>B.2.</u> The correlation table between the original GOs and those in each topic: This will show the result table of the semantic similarity between the keyword’s (e.g. TNFSF10) GO functions and each topic derived from BPs, MFs and CCs, respectively. The semantic similarity values are used to make the figures above ( The “B.1. The correlation plots between the original GOs and those in each topic” part). <br>The results are shown as below:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP3.png',height=1400))),
              div(style="text-align:left;margin-top:15px;font-size:130%;",HTML("<b>I.2.2 Data-driven Results</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("Herein, Ontolomics-P seamlessly integrates quantitative proteomic data from ten cancer types stored in the CPTAC database. These expression data were downloaded from https://proteomic.datacommons.cancer.gov/pdc/explore-quantitation-data with PDC study identifiers COAD (PDC000116), BRCA (PDC000120), UCEC (PDC000125), ccRCC (PDC000127), LUAD (PDC000153), HECA (PDC000198), HNSCC (PDC000221), PADA (PDC000270), LSCC (PDC000219) and OV (PDC000360). This integration facilitates the efficient review of the expression profiles of proteins of interest (POIs) for researchers. For example, users type in a gene name TNFSF10, the results include: <br><u>A.1.</u> The protein(s) that users type in or upload in the Step 1 is/are displayed in a boxplot or heatmap. As users only type in one gene name here, it shows boxplots for this protein across all selected cancer data.<br><u>A.2.</u> A.2. The protein(s) that users type in or upload in the Step 1 is/are displayed in the volcano plot. As users only type in one gene name here, it shows volcano plots for this protein across all selected cancer data.<br><u>A.3.</u> The gene co-expression network analysis based on expression profiles. This gene co-expression network analysis reveals relationships between the input protein and other strongly correlated proteins (based on absolute Spearman correlation coefficients) across selected cancer datasets.")
              ),
              div(style="text-align:center;margin-top:8px;margin-bottom:20px;",a(href='#',img(src='OP4.png',height=1600)))
            ),
            conditionalPanel(
              condition = "input.databasexz==2",
              div(style="text-align:left;margin-top:15px;font-size:150%;",HTML("<b>II.1 Input data preparation: Upload data</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("In the 'Import Data' part (Step 1), users could also upload/paste a list of POIs (not just a keyword). For instance, when users click the 'C. Load example data', which is a list of protein UniProt IDs, the parameters should be configured as follows:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP5.png',height=450))),
              div(style="text-align:left;margin-top:15px;font-size:150%;",HTML("<b>II.2 Results</b>")),
              div(style="text-align:left;margin-top:15px;font-size:130%;",HTML("<b>II.2.1 Ontology-guide Results</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("Herein, Ontolomics-P processes classical GO enrichment analysis based on original GO database and Topics enrichment analysis based on the pre-built topics database. Thus it outputs two kinds of results:")
              ),
              div(style="text-align:left;margin-top:15px;font-size:120%;",HTML("<b>II.2.1.A Classical GO enrichment analysis</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("'Classical GO enrichment analysis' means that this tool retains classical GO enrichment analysis and semantic similarity analysis to simply the results. The results include: <br><u>A.1.</u> Classical GO enrichment plots based on the IDs/Names that users upload: Here will show the boxplot of the top 20 classical GO enrichment terms and the simplified results of BPs, MFs and CCs of uploaded POIs using heatmaps with word clouds respectively. <br><u>A.2.</u> Classical GO enrichment table based on the IDs/Names that users upload: Here will show the classical GO enrichment result table. <br>The results are shown as below:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP6.png',height=1400))),
              div(style="text-align:left;margin-top:15px;font-size:120%;",HTML("<b>II.2.1.B Topics enrichment analysis</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("'Topics enrichment analysis' means that this tool processes topics enrichment analysis using Fisher's exact test method based on the pre-established topics database (please refer to the 'III. Topics analysis database' part). The results include: <br><u>B.1.</u> The topics enrichment analysis plots: Here will show the meteoric plots based on the topics enrichment analysis results of BPs, MFs and CCs of uploaded POIs respectively. Users could adjust the left '3. Object number for barplot' parameter to control the topics number of the plot. <br><u>B.2.</u> The topics enrichment analysis table: Here will show the topics enrichment analysis result table of BPs, MFs and CCs of uploaded POIs. The meteoric plots above are made based on this table. <br>The results are shown as below:")
              ),
              div(style="text-align:center;margin-top:8px;",a(href='#',img(src='OP7.png',height=1400))),
              div(style="text-align:left;margin-top:15px;font-size:130%;",HTML("<b>II.2.2 Data-driven Results</b>")),
              div(
                style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
                HTML("Herein, Ontolomics-P seamlessly integrates quantitative proteomic data from ten cancer types stored in the CPTAC database. These expression data were downloaded from https://proteomic.datacommons.cancer.gov/pdc/explore-quantitation-data with PDC study identifiers COAD (PDC000116), BRCA (PDC000120), UCEC (PDC000125), ccRCC (PDC000127), LUAD (PDC000153), HECA (PDC000198), HNSCC (PDC000221), PADA (PDC000270), LSCC (PDC000219) and OV (PDC000360). This integration facilitates the efficient review of the expression profiles of proteins of interest (POIs) for researchers. For example, users upload a list of protein UniProt IDs in the 'Import Data' part (Step 1), the results include: <br><u>A.1.</u> The protein(s) that users type in or upload in the Step 1 is/are displayed in a boxplot or heatmap. As users upload a list of proteins here, it shows heatmaps for uploaded proteins across all selected cancer datasets.<br><u>A.2.</u> A.2. The protein(s) that users type in or upload in the Step 1 is/are displayed in the volcano plot. As users a list of proteins here, it shows volcano plots for all uploaded proteins across all selected cancer data.<br><u>A.3.</u> The gene co-expression network analysis based on expression profiles. This gene co-expression network analysis reveals relationships between the uploaded proteins and other strongly correlated proteins (based on absolute Spearman correlation coefficients) across selected cancer datasets.")
              ),
              div(style="text-align:center;margin-top:8px;margin-bottom:20px;",a(href='#',img(src='OP8.png',height=1600)))
            ),
            conditionalPanel(
              condition = "input.databasexz==3",
              h4("III. Topics analysis database: This tool built a topics database based on the GO database using the Latent Dirichlet allocation (LDA) model. Every topic was re-annotated using the GPT-4o language model. Users can check the database from here:"),
              downloadButton("databasedata3dl","Download"),
              dataTableOutput("databasedata3")
            ),
            conditionalPanel(
              condition = "input.databasexz==4",
              h4("IV. GO terms used for topics analysis: This database was implemented using GO.db package (Version 3.17.0, https://doi.org/doi:10.18129/B9.bioc.GO.db). Database shown as below:"),
              downloadButton("databasedata4dl","Download"),
              dataTableOutput("databasedata4")
            )
          )
        ),
        icon = icon("file-alt")
      )
    )
  )
)
##Defining the server-side logic of the Shiny application.
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  ##UI for Welcome page
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    #screenheight<-input$dimension[2]
    #tryCatch({},error=function(e) NULL)
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-150
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-200
      }
      else{
        imgwidth<-250
      }
    }
    ##Here shows the main contents in the welcom page
    fluidRow(
      div(
        id="mainbody",
        column(3),
        column(
          6,
          div(style="text-align:left;margin-top:20px;font-size:140%;color:darkred",
              HTML("~~ <em>Dear Users, Welcome to Ontolomics-P</em> ~~")),
          div(style="text-align:center;margin-top: 20px",
              a(href='#',
                img(src='OntolomicsP.png',height=imgwidth))),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:15px",
              HTML("<b>Ontolomics-P</b> is a web-based tool, which possesses the core functions, including:")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;1. Topic analysis integration. Leveraging natural language processing, this feature allows researchers to identify and categorize various topics within a collection of documents. It builds a topics database based on the Gene Ontology (GO) database, aiding in the retrieval of pertinent protein/gene functional information;")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;2. Topics enrichment analysis implementation. Utilizing Fisher's exact test method, Ontolomics-P conducts topics enrichment analysis to identify significant associations and enrichments within the dataset;")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;3. Inclusion of classical GO enrichment analysis and semantic similarity analysis. To enhance practicality, Ontolomics-P retains classical GO enrichment analysis and semantic similarity analysis, offering diverse analytical approaches;")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;4. Integration of quantitative proteomic data from diverse cancer types. Ontolomics-P seamlessly integrates quantitative proteomic data from ten cancer types stored in the CPTAC database. These include colon adenocarcinoma (COAD), breast cancer (BRCA), uterine corpus endometrial carcinoma (UCEC), clear cell renal cell carcinoma (ccRCC), lung adenocarcinoma (LUAD), hepatocellular carcinoma (HECA), head and neck squamous cell carcinoma (HNSCC), pancreatic ductal adenocarcinoma (PADA), lung squamous cell carcinoma (LSCC), and ovarian cancer (OV).")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:15px",
              #HTML("<br />"),
              HTML("In addition, this tool supports both online access and local installation. The source codes and installation instructions can be available in the GitHub repository: <a href='https://github.com/wangshisheng/Ontolomics-P' target='_blank'>https://github.com/wangshisheng/Ontolomics-P</a> under an MIT license.")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:15px;font-size:120%",
              #HTML("<br />"),
              HTML("Finally, Ontolomics-P is developed by <a href='https://shiny.rstudio.com/' target='_blank'>R shiny (Version 1.6.0)</a>, and is free and open to all users with no login requirement. It can be readily accessed by all popular web browsers including Google Chrome, Mozilla Firefox, Safari and Internet Explorer 10 (or later), and so on. We would highly appreciate that if you could send your feedback about any bug or feature request to Shisheng Wang at <u>shishengwang@wchscu.cn</u>.")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:15px;font-size:120%",
              #HTML("<br />"),
              HTML("<em>Friendly suggestions</em>:")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:110%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;a) Open Ontolomics-P with Chrome, Mozilla Firefox, Safari or Firefox;")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:110%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;b) The minimum operating system specifications are: RAM 4GB, Hard drive 100 GB;")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:110%;margin-top:5px",
              #HTML("<br />"),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;c) The monitor resolution (>= 1920x1080) is better.")),
          div(style="text-align:center;margin-top:20px;font-size:140%;color:darkgreen",
              HTML("<br />"),
              HTML("^_^ <em>Enjoy yourself in Ontolomics-P</em> ^_^")),
          tags$hr(style="border-color: grey60;"),
          div(style="text-align:center;margin-top: 20px;font-size:100%",
              HTML(" &copy; 2024 <a href='http://english.cd120.com/' target='_blank'>Hao Yang's Group</a>. All Rights Reserved.")),
          div(style="text-align:center;margin-bottom: 20px;font-size:100%",
              HTML("&nbsp;&nbsp; Created by Shisheng Wang. E-mail: <u>shishengwang@wchscu.cn</u>."))
        ),
        column(3)
      )
    )
  })
  ##Here shows species
  output$metabopathspecies<-renderUI({
    metabopath_spedf1<-read.csv("metabopath-species.csv",header = T,stringsAsFactors = F)
    metabopath_spedf<-metabopath_spedf1[-2,]
    metabopath_spedf_paste<-paste(metabopath_spedf$Organism.ID,metabopath_spedf$Organism,sep = "-")
    selectizeInput('metabopathspeciesselect', h5('Species:'), choices =metabopath_spedf_paste,options = list(maxOptions = 6000))
  })
  output$metabopathspecies2<-renderUI({
    metabopath_spedfx<-read.csv("metabopath-species.csv",header = T,stringsAsFactors = F)
    metabopath_spedf<-metabopath_spedfx[c(2,1,3:nrow(metabopath_spedfx)),]
    metabopath_spedf_paste2<-paste(metabopath_spedf$Organism.ID,metabopath_spedf$Organism,sep = "-")
    selectizeInput('metabopathspeciesselect2', h5('Species:'), choices =metabopath_spedf_paste2,
                   options = list(maxOptions = 6000))#selected=metabopath_spedf_paste2,
  })
  ##Here shows example data
  exampledataout<-reactive({
    if(input$origidatatype=="MaxQuant"){
      dataread<-read.csv("MaxQuant_Exampledata.csv",stringsAsFactors = F)
    }
    else if(input$origidatatype=="Spectronaut"){
      dataread<-read.csv("Spectronaut_Exampledata.csv",stringsAsFactors = F)
    }
    else{
      dataread<-read.csv("Normal_Exampledata.csv",stringsAsFactors = F)
    }
    dataread
  })
  output$loadseqdatadownload1<-downloadHandler(
    filename = function(){paste("Example_SequenceData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampledataout(),file,row.names = FALSE)
    }
  )
  seqrawdataoutxx<-reactive({
    if(input$uploadpaste==1){
      files <- input$metabopathfile1
      if(is.null(files)){
        dataread<-data.frame(Description="Ontolomics-P detects that you did not upload your data. Please upload the data, or load the example data to check first.")
        dataread
      }else{
        if (input$metabopathfileType_Input == "1"){
          dataread<-read.xlsx(files$datapath,rowNames=input$metabopathfirstcol,
                              colNames = input$metabopathheader,sheet = input$metabopathxlsxindex)
        }
        else if(input$metabopathfileType_Input == "2"){
          if(sum(input$metabopathfirstcol)==1){
            rownametfmetabopath<-1
          }else{
            rownametfmetabopath<-NULL
          }
          dataread<-read.xls(files$datapath,sheet = input$metabopathxlsxindex,header=input$metabopathheader,
                             row.names = rownametfmetabopath, sep=input$metabopathsep,stringsAsFactors = F)
        }
        else{
          if(sum(input$metabopathfirstcol)==1){
            rownametfmetabopath<-1
          }else{
            rownametfmetabopath<-NULL
          }
          dataread<-read.csv(files$datapath,header=input$metabopathheader,
                             row.names = rownametfmetabopath,sep=input$metabopathsep,stringsAsFactors = F)
        }
      }
    }else if(input$uploadpaste==2){
      zhantieidstr<-strsplit(input$metabopath_zhantie,"\n")
      dataread<-data.frame(Paste.Data=zhantieidstr[[1]])
    }else{
      dataread<-read.csv("ExampleUniProtIDs.csv")
    }
    dataread
  })
  ##Here shows user's input
  output$seqrawdata<-renderDataTable({
    if(input$loadseqdatatype==1){
      if(input$proinputids==""){
        dataread<-data.frame(Type_in=paste0("Ontolomics-P detects that you type in nothing!"))
      }else{
        dataread<-data.frame(Type_in=paste0("Ontolomics-P detects that you type in: ",input$proinputids))#input$origidatatype,": ",
      }
    }else{
      dataread<-seqrawdataoutxx()
    }
    datatable(dataread, options = list(pageLength = 10))
  })
  ##Searching for user's input
  originalgotableout<-reactive({
    speciesid<<-strsplit(input$wuzhongid,"-")[[1]][1]
    if(input$loadseqdatatype==1){
      proinputidsx<<-tolower(input$proinputids)
      if(proinputidsx==""){
        dataread<-data.frame(Results="Nothing here, please type in a keyword in Step 1!")
      }else{
        withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
          for(i in 1:2){
            if(i==1){
              incProgress(1/2, detail = "Loading database...")
              load(file = "database/GOTERMdf.rdata")
              #load(file = paste0("database/UNIPROTids_",speciesid,".rdata"))
              load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
              load(file = paste0("database/UNIPROTids2_",speciesid,".rdata"))
            }
            if(i==2){
              incProgress(2/2, detail = "Calculating results...")
              #UNIPROTidsdf<<-cbind(UNIPROTidsdf1[,c(2,3,1)],UNIPROTidsdf2)
              proinputidsx1<-c(which(tolower(UNIPROTidsdf1[[2]])==proinputidsx),
                               which(tolower(UNIPROTidsdf1[[3]])==proinputidsx),
                               grep(paste0("\\b",proinputidsx,"\\b"),UNIPROTidsdf2[[2]],
                                    ignore.case = T,perl = T))
              if(length(proinputidsx1)>0){
                #UNIPROTidsdf1<-UNIPROTidsdf[unique(proinputidsx1),]
                #dataread<-base::merge(UNIPROTidsdf1,GOTERMdf,by.x="GOALL",by.y="GOID",sort = F)
                dataread1<-UNIPROTidsdf1[unique(proinputidsx1),]
                dataread2<-UNIPROTidsdf2[unique(proinputidsx1),]
                dataread<-cbind(dataread1,dataread2)
                colnames(dataread)[1:3]<-c("GO.IDs","UniProt.IDs","Symbol")
              }else{
                dataread<-data.frame(Results="Nothing is found here, please try another keyword!")
              }
            }
          }
        })
      }
    }else{
      dataread<-seqrawdataoutxx()
    }
    dataread
  })
  ##Calculating semantic similarity between two GO terms lists
  topicgotableout<-reactive({
    speciesid<<-strsplit(input$wuzhongid,"-")[[1]][1]#input$wuzhongid 9606-Human
    load(file = paste0("database/Topic_All_top20_","9606",".rdata"))#speciesid
    load(file = paste0("database/ForSS_",speciesid,".rdata"))
    gotabledf<<-originalgotableout()
    withProgress(message = 'Calculating Semantic Similarity', style = "notification", detail = "for BP functions.", value = 0,{
      for(i in 1:3){
        if(i==1){
          incProgress(1/3, detail = "for BP functions.")
          gotabledf1<-apply(Topic_Allx[Topic_Allx$Ontology=="BP",],1,function(x){
            x1<-strsplit(x[5],"_")[[1]]
            mgoSim(unique(gotabledf$GO.IDs), x1, semData=SSGO_BP, measure="Wang", combine="BMA")
          })
          #incProgress(1/3, detail = "for BP functions.")
        }
        if(i==2){
          incProgress(2/3, detail = "for MF functions.")
          gotabledf2<-apply(Topic_Allx[Topic_Allx$Ontology=="MF",],1,function(x){
            x1<-strsplit(x[5],"_")[[1]]
            mgoSim(unique(gotabledf$GO.IDs), x1, semData=SSGO_MF, measure="Wang", combine="BMA")
          })
        }
        if(i==3){
          incProgress(3/3, detail = "for CC functions.")
          gotabledf3<-apply(Topic_Allx[Topic_Allx$Ontology=="CC",],1,function(x){
            x1<-strsplit(x[5],"_")[[1]]
            mgoSim(unique(gotabledf$GO.IDs), x1, semData=SSGO_CC, measure="Wang", combine="BMA")
          })
        }
      }
    })
    SSdata<-data.frame(Ontology=Topic_Allx$Ontology,Topics=Topic_Allx$Terms,Semantic.Similarity=round(c(gotabledf1,gotabledf2,gotabledf3)/max(c(gotabledf1,gotabledf2,gotabledf3)),4),
                       Description=Topic_Allx$Description)
    SSdata<-SSdata[order(SSdata$Semantic.Similarity,decreasing = T),]
    SSdata
  })
  ##Classical GO enrichment analysis
  originaluploadgotableout<-reactive({
    library(clusterProfiler)
    library(simplifyEnrichment)
    speciesid<<-strsplit(input$wuzhongid,"-")[[1]][1]#input$wuzhongid 9606-Human
    withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
      for(i in 1:2){
        if(i==1){
          incProgress(1/2, detail = "Loading database...")
          load(file = "database/GOTERMdf.rdata")
          load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
          load(file = paste0("database/UNIPROTids2_",speciesid,".rdata"))
        }
        if(i==2){
          incProgress(2/2, detail = "Processing enrichment analysis...")
          uploaddata<<-seqrawdataoutxx()
          if(ncol(uploaddata)==1){
            uploaddatax<-uploaddata[[1]]
          }else{
            uploaddatax<-rownames(uploaddata)
          }
          if(input$origidatatype=="UniProt ID"){
            tabdata2<-UNIPROTidsdf1[,c(1,2)]
          }else{
            tabdata2<-UNIPROTidsdf1[,c(1,3)]
          }
          tabdata2<<-tabdata2
          yy<-enricher(gene=unique(uploaddatax), TERM2GENE=tabdata2,minGSSize=1, maxGSSize = 50000,
                       pvalueCutoff = 1, qvalueCutoff = 1)
          yydf<-yy@result
          yydf1<-base::merge(yydf,GOTERMdf,by.x = "ID", by.y = "GOID",sort-FALSE)
          yydf2<-yydf1[,c("ID","Ontology","Term","Count","GeneRatio","BgRatio","pvalue","p.adjust",
                          "Definition","geneID")]#[,c(1,12,10,9,3:6,11,8)]
          #yydf2$BgRatio<-as.numeric(unlist(lapply(yydf2$BgRatio,function(x)strsplit(x,"\\/")[[1]][1])))
          #colnames(yydf2)<-c("GO.IDs","Ontology","Term","Description","Counts","Annotated","pvalue","p.adjust","IDs")
          colnames(yydf2)[c(1,10)]<-c("GO.IDs","Uploaded.IDs")
          #yydf2<-yydf2[order(yydf2$Count,decreasing = T),]
          GeneRatiox<-unlist(lapply(yydf2$GeneRatio,function(x){
            x1<-strsplit(x,"\\/")[[1]]
            as.numeric(x1[1])/as.numeric(x1[2])
          }))
          yydf2$pvalue<-round(yydf2$pvalue,digits = 4)
          yydf2$p.adjust<-round(yydf2$p.adjust,digits = 4)
          yydf2<-yydf2[-c(which(yydf2$Term=="molecular_function"),
                          which(yydf2$Term=="cellular_component"),
                          which(yydf2$Term=="biological_process")),]
          yydf2<-yydf2[order(GeneRatiox,decreasing = T),]
          rownames(yydf2)<-1:nrow(yydf2)
          allRes.df<-yydf2[yydf2$p.adjust<=as.numeric(input$padjustval),]
        }
      }
    })
    allRes.df
  })
  ##Topics enrichment analysis
  topicuploadgotableout<-reactive({
    library(clusterProfiler)
    library(simplifyEnrichment)
    speciesid<<-strsplit(input$wuzhongid,"-")[[1]][1]#input$wuzhongid 9606-Human
    withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
      for(i in 1:2){
        if(i==1){
          incProgress(1/2, detail = "Loading database...")
          load(file = "database/GOTERMdf.rdata")
          load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
          load(file = paste0("database/UNIPROTids2_",speciesid,".rdata"))
          load(file = paste0("database/Topic_All_top20_","9606",".rdata"))#speciesid
          load(file = paste0("database/ForSS_",speciesid,".rdata"))
        }
        if(i==2){
          incProgress(2/2, detail = "Processing enrichment analysis...")
          uploaddata<<-seqrawdataoutxx()
          if(ncol(uploaddata)==1){
            uploaddatax<-uploaddata[[1]]
          }else{
            uploaddatax<-uploaddata[[1]]#rownames(uploaddata)
          }
          Topic_Allx1<-tidyr::separate_rows(Topic_Allx, GOIDs, sep ="_")
          Topic_Allx2<-base::merge(Topic_Allx1,UNIPROTidsdf1,by.x="GOIDs",by.y="GOALL",sort = F)
          if(input$origidatatype=="UniProt ID"){
            tabdata2<-unique(Topic_Allx2[,c(4,6)])
          }else{
            tabdata2<-unique(Topic_Allx2[,c(4,7)])
          }
          tabdata2<<-tabdata2
          yytp<-enricher(gene=unique(uploaddatax), TERM2GENE=tabdata2,minGSSize=1, maxGSSize = 500000,
                       pvalueCutoff = 1, qvalueCutoff = 1)
          yytpdf<-yytp@result
          #yytpdf1<-base::merge(yytpdf,GOTERMdf,by.x = "ID", by.y = "GOID",sort-FALSE)
          yytpdf2<-yytpdf[,c("ID","Count","GeneRatio","BgRatio","pvalue","p.adjust","geneID")]#[,c(2,9,3:6,8)]
          colnames(yytpdf2)[c(1,7)]<-c("Topics","Uploaded.IDs")
          yytpdf2<-base::merge(yytpdf2,Topic_Allx,by.x = "Topics",by.y = "Terms",sort = F)
          yytpdf2<-yytpdf2[,c(1,8,2:7)]
          GeneRatiotpx<-unlist(lapply(yytpdf2$GeneRatio,function(x){
            x1<-strsplit(x,"\\/")[[1]]
            as.numeric(x1[1])/as.numeric(x1[2])
          }))
          yytpdf2$pvalue<-round(yytpdf2$pvalue,digits = 4)
          yytpdf2$p.adjust<-round(yytpdf2$p.adjust,digits = 4)
          yytpdf2<-yytpdf2[order(GeneRatiotpx,decreasing = T),]
          rownames(yytpdf2)<-1:nrow(yytpdf2)
          allRes.tpdf<-yytpdf2#[yytpdf2$p.adjust<=as.numeric(input$padjustval),]
        }
      }
    })
    allRes.tpdf
  })
  ##Output results (tables and plots) of classical GO enrichment analysis or topics enrichment analysis
  observeEvent(
    input$btn_results,{
      if(input$loadseqdatatype==1){
        shinyjs::show(id = "resultsxuanze_btn", anim = FALSE)
      }else{
        shinyjs::show(id = "resuploadxuanze_btn", anim = FALSE)
      }
      output$originalgotable<-renderDataTable({
        dataread<-originalgotableout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$originalgotabledl<-downloadHandler(
        filename = function(){paste("Original.GO.table.",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(originalgotableout(),file,row.names=FALSE)
        }
      )
      output$originalgoplot<-renderPlot({
        library(tidytext)
        library(ggwordcloud)
        colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10)))
        gotabledf<<-originalgotableout()
        if(ncol(gotabledf)==1){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          cloudsizex<<-input$cloudsize
          wordnumberx<<-input$wordnumber
          proinputidsxx<<-input$proinputids
          gotabledf<-unique(gotabledf[,c(1,5,6)])
          gotabledf1<-data.frame(Ontology=names(table(gotabledf$Ontology)),
                                 Number=as.numeric(table(gotabledf$Ontology)))
          gotabledf1$Ontology<-factor(gotabledf1$Ontology,levels = c("BP","MF","CC"))
          p1<-ggplot(gotabledf1, aes(x=Ontology, y=Number,fill=Ontology))+
            geom_bar(stat="identity")+
            geom_text(aes(label=Number), vjust=-1,size = 4) +
            scale_fill_manual(values = colpalettes[1:3])+
            labs(title=paste0("Keyword: ",proinputidsxx))+
            theme_classic()
          gotabledf2<-gotabledf[gotabledf$Ontology=="BP",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf2$word <- gsub('[[:punct:]]+', '', gotabledf2$word)
          gotabledf2 <- gotabledf2 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens1 <- gotabledf2 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens1<-tokens1[tokens1$n>=wordnumberx,]
          set.seed(123)
          p2<-ggplot(
            tokens1,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens1), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",proinputidsxx," based on BP functions"))+
            theme_minimal()
          gotabledf3<-gotabledf[gotabledf$Ontology=="CC",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf3$word <- gsub('[[:punct:]]+', '', gotabledf3$word)
          gotabledf3 <- gotabledf3 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens3 <- gotabledf3 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens3<-tokens3[tokens3$n>=wordnumberx,]
          set.seed(123)
          p3<-ggplot(
            tokens3,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens3), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",proinputidsxx," based on CC functions"))+
            theme_minimal()
          gotabledf4<-gotabledf[gotabledf$Ontology=="MF",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf4$word <- gsub('[[:punct:]]+', '', gotabledf4$word)
          gotabledf4 <- gotabledf4 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens4 <- gotabledf4 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens4<-tokens4[tokens4$n>=wordnumberx,]
          set.seed(123)
          p4<-ggplot(
            tokens4,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens4), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",proinputidsxx," based on MF functions"))+
            theme_minimal()
          withProgress(message = 'Plotting...', style = "notification", detail = "", value = 0,{
            incProgress(1/1, detail = "Plotting for original GO functions...")
            p1+p2+p3+p4+plot_layout(ncol=2)
          })
        }
      })
      originalgoplotout<-reactive({
        colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10)))
        gotabledf<<-originalgotableout()
        if(ncol(gotabledf)==1){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          cloudsizex<<-input$cloudsize
          wordnumberx<<-input$wordnumber
          gotabledf<-unique(gotabledf[,c(1,5,6)])
          gotabledf1<-data.frame(Ontology=names(table(gotabledf$Ontology)),
                                 Number=as.numeric(table(gotabledf$Ontology)))
          gotabledf1$Ontology<-factor(gotabledf1$Ontology,levels = c("BP","MF","CC"))
          p1<-ggplot(gotabledf1, aes(x=Ontology, y=Number,fill=Ontology)) +
            geom_bar(stat="identity")+
            geom_text(aes(label=Number), vjust=-1,size = 4) +
            scale_fill_manual(values = colpalettes[1:3])+
            labs(title=paste0("Keyword: ",input$proinputids))+
            theme_classic()
          gotabledf2<-gotabledf[gotabledf$Ontology=="BP",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf2$word <- gsub('[[:punct:]]+', '', gotabledf2$word)
          gotabledf2 <- gotabledf2 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens1 <- gotabledf2 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens1<-tokens1[tokens1$n>=wordnumberx,]
          set.seed(123)
          p2<-ggplot(
            tokens1,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens1), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",input$proinputids," based on BP functions"))+
            theme_minimal()
          gotabledf3<-gotabledf[gotabledf$Ontology=="CC",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf3$word <- gsub('[[:punct:]]+', '', gotabledf3$word)
          gotabledf3 <- gotabledf3 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens3 <- gotabledf3 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens3<-tokens3[tokens3$n>=wordnumberx,]
          set.seed(123)
          p3<-ggplot(
            tokens3,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens3), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",input$proinputids," based on CC functions"))+
            theme_minimal()
          gotabledf4<-gotabledf[gotabledf$Ontology=="MF",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf4$word <- gsub('[[:punct:]]+', '', gotabledf4$word)
          gotabledf4 <- gotabledf4 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens4 <- gotabledf4 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens4<-tokens4[tokens4$n>=wordnumberx,]
          set.seed(123)
          p4<-ggplot(
            tokens4,
            aes(
              label = word, size = n,
              color = factor(sample.int(10, nrow(tokens4), replace = TRUE))
            )
          ) +
            geom_text_wordcloud_area() +
            scale_size_area(max_size = cloudsizex) +
            labs(title=paste0("Cloud plot for ",input$proinputids," based on MF functions"))+
            theme_minimal()
          p1+p2+p3+p4+plot_layout(ncol=2)
        }
      })
      output$originalgoplotdl<-downloadHandler(
        filename = function(){paste("Original.GO.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 12,height = 12)
          print(originalgoplotout())
          dev.off()
        }
      )
      ##Word cloud table
      wordfreqtableout<-reactive({
        library(tidytext)
        library(ggwordcloud)
        gotabledf<<-originalgotableout()
        if(ncol(gotabledf)==1){
          worldcloudtb<-data.frame(Results="Nothing here!")
        }else{
          cloudsizex<<-input$cloudsize
          wordnumberx<<-input$wordnumber
          proinputidsxx<<-input$proinputids
          gotabledf<-unique(gotabledf[,c(1,5,6)])
          gotabledf2<-gotabledf[gotabledf$Ontology=="BP",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf2$word <- gsub('[[:punct:]]+', '', gotabledf2$word)
          gotabledf2 <- gotabledf2 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens1 <- gotabledf2 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens1$Ontology<-"BP"
          #tokens1<-tokens1[tokens1$n>=wordnumberx,]
          gotabledf3<-gotabledf[gotabledf$Ontology=="CC",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf3$word <- gsub('[[:punct:]]+', '', gotabledf3$word)
          gotabledf3 <- gotabledf3 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens3 <- gotabledf3 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens3$Ontology<-"CC"
          #tokens3<-tokens3[tokens3$n>=wordnumberx,]
          gotabledf4<-gotabledf[gotabledf$Ontology=="MF",2,drop=F]%>%tidytext::unnest_tokens(word, Definition)
          gotabledf4$word <- gsub('[[:punct:]]+', '', gotabledf4$word)
          gotabledf4 <- gotabledf4 %>% filter(!(nchar(word) == 1))%>%
            anti_join(stop_words)
          tokens4 <- gotabledf4 %>% filter(!(word==""))%>%count(word, sort = TRUE)
          tokens4$Ontology<-"MF"
          #tokens4<-tokens4[tokens4$n>=wordnumberx,]
          worldcloudtb<-rbind(tokens1,tokens3,tokens4)
        }
        worldcloudtb
      })
      output$wordfreqtable<-renderDataTable({
        dataread<-wordfreqtableout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$wordfreqtabledl<-downloadHandler(
        filename = function(){paste("WordCloud.table.",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(wordfreqtableout(),file,row.names=FALSE)
        }
      )
      ##Output topics analysis results
      output$topicgotable<-renderDataTable({
        dataread<-topicgotableout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$topicgotabledl<-downloadHandler(
        filename = function(){paste("TopicsAnalysis.table.",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(topicgotableout(),file,row.names=FALSE)
        }
      )
      ##Showing Semantic Similarity histgram
      output$topicgoplot<-renderPlot({
        topicgodf1<-topicgodf<<-topicgotableout()
        topicgodf1$Topics<-factor(topicgodf$Topics,levels = topicgodf$Topics)
        topicgodf1$Ontology<-factor(topicgodf$Ontology,levels = c("BP","MF","CC"))
        p1<-ggplot(topicgodf1,aes(Topics,Semantic.Similarity))+
          geom_bar(aes(fill=Semantic.Similarity),stat="identity")+
          facet_wrap(~Ontology, scales="free", nrow=1)+
          scale_fill_gradient(low = "#5B9CD6",high = "#E86629")+
          ylab("Semantic Similarity")+#Spearman Pearson
          #coord_cartesian(ylim=c(0,1))+
          scale_y_continuous(expand = c(0, 0))+
          theme_classic()+
          theme(axis.text.x=element_blank(),axis.ticks=element_blank())
        SS_median=median(topicgodf$Semantic.Similarity, na.rm=T)
        topicgodf1$Groups<-ifelse(topicgodf$Semantic.Similarity>=SS_median,"High","Low")
        p2<-ggplot(topicgodf1, aes(x=Semantic.Similarity))+
          geom_histogram(aes(fill=Groups),color="white",binwidth = 0.02)+
          geom_density(aes(x=Semantic.Similarity,y = 0.024*..count..),linewidth=0.8,adjust = 1,inherit.aes=F)+
          #labs(title = "Spearman Correlation")+
          xlab("Semantic Similarity")+
          ylab("Number")+
          theme_bw()+
          theme(panel.grid.minor=element_blank() ,panel.grid.major = element_blank())+
          scale_fill_manual(values=c("#E86629","#5B9CD6"))+
          #coord_cartesian(xlim=c(-1,1))+
          #geom_vline(xintercept = 0, colour="red")+
          #geom_vline(xintercept =  correlation_mean)+
          #annotate("text", x= correlation_mean, y=150, label=paste("Mean = ", round(correlation_mean,3), sep=""), colour="grey")+
          geom_vline(xintercept=SS_median,colour="orange",lwd=1.2,linetype=2)+
          scale_x_continuous(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))#+
        p2<-p2+
          annotate("text",size=5,x= SS_median+0.04, y=layer_scales(p2)$y$range$range[2]*0.95,
                   label=paste("Median = ", round(SS_median,3), sep=""), colour="black")#+
          #annotate("text",size=5,x=-0.5,y=590,label=paste(round(length(which(corr$corr_coeff>=0))/dim(corr)[1]*100, 2), "% Positive correlation"), colour="black")+
          #annotate("text",size=5,x=-0.5,y=570,label=paste(round(length(which(corr$corr_coeff<0))/dim(corr)[1]*100, 2), "% Negative correlation"), colour="black")
        withProgress(message = 'Plotting', style = "notification", detail = "for topics analysis...", value = 0,{
          incProgress(1/1, detail = "Plotting for topics analysis...")
          p1+p2+plot_layout(widths = c(2, 1))
        })
      })
      topicgoplotout<-reactive({
        topicgodf1<-topicgodf<<-topicgotableout()
        topicgodf1$Topics<-factor(topicgodf$Topics,levels = topicgodf$Topics)
        topicgodf1$Ontology<-factor(topicgodf$Ontology,levels = c("BP","MF","CC"))
        p1<-ggplot(topicgodf1,aes(Topics,Semantic.Similarity))+
          geom_bar(aes(fill=Semantic.Similarity),stat="identity")+
          facet_wrap(~Ontology, scales="free", nrow=1)+
          scale_fill_gradient(low = "#5B9CD6",high = "#E86629")+
          ylab("Semantic Similarity")+#Spearman Pearson
          #coord_cartesian(ylim=c(0,1))+
          scale_y_continuous(expand = c(0, 0))+
          theme_classic()+
          theme(axis.text.x=element_blank(),axis.ticks=element_blank())
        SS_median=median(topicgodf$Semantic.Similarity, na.rm=T)
        topicgodf1$Groups<-ifelse(topicgodf$Semantic.Similarity>=SS_median,"High","Low")
        p2<-ggplot(topicgodf1, aes(x=Semantic.Similarity))+
          geom_histogram(aes(fill=Groups),color="white",binwidth = 0.02)+
          geom_density(aes(x=Semantic.Similarity,y = 0.024*..count..),linewidth=0.8,adjust = 1,inherit.aes=F)+
          #labs(title = "Spearman Correlation")+
          xlab("Semantic Similarity")+
          ylab("Number")+
          theme_bw()+
          theme(panel.grid.minor=element_blank() ,panel.grid.major = element_blank())+
          scale_fill_manual(values=c("#E86629","#5B9CD6"))+
          #coord_cartesian(xlim=c(-1,1))+
          #geom_vline(xintercept = 0, colour="red")+
          #geom_vline(xintercept =  correlation_mean)+
          #annotate("text", x= correlation_mean, y=150, label=paste("Mean = ", round(correlation_mean,3), sep=""), colour="grey")+
          geom_vline(xintercept=SS_median,colour="orange",lwd=1.2,linetype=2)+
          scale_x_continuous(expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0))#+
        p2<-p2+
          annotate("text",size=5,x= SS_median+0.04, y=layer_scales(p2)$y$range$range[2]*0.95,
                   label=paste("Median = ", round(SS_median,3), sep=""), colour="black")#+
        #annotate("text",size=5,x=-0.5,y=590,label=paste(round(length(which(corr$corr_coeff>=0))/dim(corr)[1]*100, 2), "% Positive correlation"), colour="black")+
        #annotate("text",size=5,x=-0.5,y=570,label=paste(round(length(which(corr$corr_coeff<0))/dim(corr)[1]*100, 2), "% Negative correlation"), colour="black")
        p1+p2+plot_layout(widths = c(2, 1))
      })
      output$topicgoplotdl<-downloadHandler(
        filename = function(){paste("TopicsAnalysis.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 16,height = 8)
          print(topicgoplotout())
          dev.off()
        }
      )
      ##classical go enrichment
      output$originaluploadgotable<-renderDataTable({
        dataread<-originaluploadgotableout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$originaluploadgotabledl<-downloadHandler(
        filename = function(){paste("Classical.GO.Enrichment.table.",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(originaluploadgotableout(),file,row.names=FALSE)
        }
      )
      ##classical GO similarity analysis
      output$originaluploadgoplot<-renderPlot({
        library(cowplot)
        library(grid)
        load(file = paste0("database/Topic_All_top20_","9606",".rdata"))#speciesid
        load(file = paste0("database/ForSS_",speciesid,".rdata"))
        originaluploadgodf1<-originaluploadgodf<<-originaluploadgotableout()
        originaluploadgodf1$GeneRatio<-unlist(lapply(originaluploadgodf$GeneRatio,function(x){
          x1<-strsplit(x,"\\/")[[1]]
          as.numeric(x1[1])/as.numeric(x1[2])
        }))
        originaluploadgodf1<-originaluploadgodf1[order(originaluploadgodf1$GeneRatio,decreasing = T),]
        simplegonumx<<-input$simplegonum
        p1<-ggplot(originaluploadgodf1[1:input$barplotnum,], aes(x = reorder(Term,GeneRatio), y = GeneRatio,fill=p.adjust))+
          geom_bar(stat = "identity")+
          coord_flip()+scale_fill_gradient(low="blue", high="red")+
          theme_bw()+
          xlab("Term")+
          ylab("GeneRatio")+
          theme(text = element_text(size = 13))
        w = convertWidth(unit(1, "npc")*(9/10), "inch", valueOnly = TRUE)
        h = convertHeight(unit(1, "npc")*(4/5), "inch", valueOnly = TRUE)
        originaluploadgodf2<-originaluploadgodf1%>%#arrange(GO.IDs, desc(GeneRatio))%>%
          group_by(Ontology) %>%
          top_n(simplegonumx, GeneRatio)
        #matgo = GO_similarity(originaluploadgodf1[[1]])
        withProgress(message = 'Calculating Semantic Similarity', style = "notification", detail = "for BP functions.", value = 0,{
          for(i in 1:3){
            if(i==1){
              incProgress(1/3, detail = "for BP functions.")
              matgo_BP1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="BP"],
                                originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="BP"],
                                semData=SSGO_BP, measure="Rel", combine=NULL)
              matgo_BP1[is.na(matgo_BP1)]<-0
              cl_BP = cluster_terms(matgo_BP1,method = "kmeans",control = list(max_k=6))
              p2 = grid.grabExpr(ht_clusters(matgo_BP1, cl_BP, word_cloud_grob_param = list(max_width = 80),
                          order_by_size = TRUE,stat="count",min_stat=2,
                          column_title = "GO terms clustered by BP"), width = w, height = h)
            }
            if(i==2){
              incProgress(2/3, detail = "for MF functions.")
              matgo_MF1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="MF"],
                                originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="MF"],
                                semData=SSGO_MF, measure="Rel", combine=NULL)
              matgo_MF1[is.na(matgo_MF1)]<-0
              cl_MF = cluster_terms(matgo_MF1,method = "kmeans",control = list(max_k=6))#binary_cut(matgo_MF1)
              p3 = grid.grabExpr(ht_clusters(matgo_MF1, cl_MF, word_cloud_grob_param = list(max_width = 80),
                          order_by_size = TRUE,stat="count",min_stat=2,
                          column_title = "GO terms clustered by MF"), width = w, height = h)
            }
            if(i==3){
              incProgress(3/3, detail = "for CC functions.")
              matgo_CC1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="CC"],
                                originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="CC"],
                                semData=SSGO_CC, measure="Rel", combine=NULL)
              matgo_CC1[is.na(matgo_CC1)]<-0
              cl_CC = cluster_terms(matgo_CC1,method = "kmeans",control = list(max_k=6))#binary_cut(matgo_CC1)
              p4 = grid.grabExpr(ht_clusters(matgo_CC1, cl_CC, word_cloud_grob_param = list(max_width = 80),
                          order_by_size = TRUE,stat="count",min_stat=2,
                          column_title = "GO terms clustered by CC"), width = w, height = h)
            }
          }
        })
        plot_grid(p1,p2,p3,p4,nrow = 2)
      })
      originaluploadgoplotout<-reactive({
        load(file = paste0("database/Topic_All_top20_","9606",".rdata"))#speciesid
        load(file = paste0("database/ForSS_",speciesid,".rdata"))
        originaluploadgodf1<-originaluploadgodf<<-originaluploadgotableout()
        originaluploadgodf1$GeneRatio<-unlist(lapply(originaluploadgodf$GeneRatio,function(x){
          x1<-strsplit(x,"\\/")[[1]]
          as.numeric(x1[1])/as.numeric(x1[2])
        }))
        originaluploadgodf1<-originaluploadgodf1[order(originaluploadgodf1$GeneRatio,decreasing = T),]
        simplegonumx<<-input$simplegonum
        p1<-ggplot(originaluploadgodf1[1:input$barplotnum,], aes(x = reorder(Term,GeneRatio), y = GeneRatio,fill=p.adjust))+
          geom_bar(stat = "identity")+
          coord_flip()+scale_fill_gradient(low="blue", high="red")+
          theme_bw()+
          xlab("Term")+
          ylab("GeneRatio")+
          theme(text = element_text(size = 13))
        w = convertWidth(unit(1, "npc")*(9/10), "inch", valueOnly = TRUE)
        h = convertHeight(unit(1, "npc")*(4/5), "inch", valueOnly = TRUE)
        originaluploadgodf2<-originaluploadgodf1%>%#arrange(GO.IDs, desc(GeneRatio))%>%
          group_by(Ontology) %>%
          top_n(simplegonumx, GeneRatio)
        matgo_BP1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="BP"],
                          originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="BP"],
                          semData=SSGO_BP, measure="Rel", combine=NULL)
        matgo_BP1[is.na(matgo_BP1)]<-0
        cl_BP = cluster_terms(matgo_BP1,method = "kmeans",control = list(max_k=6))
        p2 = grid.grabExpr(ht_clusters(matgo_BP1, cl_BP, word_cloud_grob_param = list(max_width = 80),
                                       order_by_size = TRUE,stat="count",min_stat=2,
                                       column_title = "GO terms clustered by BP"), width = w, height = h)
        matgo_MF1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="MF"],
                          originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="MF"],
                          semData=SSGO_MF, measure="Rel", combine=NULL)
        matgo_MF1[is.na(matgo_MF1)]<-0
        cl_MF = cluster_terms(matgo_MF1,method = "kmeans",control = list(max_k=6))#binary_cut(matgo_MF1)
        p3 = grid.grabExpr(ht_clusters(matgo_MF1, cl_MF, word_cloud_grob_param = list(max_width = 80),
                                       order_by_size = TRUE,stat="count",min_stat=2,
                                       column_title = "GO terms clustered by MF"), width = w, height = h)
        matgo_CC1<-mgoSim(originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="CC"],
                          originaluploadgodf2[[1]][originaluploadgodf2$Ontology=="CC"],
                          semData=SSGO_CC, measure="Rel", combine=NULL)
        matgo_CC1[is.na(matgo_CC1)]<-0
        cl_CC = cluster_terms(matgo_CC1,method = "kmeans",control = list(max_k=6))#binary_cut(matgo_CC1)
        p4 = grid.grabExpr(ht_clusters(matgo_CC1, cl_CC, word_cloud_grob_param = list(max_width = 80),
                                       order_by_size = TRUE,stat="count",min_stat=2,
                                       column_title = "GO terms clustered by CC"), width = w, height = h)
        plot_grid(p1,p2,p3,p4,nrow = 2)
      })
      output$originaluploadgoplotdl<-downloadHandler(
        filename = function(){paste("Classical.GO.Enrichment.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 16,height = 14)
          print(originaluploadgoplotout())
          dev.off()
        }
      )
      ##
      output$topicuploadgotable<-renderDataTable({
        dataread<-topicuploadgotableout()
        datatable(dataread, options = list(pageLength = 10))
      })
      output$topicuploadgotabledl<-downloadHandler(
        filename = function(){paste("Topics.Enrichment.table.",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(topicuploadgotableout(),file,row.names=FALSE)
        }
      )
      ##The topics enrichment analysis plots
      output$topicuploadgoplot<-renderPlot({
        topicuploadgodf1<-topicuploadgodf<<-topicuploadgotableout()
        topicuploadgodf1$GeneRatio<-unlist(lapply(topicuploadgodf$GeneRatio,function(x){
          x1<-strsplit(x,"\\/")[[1]]
          as.numeric(x1[1])/as.numeric(x1[2])
        }))
        topicuploadgodf1<-topicuploadgodf1[order(topicuploadgodf1$p.adjust,decreasing = F),]
        #simplegonumx<<-input$simplegonum
        topicuploadgodf1<-rbind(topicuploadgodf1[topicuploadgodf1$Ontology=="BP",][1:input$barplotnum,],
                                topicuploadgodf1[topicuploadgodf1$Ontology=="MF",][1:input$barplotnum,],
                                topicuploadgodf1[topicuploadgodf1$Ontology=="CC",][1:input$barplotnum,])
        topicuploadgodf1<-topicuploadgodf1[order(topicuploadgodf1$GeneRatio,decreasing = T),]
        topicuploadgodf1$Ontology<-factor(topicuploadgodf1$Ontology,levels = c("BP","MF","CC"))
        topicuploadgodf1$index<-1:nrow(topicuploadgodf1)
        #[1:input$barplotnum,]
        #p1<-ggplot(topicuploadgodf1,aes(x = reorder(Topics,GeneRatio),y=GeneRatio,fill=p.adjust))+
        #  geom_bar(stat = "identity")+
        #  #facet_wrap(~Ontology, scales="free", nrow=1)+
        #  facet_grid(Ontology~., scales="free")+
        #  coord_flip()+scale_fill_gradient(low="blue", high="red")+
        #  theme_bw()+
        #  xlab("Topics")+
        #  ylab("GeneRatio")+
        #  theme(text = element_text(size = 13))
        p1<-ggplot() +
          ggforce::geom_link(data = topicuploadgodf1, aes(x = 0, y = reorder(Topics,GeneRatio),
                                                          xend =GeneRatio,yend=Topics,
                                                          size = after_stat(index),
                                                          alpha = after_stat(index),
                                                          color = p.adjust),show.legend = F) +
          geom_point(data = topicuploadgodf1, aes(x = GeneRatio, y = Topics, color = p.adjust), size = 5, shape = 21, fill = "white")+
          facet_grid(Ontology~., scales="free")+
          scale_color_gradient(low="blue", high="red")+#scale_size(range = c(1, 3))+
          xlab("GeneRatio")+
          ylab("Topics")+
          theme_test()
        p1
      })
      topicuploadgoplotout<-reactive({
        topicuploadgodf1<-topicuploadgodf<<-topicuploadgotableout()
        topicuploadgodf1$GeneRatio<-unlist(lapply(topicuploadgodf$GeneRatio,function(x){
          x1<-strsplit(x,"\\/")[[1]]
          as.numeric(x1[1])/as.numeric(x1[2])
        }))
        topicuploadgodf1<-topicuploadgodf1[order(topicuploadgodf1$p.adjust,decreasing = F),]
        #simplegonumx<<-input$simplegonum
        topicuploadgodf1<-rbind(topicuploadgodf1[topicuploadgodf1$Ontology=="BP",][1:input$barplotnum,],
                                topicuploadgodf1[topicuploadgodf1$Ontology=="MF",][1:input$barplotnum,],
                                topicuploadgodf1[topicuploadgodf1$Ontology=="CC",][1:input$barplotnum,])
        topicuploadgodf1$Ontology<-factor(topicuploadgodf1$Ontology,levels = c("BP","MF","CC"))
        #[1:input$barplotnum,]
        p1<-ggplot() +
          ggforce::geom_link(data = topicuploadgodf1, aes(x = 0, y = reorder(Topics,GeneRatio),
                                                          xend =GeneRatio,yend=Topics,
                                                          size = after_stat(index),
                                                          alpha = after_stat(index),
                                                          color = p.adjust),show.legend = F) +
          geom_point(data = topicuploadgodf1, aes(x = GeneRatio, y = Topics, color = p.adjust), size = 5, shape = 21, fill = "white")+
          facet_grid(Ontology~., scales="free")+
          scale_color_gradient(low="blue", high="red")+
          xlab("GeneRatio")+
          ylab("Topics")+
          theme_test()
        p1
      })
      output$topicuploadgoplotdl<-downloadHandler(
        filename = function(){paste("Topics.Enrichment.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 18,height = 11)
          print(topicuploadgoplotout())
          dev.off()
        }
      )

    }
  )
  #
  #observeEvent(
  #  input$resuploadxuanze_btn,{
  #
  #  }
  #)
  ##Loading Protein/Gene Name database
  datadrivendfout<-reactive({
    speciesid<-"9606"
    if(input$loadseqdatatype==1){
      proinputidsx<<-tolower(input$proinputids)
      if(proinputidsx==""){
        dataread<-NULL#data.frame(Results="Nothing here, please type in a keyword in Step 1!")
      }else{
        withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
          for(i in 1:2){
            if(i==1){
              incProgress(1/2, detail = "Loading database...")
              #load(file = "database/GOTERMdf.rdata")
              #load(file = paste0("database/UNIPROTids_",speciesid,".rdata"))
              load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
              UNIPROTidsdf1<<-UNIPROTidsdf1
              #load(file = paste0("database/UNIPROTids2_",speciesid,".rdata"))
            }
            if(i==2){
              incProgress(2/2, detail = "Calculating results...")
              #UNIPROTidsdf<<-cbind(UNIPROTidsdf1[,c(2,3,1)],UNIPROTidsdf2)
              if(input$origidatatype=="UniProt ID"){
                proinputidsx1<-which(tolower(UNIPROTidsdf1[[2]])==proinputidsx)
              }else{
                proinputidsx1<-which(tolower(UNIPROTidsdf1[[3]])==proinputidsx)
              }
              if(length(proinputidsx1)>0){
                #UNIPROTidsdf1<-UNIPROTidsdf[unique(proinputidsx1),]
                #dataread<-base::merge(UNIPROTidsdf1,GOTERMdf,by.x="GOALL",by.y="GOID",sort = F)
                dataread1<-unique(UNIPROTidsdf1[unique(proinputidsx1),-1])
                #dataread2<-UNIPROTidsdf2[unique(proinputidsx1),]
                dataread<-dataread1#cbind(dataread1,dataread2)
                colnames(dataread)<-c("UniProt.IDs","Symbol")
              }else{
                dataread<-NULL#data.frame(Results="Nothing is found here, please try another keyword!")
              }
            }
          }
        })
      }
    }else{
      uploaddata1<-uploaddata<-seqrawdataoutxx()
      uploaddata1[[1]]<-tolower(uploaddata[[1]])
      withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
        for(i in 1:2){
          if(i==1){
            incProgress(1/2, detail = "Loading database...")
            load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
            UNIPROTidsdf1<<-UNIPROTidsdf1
          }
          if(i==2){
            incProgress(2/2, detail = "Calculating results...")
            if(input$origidatatype=="UniProt ID"){
              proinputidsx1<-which(tolower(UNIPROTidsdf1[[2]])%in%uploaddata1[[1]])
            }else{
              proinputidsx1<-which(tolower(UNIPROTidsdf1[[3]])%in%uploaddata1[[1]])
            }
            if(length(proinputidsx1)>0){
              #UNIPROTidsdf1<-UNIPROTidsdf[unique(proinputidsx1),]
              #dataread<-base::merge(UNIPROTidsdf1,GOTERMdf,by.x="GOALL",by.y="GOID",sort = F)
              dataread1<-unique(UNIPROTidsdf1[unique(proinputidsx1),-1])
              #dataread2<-UNIPROTidsdf2[unique(proinputidsx1),]
              dataread<-dataread1#cbind(dataread1,dataread2)
              colnames(dataread)<-c("UniProt.IDs","Symbol")
            }else{
              dataread<-NULL#data.frame(Results="Nothing is found here, please try another keyword!")
            }
          }
        }
      })
    }
    dataread
  })
  observeEvent(
    input$btn_dataresults,{
      shinyjs::show(id = "resultsxuanze2_btn", anim = FALSE)
      ##Boxplot/Heatmap for input proteins/gene name based on CPTAC database
      output$boxheatplot<-renderPlot({
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            library(impute)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              colnames(cptacmatrix_All)<-make.unique(colnames(cptacmatrix_All))
              cptacsamples_All[[1]]<-make.unique(cptacsamples_All[[1]])
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                if(input$datazscoreif){
                  datadrivendf2<-scale(t(datadrivendf1))#as.data.frame(scale(t(datadrivendf1)))
                }else{
                  datadrivendf2<-t(datadrivendf1)
                }
                datadrivendf3<-reshape2::melt(datadrivendf2)
                #colnames(datadrivendf3)[3]<-"Value"
                if(length(unique(cptacsamples_All[[2]]))==1){
                  gr<-c("Solid Tissue Normal","Primary Tumor")
                }else{
                  gr<-unique(cptacsamples_All[[2]])
                }
                colx<-c("#7EA2CA","#C67877")
                names(colx)<-gr
                datadrivendf3$Groups<-factor(cptacsamples_All[[2]],levels = gr)
                datadrivendf3$StudyName<-studynamesxi
                #datadrivendf3$GNames<-paste(datadrivendf3$Var2,datadrivendf3$Groups,sep = "_")
                if(nrow(datadrivendf1)<=10){
                  if(input$dataplottype=="Boxplot"){
                    #library(plyr)
                    #source("vioplotsplit.R")
                    #dens_at_mean<<-dens_at_mean
                    #geom_split_violin<<-geom_split_violin
                    boxplotcol<-c('#7EA2CA', '#9C88C5',"#CF87CE","#A5C585","#5292A4","#7F7298","#C67877",
                                  "#869079","#3F698C","#3F86C6")
                    #datadrivendf4 <- ddply(datadrivendf3, .(GNames), dens_at_mean)
                    #vioplot2geban_grp.mean<-datadrivendf4
                    #jianshu<-rep(1:ncol(datadrivendf2),rep(2,ncol(datadrivendf2)))
                    #if(nrow(vioplot2geban_grp.mean)>1){
                    #  vioplot2geban_grp.mean$est.dens[seq(1,dim(vioplot2geban_grp.mean)[1],2)]<- c(jianshu-datadrivendf4$est.dens)[seq(1,dim(vioplot2geban_grp.mean)[1],2)]
                    #  vioplot2geban_grp.mean$est.dens[seq(2,dim(vioplot2geban_grp.mean)[1],2)]<- c(jianshu+datadrivendf4$est.dens)[seq(2,dim(vioplot2geban_grp.mean)[1],2)]
                    #}else{
                    #
                    #}
                    ppi<-ggplot(datadrivendf3, aes(x=Var2, y=value,color=Groups)) +
                      #geom_jitter(shape=16, position=position_jitter(0.2,seed = 123),size=3)+
                      geom_boxplot(alpha=0.2,outlier.shape=NA,size=0.65)+
                      geom_point(position=position_jitterdodge(seed = 123),size=2)+
                      scale_color_manual(values = colx)+
                      labs(x="",y="Expression Values",title = studynamesxi) +
                      theme_classic()+
                      theme(plot.title = element_text(size = 10))
                  }else{
                    library(pheatmap)
                    library(ggplotify)
                    annotation_col_hca = data.frame(
                      Groups = factor(cptacsamples_All[[2]],levels = gr)
                    )
                    rownames(annotation_col_hca) = colnames(datadrivendf1)
                    #proindex<-apply(t(datadrivendf2),1,function(x){sum(is.na(x))/ncol(t(datadrivendf2))})
                    #datadrivendf2x<-t(datadrivendf2)[proindex<0.5,]
                    datadrivendf2x<-t(datadrivendf2)
                    datadrivendf2x[is.na(datadrivendf2x)]<-min(datadrivendf2,na.rm = T)
                    pp<-pheatmap(datadrivendf2x,scale="none",clustering_distance_rows="correlation",
                                 show_rownames = T, show_colnames = F,
                                 cluster_rows = T,cluster_cols = T,
                                 clustering_distance_cols = "euclidean",
                                 clustering_method = "complete",annotation_col = annotation_col_hca,
                                 color = colorRampPalette(c("navy","white","firebrick3"))(50))
                    ppi<-as.ggplot(pp)
                  }
                }else{
                  library(pheatmap)
                  library(ggplotify)
                  annotation_col_hca = data.frame(
                    Groups = factor(cptacsamples_All[[2]],levels = gr)
                  )
                  rownames(annotation_col_hca) = colnames(datadrivendf1)
                  #proindex<-apply(t(datadrivendf2),1,function(x){sum(is.na(x))/ncol(t(datadrivendf2))})
                  #datadrivendf2x<-t(datadrivendf2)[proindex<0.5,]
                  datadrivendf2x<-t(datadrivendf2)
                  datadrivendf2x[is.na(datadrivendf2x)]<-min(datadrivendf2,na.rm = T)
                  pp<-pheatmap(datadrivendf2x,scale="none",clustering_distance_rows="correlation",
                               show_rownames = T, show_colnames = F,
                               cluster_rows = T,cluster_cols = T,
                               clustering_distance_cols = "euclidean",
                               clustering_method = "complete",annotation_col = annotation_col_hca,
                               color = colorRampPalette(c("navy","white","firebrick3"))(50))
                  ppi<-as.ggplot(pp)
                }
              }
              pplotlist[[i]]<-ppi
            }
            pplotlistx<<-pplotlist
            withProgress(message = 'Plotting...', style = "notification", detail = "", value = 0,{
              incProgress(1/1, detail = "Plotting...")
              if(length(studynamesx)>1){
                ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
              }else{
                ggarrange(plotlist = pplotlist,ncol = 1)
              }
            })
          }
        }
      })
      boxheatplotout<-reactive({
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              colnames(cptacmatrix_All)<-make.unique(colnames(cptacmatrix_All))
              cptacsamples_All[[1]]<-make.unique(cptacsamples_All[[1]])
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                if(input$datazscoreif){
                  datadrivendf2<-scale(t(datadrivendf1))#as.data.frame(scale(t(datadrivendf1)))
                }else{
                  datadrivendf2<-t(datadrivendf1)
                }
                datadrivendf3<-reshape2::melt(datadrivendf2)
                #colnames(datadrivendf3)[3]<-"Value"
                if(length(unique(cptacsamples_All[[2]]))==1){
                  gr<-c("Solid Tissue Normal","Primary Tumor")
                }else{
                  gr<-unique(cptacsamples_All[[2]])
                }
                colx<-c("#7EA2CA","#C67877")
                names(colx)<-gr
                datadrivendf3$Groups<-factor(cptacsamples_All[[2]],levels = gr)
                datadrivendf3$StudyName<-studynamesxi
                #datadrivendf3$GNames<-paste(datadrivendf3$Var2,datadrivendf3$Groups,sep = "_")
                if(nrow(datadrivendf1)<=10){
                  if(input$dataplottype=="Boxplot"){
                    #library(plyr)
                    #source("vioplotsplit.R")
                    #dens_at_mean<<-dens_at_mean
                    #geom_split_violin<<-geom_split_violin
                    boxplotcol<-c('#7EA2CA', '#9C88C5',"#CF87CE","#A5C585","#5292A4","#7F7298","#C67877",
                                  "#869079","#3F698C","#3F86C6")
                    #datadrivendf4 <- ddply(datadrivendf3, .(GNames), dens_at_mean)
                    #vioplot2geban_grp.mean<-datadrivendf4
                    #jianshu<-rep(1:ncol(datadrivendf2),rep(2,ncol(datadrivendf2)))
                    #if(nrow(vioplot2geban_grp.mean)>1){
                    #  vioplot2geban_grp.mean$est.dens[seq(1,dim(vioplot2geban_grp.mean)[1],2)]<- c(jianshu-datadrivendf4$est.dens)[seq(1,dim(vioplot2geban_grp.mean)[1],2)]
                    #  vioplot2geban_grp.mean$est.dens[seq(2,dim(vioplot2geban_grp.mean)[1],2)]<- c(jianshu+datadrivendf4$est.dens)[seq(2,dim(vioplot2geban_grp.mean)[1],2)]
                    #}else{
                    #
                    #}
                    ppi<-ggplot(datadrivendf3, aes(x=Var2, y=value,color=Groups)) +
                      #geom_jitter(shape=16, position=position_jitter(0.2,seed = 123),size=3)+
                      geom_boxplot(alpha=0.2,outlier.shape=NA,size=0.65)+
                      geom_point(position=position_jitterdodge(seed = 123),size=2)+
                      scale_color_manual(values = colx)+
                      labs(x="",y="Expression Values",title = studynamesxi) +
                      theme_classic()+
                      theme(plot.title = element_text(size = 10))
                  }else{
                    library(pheatmap)
                    library(ggplotify)
                    annotation_col_hca = data.frame(
                      Groups = factor(cptacsamples_All[[2]],levels = gr)
                    )
                    rownames(annotation_col_hca) = colnames(datadrivendf1)
                    #proindex<-apply(t(datadrivendf2),1,function(x){sum(is.na(x))/ncol(t(datadrivendf2))})
                    #datadrivendf2x<-t(datadrivendf2)[proindex<0.5,]
                    datadrivendf2x<-t(datadrivendf2)
                    datadrivendf2x[is.na(datadrivendf2x)]<-min(datadrivendf2,na.rm = T)
                    pp<-pheatmap(datadrivendf2x,scale="none",clustering_distance_rows="correlation",
                                 show_rownames = T, show_colnames = F,
                                 cluster_rows = T,cluster_cols = T,
                                 clustering_distance_cols = "euclidean",
                                 clustering_method = "complete",annotation_col = annotation_col_hca,
                                 color = colorRampPalette(c("navy","white","firebrick3"))(50))
                    ppi<-as.ggplot(pp)
                  }
                }else{
                  library(pheatmap)
                  library(ggplotify)
                  annotation_col_hca = data.frame(
                    Groups = factor(cptacsamples_All[[2]],levels = gr)
                  )
                  rownames(annotation_col_hca) = colnames(datadrivendf1)
                  #proindex<-apply(t(datadrivendf2),1,function(x){sum(is.na(x))/ncol(t(datadrivendf2))})
                  #datadrivendf2x<-t(datadrivendf2)[proindex<0.5,]
                  datadrivendf2x<-t(datadrivendf2)
                  datadrivendf2x[is.na(datadrivendf2x)]<-min(datadrivendf2,na.rm = T)
                  pp<-pheatmap(datadrivendf2x,scale="none",clustering_distance_rows="correlation",
                               show_rownames = T, show_colnames = F,
                               cluster_rows = T,cluster_cols = T,
                               clustering_distance_cols = "euclidean",
                               clustering_method = "complete",annotation_col = annotation_col_hca,
                               color = colorRampPalette(c("navy","white","firebrick3"))(50))
                  ppi<-as.ggplot(pp)
                }
              }
              pplotlist[[i]]<-ppi
            }
            pplotlistx<<-pplotlist
            if(length(studynamesx)>1){
              ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
            }else{
              ggarrange(plotlist = pplotlist,ncol = 1)
            }
          }
        }
      })
      output$boxheatplotdl<-downloadHandler(
        filename = function(){paste("Box.or.Heatmap.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 16,height = 16)
          print(boxheatplotout())
          dev.off()
        }
      )
      ##Volcano plot for input proteins/gene name based on CPTAC database
      output$volcanoplot<-renderPlotly({
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
                volcanofcbig<-log2(1.5)
                volcanofcsmall<- log2(1/1.5)
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
                volcanofcbig<-1.5
                volcanofcsmall<- 1/1.5
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              volcanopval<-0.05
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                ##Hypothesis
                library(limma)
                library(qvalue)
                source("hypothesis.R")
                datapro_tianchong<-cptacmatrix_All3
                datafenzudf<-cptacsamples_All
                colnames(datafenzudf)<-c("sample","class")
                hytestdf<-hytestvalue(df=datapro_tianchong,dffenzu=datafenzudf,method="limma",
                                      originaldf=datapro_tianchong,adjust.method="BH",
                                      qvaluethresh=0.05,FCthreshlow=1/1.5,FCthreshbig=1.5,logdataif=input$datalogif)
                volcanorawdata<-hytestdf$hytestdf_orginal
                volcanorawdata$proid<-rownames(volcanorawdata)
                volcanorawdata$p.adjust<-volcanorawdata$p.adjust#p.aov.test#
                volcanorawdata$p.adjust[which(volcanorawdata$p.adjust==0)]<-10^-7
                volcanorawdata$Threshold<-c("NoChange")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change>=volcanofcbig & volcanorawdata$p.adjust <= volcanopval]<-c("UP")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change<=volcanofcsmall & volcanorawdata$p.adjust <= volcanopval]<-c("DOWN")
                volcanorawdata$label = dplyr::if_else(volcanorawdata$proid %in% c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),
                                                      volcanorawdata$proid, NA)
                volcanorawdata$markedcol<-"grey"
                volcanorawdata$markedsize<-1.5
                volcanorawdata$pointshape<-16
                markedcolindex<-which(volcanorawdata$proid %in% c(datadrivendf$UniProt.IDs,datadrivendf$Symbol))
                for(j in 1:length(markedcolindex)){
                  if(volcanorawdata$Threshold[markedcolindex[j]]=="UP"){
                    volcanorawdata$markedcol[markedcolindex[j]]<-"red"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                    volcanorawdata$pointshape[markedcolindex[j]]<-17
                  }else if(volcanorawdata$Threshold[markedcolindex[j]]=="DOWN"){
                    volcanorawdata$markedcol[markedcolindex[j]]<-"blue"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                    volcanorawdata$pointshape[markedcolindex[j]]<-15
                  }else{
                    volcanorawdata$markedcol[markedcolindex[j]]<-"grey"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                  }
                }
                volcanorawdatax<<-volcanorawdata
                volcanorawdatax1<<-volcanorawdatax2<-volcanorawdata[,c("Fold.Change","p.adjust","Threshold","label","markedcol","markedsize","pointshape")]
                volcanorawdata1<<-volcanorawdatax2[markedcolindex,]
                volcanocolx<<-isolate(strsplit(input$volcanocol,";")[[1]])
                volcanorawdatax1$label<-paste0("GeneName:",rownames(volcanorawdatax1),"\n","FC: ",
                                               round(volcanorawdatax1$Fold.Change,4),"\n",
                                               "P.adj: ",round(volcanorawdatax1$p.adjust,4))
                ppi<-ggplot(data=volcanorawdatax1,aes(x=Fold.Change,y=-log10(p.adjust),text = label))+
                  theme_bw()+
                  theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
                  geom_point(aes(colour = Threshold,shape=Threshold),alpha = 0.5,show.legend = F)+
                  geom_point(data = volcanorawdata1,shape = volcanorawdata1$pointshape,show.legend = F,
                             size = volcanorawdata1$markedsize,color = volcanorawdata1$markedcol)+
                  geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=0.8)+
                  geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=0.8)+
                  geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=0.8)+
                  #geom_point(aes(shape=Threshold),colour=volcanorawdata$markedcol,alpha=0.5,
                  #           size=volcanorawdata$markedsize)+
                  theme(legend.position ="right")+
                  scale_color_manual(values = c("DOWN"=volcanocolx[1],
                                                "NoChange"=volcanocolx[2],
                                                "UP"=volcanocolx[3]))+
                  scale_shape_manual(values = c("DOWN"=15,"NoChange"=16,"UP"=17))+
                  #ggrepel::geom_text_repel(data = volcanorawdata1,aes(label = label),
                  #                         size = 3, show.legend = FALSE,
                  #                         force = 2,nudge_y = 1,max.overlaps=100)+
                  geom_text(data = volcanorawdata1,aes(label = label),size = 3, show.legend = FALSE)+
                  xlab("Fold Change (log2)") + ylab("P Value (-log10)") +ggtitle(studynamesxi)
              }
              pplotlist[[i]]<-ggplotly(ppi, tooltip = "text")#ppi#
            }
            pplotlistvolx<<-pplotlist
            withProgress(message = 'Plotting...', style = "notification", detail = "", value = 0,{
              incProgress(1/1, detail = "Plotting...")
              if(length(studynamesx)>1){
                #ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
                subplot(pplotlist,nrows = ceiling(length(studynamesx)/2))
              }else{
                #ggarrange(plotlist = pplotlist,ncol = 1)
                subplot(pplotlist)
              }
            })
            #if(length(studynamesx)>1){
            #  ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
            #}else{
            #  ggarrange(plotlist = pplotlist,ncol = 1)
            #}
          }
        }
      })
      volcanoplotout<-reactive({
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
                volcanofcbig<-log2(1.5)
                volcanofcsmall<- log2(1/1.5)
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
                volcanofcbig<-1.5
                volcanofcsmall<- 1/1.5
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              volcanopval<-0.05
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                ##Hypothesis
                library(limma)
                library(qvalue)
                source("hypothesis.R")
                datapro_tianchong<-cptacmatrix_All3
                datafenzudf<-cptacsamples_All
                colnames(datafenzudf)<-c("sample","class")
                hytestdf<-hytestvalue(df=datapro_tianchong,dffenzu=datafenzudf,method="limma",
                                      originaldf=datapro_tianchong,adjust.method="BH",
                                      qvaluethresh=0.05,FCthreshlow=1/1.5,FCthreshbig=1.5,
                                      logdataif=input$datalogif)
                volcanorawdata<-hytestdf$hytestdf_orginal
                volcanorawdata$proid<-rownames(volcanorawdata)
                volcanorawdata$p.adjust<-volcanorawdata$p.adjust#p.aov.test#
                volcanorawdata$p.adjust[which(volcanorawdata$p.adjust==0)]<-10^-7
                volcanorawdata$Threshold<-c("NoChange")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change>=volcanofcbig & volcanorawdata$p.adjust <= volcanopval]<-c("UP")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change<=volcanofcsmall & volcanorawdata$p.adjust <= volcanopval]<-c("DOWN")
                volcanorawdata$label = dplyr::if_else(volcanorawdata$proid %in% c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),
                                                      volcanorawdata$proid, NA)
                volcanorawdata$markedcol<-"grey"
                volcanorawdata$markedsize<-1.5
                volcanorawdata$pointshape<-16
                markedcolindex<-which(volcanorawdata$proid %in% c(datadrivendf$UniProt.IDs,datadrivendf$Symbol))
                for(j in 1:length(markedcolindex)){
                  if(volcanorawdata$Threshold[markedcolindex[j]]=="UP"){
                    volcanorawdata$markedcol[markedcolindex[j]]<-"red"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                    volcanorawdata$pointshape[markedcolindex[j]]<-17
                  }else if(volcanorawdata$Threshold[markedcolindex[j]]=="DOWN"){
                    volcanorawdata$markedcol[markedcolindex[j]]<-"blue"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                    volcanorawdata$pointshape[markedcolindex[j]]<-15
                  }else{
                    volcanorawdata$markedcol[markedcolindex[j]]<-"grey"
                    volcanorawdata$markedsize[markedcolindex[j]]<-3
                  }
                }
                volcanorawdatax<<-volcanorawdata
                volcanorawdatax1<-volcanorawdata[,c("Fold.Change","p.adjust","Threshold","label","markedcol","markedsize","pointshape")]
                volcanorawdata1<-volcanorawdatax1[markedcolindex,]
                volcanocolx<<-isolate(strsplit(input$volcanocol,";")[[1]])
                ppi<-ggplot(data=volcanorawdatax1,aes(x=Fold.Change,y=-log10(p.adjust)))+
                  theme_bw()+
                  theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
                  geom_point(aes(colour = Threshold,shape=Threshold),alpha = 0.5)+
                  geom_point(data = volcanorawdata1,shape = volcanorawdata1$pointshape,
                             size = volcanorawdata1$markedsize,color = volcanorawdata1$markedcol)+
                  geom_hline(yintercept =-log10(volcanopval),col="grey",lty=2,lwd=0.8)+
                  geom_vline(xintercept =volcanofcsmall,col="grey",lty=2,lwd=0.8)+
                  geom_vline(xintercept =volcanofcbig,col="grey",lty=2,lwd=0.8)+
                  #geom_point(aes(shape=Threshold),colour=volcanorawdata$markedcol,alpha=0.5,
                  #           size=volcanorawdata$markedsize)+
                  theme(legend.position ="right")+
                  scale_color_manual(values = c("DOWN"=volcanocolx[1],
                                                "NoChange"=volcanocolx[2],
                                                "UP"=volcanocolx[3]))+
                  scale_shape_manual(values = c("DOWN"=15,"NoChange"=16,"UP"=17))+
                  #ggrepel::geom_text_repel(data = volcanorawdata1,aes(label = label),
                  #                         size = 3, show.legend = FALSE,
                  #                         force = 2,nudge_y = 1,max.overlaps=100)+
                  geom_text(data = volcanorawdata1,aes(label = label),size = 3, show.legend = FALSE)+
                  xlab("Fold Change (log2)") + ylab("P Value (-log10)") +ggtitle(studynamesxi)
              }
              pplotlist[[i]]<-ppi
            }
            pplotlistx<<-pplotlist
            if(length(studynamesx)>1){
              ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
            }else{
              ggarrange(plotlist = pplotlist,ncol = 1)
            }
          }
        }
      })
      output$volcanoplotdl<-downloadHandler(
        filename = function(){paste("Volcano.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 16,height = 16)
          print(volcanoplotout())
          dev.off()
        }
      )
      volcanoplotdataout<-reactive({
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          datares<-data.frame(Description="Nothing here, please check whether you have typed in a proper keyword!")
          tableres<-list("Nothing"=datares)
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            datares<-data.frame(Description="Nothing here, please select at least one study database!")
            tableres<-list("Nothing"=datares)
          }else{
            library(ggpubr)
            tableres<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              colnames(cptacmatrix_All)<-paste0(cptacsamples_All[[1]],"_",
                                                cptacsamples_All[[2]])
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
                volcanofcbig<-log2(1.5)
                volcanofcsmall<- log2(1/1.5)
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
                volcanofcbig<-1.5
                volcanofcsmall<- 1/1.5
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              volcanopval<-0.05
              if(nrow(datadrivendf1)==0){
                datares<-data.frame(Description="Nothing found here!")
                #tableres<-list(datares)
              }else{
                ##Hypothesis
                library(limma)
                library(qvalue)
                source("hypothesis.R")
                datapro_tianchong<-cptacmatrix_All3
                datafenzudf<-cptacsamples_All
                colnames(datafenzudf)<-c("sample","class")
                hytestdf<-hytestvalue(df=datapro_tianchong,dffenzu=datafenzudf,method="limma",
                                      originaldf=datapro_tianchong,adjust.method="BH",
                                      qvaluethresh=0.05,FCthreshlow=1/1.5,FCthreshbig=1.5,logdataif=input$datalogif)
                volcanorawdata<-round(hytestdf$hytestdf_orginal,4)
                volcanorawdata$proid<-rownames(volcanorawdata)
                volcanorawdata$p.adjust<-volcanorawdata$p.adjust#p.aov.test#
                volcanorawdata$p.adjust[which(volcanorawdata$p.adjust==0)]<-10^-7
                volcanorawdata$Threshold<-c("NoChange")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change>=volcanofcbig & volcanorawdata$p.adjust <= volcanopval]<-c("UP")
                volcanorawdata$Threshold[volcanorawdata$Fold.Change<=volcanofcsmall & volcanorawdata$p.adjust <= volcanopval]<-c("DOWN")
                markedcolindex<-which(volcanorawdata$proid %in% c(datadrivendf$UniProt.IDs,datadrivendf$Symbol))
                datares<-volcanorawdata[markedcolindex,-which(colnames(volcanorawdata)=="proid")]
              }
              tableres[[i]]<-datares
            }
            names(tableres)<-unlist(lapply(studynamesx,function(x){strsplit(x,"_")[[1]][1]}))
          }
        }
        tableres
      })
      ##Output statistical results for input proteins/gene name based on CPTAC database
      output$volcanoplotdata<-renderDataTable({
        dataread<<-volcanoplotdataout()
        if(names(dataread)[1]=="Nothing"){
          datatable(dataread[[1]], options = list(pageLength = 10),caption = "Nothing here")
        }else{
          vpdataindexx<<-input$vpdataindex
          studynamesx<<-input$studynames
          datatable(dataread[[vpdataindexx]], options = list(pageLength = 10),caption = studynamesx[vpdataindexx])
        }
      })
      output$volcanoplotdatadl<-downloadHandler(
        filename = function(){paste("DataDriven.ResultTable.",usertimenum,".xlsx",sep="")},
        content = function(file){
          write.xlsx(volcanoplotdataout(),file,rowNames=TRUE)
        }
      )
      ##Network plot for input proteins/gene name based on CPTAC database
      output$conetworkplot<-renderPlot({
        #library(qgraph)
        #load(file = paste0("database/MEGENA.cancer.output.rdata"))
        #studynamesx<<-isolate(input$studynames)
        #plotindex<-grep(studynamesx[length(studynamesx)],names(pplotlist),ignore.case = T)
        #if(is.null(pplotlist[[plotindex]])){
        #  ggplot() +
        #    theme_void() +
        #    geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesx[length(studynamesx)]))) +
        #    xlab(NULL)
        #}else{
        #  qgraph(pplotlist[[plotindex]],labels=T,title=studynamesx[length(studynamesx)])
        #}
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            library(igraph)
            library(ggraph)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                datapro_tianchong<-cptacmatrix_All3
                datafenzudf<-cptacsamples_All
                colnames(datafenzudf)<-c("sample","class")
                aax<-cor(t(datadrivendf1),t(cptacmatrix_All3),method = "spearman")
                aax1<-reshape2::melt(aax)
                #aax2<-aax1[abs(aax1$value)>=0.85,]
                #if(nrow(aax2)==1){
                #  aax2<-aax1[abs(aax1$value)>=0.65,]
                #}
                #if(nrow(aax2)==1){
                #  aax2<-aax1[abs(aax1$value)>=0.4,]
                #}
                aax1<-aax1[as.character(aax1$Var1)!=as.character(aax1$Var2),]
                aax2<-aax1[order(abs(aax1$value),decreasing = TRUE),]
                aax2<-aax2[1:50,]
                graph_cors<-graph_from_data_frame(aax2,directed = F)
                ppi<-ggraph(graph_cors, layout = "stress") +
                  geom_edge_link(aes(edge_alpha = abs(value), edge_width = abs(value), color = value)) +
                  guides(edge_alpha = "none", edge_width = "none") +
                  scale_edge_colour_gradientn(limits = c(-1, 1), name="Correlation (rho)",
                                              colors = c("dodgerblue2","firebrick2")) +
                  geom_node_point(color = "white", size = 5) +
                  geom_node_text(aes(label = name), repel = TRUE) +
                  theme_void()+
                  #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
                  ggtitle(studynamesxi)
              }
              pplotlist[[i]]<-ppi
            }
            pplotlistx<<-pplotlist
            withProgress(message = 'Plotting network...', style = "notification", detail = "", value = 0,{
              incProgress(1/1, detail = "Plotting...")
              if(length(studynamesx)>1){
                ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
              }else{
                ggarrange(plotlist = pplotlist,ncol = 1)
              }
            })
          }
        }
      })
      conetworkplotout<-reactive({
        #library(qgraph)
        #load(file = paste0("database/MEGENA.cancer.output.rdata"))
        #studynamesx<<-isolate(input$studynames)
        #plotindex<-grep(studynamesx[length(studynamesx)],names(pplotlist),ignore.case = T)
        #if(is.null(pplotlist[[plotindex]])){
        #  ggplot() +
        #    theme_void() +
        #    geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesx[length(studynamesx)]))) +
        #    xlab(NULL)
        #}else{
        #  qgraph(pplotlist[[plotindex]],labels=T,title=studynamesx[length(studynamesx)])
        #}
        datadrivendf<<-datadrivendfout()
        if(is.null(datadrivendf)){
          plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
               main = "Nothing here, please check whether you have typed in a proper keyword!")
        }else{
          studynamesx<<-isolate(input$studynames)
          if(is.null(studynamesx)){
            plot(c(0,0),axes = FALSE,type="n",xlab="",ylab="",
                 main = "Nothing here, please select at least one study database!")
          }else{
            library(ggpubr)
            library(ggraph)
            pplotlist<-list()
            for(i in 1:length(studynamesx)){
              studynamesxi<<-studynamesx[i]
              load(file = paste0("CPTACdatabase/",studynamesxi,".rdata"))
              cptacmatrix_All<<-cptacmatrix_All
              cptacsamples_All<<-cptacsamples_All
              if(strsplit(studynamesxi,"_")[[1]][1]=="PDC000109"){
                cptacmatrix_All1<<-10^cptacmatrix_All
              }else{
                cptacmatrix_All1<<-2^cptacmatrix_All
              }
              cptacmatrix_All2<-sweep(cptacmatrix_All1,2,apply(cptacmatrix_All1, 2, median,na.rm=T),FUN = "/")
              if(input$datalogif){
                cptacmatrix_All3x<<-as.data.frame(log2(cptacmatrix_All2))
              }else{
                cptacmatrix_All3x<<-as.data.frame(cptacmatrix_All2)
              }
              cptacmatrix_All3x1<-impute.knn(as.matrix(cptacmatrix_All3x),k = 10,rowmax = 0.9, colmax = 0.9)
              cptacmatrix_All3<-as.data.frame(cptacmatrix_All3x1$data)
              datadrivendf1<-cptacmatrix_All3[rownames(cptacmatrix_All3)%in%c(datadrivendf$UniProt.IDs,datadrivendf$Symbol),]
              if(nrow(datadrivendf1)==0){
                ppi<-ggplot() +
                  theme_void() +
                  geom_text(aes(0,0,label=paste0('NO protein found in the ',studynamesxi))) +
                  xlab(NULL)
              }else{
                datapro_tianchong<-cptacmatrix_All3
                datafenzudf<-cptacsamples_All
                colnames(datafenzudf)<-c("sample","class")
                aax<-cor(t(datadrivendf1),t(cptacmatrix_All3),method = "spearman")
                aax1<-reshape2::melt(aax)
                #aax2<-aax1[abs(aax1$value)>=0.85,]
                #if(nrow(aax2)==1){
                #  aax2<-aax1[abs(aax1$value)>=0.65,]
                #}
                #if(nrow(aax2)==1){
                #  aax2<-aax1[abs(aax1$value)>=0.4,]
                #}
                aax1<-aax1[as.character(aax1$Var1)!=as.character(aax1$Var2),]
                aax2<-aax1[order(abs(aax1$value),decreasing = TRUE),]
                aax2<-aax2[1:50,]
                graph_cors<-graph_from_data_frame(aax2,directed = F)
                ppi<-ggraph(graph_cors, layout = "stress") +
                  geom_edge_link(aes(edge_alpha = abs(value), edge_width = abs(value), color = value)) +
                  guides(edge_alpha = "none", edge_width = "none") +
                  scale_edge_colour_gradientn(limits = c(-1, 1), name="Correlation (rho)",
                                              colors = c("dodgerblue2","firebrick2")) +
                  geom_node_point(color = "white", size = 5) +
                  geom_node_text(aes(label = name), repel = TRUE) +
                  theme_void()+
                  #theme(panel.grid.minor = element_blank() ,panel.grid.major = element_blank())+
                  ggtitle(studynamesxi)
              }
              pplotlist[[i]]<-ppi
            }
            pplotlistx<<-pplotlist
            withProgress(message = 'Plotting network...', style = "notification", detail = "", value = 0,{
              incProgress(1/1, detail = "Plotting...")
              if(length(studynamesx)>1){
                ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
              }else{
                ggarrange(plotlist = pplotlist,ncol = 1)
              }
            })
          }
        }
      })
      output$conetworkplotdl<-downloadHandler(
        filename = function(){paste("CoexpressionNetwork.plot.",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = 16,height = 16)
          print(conetworkplotout())
          dev.off()
        }
      )
    }
  )
  ##Showing topic ontology database
  databasedata3out<-reactive({
    load(file = paste0("database/Topic_All_top20_","9606",".rdata"))#speciesid
    Topic_Allx
  })
  output$databasedata3<-renderDataTable({
    datatable(databasedata3out(), options = list(pageLength = 10))
  })
  output$databasedata3dl<-downloadHandler(
    filename = function(){paste("Topic.Database.Used.",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(databasedata3out(),file,row.names = F)
    }
  )
  ##Showing GO database
  databasedata4out<-reactive({
    load(file = "database/GOTERMdf.rdata")
    GOTERMdf
  })
  output$databasedata4<-renderDataTable({
    datatable(databasedata4out(), options = list(pageLength = 10))
  })
  output$databasedata4dl<-downloadHandler(
    filename = function(){paste("GO.Database.Used.",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(databasedata4out(),file,row.names = F)
    }
  )

})
##Creating a Shiny app object
shinyApp(ui = ui, server = server)
