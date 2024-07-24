library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(data.table)
library(ggsci)
library(ggplot2)
#library(ggrepel)
library(patchwork)
library(dplyr)
library(GOSemSim)
library(openxlsx)
##
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
#
ui<-renderUI(
  fluidPage(
    title="Ontolomics-P",
    shinyjs::useShinyjs(),
    fluidRow(div(
      HTML(
        "<div style='text-align:center;margin-top:20px;margin-right:0px'>
          <a href='#' target=''><img src='OntolomicsPti.png' width='200px'>
          </a>
          </div>"
      )
    )),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
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
                    '),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),#F5F5DC
        tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip6 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
      )
    ),

    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      tabPanel(
        "Welcome",
        uiOutput("welcomeui")
      ),
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
                In this part, users can type in a UniProt ID/protein name/noun or upload their own proteome expression matrix. The example data were obtained in the "" part.
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
            #radioButtons(
            #  "loadseqdatatype",
            #  label = "",
            #  choices = list("Import modified sequences" = 1,"Load example data"=2),
            #  selected = 1,
            #  inline = TRUE
            #),
            #tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loadseqdatatype==1",
              textInput("proinputids",h5("1. Please type in a protein name/UniProt ID/noun:"),value="",width = NULL, placeholder = 'e.g. P04217, A1BG or liver')
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
            selectInput("origidatatype",h5("2. Data type:"),choices = c("UniProt ID","Protein Name","Noun")),
            bsTooltip("origidatatype",'Here means that what users type in or upload above. "UniProt ID" means users type in a UniProt ID or upload a table with UniProt IDs. "Protein Name" means users type in a protein name or upload a table with protein names. "Noun" means users type in a noun which they want to check, such as liver.',
                      placement = "right",options = list(container = "body"))#,
            #selectInput("wuzhongid",h5("3. Species:"),choices = c("9606-Human","10090-Mouse","10116-Rat"))#,
            #bsTooltip("origidatatype",'.',placement = "right",options = list(container = "body"))
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("Data view:"),
            dataTableOutput("seqrawdata")
          )
        )
      ),
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
            #div(id="seqalignif_div",checkboxInput('seqalignif', '1. Pre-aligned or not?', TRUE)),
            #bsTooltip("seqalignif_div","Whether to pre-align your sequences. If your sequences are standard (e.g. 15 length amino acids), you can unselect this parameter. Default is true.",
            #          placement = "right",options = list(container = "body")),
            #div(id="classicmultisiteif_div",checkboxInput('classicmultisiteif', '2. Classical multiple sites analysis or not?', TRUE)),
            #bsTooltip("classicmultisiteif_div",'Whether to process classical analysis. Classical analysis means not replacing the other modified sites with letter "Z" after pre-alignment, for example "TSLWNPT#Y#GSWFTEK" to "TSLWNPTYGSWFTEK", not to "TSLWNPZYGSWFTEK". If true, do not process transformation, otherwise, transformation.',
            #          placement = "right",options = list(container = "body")),
            #div(id="seqalignhanif_div",checkboxInput('seqalignhanif', '2. Check if containing some regular sequence?', FALSE)),
            #bsTooltip("seqalignhanif_div",'If users want to check whether the aligned peptides contain some specific sequences, for example, you want to find those peptides whose 3th and 5th position are R (arginine), then you can select this parameter and type in a simple regular expression, like "^\\\\w{2}R\\\\w{1}R". Otherwise, you just unselect it.',
            #          placement = "right",options = list(container = "body")),
            #conditionalPanel(
            #  condition = "input.seqalignhanif==true",
            #  textInput("seqalignhan",h5("Regular expression:"),value = "^\\w{2}R\\w{1}R")
            #),
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
              #numericInput("wordnumber",h5("1. Minimum word number:"),value = 3),
              #bsTooltip("wordnumber",'Those words below this number will not be shown in the cloud plot.',
              #          placement = "right",options = list(container = "body")),
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
                    dataTableOutput("originalgotable")
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
        )
      ),
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
            #numericInput("wordnumber",h5("1. Minimum word number:"),value = 3),
            #bsTooltip("wordnumber",'Those words below this number will not be shown in the cloud plot.',
            #          placement = "right",options = list(container = "body")),
            #numericInput("cloudsize",h5("2. Cloud area size:"),value = 18),
            #bsTooltip("cloudsize",'The area size of cloud plot.',
            #          placement = "right",options = list(container = "body")),
            #selectInput("quantitationtype",h5("1. Quantitation type:"),choices = c("All","Unshared")),
            #bsTooltip("quantitationtype",'All: Average, normalized log-ratio of reporter ion intensity to ion intensity from a pooled, common reference associated with the gene in acquisitions from a specific aliquot of a biological sample. Unshared: Average, normalized log-ratio of reporter ion intensity to ion intensity from a pooled, common reference  from unshared peptides associated with the gene in acquisitions from a specific aliquot of a biological sample.',placement = "right",options = list(container = "body")),
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
                  plotOutput("volcanoplot",height="800")
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
        )
      ),
      tabPanel(
        "Database View",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step4: Database View",#Human proteins
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                This step will show the databases used in this software and users can download them freely.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            tags$hr(style="border-color: grey;"),
            radioButtons(
              "databasexz",
              label = NULL,
              choices = list("1. Topics analysis database" = 1,"2. GO terms used for topics analysis"=2),
              selected = 1,
              inline = F
            )
          ),
          mainPanel(
            width = 9,
            h4("Database shown as below:"),
            downloadButton("databasedatadl","Download"),
            dataTableOutput("databasedata")
          )
        )
      )#,
      #tabPanel(
      #  "Topics Analysis",
      #  sidebarLayout(
      #    sidebarPanel(
      #      width = 3,
      #      h3(
      #        "Step 5: Motif Enrichment and Plot",
      #        tags$span(
      #          id = 'span5',
      #          `data-toggle` = "tooltip5",
      #          title = '
      #          This step will find overrepresented sequence motifs for uploaded peptides and blasted peptides respectively, then visualize them. Uploaded peptides here means those modified peptides uploaded directly by users.
      #          Blasted peptides here means those modified peptides mapped to human after blasting.
      #          ',
      #          tags$span(class = "glyphicon glyphicon-question-sign")
      #        )
      #      ),
      #      tags$hr(style="border-color: grey;"),
      #      actionButton("mcsbtn_motifquanbu","Calculate",icon("paper-plane"),
      #                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #    ),
      #    mainPanel(
      #      width = 9,
      #      h4("")
      #    )
      #  )
      #),
      #tabPanel(
      #  "Help",
      #  navlistPanel(
      #    id="helpmanualid",
      #    "Detailed description",
      #    tabPanel(
      #      "1. Overview of Ontolomics-P",
      #      div(style="text-align:left;margin-top:15px;font-size:140%;",HTML("<b>Brief description</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px",
      #        "Post-translational modification (PTM) proteomics analyses such as phosphoproteomics are often performed in non-human species, whereas site-specific PTM functions and biological annotations are much more established in human than other speices. Ontolomics-P is a web-based platform that built in R (Version 4.1.1), and the graphical user interface was implemented using R Shiny framework (version 1.6.0). In this importamt update, we have implemented three new important functions. 1. Mapping the PTM site and protein sequences and identifiers between non-human species and H. sapiens. The query protein sequences were blasted to the reference proteome sequences of H.sapiens using public and widely used packages such as metabalstr and ClustualW. The resulting alignments were used to convert the sequence positions of detected PTMs in non-human species to positions in H.sapiens protein sequences. 2. Calculating sequence window similarity and allowing thresholds of similarity filtering during the mapping, which can be iteratively performed together with human database-based PTM site annotation so that the users can set flexible thresholds 3. Visualizing the expression of modification sites on interacting proteins on the basis of public or user-uploaded protein-protein interaction (PPI) data. In Ontolomics-P, users can choose to upload a PTM sites expression matrix, as well as a PPI database (e.g. SARS-CoV-2 virus-Human PPI database). Then, this tool can plot the expression of PTM sites on interacting proteins with the igraph package. Notebly, all the previous functions of motifeR 1.0, such as handling data outputs from multiple proteomic software, locating the phosphoproteomic data to PTM site identities in the peptide and protein sequence, ggseqlogo package based visualization of enriched motifs, and illustration of kinase-substrate networks in humans are continued to be fully supported. Furthermore, Ontolomics-P can provide downloadable tables during all analytical steps and figures supporting following data analysis."
      #      ),
      #      div(style="text-align:center;margin-top: 8px",
      #          a(href='#',
      #            img(src='Figure1app.png',height=450))),
      #      #div(style="text-align:left;margin-top:20px;font-size:140%;",HTML("<b>1.2 What Ontolomics-P exactly does in each step?</b>")),
      #      #div(
      #      #  style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #      #  "As described above, there are four main steps in the data analysis process of Ontolomics-P: (1) Upload of proteomics data; (2) Data quality control;
      #      #  (3) Missing value imputation; (4) Performance evaluation. However, many users care about the detailed operation in each step.
      #      #  The figure below shows the major steps of the data analysis process in Ontolomics-P. We take two groups of samples
      #      #  (five biological replicates in each group, labeled A1, A2, A3, A4, A5, B1, B2, B3, B4, B5 in the original intensity data)
      #      #  for example. Feature means the identified proteins/peptides."
      #      #),
      #      #div(style="text-align:center;margin-top:8px;margin-bottom:30px;",
      #      #    a(href='#',
      #      #      img(src='FigureS2app.png',height=1000))),
      #      icon = icon("dashboard")
      #    ),
      #    tabPanel(
      #      "2. User manual",
      #      div(style="text-align:left;margin-top:15px;font-size:180%;",HTML("<b>How to use this tool step by step?</b>")),
      #      div(style="text-align:left;margin-top:15px;font-size:160%;",HTML("<b>Please note: Here is a quite overview about how to use Ontolomics-P. More detailed user manual can be found here: .</b>")),
      #      div(style="text-align:left;margin-top:20px;font-size:150%;",HTML("<b>2.1 Input data preparation</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("Ontolomics-P supports five basic file formats (.csv, .txt, .xlsx, .xls and .fasta). Before analysis, users should prepare their peptide sequences with modification.
      #        The data required here could be readily generated based on results of several popular tools such as <a href='https://www.maxquant.org/' target='_blank'>MaxQuant</a>,
      #        <a href='https://biognosys.com/shop/spectronaut' target='_blank'>Spectronaut</a>, and so on. Then
      #        can upload the sequence data into Ontolomics-P with right formats respectively and start subsequent analysis.")
      #      ),
      #      div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.1 Modified peptide sequences with normal type</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("In this situation, users may mark those modified residues (e.g. S, T, Y with phosphorylation) with some label they like (such as '#' or '@') in advance. The peptide sequences like this (download <a href='https://github.com/wangshisheng/Ontolomics-P/blob/main/inst/Ontolomics-Papp/Normal_Exampledata.csv' target='_blank'>example data</a>):")
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help1.png',height=400))),
      #      div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.2 Modified peptide sequences from MaxQuant</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("If the sequence data are obtained from MaxQuant, then users can found the modified peptide sequences in the modification txt file, for example, the Phospho (STY)Sites.txt file in the output tables from MaxQuant. The peptide sequences like this (download <a href='https://github.com/wangshisheng/Ontolomics-P/blob/main/inst/Ontolomics-Papp/MaxQuant_Exampledata.csv' target='_blank'>example data</a>):")
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help2.png',height=400))),
      #      div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.3 Modified peptide sequences from Spectronaut</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("If the sequence data are obtained from Spectronaut, then users can found the modified peptide sequences in the Standard Report part of Spectronaut, for example, export the Peptide Quant results from the Pivot Report and extract the modified peptide sequeces from the EG.ModifiedSequence column. The peptide sequences like this (download <a href='https://github.com/wangshisheng/Ontolomics-P/blob/main/inst/Ontolomics-Papp/Spectronaut_Exampledata.csv' target='_blank'>example data</a>):")
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help3.png',height=400))),
      #      div(style="text-align:left;margin-top:10px;font-size:120%;",HTML("<b>2.1.4 Background data</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("Background data here means the protein sequences (.fasta format). Users should use the same protein sequences file as the background database they obtain the modified peptide sequences from some common software, such as MaxQuant, Spectronaut. For example, users can download the protein sequences from <a href='https://www.uniprot.org/' target='_blank'>UniProt</a>. The protein sequences like this (download <a href='https://github.com/wangshisheng/Ontolomics-P/blob/main/inst/Ontolomics-Papp/fasta/10116.fasta' target='_blank'>example data from Rat</a>):")
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help4.png',height=500))),
      #      div(style="text-align:left;margin-top:50px;font-size:150%;",HTML("<b>2.2 Operating Procedure of Ontolomics-P (Six steps)</b>")),
      #      div(style="text-align:left;margin-top:20px;font-size:120%;",HTML("<b>Step 1. Import Sequence Data</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "After preparing required data (please check part 2.1 above), users can click 'Import data' and upload their own data in the left parameter panel:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help5.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "If users do not upload any data, Ontolomics-P will tell them 'Ontolomics-P detects that you did not upload your data. Please upload the sequence data, or load the example data to check first.' in the right result panel. Then they can choose 'Load example data' and download these example data by clicking relative button:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help6.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "For every parameter in the parameter panel, users can check its detailed description when they mouse over this parameter. Shown as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help7.png',width=1200))),
      #      div(style="text-align:left;margin-top:20px;font-size:130%;",HTML("<b>Step 2. Pre-alignment</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "This step aligns those peptide sequences with the background database (protein sequences) and force the modified sites/residues to be central sites, then users can get the standard peptide window sequences. After selecting suitable methods, users click the 'Calculate' button and obtain results as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help8.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Then users can click the 'Result description' button to check the meaning of each column."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help9.png',width=1200))),
      #      div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 3. Blast to Human proteins</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "This step will map the PTM site and protein sequences and identifiers between non-human species and Human. After selecting suitable methods, users click the 'Calculate' button and obtain results as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help10.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Similarly, users can also click the 'Result description' button to check the meaning of each column."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help11.png',width=1200))),
      #      div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 4. Motif Enrichment and Plot</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "This step will find overrepresented sequence motifs for uploaded peptides and blasted peptides respectively, then plot them. Uploaded peptides here means those modified peptides uploaded directly by users.
      #          Blasted peptides here means those modified peptides mapped to human after blasting. After selecting suitable methods, users click the 'Calculate' button and obtain the motif plot as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help12.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Then the motif table results as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help13.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Similarly, users can also click the 'Result description' button to check the meaning of each column."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help14.png',width=1200))),
      #      div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 5. Annotation based on Kinase-Substrate database</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "This step will Offer more flexible annotation based on kinase-substrate databases (e.g. PhosphoSitePlus) and network plots. After selecting suitable methods, users click the 'Calculate' button and obtain the annotation results for uploaded peptides as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help15.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "The annotation results for blasted peptides as below, and we can find that users can obtain much more abundant information after mapping to human proteins, for instance, the rows of annotated results of example data become from 278 to 6951."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help16.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Then click 'Node and edge table' panel. The node table and the edge table are used for network plot next. Please note: When the network is large, actually the network plot can not be shown immediately and may be corrupted in the next panel (i.e. Network Plot), thus users can download the two tables and input them into other tools (e.g. Cytoscape)."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help17.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "The network plot is shown as below:"
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help18.png',width=1200))),
      #      div(style="text-align:left;margin-top:16px;font-size:130%;",HTML("<b>Step 6. Interaction between non-human proteins and human proteins</b>")),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "This step will visualize the expression of modification sites on interacting proteins on the basis of protein-protein interaction data. Users need prepare the PTM site expression data, samples information and interaction data. Users can click 'Load example data' to check first."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help19.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        HTML("For the example data: 1. The expression matrix were obtained using Spectronaut software from the C. sabaeus in COVID research (<a href='https://doi.org/10.1016/j.cell.2020.06.034' target='_blank'>doi: 10.1016/j.cell.2020.06.034</a>). The first column is protein ids (PTM.ProteinId), the second column is modified amino acids (PTM.SiteAA), the third is PTM locations in protein sequeces (PTM.SiteLocation), and the next columns are sample names. The missing values should be replcaed with NA. Therefore, users should also prepare their expression data like this. 2. Users need type in the right samples information about the groups, replicates and group names, for example, there 6 groups and 2, 2, 3, 3, 3, 3 replicates for each group, the group names are 0h, 2h, 4h, 8h, 12h, 24h.
      #        3. The interaction data were obtained from a SARS-CoV-2 virus-Human PPI database (<a href='https://doi.org/10.1038/s41586-020-2286-9' target='_blank'>doi: 10.1038/s41586-020-2286-9</a>). The first two columns are interacting protein names/IDs (Bait for SARS-CoV-2 virus protein names and Preys for human protein UniProt ids), the third column is protein/gene names for the second column.")
      #      ),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Then click '2. Processed expression data' panel. Ontolomics-P will process median normalization, missing value imputation with KNN method, log data with base 2 by default."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help20.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:10px;margin-right:20px;",
      #        "Finally, click '3. Interaction plot' panel. Users can obtain the interacting proteins plot. If users change the '3.5 Select one interacting protein' parameter, they can obtain different interacting proteins plot."
      #      ),
      #      div(style="text-align:center;margin-top:8px;",
      #          a(href='#',
      #            img(src='help21.png',width=1200))),
      #      div(
      #        style="text-align:justify;width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:50px;margin-right:20px;margin-bottom:30px;",
      #        HTML("If you have any questions, comments or suggestions about Ontolomics-P, please feel free to contact: <u>shishengwang@wchscu.cn</u>. We really appreciate that you use Ontolomics-P, and your suggestions should be valuable to its improvement in the future.")
      #      ),
      #      icon = icon("file-alt")
      #    ),
      #    widths=c(3,9)
      #  )#,
      #  #icon = icon("info-circle")
      #)
      #tabPanel(
      #  "Building Species Database",
      #  sidebarLayout(
      #    sidebarPanel(
      #      width = 3,
      #      div(id='refastafileif_div',checkboxInput("refastafileif","Re-upload?",FALSE)),
      #      bsTooltip("refastafileif_div",'This step can build the standard database based on the fata file that users upload, herein there is no species limit. And this results can also be used in "Own background" step. If users want to build their own database, they can select this parameter and then upload a fasta file.',
      #                placement = "bottom",options = list(container = "body")),
      #      conditionalPanel(
      #        condition = "input.refastafileif==true",
      #        fileInput('fastafile', 'Please upload your fasta file:',accept=c('.fasta'))
      #      ),
      #      tags$hr(style="border-color: grey;"),
      #      actionButton("mcsbtn_fastaalign","Calculate",icon("paper-plane"),
      #                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #    ),
      #    mainPanel(
      #      width = 9,
      #      fluidRow(
      #        column(
      #          2,
      #          downloadButton("allfastadl","Download")
      #        ),
      #        column(
      #          2,
      #          actionButton("mcsbtn_resjieshi5","Result description",icon("file-alt"),
      #                       style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
      #        )
      #      ),
      #      dataTableOutput("allfasta")
      #    )
      #  )
      #)
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
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

    fluidRow(
      #div(style="text-align:center",h1("~~Welcome~~")),
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
  #show data
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
  #######
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

  originalgotableout<-reactive({
    speciesid<<-strsplit("9606-Human","-")[[1]][1]#input$wuzhongid
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
                               grep(paste0("\\b",proinputidsx,"\\b"),UNIPROTidsdf2[[2]],ignore.case = T,perl = T))
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
  topicgotableout<-reactive({
    speciesid<<-strsplit("9606-Human","-")[[1]][1]#input$wuzhongid
    load(file = paste0("database/Topic_All_top20_",speciesid,".rdata"))
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
    SSdata<-data.frame(Ontology=Topic_Allx$Ontology,Topics=Topic_Allx$Terms,Semantic.Similarity=c(gotabledf1,gotabledf2,gotabledf3),
                       Description=Topic_Allx$Description)
    SSdata<-SSdata[order(SSdata$Semantic.Similarity,decreasing = T),]
    SSdata
  })
  originaluploadgotableout<-reactive({
    library(clusterProfiler)
    library(simplifyEnrichment)
    speciesid<<-strsplit("9606-Human","-")[[1]][1]#input$wuzhongid
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
          yydf2<-yydf1[,c(1,12,10,9,3:6,11,8)]
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
  ##
  topicuploadgotableout<-reactive({
    library(clusterProfiler)
    library(simplifyEnrichment)
    speciesid<<-strsplit("9606-Human","-")[[1]][1]#input$wuzhongid
    withProgress(message = 'Starting...', style = "notification", detail = "", value = 0,{
      for(i in 1:2){
        if(i==1){
          incProgress(1/2, detail = "Loading database...")
          load(file = "database/GOTERMdf.rdata")
          load(file = paste0("database/UNIPROTids1_",speciesid,".rdata"))
          load(file = paste0("database/UNIPROTids2_",speciesid,".rdata"))
          load(file = paste0("database/Topic_All_top20_",speciesid,".rdata"))
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
          yytpdf2<-yytpdf[,c(2,9,3:6,8)]
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
          gotabledf<-unique(gotabledf[,c(1,5,6)])
          gotabledf1<-data.frame(Ontology=names(table(gotabledf$Ontology)),
                                 Number=as.numeric(table(gotabledf$Ontology)))
          gotabledf1$Ontology<-factor(gotabledf1$Ontology,levels = c("BP","MF","CC"))
          p1<-ggplot(gotabledf1, aes(x=Ontology, y=Number,fill=Ontology))+
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
      ##
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
      output$originaluploadgoplot<-renderPlot({
        library(cowplot)
        library(grid)
        load(file = paste0("database/Topic_All_top20_",speciesid,".rdata"))
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
        load(file = paste0("database/Topic_All_top20_",speciesid,".rdata"))
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
      ##
      output$volcanoplot<-renderPlot({
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
            withProgress(message = 'Plotting...', style = "notification", detail = "", value = 0,{
              incProgress(1/1, detail = "Plotting...")
              if(length(studynamesx)>1){
                ggarrange(plotlist = pplotlist,ncol = 2,nrow = ceiling(length(studynamesx)/2))
              }else{
                ggarrange(plotlist = pplotlist,ncol = 1)
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
      ##
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
    }
  )
  databasedataout<-reactive({
    speciesid<<-strsplit("9606-Human","-")[[1]][1]#input$wuzhongid
    if(input$databasexz==1){
      load(file = paste0("database/Topic_All_top20_",speciesid,".rdata"))
      dbdata<-Topic_Allx
    }else{
      load(file = "database/GOTERMdf.rdata")
      dbdata<-GOTERMdf
    }
    dbdata
  })
  output$databasedata<-renderDataTable({
    datatable(databasedataout(), options = list(pageLength = 10))
  })
  output$databasedatadl<-downloadHandler(
    filename = function(){paste("DatabaseUsed.",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(databasedataout(),file,row.names = F)
    }
  )
})

shinyApp(ui = ui, server = server)
