getwd()
setwd('/Users/yoshie/Library/CloudStorage/OneDrive-UniversityofGlasgow/PROJECT/liverpool_example')


# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(RMySQL)
library(stringr)


#connect to database
conn <- dbConnect(RMySQL::MySQL(), dbname='covid_v2', Server='localhost', port=3306, user='yoshie', password='0105')

#read table
Mutation_table <- dbReadTable(conn,'mutations')
Sample_table <- dbReadTable(conn,'samples')

#list of ORFs
list_orfs <- list('S','E','M','N','ORF1a','ORF1b','ORF3a','ORF6','ORF7a','ORF7b','ORF8','ORF10')


#--------------------User interface -------------------------

#header part
header <- dashboardHeader(
  title = "Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'tabs',style = "position:fixed;width:inherit;",startExpanded = TRUE,
              menuItem("Main",tabName = 'main',icon = icon("house")),
              menuItem("Samples Table",tabName = "table",icon = icon("th")),
              menuItem("Sample info",tabName = "sample",icon = icon("dashboard")),
              menuItem("Mutation info",tabName = 'mutInfo',icon = icon("fa-sharp fa-regular fa-dna")),
              menuItem("HELP",tabName = 'help', icon = icon("fa-solid fa-question"),
                       menuSubItem("About",tabName = 'about'),
                       menuSubItem("Datasets",tabName = 'dataset'))
              
  ))

#chart-line
body <- dashboardBody(
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 22px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: #eff3ff;
      }
    '))),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> SARS-CoV-2 Wastewater Mutation Search Tool</span>\');
      })
     ')),
  
  tabItems(
    #####content of page0----
    tabItem(tabName = "main",
            fluidRow(
              column(6,
                     tags$div(
                       h2("SARS-CoV-2 Wastewater Mutation Search Tool",style = "color: #3182bd;"))
              )
            ),
            tags$div(
              br(),
              p("HOW TO USE -Two ways of usage-", style = 'font-size:16px; font-weight:bold;'),
              p(span("1.  Enter Mutation:",style = 'font-weight:bold;'),"Please input the mutation (e.g.,'D614G') into the search box provided below."),
              p("After entering a mutation in the search box and selecting the ORF from the dropdown list, the user is redirected to the 'Mutation info' page which is a detailed page with",style = "text-indent:20px;"),
              p("comprehensive information about the mutation specified.",style = "text-indent:20px;"),
              p(span("2.  Browse the Samples Table:",style = 'font-weight:bold;'),"Got to the 'Samples Table' page, find a sample of interest, select one sample from the table,"),
              p("now you can move to the 'Sample info' page where detailed information on the mutations within the sample are shown,", style = "text-indent:20px;"), 
              p("you can move to the 'Mutation info' page after selecting a mutation of interest from the table.",style = "text-indent:20px;"),
              br(),
              p("Please enter mutation that you are interested in in the searchbox."),
              p("Search For a Mutation and ORFs", style = 'font-size:16px; font-weight:bold;')
            ),
            
            fluidRow(
              column(3,
                     #searchInput
                     textInput("user_selected_mutation",
                               label = NULL,
                               placeholder = "Search by mutation e.g.,D614G",
                               value = "")
              ),
              column(2,
                     selectInput("user_selected_orf", label = NULL,choices = list_orfs)
              ),
              column(1,actionButton('search_btn',label = 'Search'))
            ),
            
            tags$div(
              p("Dataset list", style = 'font-size:16px; font-weight:bold;'),
              p("Here is a select tab for datasets. "),
              p("Choose dataset from the dropdown list.")
            ),
            
            fluidRow(
              column(12,
                     selectInput("placelist",label = NULL,c('Liverpool' = 1,'UK-Test' = 2,'ALL' = 'all')
                     ))
            ),
            
            tags$div(
              style = "position: absolute; bottom: 10px; width 100%;",
              tags$img(src='Media_850375_smxx.png.webp',height='100px')
            )
            
    ), 
    
    #####content of page1 == "SAMPLES TABLE PAGE"----
    tabItem(tabName = "table",
            h2("Samples Data",style='color: skyblue4;')
            ,
            fluidRow(
              column(12,
                     tags$div(
                       p("Select one sample from the table to see sample info in the next page")))
            ),
            
            fluidRow(
              column(3,sliderInput("mapped_reads",
                                   label = "Mapped Reads",
                                   value = c(0,20000000), 
                                   min = 0, 
                                   max = 20000000, 
                                   step = 10000) 
              ),
              
              column(3,sliderInput("genome_depth", 
                                   label = "Genome Depth",
                                   value = c(0,100000),
                                   min = 0, 
                                   max = 100000, 
                                   step = 1000)
              ),
              
              column(3,sliderInput("s_depth",
                                   label = "S Depth",
                                   value = c(0,100000),
                                   min = 0,
                                   max = 100000,
                                   step = 1000
              )
              ),
              column(3,
                     sliderInput("s_breadth",
                                 label = "S Breadth",
                                 value = c(0,100),
                                 min = 0,
                                 max = 100,
                                 step = 5
                                 
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     tags$div(
                       br(),
                       p(tags$b("NOTE:Location Abbreviations:"), "BHR= Bank Hall Relief, FZH= Fazakerley High, FZL= Fazakerley Low, LNO= Liverpool North, MRD= Mersey Road, PST= Park Street, RRO= Rimrose, STS= Strand SSO, MWO= Sandon Dock Main Works"),
                       p("Plase refer to the map at the botom of this page.")))
            ),
            
            # Create a new row for the table.
            fluidRow(
              column(12,
                     DT::dataTableOutput("table")
              )),
            fluidRow(
              column(12,
                     tags$div(
                       p(tags$b("Plsease note:"),"The location indicated by the circle on the map is approximate. By clicking the circle, the location name wiil be displayed."),
                       p("For more detailed and accurate information, please refer to oginal paper.")
                     ))
            ),
            
            fluidRow(
              column(12,
                     leafletOutput("map"))
            ),
            
            fluidRow(
              column(12,
                     tags$div(
                       p("The original paper for this dataset is available",tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9614697/","City-wide wastewater genomic surveillance through the successive emergence of SARS-CoV-2 Alpha and Delta vatiants",target='_blank'), "Brunner et al.(2022). Water Res. 2022 Nov 1;226;119306.",style = 'text-indent:20px;')
                     ))
            )
            
    )
    ,
    
    #####content of page2 == SAMPLE INFO PAGE----
    tabItem(tabName = "sample",
            tags$div(h2("Sample info", style ="font-weight: normal;"),
                     p(htmlOutput("sampleName_text")),
                     p(htmlOutput("sampleDate_text"))
            )
            ,
            fluidRow(
              column(2,
                     radioButtons("axes",label = "Scale",c("Linear","Log")))
            ),
            
            fluidRow(
              #Plot data
              column(12,
                     plotOutput("plot_coverage"))
            ),
            
            fluidRow(
              column(12,
                     br(),br())
            ),
            
            fluidRow(
              #Title
              column(12,
                     p("COVERAGE SUMMARY", style = 'font-size:18px;font-weight:bold;')) 
            ),
            
            fluidRow(
              column(12,
                     DT::dataTableOutput("DepthTable"))
            ),
            
            #frequency plot across codon
            fluidRow(
              column(12,
                     tags$div(
                       p("Hover mouse over the plot to see each plot data")
                     ))
            ),
            
            fluidRow(
              column(12,
                     plotlyOutput("freq_plot"))
            ),
            
            fluidRow(
              #Title
              column(12,
                     p("SAMPLE TABLE", style = 'font-size:18px;font-weight:bold;')) 
            ),
            
            fluidRow(
              column(12,
                     tags$div(
                       p("Select one mutation to see the frequency of apperence per month. "),
                       p("Alternatively, enter the specific mutation of interests into the Mutation search box to view the detailed data in the table below.")))
            ),
            
            fluidRow(
              column(3,
                     sliderInput("freq",label = "Frequency",value = c(0,1), min = 0, max = 1, step = 0.05) 
              ),
              column(2,
                     uiOutput("orf_ui")
              ),
              column(3,
                     uiOutput("mutType_ui")
              ),      
              column(3,
                     textInput('mutation2',
                               label = "Mutation",
                               placeholder = "Search by name",
                               value = ""))
            ),
            
            fluidRow(
              #table
              column(12,
                     DT::dataTableOutput("table_sampleInfo"))
            ),
            
            fluidRow(
              #Title
              column(12,
                     p("Freyja Plots", style = 'font-size:18px;font-weight:bold;'))
            ),
            
            fluidRow(
              column(12,
                     imageOutput("freyjaPlot"))
            )
            
    ),
    
    
    #####content of page3----
    tabItem(tabName = 'mutInfo',
            tags$div(h2('Mutation info',style='color: deepskyblue4')
            ),
            
            fluidRow(
              column(4,
                     p(htmlOutput("selected_mutation"))
              ),
              column(2,
                     uiOutput("orf_ui2"))
            ),
            
            fluidRow(
              column(12,
                     plotlyOutput('plot3'))
            ),
            
            
            fluidRow(
              column(12,
                     DT::dataTableOutput('table3'))
            )
            
    ),
    tabItem(tabName = 'help',
            h2("HELP")
    ),
    
    tabItem(tabName = "about",
            fluidRow(
              column(6,
                     h2("About"))
              
            ),
            
            tags$div(
              p("This is a tool for visualizing and exploring complex data with a user-friendly interface which also allows",style='font-size:16px'),
              p("easy insight into capture the trends of cryptic mutation in wastewater samples.",style='font-size:16px'),
              br(),
              p("To create 'SARS-CoV-2 Wastewater Mutation Search Tool', R Shiny app was used for the front-end and MySQL for the back-end.",style='font-size:16px'),
              p("This tool consists of four interconnected pages: the Main page, Samples page, Sample info page, and Mutation info page,",style='font-size:16px'),
              p("each offering a different level of detail and analysis, which allows users to narrow down their analysis by moving from page to page.",style='font-size:16px'),
              br(),
              p("In addition, depending on the dataset that users wish to use, the dataset selection box is set on the Main page, allowing them to update the existing dataset.",style='font-size:16px'),
            ),
            tags$div(
              style = "position: absolute; bottom: 10px; width: 100%;",
              tags$img(src='Media_850375_smxx.png.webp',height='100px'))
            
    ),
    
    tabItem(tabName = 'dataset',
            fluidRow(
              column(6,   h2("Datasets"))
            ),
            tags$div(
              p("1. Overview of the Dataset",style = 'font-weight:bold;'),
              p("Generated datasets consist of two files: one containing mutation data and the other one containing sample data.", style = "text-indent:20px;"),
              p("Each sample is associated with its sample name which enables the examination of mutations over time.",style = "text-indent:20px;"),
              p("All codes and detailed descriptions are available at (github URL)",style = "text-indent:20px;"),
              br(),
              p("The reference sequence used when calling mutations is the first SARS-CoV-2 genome sequence made available, known as Wuhan-Hu-1",style = "text-indent:20px;"),
              p("GenBank accession number MN908947:", style = "text-indent:20px;"),
              p(tags$a(href="https://www.ncbi.nlm.nih.gov/nuccore/MN908947","https://www.ncbi.nlm.nih.gov/nuccore/MN908947",target='_blank'),style = "text-indent:20px;"),
              p("2. Data Link ",style = 'font-weight:bold;'),
              p("The Liverpool wastewater dataset is available at the European Nucleotide Archive (ENA) : below the link", style = "text-indent:20px;"),
              p(tags$a(href="https://www.ebi.ac.uk/ena/browser/view/PRJEB53325","https://www.ebi.ac.uk/ena/browser/view/PRJEB53325",target='_blank'),style = "text-indent:20px;"),
              p("The original paper for this dataset is available",tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9614697/","City-wide wastewater genomic surveillance through the successive emergence of SARS-CoV-2 Alpha and Delta vatiants",target='_blank'), "Brunner et al.(2022). Water Res. 2022 Nov 1;226;119306.",style = 'text-indent:20px;'),
              p("The UK wastewater dataset is also available at the ENA : below the link",style = "text-indent:20px;"),
              p(tags$a(href="https://www.ebi.ac.uk/ena/browser/view/PRJEB55313","https://www.ebi.ac.uk/ena/browser/view/PRJEB55313",target='_blank'),style = "text-indent:20px;"),
              p("The original paper for this dataset can be found ",tags$a(href="https://www.medrxiv.org/content/10.1101/2023.02.15.23285942v1","Utility of wastewater genomic surveillance compared to clinical surveilance to track the spread of the SARS-CoV-2 Omicron variant across England",target='_blank'), "Brunner et al.(2023). Water Res. 2023 Dec 1;247;120804",style = 'text-indent:20px;')
              
            ),
            tags$div(
              style = "position: absolute; bottom: 10px; width: 100%;",
              tags$img(src='Media_850375_smxx.png.webp',height='100px'))
    )
    
  )
)

ui <-dashboardPage(header, sidebar, body)


#--------------------------- Define server logic -----------------

server <- function(input, output, session) {
  
  #Server for MAIN PAGE---- 
  #from main page research box to mutation page
  
  #source data
  
  output$search <- renderTable ({
    event_data(event = c("plotly_click"))
    updateTabsetPanel(session, "mainTomutation", selected = "mutInfo")
    if(is.null(clickData)) return(NULL) 
    
  })
  
  #set reactiveValues
  rv <-reactiveValues(
    last_mutation_info = NULL,
    last_sample_info = NULL,
    last_selected_sample_info = NULL
  )
  
  ###########################################Page1-Sample Data page #####################################
  
  selectData <- reactive({
    dataset_id <- input$placelist
    
    if (dataset_id == 1) {
      #Liverpool data
    mydata <- dbGetQuery(conn, statement = paste0(
      "SELECT Sample_accession AS `Sample accession`, 
                        Sample_date AS `Sample date`, Location, 
                        mapped_Reads AS `Mapped Reads`,
                        TRUNCATE(Genome_Depth,2) AS `Genome Depth`, 
                        TRUNCATE(S_Depth,2) AS `S Depth`, 
                        TRUNCATE(S_Breadth,2) AS `S Breadth`
                  FROM samples
                  WHERE dataset_id = 1;"))
    }else if (dataset_id == 2){
      #uk-test-data
      mydata <- dbGetQuery(conn, statement = paste0(
        "SELECT Sample_accession AS `Sample accession`, 
                        Sample_date AS `Sample date`, Location, 
                        mapped_Reads AS `Mapped Reads`,
                        TRUNCATE(Genome_Depth,2) AS `Genome Depth`, 
                        TRUNCATE(S_Depth,2) AS `S Depth`, 
                        TRUNCATE(S_Breadth,2) AS `S Breadth`
                  FROM samples
                  WHERE dataset_id = 2;"))
    }else{
      #ALL data
      mydata <- dbGetQuery(conn, statement = paste0(
        "SELECT Sample_accession AS `Sample accession`, 
                        Sample_date AS `Sample date`, Location, 
                        mapped_Reads AS `Mapped Reads`,
                        TRUNCATE(Genome_Depth,2) AS `Genome Depth`, 
                        TRUNCATE(S_Depth,2) AS `S Depth`, 
                        TRUNCATE(S_Breadth,2) AS `S Breadth`
                  FROM samples;"))

    }
    
    mydata
  })
  
  #filtering table data
  filterTableData <- reactive({
    mydata <- selectData()
    
    if (isTRUE(input$SampleAccession != "ALL")){
      mydata <- mydata[mydata$`Sample Accession` == input$SampleAccession,]
    }
    if (isTRUE(input$dateInput != "ALL") ){
      mydata <- mydata[mydata$`Sample date` == input$dateInput,]
    }
    mydata <- mydata[mydata$`Mapped Reads`>= input$mapped_reads[1]& mydata$`Mapped Reads`<= input$mapped_reads[2],] 
    
    mydata <- mydata[mydata$`Genome Depth`>= input$genome_depth[1]& mydata$`Genome Depth`<= input$genome_depth[2],] 
    
    mydata <- mydata[mydata$`S Depth`>= input$s_depth[1]& mydata$`S Depth`<= input$s_depth[2],] 
    
    mydata <- mydata[mydata$`S Breadth`>= input$s_breadth[1]& mydata$`S Breadth`<= input$s_breadth[2],] 
    
    mydata
  })
  
  #output table in page1----
  output$table <- DT::renderDataTable({
    
    datatable(
      filterTableData(), filter = "top", selection="single", options = list(paging=TRUE, pageLength=100))

  })
  
  #graph part in samples page----
  output$map <- renderLeaflet({
    places <- list(
      
      list(name = "BHR",color = "#ff595e", lat = 53.430747, lng = -2.980573, radius = 500),
      list(name = "FZH",color = "#ff924c", lat = 53.449191, lng = -2.879282, radius = 1000),
      list(name = "FZL",color = "#ffca3a", lat = 53.48185324408829, lng = -2.889935867034207, radius = 1000),
      list(name = "LNO",color = "#c5ca30", lat = 53.446321, lng = -2.917622, radius = 1500),
      list(name = "MRD",color = "#8ac926", lat = 53.361164, lng = -2.883591, radius = 500),
      list(name = "PST",color = "#52a675", lat = 53.384192, lng = -2.967013, radius = 500),
      list(name = "RRO",color = "#1982c4", lat = 53.496969, lng = -3.006666, radius = 2000),
      list(name = 'STS',color = "#4267ac", lat = 53.449970, lng = -2.988126, radius = 500),
      list(name = "MWO",color = "#6a4c93", lat = 53.406603, lng = -2.948987, radius = 3000)    
    )
    
    map <- leaflet()%>%
      addTiles()
    
    #add polygons which disctibe multipul shape area
    for (place in places){
      map <- map %>% addCircles(
        lng = place$lng,
        lat = place$lat,
        radius = place$radius,
        color = place$color,
        fillColor = place$color,
        fillOpacity = 0.5,
        weight = 2,
        popup = place$name
      )
    }
    
    map
    
  })
  
  #handling error message for navigate users
  output$sampleName_text <-renderText({
    sel_row <- input$table_rows_selected
    
    if (!is.null(sel_row)) {
      sel_val <- filterTableData()[sel_row,1]
      mess <- "SAMPLE NAME"
      paste("<p style='font-size:18px;'>","<span style='color: dimgray;'><B>SAMPLE NAME</B></span>",":", sel_val,"</p>", sep="")
    }
    else {
      mess<- "= A SAMPLE HAS NOT BEEN SELECTED YET FROM THE SAMPLES PAGE - SO NO COVERGAE PLOT IS DISPLAYED="
      paste("<p style='font-size:20px; color: #FE4081; white-space: nowrap;'>", mess,"</p>")
    }
  })
  
  #show samples information 
  output$sampleDate_text <- renderText({
    sel_row <- input$table_rows_selected
    sel_col <- input$table_cols_selected
    if (!is.null(sel_row)){
      sel_column <- filterTableData()[1,sel_col]
      sel_val <- filterTableData()[sel_row,2]
      paste("<p style='font-size:18px;'>","<span style='color: dimgray;'><B>Sample Collected Date</B></span>",":",sel_val, sep="","</p>")
    }
  })
  
  ############################Page2-samples table-get chart data################################
  
  #reactive value store NULL

  selected_accession <- reactiveVal(NULL)
  
  #call database for plot and table 
  select_sample_info_original <- reactive({
    
    #set the condition to select specific element
    sel_row <- input$table_rows_selected
    sel_val <- filterTableData()[sel_row,1]
    dataset_id <- input$placelist

    oneSample <- dbGetQuery(conn, statement = paste0(
      "SELECT s.Sample_accession AS `Sample accession`,
              m.Sample_date AS `Sample Date`,
              m.ORF,
              m.Mutation,
              m.Mutation_type AS `Mutation type`,
              m.C1 AS `Codon1`,
              m.C2 AS `Codon2`,
              m.C3 AS `Codon3`,
              m.Ref_codon AS `Ref codon`,
              m.Mut_codon AS `Mut codon`,
              m.Count,
              m.Freq
      FROM samples AS s 
      INNER JOIN mutations AS m 
      ON  m.Sample_name = s.BAM 
      WHERE s.dataset_id = ", dataset_id, " AND s.Sample_accession='", sel_val,"';"))
    
    oneSample 
  })
  
  #filtering / drop down list
  output$orf_ui <- renderUI({
    req(select_sample_info_original())
    selectInput("orf",label = 'ORF',choices = c("ALL", unique(as.character(select_sample_info_original()$ORF))))
  })
  
  output$mutType_ui <- renderUI({
    req(select_sample_info_original())
    selectInput("mutType",label = 'Mutation Type', choices = c("ALL", unique(as.character(select_sample_info_original()$`Mutation type`))))
  })
  
  
  #updata select_sample_info_original
  select_sample_info <- reactive({
    oneSample <- select_sample_info_original()
    
    #filtering by drop down list
    oneSample <- oneSample[oneSample$Freq>= input$freq[1]& oneSample$Freq<= input$freq[2],] 
    
    #filtering by drop down list
    if (input$orf != "ALL") {
      oneSample <- oneSample[oneSample$ORF == input$orf,]
    }
    #filtering by drop down list
    if (input$mutType != "ALL") {
      oneSample <- oneSample[oneSample$`Mutation type` == input$mutType,]
    }
    rv$last_sample_info <- oneSample
    
    oneSample
  })
  
  #output graph in page2----coverage plot 
  output$plot_coverage <- renderPlot({
    
    sel_row <- input$table_rows_selected
    dataset_id <- input$placelist
    
    if (!is.null(sel_row)) {
      sel_val <- filterTableData()[sel_row,1]
      sel_bam <- dbGetQuery(conn, statement = paste0(
        "SELECT BAM FROM samples WHERE Sample_accession='", sel_val, "';"))
      sel_depth <- paste(str_sub(sel_bam, 1, str_length(sel_bam)-4),"_depth.txt",sep="")

      if (dataset_id == 1){
      sel_file <- paste("./Depth/", sel_depth, sep="")
      
      }else if (dataset_id ==2){
      sel_file <- paste("./uk-test/Depth/", sel_depth, sep="")
      }
      plotData <- read.csv(sel_file, sep="\t", header=FALSE)
  
      p <- ggplot(data = plotData,
                  aes(x=V2, y=V3)) +
        geom_line(color = 'darkblue')+
        labs(x = 'Postion', y= 'Depth', title = 'Coverage Plot')+
        theme_classic()+
        theme(axis.text=element_text(size=14),
              axis.title = element_text(size=14),
              plot.title = element_text(size=18))
    }
    #log axes
    if (input$axes == "Log"){
      p <- p + scale_y_log10(labels=scales::comma)
    }
    p
  })
  
  #call db for summary table for coverage below the plot space 
  coverage_info <- reactive({
    #when user select mutation from 
    sel_row <- input$table_rows_selected
    sel_val <- filterTableData()[sel_row,1]
    
    oneSample <- dbGetQuery(conn, statement = paste0(
      "SELECT Mapped_Reads , 
              ROUND(Genome_Depth,2) AS `Depth`,
              ROUND(ORF1a_Depth,2) AS `ORF1a Depth` ,
              ROUND(ORF1b_Depth,2) AS `ORF1b Depth` ,
              ROUND(S_Depth,2) AS `S Depth`,
              ROUND(ORF3a_Depth,2) AS `ORF3a Depth` ,
              ROUND(E_Depth,2) AS `E Depth`,ROUND(M_Depth,2) AS `M Depth` ,ROUND(ORF6_Depth,2) AS `ORF6 Depth` ,
              ROUND(ORF7a_Depth,2) AS `ORF7a Depth`, ROUND(ORF7a_Depth,2) AS `ORF7a Depth` ,
              ROUND(ORF8_Depth,2) AS `ORF8 Depth`, ROUND(N_Depth,2) AS `N Depth` ,ROUND(ORF10_Depth,2) AS `ORF10 Depth` ,
              ROUND(Genome_Breadth,2) AS `Genome Breadth` ,ROUND(ORF1a_Breadth,2) AS `ORF1a Breadth`,ROUND(ORF1b_Breadth,2) AS `ORF1b Breadth` ,
              ROUND(S_Breadth,2) AS `S Breadth`,ROUND(ORF3a_Breadth,2) AS `ORF3a Breadth`,
              ROUND(E_Breadth,2) AS `E Breadth` ,ROUND(E_Breadth,2) AS `E Breadth`,
              ROUND(ORF6_Breadth,2) AS `ORF6 Breadth` , ROUND(ORF7a_Breadth,2) AS `ORF7a Breadth`,
              ROUND(ORF7b_Breadth,2) AS `ORF7b Breadth` ,ROUND(ORF8_Breadth,2) AS `ORF8 Breadth` ,
              ROUND(N_Breadth,2) AS `N Breadth`,ROUND(ORF10_Breadth,2) AS `ORF10 Breadth` 
      FROM samples WHERE sample_accession ='", sel_val, "';"))
    
    oneSample
  })
  
  
  #coverage summary table 
  output$DepthTable <- DT::renderDataTable({
    datatable(
      coverage_info(),options = list(scrollX = TRUE)
    )
  })
  
  #mutation plot
  output$freq_plot <- renderPlotly({
    data <- select_sample_info()
    p <- ggplot(data = data,
                aes(x = Codon1,
                    y = Freq, 
                    group = Mutation,
                    text = paste("Position:",Codon1,
                                 "<br>Frequecy:",Freq,
                                 "<br>Mutation:",Mutation,
                                 "<br>ORF:",ORF
                    ))) +
      geom_point(color = '#a6cee3',size=1.5) +
      labs(x = 'Postion', y= 'Freq', title = 'Mutation Plot') +
      theme_classic() +
      theme(#axis.text=element_text(size=14),
        axis.title = element_text(size=14),
        plot.title = element_text(size=18)) 
    
    ggplotly(p,tooltip = "text")
    #event_register('plotly_click')
    
  })
  
  #captures click events
  observeEvent(input$plot_click,{
    click_info <- nearPoints(select_sample_info(), input$plot_click, threshold = 5, maxpoints = 1)
    if(nrow(click_info) != 0){
      selected_row <- which(select_sample_info() %in% click_info)
      proxy <- dataTableProxy('table_sampleInfo')
      selectRows(proxy, selected_row)
    } 
  })
  
  #Mutation table output in page2----
  output$table_sampleInfo <- DT::renderDataTable({
    dt <- 
      datatable(
        select_sample_info(),
        filter = 'top',
        selection = 'single',
        options = list(paging=TRUE,pageLength=50)
      )
    
    dt
    
  })
  
  #output Freyja plots
  output$freyjaPlot <- renderImage({
    sel_row <- input$table_rows_selected
    if (!is.null(sel_row)) {
      sel_val <- filterTableData()[sel_row,1]
      sel_bam <- dbGetQuery(conn, statement = paste0(
        "SELECT BAM FROM samples WHERE Sample_accession='", sel_val, "';"))
      #read image 
      sel_pic <- paste0(str_sub(sel_bam, 1, str_length(sel_bam)-4),"_freyja",sep="")
      
      image_data_file <- paste0("./Freyja/",sel_pic,".png")
      
      list(
        src = image_data_file,
        contentType = "image/png",
        width = "70%",
        height = "auto"
      )
    }
  },deleteFile = FALSE)
  
  #enter search text 
  DTproxy <- dataTableProxy("table_sampleInfo")
  #output$table2 <- renderDT(select_sample_info())
  observeEvent(input$mutation2,{
    updateSearch(DTproxy, keywords = list(global = input$mutation2, columns = NULL))
  })
  
  
  
  ######################################Page3-mutation table#############################
  #link from main page of search button
  observeEvent(input$search_btn,{
    updateTabItems(session,"tabs","mutInfo")
    user_selected <- input$user_selected_mutation
  })
  
  
  output$selected_mutation <- renderText({
    sel_row2 <- input$table_sampleInfo_rows_selected  
    
    if (!is.null(sel_row2)){
      #set the condition to select specific element
      sel_val2 <- select_sample_info()[sel_row2,"Mutation"]
      sel_val_orf <- select_sample_info()[sel_row2,"ORF"]
      paste("<p style='font-size:18px;'>","<span style='color: dimgray;'><B>Selected Mutation</B></span>",":",sel_val2,"</p>", 
            "<p style='font-size:18px;'>","<span style='color: dimgray;'><B>Selected ORF region</B></span>",":",sel_val_orf,"</p>", 
            "Hover mouse over the plot to see each sample data",sep="")
    }
    else if (input$user_selected_mutation != ""){
      user_selected_orf <- input$user_selected_orf
      paste("<p style='font-size:18px;'>","<span style='color: dimgray;'><B>Selected Mutation</B></span>",":",input$user_selected_mutation,"</p>", 
            "<p style='font-size:18px;'>","<span style='color: dimgray;'><B>Selected ORF region</B></span>",":",user_selected_orf,"</p>", 
            "Hover mouse over the plot to see each sample data",sep="")
    }
    else {
      mess<- "= A MUTATION HAS NOT BEEN SELECTED YET - SO NO DATA IS DISPLAYED ="
      paste("<p style='font-size:20px; color: #FE4081; white-space: nowrap;'>", mess,"</p>")
    }
    
    
  })
  
  #Reactive value to store data
  mutdata <- reactiveVal(NULL)
  
  #for renderPlotly (with filter by ORF)
  select_mut_info_filtered <- reactive({
    #1. get the mutation from table_sampleInfo table
    sel_row2 <- input$table_sampleInfo_rows_selected
    
    #2. get the mutation entered by the user in the searchInput box
    user_selected <- input$user_selected_mutation
    user_selected_orf <- input$user_selected_orf
    
    #3. get the place infor either Liverpool or Uk-test
    dataset_id <- input$placelist
    
    #Per Sample Frequency","Mean Frequecny
    if(!is.null(sel_row2)) {
      sel_val_mut <- select_sample_info()[sel_row2,"Mutation"]
      sel_val_orf <- select_sample_info()[sel_row2,"ORF"]
      mutdata <- dbGetQuery(conn, statement = paste0(
        "SELECT m.Sample_date AS `Sample Date`,
                  s.Sample_accession,
                  m.ORF,
                  m.Mutation,
                  m.Freq AS `Frequency`,
                  m.Ref_codon AS `Ref codon`,
                  m.Mut_codon AS `Mut codon`,
                  m.Count
        FROM mutations AS m
        INNER JOIN samples AS s 
        ON  m.Sample_name = s.BAM 
        WHERE m.Mutation='", sel_val_mut, "' AND m.ORF='",sel_val_orf,"' AND s.dataset_id=", dataset_id,"
        ORDER BY `Sample Date`;"))
      
      #update last selected mutation
      rv$last_mutation_info <- mutdata
    }
    else if( user_selected != ""){
      mutdata <- dbGetQuery(conn, statement = paste0(
        "SELECT m.Sample_date AS `Sample Date`,
                  s.Sample_accession,
                  m.ORF,
                  m.Mutation,
                  m.Freq AS `Frequency`,
                  m.Ref_codon AS `Ref codon`,
                  m.Mut_codon AS `Mut codon`,
                  m.Count
        FROM mutations AS m
        INNER JOIN samples AS s 
        ON  m.Sample_name = s.BAM 
        WHERE m.Mutation='", user_selected, "' AND m.ORF='",user_selected_orf,"' AND s.dataset_id=", dataset_id,"
        ORDER BY `Sample Date`;"))
      rv$last_mutation_info <- mutdata
    }
    else{
      mutdata <- rv$last_mutation_info
    }
    mutdata
  })
  
  #display mutation plot
  output$plot3 <- renderPlotly({
    data <- select_mut_info_filtered()

    data$`Sample Date` <- as.Date(data$`Sample Date`, format = "%Y-%m-%d")
    
    p <- ggplot(data = data,
                aes(x = `Sample Date`,
                    y = Frequency, 
                    group = Mutation,
                    color = `Mut codon`,
                    text=paste('Ref codon:', `Ref codon`,
                               '<br>Accession Number:', Sample_accession)))+
      geom_point(alpha = 0.7,size = 2.0)+
      labs(x="Date", y="Frequency",title = "Monthly Mutation Frequency Distibution",
           color = "Mut Codon",
           shape = "Ref Codon")+
      scale_x_date(date_breaks ='1 month',date_labels = "%Y %b")+
      theme(axis.title = element_text(size=14),
            axis.text = element_text(size=14))+
      theme_classic()+
      scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")) 
    
    
    ggplotly(p)
    
  })
  
  
  
  #render filtering 
  
  #output table in page3----
  output$table3 <- DT::renderDataTable({
    req(rv$last_mutation_info)
    datatable(
      rv$last_mutation_info,
      #select_mut_info_filtered(),
      filter ='top',
      selection="single", 
      options = list(paging=TRUE, pageLength=100))
  })
  
  
  #close database
  onSessionEnded(function() {
    dbDisconnect(conn)
  })
  
}

shinyApp(ui,server)
