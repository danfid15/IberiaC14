
#Packages
library(shiny)
library(tidyverse)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rcarbon)
library(shinythemes)

# Load datasets
c14 <- read_csv("c14.csv")
site_coord <- read_csv("site_coord.csv")
references <- read.csv("references.csv",encoding = "UTF-8")
colnames(references) <- c("Reference","MLA_reference")

# Calibrate the C14 dates
caldatesBP<- summary(calibrate(x= c14$C14_Age, errors = c14$C14_SD,
                               calCurves = c14$calCurve))

caldates_temp<- caldatesBP%>%
  select(TwoSigma_BP_1,TwoSigma_BP_2,TwoSigma_BP_3,TwoSigma_BP_4,TwoSigma_BP_5,TwoSigma_BP_6)%>%
  separate(TwoSigma_BP_1,into = c("BP_1_Max","BP_1_Min"), sep = " to ")%>%
  separate(TwoSigma_BP_2,into = c("BP_2_Max","BP_2_Min"), sep = " to ")%>%
  separate(TwoSigma_BP_3,into = c("BP_3_Max","BP_3_Min"), sep = " to ")%>%
  separate(TwoSigma_BP_4,into = c("BP_4_Max","BP_4_Min"), sep = " to ")%>%
  separate(TwoSigma_BP_5,into = c("BP_5_Max","BP_5_Min"), sep = " to ")%>%
  separate(TwoSigma_BP_6,into = c("BP_6_Max","BP_6_Min"), sep = " to ")

TwoSigma_BP_Max <- c()
TwoSigma_BP_Min <- c()

for (a in 1:nrow(caldates_temp)) {
  TwoSigma_BP_Max[a]<- max(as.numeric(caldates_temp[a,]),na.rm = TRUE)
  TwoSigma_BP_Min[a] <-min(as.numeric(caldates_temp[a,]),na.rm = TRUE)
  
}

caldatesBC<- caldatesBP%>%
  select(MedianBP)%>%
  cbind(TwoSigma_BP_Max,TwoSigma_BP_Min)%>%
  mutate(MedianBC = BPtoBCAD(MedianBP),
         MaxTwoSigmaBC = BPtoBCAD(as.numeric(TwoSigma_BP_Max)),
         MinTwoSigmaBC = BPtoBCAD(as.numeric(TwoSigma_BP_Min)))

# Subset the information for the map
map_data<- c14%>%
  select(Site,Period)%>%
  cbind(caldatesBC%>%
          select(MaxTwoSigmaBC,MinTwoSigmaBC,MedianBC))%>%
  left_join(site_coord%>%
              select(Site,Latitude,Longitude), by = "Site")%>%
  drop_na()

# Subset the information for the map
table_data<- c14%>%
  select(Site,Period,Sample = Lab_ID, Material, Species, Individual_ID,Reference)%>%
  mutate(unCal.BP = paste(c14$C14_Age,"±",c14$C14_SD))%>%
  cbind(caldatesBC%>%
          select(MaxTwoSigmaBC,MinTwoSigmaBC,MedianBC))%>%
  drop_na(Period)

# object glossary 
periods <- levels(as.factor(map_data$Period))
sites <- levels(as.factor(map_data$Site))
maxbc <- max(map_data$MinTwoSigmaBC)
minbc <- min(map_data$MaxTwoSigmaBC)

portugal_map <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf',country = "Portugal")
spain_map <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf',country = "Spain")

iberia_map <- rbind(portugal_map,spain_map)
iberia_map <- sf::st_crop(iberia_map, xmin = -10, xmax = 10, ymin = 35, ymax = 45)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  titlePanel("Prehistoric Iberia (C14)"),
  
  # Page 1 - Map
  tabsetPanel(
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(width = 3,
        
          checkboxGroupInput("period", "Period", periods,selected = periods),
      
          selectInput("hulls", "Convex hulls", c("True", "False"),selected = "False"),
          
          sliderInput("mindate", "From (cal.BC):", value= minbc,
                      min = minbc, max = maxbc),
          sliderInput("maxdate", "To (cal.BC):", value= maxbc,
                    min = minbc, max = maxbc),
          
          "Calibrations made using rcarbon package (Crema and Bevan, 2020);
          intCal20 used for terrestrial samples; marine20 used for marine samples"
          
          ),
        mainPanel(width = 9,
          plotOutput("map", width = "100%"),
          downloadButton("downloadmap", "Download"),
          plotOutput("hist",width = "100%"),
          downloadButton("downloadhist", "Download"),
        )
      )
    ), # End Page 1
    
    # Page 2 - Summary
    tabPanel("Summary",
             fluidRow(
               column(width = 4,
                      tableOutput("summary"),
                      downloadButton("downloadsummary","Download")),
               column(width = 8,
                      plotOutput("boxplot"),
                      downloadButton("downloadboxplot", "Download"))),
             fluidRow(
               column(width = 4,
                      tableOutput("material"),
                      downloadButton("downloadmaterial","Download")),
               column(width = 8,
                      plotOutput("barplot"),
                      downloadButton("downloadbarplot", "Download")))
            
    ), # End Page 2
    
    # Page 3 - Table
    tabPanel("Table",
             fluidRow(
               column(width = 12,
                      dataTableOutput("table"))),
             downloadButton("downloadtable","Download")
    ), # End Page 3
    
    # Page 4 - References
    tabPanel("References",
             fluidRow(
               column(width = 12,
                      tableOutput("biblio"))),
             downloadButton("downloadreference","Download")
    ),# End Page 4
    
    # Page 5 - About
    navbarMenu("More", icon = icon("info-circle"),
               tabPanel("About",
                        fluidPage(
                          column(width = 6,
                                 tags$h1("The project"),
                                  "This project is designed to help researchers search and filter 
                                  for radiocarbon dates related to Prehistoric Iberia, taking into account some cultural, 
                                  geographic, and chronological aspects.",tags$br(),
                                  "The database will be periodically updated by the author. If you have any questions,
                                  please refer to the github page where the all the code and resources are available.",
                                  tags$h1("The author"),
                                  "Daniel Fidalgo",tags$br(),
                                  tags$a(href="https://orcid.org/0000-0001-7175-1686","ORCID"),tags$br(),
                                  tags$a(href="https://www.researchgate.net/profile/Daniel-Fidalgo","ResearchGate"),tags$br(),
                                  tags$a(href="www.linkedin.com/in/daniel-fidalgo-48367b1a3","Linkedin"),tags$br(),
                                  tags$a(href="https://github.com/danfid15","GitHub"))
             )
            ),# End Tab Panel 1
            tabPanel("MIT License",
                     fluidPage(
                       column(width = 12,
                              tags$h1("MIT"),
                              "Copyright (c) 2022 Daniel Fidalgo", tags$br(),
                              "Permission is hereby granted, free of charge, to any person obtaining a copy,
                              of this software and associated documentation files (the Software), to deal
                              in the Software without restriction, including without limitation the rights
                              to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                              copies of the Software, and to permit persons to whom the Software is
                              furnished to do so, subject to the following conditions:",tags$br(),
                              "The above copyright notice and this permission notice shall be included in all
                              copies or substantial portions of the Software.",tags$br(),
                              "THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
                              IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                              FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                              AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                              LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                              OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                              SOFTWARE.)")
                       )
            )
    ) # End Navigation Menu
  ) # End Tabset Panel
) # End fluid Page

server <- function(input, output, session) {
  
  selected_map <- reactive(map_data %>% 
                         filter(Period %in% input$period,
                                MaxTwoSigmaBC >= input$mindate & MinTwoSigmaBC <= input$maxdate))
  
  # Page 1 
  ## Map 
  p1<- function(){
    if(input$hulls == "False"){
      ggplot()+
        geom_sf(data = iberia_map,fill = "gray90",color= "gray50")+
        geom_point(data=selected_map(),aes(x= Longitude, y= Latitude,color= Period),
                   size=3,alpha=0.8)+
        scale_color_viridis_d()+
        annotation_scale()+
        annotation_north_arrow(style = north_arrow_nautical,location="tr")+
        theme_void()+
        theme(legend.title = element_text(face = "bold"))+
        labs(caption = paste("Total number of dates:", nrow(selected_map()),
                             " Total number of sites:",nlevels(as.factor(selected_map()[,1]))))
      
    }else{
      
      hulls <- selected_map() %>%
        group_by(Period) %>%
        distinct(Site,Period,.keep_all = TRUE)%>%
        slice(chull(Longitude, Latitude))
      
      hulls<- data.frame(hulls)
      
      ggplot()+
        geom_sf(data = iberia_map,fill = "gray90",color= "gray50")+
        geom_polygon(data=hulls,aes(x=Longitude,y=Latitude, color= Period,fill= Period),alpha = 0.5)+
        scale_fill_viridis_d()+
        scale_color_viridis_d()+
        annotation_scale()+
        annotation_north_arrow(style = north_arrow_nautical,location="tr")+
        theme_void()+
        theme(legend.title = element_text(face = "bold"))+
        scale_alpha(guide = 'none')+
        labs(caption = paste("Total number of dates:", nrow(selected_map()),
                             " Total number of sites:",nlevels(as.factor(selected_map()[,1]))))
    }
  }
  
  output$map <- renderPlot({
    p1()
  }, res = 96)
  
  output$downloadmap <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      ggsave(file, plot = p1(), device = "png", bg = "white",dpi = 300,
             width = 10, height = 8)
    }
  )
    ## Histogram
    p2<- function(){
      ggplot(selected_map(),aes(x=MedianBC,fill=Period))+
        geom_density(alpha=0.5)+
        scale_fill_viridis_d()+
        theme_classic()+
        theme(legend.title = element_text(face = "bold"))
    }
    
    output$hist <- renderPlot({
      p2()
    }, res = 96)
  
  output$downloadhist <- downloadHandler(
    filename = "hist.png",
    content = function(file) {
      ggsave(file, plot = p2(), device = "png", bg = "white",dpi = 300,
             width = 10, height = 8)
      }
    )
  
  #Page 2 - Summary
  selected_table <- reactive(table_data %>% 
                               filter(Period %in% input$period,
                                      MaxTwoSigmaBC >= input$mindate & MinTwoSigmaBC <= input$maxdate))
  
  #2.1 - Summary table
  summary_table <- function(){
    selected_table()%>%
      group_by(Period)%>%
      summarise(N = n(),From_cal.BC = min(MaxTwoSigmaBC),To_cal.BC = max(MinTwoSigmaBC))%>%
      mutate(Percent = (N/sum(N))*100)%>%
      relocate(Percent, .after = N)%>%
      ungroup()
  }
  
  output$summary<- 
    renderTable(
      summary_table()
  )
  
  output$downloadsummary <- downloadHandler(
    filename = function() {
      paste0("summarytable", ".csv")
    },
    content = function(file) {
      write.csv(summary_table(), file)
    }
  )
  
  p3 <- function(){
    ggplot(selected_table(),aes(x=MedianBC, y = reorder(Period,MedianBC), fill=Period))+
      geom_boxplot(alpha = 0.5)+
      scale_fill_viridis_d()+
      theme_classic()+
      theme(legend.title = element_text(face = "bold"))+
      labs(x= "Median cal.BC",y = "Period")
  }
  
  output$boxplot <- renderPlot({
    p3()
  }, res = 96)
  
  output$downloadboxplot <- downloadHandler(
    filename = "boxplot.png",
    content = function(file) {
      ggsave(file, plot = p3(), device = "png", bg = "white",dpi = 300,
             width = 10, height = 8)
    }
  )
  
  #Material summary
  material_table <- function(){
    selected_table()%>%
      group_by(Material)%>%
      summarise(N = n())%>%
      mutate(Percent = (N/sum(N))*100)
  }
  
  output$material<- 
    renderTable(
      material_table()
    )
  
  output$downloadmaterial <- downloadHandler(
    filename = function() {
      paste0("material", ".csv")
    },
    content = function(file) {
      write.csv(material_table(), file)
    }
  )
  
  p4<- function(){
    ggplot(selected_table()%>%
             group_by(Material,Period)%>%
             summarise(N = n()),aes(x = N, y= reorder(Material,N),fill=Period))+
      geom_col(alpha = 0.5)+
      scale_fill_viridis_d()+
      theme_classic()+
      theme(legend.title = element_text(face = "bold"))+
      labs(x= "Count", y="Dated material")
  }
  
  output$barplot <- renderPlot({
    p4()
  }, res = 96)
  
  output$downloadbarplot <- downloadHandler(
    filename = "barplot.png",
    content = function(file) {
      ggsave(file, plot = p4(), device = "png", bg = "white",dpi = 300,
             width = 10, height = 8)
    }
  )
    
  
  # End output - Page 2
  
  #Page 3 - Table
  output$table <- renderDataTable(
    selected_table()%>%
      unite(cal.BC.2sigma,MaxTwoSigmaBC,MinTwoSigmaBC,sep = " to ")%>%
    relocate(Reference, .after = last_col())
    ,options = list(pageLength = 5))
  
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste0("c14table", ".csv")
    },
    content = function(file) {
      write.csv(selected_table(), file)
    }
  )
  
  #Page 4 - References
  reference_table <- function(){
    selected_table()%>%
      select(Reference)%>%
      distinct(Reference,.keep_all = TRUE)%>%
      left_join(references%>%
                  select(Reference, MLA_reference),by = "Reference")
  }
  
  output$biblio<- 
    renderTable(
      reference_table()
    )
  
  output$downloadreference<- downloadHandler(
      filename = function() {
        paste0("c14references", ".csv")
      },
      content = function(file) {
        write.csv(reference_table(), file)
      }
    )
    
}

shinyApp(ui, server)