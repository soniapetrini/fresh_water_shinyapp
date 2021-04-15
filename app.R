
#library(shiny)
#library(leaflet)
#library(tidyverse)
#library(ggplot2)
#library(RColorBrewer)
#library(viridis)
#library(shinydashboard)
#library(plotly)
#library(janitor)
#library(DT)


pckg_check <- function(){
  if(!require('pacman'))install.packages('pacman')
  pacman::p_load(shiny,leaflet,tidyverse,ggplot2,RColorBrewer,viridis,shinydashboard,
                 plotly,janitor,DT,shinyWidgets,xts)
}

pckg_check()

last_date_tib <- read.csv("last_date.csv")
attach(last_date_tib)


###### Preparation
options(DT.options = list(pageLength = 5))
cat_choice <- c("Extinct","Critically Endangered","Endangered",
                "Vulnerable", "Near Threatened",
                "Least Concern","Data Deficient")

colors = c("#020103","#FC1C03","#FCB103",
           "#FCE703","#BAFC03",
           "#429126","#929E90")

pal <- colorFactor(colors,domain = factor(category,levels=cat_choice))

area_choice <- levels(factor(area))
class_choice <- levels(factor(class))
king_choice <-  levels(factor(kingdom))
cl_phy_choice <- c("class","phylum")

threatened = c("Endangered","Critically Endangered","Vulnerable")
near_threatened = c("Near Threatened")


################################## UI
ui <- fluidPage(
  fluidRow(
    column(6,
           titlePanel("Fresh Water Systems' Endangered Species Observatory"),
    ),
    column(6,
           align = "center",
           br(),
           img(src='IUCN.png',height = 60)
    )
  ),
  fluidRow(
    
    tabsetPanel(
      
      tabPanel("Main",
         dashboardPage(
           
           dashboardHeader(disable = TRUE),
           
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
                    column(8,
                           fluidRow(
                             column(7,
                                  box(title = "Overview", solidHeader = TRUE, status = "info", width = 490,height = 180,
                                    textOutput("descr"))),
                             column(5,
                                  tabBox(title = "Support IUCN",id = "support",width = 250,height = 180,
                                      tabPanel("Contact",
                                               strong("IUCN Global Species Programme Red List Unit"),
                                               textOutput("t1"),
                                               textOutput("t2"),
                                               textOutput("t3"),
                                               strong("Tel:"), "+44 (0)1223 331199",
                                               strong("- Email:"), "redlist@iucn.org"),
                                      tabPanel("Donate",
                                               "You can help IUCN to reach its goal of assessing 160.000 species through a donation:",
                                               fluidRow(br(), 
                                                        a(href = "https://www.iucnredlist.org/support/donate", "Donate now!"),
                                                        align = "center"))
                                      )
                                  )
                             
                             ),
                           leafletOutput("map",height = 470)
                           ),
                    column(4,
                           fluidRow(
                               box(title = "Control panel", solidHeader = TRUE, status = "warning", width = 350, height = 340,
                                  pickerInput("kingdom","Choose a kingdom",
                                    choices = king_choice,
                                    multiple=TRUE,
                                    selected = king_choice),
                                  
                                  pickerInput("category","Choose a category",
                                    choices = cat_choice,
                                    multiple=TRUE,
                                    selected = cat_choice),
                                  
                                  pickerInput("area","Choose an area",
                                    choices = area_choice,
                                    multiple=TRUE,
                                    selected = area_choice),
                                  
                                  uiOutput('class'))),
                           
                          fluidRow(
                               box(title = "Number of threatened and near threatened species", status = "success", 
                                   width = 350, height = 300, 
                                   plotOutput("threat", height = 240)))
                      )
          ))),
      
      
      tabPanel("Species",
        dashboardPage(
            
            dashboardHeader(disable = TRUE),
            
            dashboardSidebar(disable = TRUE),
            
            dashboardBody(
              column(3,
                     h4("You can search a species:"),
                     fluidRow(
                       column(10,
                              textInput("species","Insert species")),
                       column(2,
                              br(),
                              actionButton("chosen","Go"))
                     ),
                     h4("Or look up a random one:"),
                     actionButton("random_an","Animal"),
                     actionButton("random_pl","Plant"),
                     verbatimTextOutput(outputId = "selected_species",placeholder = TRUE)
              ),
              column(9,
                    fluidRow(
                        tabBox(title = "Class' Risk",id = "tabset1", height = 310,
                          tabPanel("Numbers",
                                   strong(textOutput("class_n")),
                                   textOutput("numbers_c"),
                                   plotOutput("bar_species_class",height = 200)),
                          tabPanel("Georisk", 
                                   strong(textOutput("class_g")),
                                   textOutput("georisk_c"),
                                   plotlyOutput("bubble_species_class",height = 200))),
                        
                        tabBox(title = "Phylum's Risk",id = "tabset2", height = 310,selected = "Georisk",
                          tabPanel("Numbers", 
                                   strong(textOutput("phylum_n")),
                                   textOutput("numbers_p"),
                                   plotOutput("bar_species_phylum",height = 200)),
                          tabPanel("Georisk", 
                                   strong(textOutput("phylum_g")),
                                   textOutput("georisk_p"),
                                   plotlyOutput("bubble_species_phylum",height = 200)))
                         ),
                    fluidRow(
                      box(title = "Tables", solidHeader = TRUE, status = "info", height = 320,
                          fluidRow(
                            column(6,
                                   DT::dataTableOutput("gen_tab")),
                            column(6,
                                   DT::dataTableOutput("det_tab"))
                            )),
                      box(title = "Map", solidHeader = TRUE, status = "success", height = 320,
                          leafletOutput("species_map", height = 250))
                      )
                )
              ))),
      
      
      tabPanel("Area",
         dashboardPage(
           
           dashboardHeader(disable = TRUE),
           
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
               column(6,
                  box(title = "Control panel", solidHeader = TRUE, status = "warning", width = 700,
                      fluidRow(
                          column(7,
                                 pickerInput("category1","Choose a category",
                                   choices = cat_choice,
                                   multiple=TRUE,
                                   selected = cat_choice),
                                 
                                 sliderTextInput("area1","Choose an area",
                                   choices = area_choice,
                                   selected = "Africa",
                                   grid = T)
                          ),
                          column(5,
                                 fluidRow(
                                   column(6,
                                      checkboxGroupInput("kingdom1","Choose a kingdom",
                                                          choices = king_choice,
                                                          selected = "animals")),
                                   column(6,
                                      radioButtons("class_phyl","Select one",
                                                    choices = cl_phy_choice,
                                                    selected = "class"))
                                  ),
                                fluidRow(
                                    align = "center",
                                    h1(textOutput(outputId = "selected_area"))
                                   )
                          )
                      )),  #box and row
                 fluidRow(
                   leafletOutput("area_map", width = 680)
                 )
               ),
               column(6,
                      box(title = "Endangered species found in area", solidHeader = TRUE, status = "primary", width = 650, height = 350,
                          plotOutput("plot2", width = 650, height = 280)),
                      fluidRow(
                            column(6,
                              box(title = "Total Observed Species - Pie", status = "success", width = 300, height = 280,
                                  plotOutput("cat_pie", height = 220))),
                            column(6,
                              box(title = "Total Observed Species - Table",status = "success", width = 300, height = 280,
                               dataTableOutput('table'))))
              ) # col
           )) # dash page and body
     ) # area
     
              
)))  # page,row,tabset









server <- function(input,output, session){
  
  ### MAIN
  
  # descr box
  output$descr <- renderText({
  descr <- "Welcome to the Fresh Water Systems' Endangered Species Observatory.
  Here you can visualize the Data provided by IUCN Red List, concerning the status of Fresh Water animals and plants.
  Each point on the map is a spcies which has been last observed after 2000, marked according to the IUCN classification of threat:
  you can navigate the map through the control panel, or see more details about a specific area or species in the other sections."
  descr
    })
  
  # support
  output$t1 <- renderText("IUCN UK Office, The David Attenborough Building")
  output$t2 <- renderText("Pembroke Street, Cambridge CB2 3QZ")
  output$t3 <- renderText("United Kingdom")
  
  # reactive input choice
  output$class <- renderUI({
    choices = levels(factor(class[kingdom %in% input$kingdom]))
    pickerInput("class","Choose a class",
      choices = choices,
      multiple=TRUE,
      selected = choices)
  })

    # map
  df<-reactive({
    last_date_tib %>%
      arrange(factor(category,levels = cat_choice)) %>%
      filter(kingdom %in% as.character(input$kingdom) & 
             category %in% as.character(input$category) &
             area %in% as.character(input$area) & 
             class %in% as.character(input$class))
  })
  
  output$map <- renderLeaflet({
    print(head(na.omit(df())))
    print(class(na.omit(df())))
    leaflet(data=na.omit(df())) %>% 
      addProviderTiles("OpenStreetMap.HOT",
                       options = providerTileOptions(minZoom = 1)) %>%
      
      addCircleMarkers(
        radius = ~ifelse(category %in% threatened, 8, ifelse(category %in% near_threatened,6,4)),
        color = ~pal(category),
        stroke = FALSE, fillOpacity = 0.5,
        popup = paste(df()$binomial, "<br/>Risk: ",
                      df()$category, "<br/>Phylum: ",
                      df()$phylum, "<br/>Class: ",
                      df()$class, "<br/>Order: ",
                      df()$order, "<br/>Family: ",
                      df()$family, "<br/>Last observation: ",
                      df()$event_year)) %>%
      leaflet::addLegend(pal = pal,
                values = factor(category, levels=cat_choice),
                title = "Risk Categories",
                opacity = 1)
  })
  
  # theat BAR
 
  threat <- reactive({
    last_date_tib %>%
    filter(kingdom %in% input$kingdom,class %in% input$class,
           threat_level %in% c("Threatened","Near Threatened")) %>%
    mutate(threat_level = factor(threat_level)) %>%
    group_by(threat_level,area) %>%
    count()
  })
  
  output$threat <- renderPlot({
    ggplot(na.omit(threat()),(aes(x = area, y = n, fill = threat_level))) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      geom_text(aes(label=n),position=position_dodge(width=1), vjust=-0.5) +
      labs(y = "number of species") +
      scale_fill_manual(values = c("#BAFC03","orangered1")) +
      ylim(0,80)
  })
  
  
  ############## SPECIES ##############
  
  # species
  values <- reactiveValues(species = "Umma purpurea")
  
  observeEvent( input$chosen, {
    values$species <- as.character(input$species)
  })
  
  observeEvent( input$random_an, {
    king <- last_date_tib %>% filter(kingdom == 'animals',category != "Least Concern")
    wdf <- king[sample(nrow(king), 1), ]
    values$species <- wdf$binomial[1]
  })
  
  observeEvent( input$random_pl, {
    king1 <- last_date_tib %>% filter(kingdom == 'plants',category != "Least Concern")
    wdf1 <- king1[sample(nrow(king1), 1), ]
    values$species <- wdf1$binomial[1]
  })
  
  
  # text
  output$selected_species <- renderText({
    values$species
  })
  
  output$class_n <- renderText({
    last_date_tib$class[binomial == values$species]
  })
  output$class_g <- renderText({
    last_date_tib$class[binomial == values$species]
  })
  
  
  output$phylum_n <- renderText({
    last_date_tib$phylum[binomial == values$species]
  })
  output$phylum_g <- renderText({
    last_date_tib$phylum[binomial == values$species]
  })
  
  output$numbers_c <- renderText(
    "Spread by area and threat level."
  )
  output$numbers_p <- renderText(
    "Spread by area and threat level."
  )
  
  output$georisk_c <- renderText(
    "Share of species found in area for each category."
  )
  
  output$georisk_p <- renderText(
    "Share of species found in area for each category."
  )
  
  
  
  ## BAR THREAT CLASS
  risk_c <- reactive({
    last_date_tib %>% 
      mutate(category = factor(category,levels=rev(cat_choice)),
             class = factor(class)) %>%
      filter(class == class[binomial == values$species],category != "Least Concern") %>%
      group_by(area,category,class) %>%
      count()
  })
  
  output$bar_species_class <- renderPlot({
    if (is.null(values$species)) return()
    ggplot(risk_c(),aes(fill=category, y=n, x=category)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_wrap(~area,scales = "free_x",nrow = 1) +
      scale_fill_manual(breaks = rev(cat_choice), values = rev(colors)) +
      labs(fill='IUCN Classification') +
      theme_bw() + 
      theme(axis.text.x=element_blank()) +
      labs(subtitle = paste("By area and IUCN threat classification"),
           y = "number of species",
           x = "")  + 
      geom_text(aes(label=n),position=position_dodge(width=1), vjust=-0.5) +
      ylim(0,30)
  })
  
  ## BUBBLE CLASS
  
  bub_class <- reactive({
    last_date_tib %>% 
    mutate(category = factor(category,levels=rev(cat_choice)),
           class = factor(class)) %>%
    filter(class == class[binomial == values$species]) %>%
    group_by(area,category,class) %>%
    count() %>%
    group_by(category) %>%
    mutate(perc = n/sum(n)) %>%
    select(area,category,perc) %>%
    ungroup() %>%
    mutate(ID = row_number())
  })
  
  output$bubble_species_class <- renderPlotly({
    plot_ly(bub_class(), x = ~category, y = ~area, text =~ID, type = 'scatter', mode = 'markers',
                 size = ~perc, color = ~category, colors = rev(colors),
                 sizes = c(5,50),
                 marker = list(opacity = 0.8, sizemode = "diameter")) %>% 
    layout(xaxis = list(title = "",showticklabels = FALSE))
    })
  

  
  
  ## BAR THREAT PHYLUM
  risk_p <- reactive({
    last_date_tib %>% 
      mutate(phylum = factor(phylum)) %>%
      mutate(category = factor(category,levels=rev(cat_choice))) %>%
      filter(phylum == phylum[binomial == values$species],category != "Least Concern") %>%
      group_by(area,category,phylum) %>%
      count()
  })
  
  output$bar_species_phylum <- renderPlot({
    if (is.null(values$species)) return()
    ggplot(risk_p(),aes(fill=category, y=n, x=category)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_wrap(~area,scales = "free_x",nrow = 1) +
      scale_fill_manual(breaks = rev(cat_choice), values = rev(colors)) +
      labs(fill='IUCN Classification') +
      theme_bw() + 
      theme(axis.text.x=element_blank()) +
      labs(subtitle = paste("By area and IUCN threat classification"),
           y = "number of species",
           x = "")  + 
      geom_text(aes(label=n),position=position_dodge(width=1), vjust=-0.5) +
      ylim(0,30)
  })
  
  ## BUBBLE PHYLUM
  
  bub_phylum <- reactive({
    last_date_tib %>% 
      mutate(category = factor(category,levels=rev(cat_choice)),
             phylum = factor(phylum)) %>%
      filter(phylum == phylum[binomial == values$species]) %>%
      group_by(area,category,phylum) %>%
      count() %>%
      group_by(category) %>%
      mutate(perc = n/sum(n)) %>%
      select(area,category,perc) %>%
      ungroup() %>%
      mutate(ID = row_number())
  })
  
  output$bubble_species_phylum <- renderPlotly({
    plot_ly(bub_phylum(), x = ~category, y = ~area, text =~ID, type = 'scatter', mode = 'markers',
            size = ~perc, color = ~category, colors = rev(colors),
            sizes = c(5,50),
            marker = list(opacity = 0.8, sizemode = "diameter")) %>% 
      layout(xaxis = list(title = "", showticklabels = FALSE))
  })
  
  
  
  
  ### species TABLES
  
  general <- reactive({
    last_date_tib %>% 
    filter(binomial == values$species) %>% 
    select(binomial,event_year,legend,category,area,threat_level) %>%
    gather(Info,Value)
  })
  
  details <- reactive({
    last_date_tib %>% 
    filter(binomial == values$species) %>% 
    select(kingdom,phylum,class,order,family,genus) %>%
    gather(Species,Value)
  })
  
  output$gen_tab <- renderDataTable({
    DT::datatable(general(), rownames = FALSE, options = list(dom = 't')) %>% 
      DT::formatStyle('Info',
                  backgroundColor = "lightblue")
  })
  
  output$det_tab <- renderDataTable({
    datatable(details(), rownames = FALSE, options = list(dom = 't')) %>% 
      formatStyle('Species',
                  backgroundColor = "lightblue")
    
  })
  
  
  ### species MAP
  species_map <- reactive({
    last_date_tib %>% filter(binomial == values$species)
  })
  
  output$species_map <- renderLeaflet({
    if (is.null(values$species)) return()
    leaflet(species_map()) %>% 
      addProviderTiles("OpenStreetMap.HOT",
                       options = providerTileOptions(maxZoom = 5)) %>%
      
      addCircleMarkers(
        radius = ~ifelse(category %in% threatened, 8, ifelse(category %in% near_threatened,6,4)),
        color = ~pal(category),
        stroke = FALSE, fillOpacity = 0.5,
        popup = paste(species_map()$binomial, "<br/>Class: ",
                      species_map()$class, "<br/>Order: ",
                      species_map()$order, "<br/>Family: ",
                      species_map()$family, "<br/>Genus: ",
                      species_map()$genus, "<br/>Last observation: ",
                      species_map()$event_year)) %>%
      leaflet::addLegend(pal = pal,
                values = factor(category, levels=cat_choice),
                title = "Risk Categories",
                opacity = 1)
  })
    
  
  
  
  
  ################ AREA ###############
  ## map
  areas <- reactive({
    last_date_tib %>% filter(area == input$area1,
                             kingdom %in% as.character(input$kingdom1),
                             category %in% as.character(input$category1))
  })
  
  output$area_map <- renderLeaflet({
    leaflet(areas()) %>% 
      addProviderTiles("OpenStreetMap.HOT") %>%
      
      addCircleMarkers(
        radius = ~ifelse(category %in% threatened, 8, ifelse(category %in% near_threatened,6,4)),
        color = ~pal(category),
        stroke = FALSE, fillOpacity = 0.5,
        popup = paste(areas()$binomial, "<br/>Class: ",
                      areas()$class, "<br/>Order: ",
                      areas()$order, "<br/>Family: ",
                      areas()$family, "<br/>Genus: ",
                      areas()$genus, "<br/>Last observation: ",
                      areas()$event_year)) %>%
      leaflet::addLegend(pal = pal,
                values = factor(category, levels=cat_choice),
                title = "Risk Categories",
                opacity = 1)
    
  })
  
  ### area NAME
  output$selected_area <- renderText(input$area1)
  
  ## BAR threat
  var <- reactive({
    if ( "class" %in% input$class_phyl) return(last_date_tib$class)
    if ( "phylum" %in% input$class_phyl) return(last_date_tib$phylum)
  })
  
  type <- reactive({
    if ( "class" %in% input$class_phyl) return("class")
    if ( "phylum" %in% input$class_phyl) return("phylum")
  })
  area <- reactive({
    if ( "Africa" %in% input$area1) return("Africa")
    if ( "America" %in% input$area1) return("America")
    if ( "Europe" %in% input$area1) return("Europe")
    if ( "Asia" %in% input$area1) return("Asia")
    if ( "Oceania" %in% input$area1) return("Oceania")
    if ( "Other" %in% input$area1) return("Other")
  })

  risk <- reactive({
    last_date_tib %>%                            # parallel computation
      mutate(var = factor(var())) %>%
      mutate(category = factor(category,levels=rev(cat_choice))) %>%
      filter(kingdom %in% input$kingdom1) %>%
      group_by(area,category,var) %>%
      count() %>%
      filter(area == input$area1, category != "Least Concern")
  })
  
  totals <- reactive({
    risk() %>%
    group_by(var) %>%
    summarize(total = sum(n))
  })
  
  output$plot2 <- renderPlot({
    ggplot(risk(),aes(x = reorder(var, -n), y = n, fill = category)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(breaks = rev(cat_choice), values = rev(colors)) +
      labs(fill='IUCN Classification') +
    theme_bw() + 
    theme(axis.text=element_text(size=11)) +
    labs(subtitle = paste("By IUCN threat classification and ",type()),
         y = "number of species",
         x = paste(type())) + 
    geom_text(aes(var, total+2, label = total, fill = NULL), data = totals()) +
    coord_flip()
    
  })

  ## pie THREAT CATEGORIES
  cat_pie <- reactive({
    last_date_tib %>%
      filter(kingdom %in% input$kingdom1) %>%
      mutate(threat_level = factor(threat_level)) %>%
      group_by(area,threat_level) %>%
      count() %>%
      filter(area == input$area1)  %>%
      group_by(area) %>%
      mutate(area_perc = n/sum(n))
  })
  
  output$cat_pie <- renderPlot({
    ggplot(na.omit(cat_pie()), aes(x="", y=area_perc, fill=threat_level)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y",start=10) +
      theme_void() +
      scale_fill_manual(values = c("Data Deficient" = "#929E90","Extinct" = "#020103",
                                   "Least Concern" = "#429126",
                                   "Near Threatened" = "#BAFC03", "Threatened" = "orangered1")) +
      labs(fill='IUCN Classification')
  })
  
  
  ## cat TABLE 
  prova <- reactive({
    last_date_tib %>% 
    filter(kingdom %in% input$kingdom1) %>%
    group_by(area,threat_level) %>%
    count() %>% 
    group_by(area) %>%
    filter(area == input$area1) %>%
    adorn_totals("row") %>%
    mutate(threat_level = case_when(threat_level == "-"  ~ "Total observed",
                                    threat_level == "Threatened"  ~ "Threatened",
                                    threat_level == "Least Concern"  ~ "Least Concern",
                                    threat_level == "Data Deficient"  ~ "Data Deficient",
                                    threat_level == "Near Threatened"  ~ "Near Threatened")) %>%
    select(threat_level,n) %>%
    rename(category = threat_level) %>%
    rename(number = n)
  })
  
  # table
  output$table <- renderDataTable(
    datatable(prova(), rownames = FALSE, options = list(dom = 't')) %>% 
      formatStyle('category',
                  backgroundColor = styleEqual(c("Data Deficient","Least Concern","Near Threatened","Threatened","Total observed"), 
                                                                         c("#929E90","#429126","#BAFC03","#EB4F34","#FFFFFF"))))
  
  
}


shinyApp(ui = ui, server = server)







  
