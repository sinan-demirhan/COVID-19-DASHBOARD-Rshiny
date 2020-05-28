
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)
library(ggplot2)

theurl <- "https://www.worldometers.info/coronavirus/"
file<-read_html(theurl)
tables<-html_nodes(file, "table")
table1 <- html_table(tables[1], fill = TRUE)
table1<-data.frame(table1)
table1$Country.Other[7]<-"Others"
table1<-table1[,c(-1,-4,-6)]

for(j in c(1:nrow(table1))){
  if(nchar(table1$TotalCases[j])==0 || table1$TotalCases[j]=="N/A"){
    table1$TotalCases[j]=0
  }
  if(nchar(table1$TotalDeaths[j])==0 || table1$TotalDeaths[j]=="N/A"){
    table1$TotalDeaths[j]=0
  }
  if(nchar(table1$TotalRecovered[j])==0 || table1$TotalRecovered[j]=="N/A"){
    table1$TotalRecovered[j]=0
  }
  if(nchar(table1$ActiveCases[j])==0 || table1$ActiveCases[j]=="N/A"){
    table1$ActiveCases[j]=0
  }
  if(nchar(table1$Serious.Critical[j])==0 || table1$Serious.Critical[j]=="N/A"){
    table1$Serious.Critical[j]=0
  }
  if(nchar(table1$Tot.Cases.1M.pop[j])==0 || table1$Tot.Cases.1M.pop[j]=="N/A"){
    table1$Tot.Cases.1M.pop[j]=0
  }
  if(nchar(table1$Deaths.1M.pop[j])==0 || table1$Deaths.1M.pop[j]=="N/A"){
    table1$Deaths.1M.pop[j]=0
  }
  if(nchar(table1$TotalTests[j])==0 || table1$TotalTests[j]=="N/A"){
    table1$TotalTests[j]=0
  }
  if(nchar(table1$Tests.1M.pop[j])==0 || table1$Tests.1M.pop[j]=="N/A"){
    table1$Tests.1M.pop[j]=0
  }
  
}

  
table1$TotalCases<-as.integer(gsub("\\,", "", table1$TotalCases))
table1$TotalDeaths<-as.integer(gsub("\\,", "", table1$TotalDeaths))
table1$TotalRecovered<-as.integer(gsub("\\,", "", table1$TotalRecovered))
table1$ActiveCases<-as.integer(gsub("\\,", "", table1$ActiveCases))
table1$Serious.Critical<-as.integer(gsub("\\,", "", table1$Serious.Critical))
table1$Tot.Cases.1M.pop<-as.integer(gsub("\\,", "", table1$Tot.Cases.1M.pop))
table1$Deaths.1M.pop<-as.integer(gsub("\\,", "", table1$Deaths.1M.pop))
table1$TotalTests<-as.integer(gsub("\\,", "", table1$TotalTests))
table1$Tests.1M.pop<-as.integer(gsub("\\,", "", table1$Tests.1M.pop))

world_corona<-table1[9:220,1:2]
continents_corona<-table1[1:7,1:4]
colnames(continents_corona)<-c("Continent","Total Case","Total Deaths","Total Recovered")
colnames(world_corona)<-c("state","case")

world_corona$state[world_corona$state == "USA"]<-"United States of America"
world_corona$state[world_corona$state == "Palestine"]<-"West Bank"
world_corona$state[world_corona$state == "Vatican City"]<-"Vatican"
world_corona$state[world_corona$state == "UK"]<-"United Kingdom"
world_corona$state[world_corona$state == "UAE"]<-"United Arab Emirates"
world_corona$state[world_corona$state == "Tanzania"]<-"United Republic of Tanzania"
world_corona$state[world_corona$state == "St. Vincent Grenadines"]<-"Saint Vincent and the Grenadines"
world_corona$state[world_corona$state == "Serbia"]<-"Republic of Serbia"
world_corona$state[world_corona$state == "Turks and Caicos"]<-"Northern Cyprus"
world_corona$state[world_corona$state == "Guinea-Bissau"]<-"Guinea Bissau"
world_corona$state[world_corona$state == "Czechia"]<-"Czech Republic"
world_corona$state[world_corona$state == "Cabo Verde"]<-"Cape Verde"
world_corona$state[world_corona$state == "CAR"]<-"Central African Republic"
world_corona$state[world_corona$state == "British Virgin Islands"]<-"United States Virgin Islands"
world_corona$state[world_corona$state == "North Macedonia"]<-"Macedonia"
world_corona$state[world_corona$state == "Timor-Leste"]<-"East Timor"
world_corona$state[world_corona$state == "DRC"]<-"Democratic Republic of the Congo"
world_corona$state[world_corona$state == "Congo"]<-"Republic of Congo"
world_corona$state[world_corona$state == "Eswatini"]<-"Swaziland"
world_corona$state[world_corona$state == "S. Korea"]<-"South Korea"


corona<-read.csv(url("https://datahub.io/core/covid-19/r/countries-aggregated.csv"))
worldwide<-read.csv(url("https://datahub.io/core/covid-19/r/worldwide-aggregated.csv"))
states<-read.csv(url("https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"))

China_states<-states %>% filter(Country.Region=="China")
China_map<-China_states %>% select(c(1,3,6))
China_map<-China_map %>% filter(Date==as.character(China_map$Date[nrow(China_map)]))
China_map$Province.State<-as.character(China_map$Province.State)
China_map$Province.State[30]<-as.character("Xizang")
China_map$Province.State[16]<-as.character("Inner Mongol")
China_map<-China_map %>% .[,2:3] %>% setNames(c("state", "case"))
China_map$case<-replace(China_map$case,is.na(China_map$case),0)


Canada_states<-states %>% filter(Country.Region=="Canada") 
Canada_map<-Canada_states %>% select(c(1,3,6))
Canada_map<-Canada_map %>% filter(Date==as.character(Canada_map$Date[nrow(Canada_map)]))
Canada_map$Province.State<-as.character(Canada_map$Province.State)
Canada_map$Province.State[12]<-as.character("Québec")
Canada_map<-Canada_map %>% .[,2:3] %>% setNames(c("state", "case"))
Canada_map$case<-replace(Canada_map$case,is.na(Canada_map$case),0)
if(length(Canada_map$state[Canada_map$state == "Nunavut"])==0){
  Canada_map<-rbind(Canada_map,data.frame(state="Nunavut",case=0))
}

Australia_states<-states %>% filter(Country.Region=="Australia") 
Australia_map<-Australia_states %>% select(c(1,3,6))
Australia_map<-Australia_map %>% filter(Date==as.character(Australia_map$Date[nrow(Australia_map)]))
Australia_map<-Australia_map %>% .[,2:3] %>% setNames(c("state", "case"))
Australia_map$case<-replace(Australia_map$case,is.na(Australia_map$case),0)


germany<-read_html("https://www.citypopulation.de/en/germany/covid/")
germany<-html_nodes(germany, "table")
germany <- html_table(germany[1], fill = TRUE)
germany<-data.frame(germany)
germany<-germany[,-c(ncol(germany))]
germany_map<-germany %>% filter(Status=="State") %>% .[,c(1,ncol(germany))]
colnames(germany_map)<-c("state","case")
germany_map$case<-as.numeric(gsub(",", "", germany_map$case))
for(i in c(1:nrow(germany_map))){
  germany_map$state[i]<-strsplit(germany_map$state, " ")[[i]][1]
}
germany_map$case<-replace(germany_map$case,is.na(germany_map$case),0)


uk<-read_html("https://www.citypopulation.de/en/uk/covid/")
uk<-html_nodes(uk, "table")
uk <- html_table(uk[1], fill = TRUE)
uk<-data.frame(uk)
uk<-uk[,-c(ncol(uk))]
uk_map<-uk  %>% .[,c(1,ncol(uk))]
colnames(uk_map)<-c("state","case")
uk_map$case<-as.numeric(gsub(",", "", uk_map$case))
for(i in c(1:nrow(germany_map))){
  uk_map$state[i]<-strsplit(uk_map$state, "\\(")[[i]][1]
}
uk_map$case<-replace(uk_map$case,is.na(uk_map$case),0)


italy<-read_html("https://www.citypopulation.de/en/italy/covid/")
italy<-html_nodes(italy, "table")
italy <- html_table(italy[1], fill = TRUE)
italy<-data.frame(italy)
italy<-italy[,-c(ncol(italy))]
italy_map<-italy %>% .[,c(1,ncol(italy))]
colnames(italy_map)<-c("state","case")
italy_map<-italy_map[1:nrow(italy_map)-1,]
italy_map$case<-as.numeric(gsub(",", "", italy_map$case))
for(i in c(1:nrow(italy_map))){
  italy_map$state[i]<-strsplit(italy_map$state, " \\(")[[i]][1]
  italy_map$state[i]<-strsplit(italy_map$state, " \\[")[[i]][1]
}


italy_map$state[italy_map$state == "Oristano"]<-"Oristrano"
italy_map$state[italy_map$state == "Monza e della Brianza"]<-"Monza e Brianza"
italy_map$state[italy_map$state == "Reggio nell'Emilia"]<-"Reggio Emilia"
italy_map$state[italy_map$state == "Valle d'Aosta"]<-"Aoste"
italy_map$state[italy_map$state == "Torino"]<-"Turin"
italy_map$state[italy_map$state == "Reggio di Calabria"]<-"Reggio Calabria"
italy_map$state[italy_map$state == "Barletta-Andria-Trani"]<-"Barletta-Andria Trani"


my_states_italy<-c("Bozen","Carbonia-Iglesias","Medio Campidano",
                   "Ogliastra","Olbia-Tempio")

for(i in c(1:5)){
  if(length(italy_map$state[italy_map$state == my_states_italy[i]])==0){
    italy_map<-rbind(italy_map,data.frame(state=my_states_italy[i],case=0))
  }
}

italy_map$case<-replace(italy_map$case,is.na(italy_map$case),0)



df_chart<-spread(corona[1:3],Country,Confirmed)


usa_url<-"https://www.worldometers.info/coronavirus/country/us/"
usa_data<-read_html(usa_url)
usa_table<-html_nodes(usa_data, "table")
usa_table <- html_table(usa_table[1], fill = TRUE)
usa_table<-data.frame(usa_table)
usa_table<-usa_table[,-c(ncol(usa_table))]
usa_map<-usa_table[,c(1,2)]
usa_map[,2]<-as.integer(gsub("\\,", "", usa_map[,2]))
usa_map<-usa_map[2:60,]
colnames(usa_map)<-c("state","case")
usa_map$case<-replace(usa_map$case,is.na(usa_map$case),0)


india<-read_html("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_India")
india<-html_nodes(india, "table")
india <- html_table(india[7], fill = TRUE)
india<-data.frame(india)
colnames(india)<-india[1,]
india<-india[c(-1,-38,-39,-40),]
india_map<-india %>% .[,c(2,3)]
colnames(india_map)<-c("state","case")
india_map$state[26]<-"Orissa"

india_map$case<-replace(india_map$case,is.na(india_map$case),0)





treedata<-table1[9:220,c(1,2,3,4,12)]
treedata[,c(2,3,4)]<-replace(treedata[,c(2,3,4)],is.na(treedata[,c(2,3,4)]),0)
treedata$Continent[treedata$Continent == ""] <-"Europe"


labels<-rbind(data.frame(labels=unique(treedata$Continent)),data.frame(labels=treedata$Country.Other))
parents<-rbind(data.frame(parents=c("World","World","World","World","World","World")),data.frame(parents=treedata$Continent))
cases<-rbind(data.frame(values=as.integer(c(0,0,0,0,0,0))),data.frame(values=treedata$TotalCases))
recovered<-rbind(data.frame(values=as.integer(c(0,0,0,0,0,0))),data.frame(values=treedata$TotalRecovered))
deaths<-rbind(data.frame(values=as.integer(c(0,0,0,0,0,0))),data.frame(values=treedata$TotalDeaths))
data_tree <- cbind(labels,parents,cases,recovered,deaths)
data_bar<-rbind(data_tree[7:35,],data_tree[nrow(data_tree),])
data_bar<-data_bar[,c(1,2,4)]



map_choices<-c("World", "USA", "China","Canada","Australia","Germany","United Kingdom","Italy","India")
case_choices<-c("Total Cases","Total Recovered","Total Deaths")

data_to_download<-c("Worldometer Live", "Daily Data" ,"Daily World" ,"China States" ,"Canada States", "Australia States",
                    "Germany States" ,"United Kingdom States", "Italy States", "USA States" ,"India States")

namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

#############################################################################################################

shinyApp(
    ui=bootstrapPage(
        navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                   "COVID-19 TRACKER", id="nav",
                   
                   tabPanel("COVID-19 MAPS",
                            sidebarLayout(
                                sidebarPanel(
                                    
                                    pickerInput("selected_country", "Map:",   
                                                choices =map_choices , 
                                                selected = c("World"),
                                                multiple = FALSE),
                                    br(),
                                    pickerInput("case_choice", "Type:",   
                                                choices = case_choices,
                                                selected = "Total Cases",
                                                multiple = FALSE), 
                                    br(),
                                    tableOutput("data1"),width = 2
                                ),
                                
                                mainPanel(
                                    highchartOutput("country_map", width = "120%", height = "800px")
                                )
                            )
                            
                   ),
                   
                   tabPanel("COUNTRY COMPARISON",
                            
                            sidebarLayout(
                                sidebarPanel(
                                    
                                    uiOutput("group_first"),br(),
                                    sliderInput("time", 
                                                label = "Range of timeline for the first plot:",
                                                min = 1, max = nrow(df_chart), value = c(nrow(df_chart)-10, nrow(df_chart))),
                                    br(),uiOutput("group_second"),
                                    sliderInput("gaps", 
                                                label = "Range of timeline for the second plot:",
                                                min = 1, max = nrow(df_chart), value = c(nrow(df_chart)-10, nrow(df_chart))),width = 2
                                ),
                                
                                mainPanel(
                                  fluidRow(
                                    box(solidHeader = TRUE,background = NULL,
                                        plotlyOutput("country_plot_cumulative",height = "500px",width = "1500px"))),
                                  fluidRow(
                                    box(solidHeader = TRUE,background = NULL,
                                        plotlyOutput("last_ten_days",height = "400px",width = "650px")),
                                    box(solidHeader = TRUE,background = NULL,plotlyOutput("gap_plot",height = "400px",width = "800px"))
                                  )
                                  
                                )
                            )
                   ),
                   tabPanel("CONTINENTS",
                            
                            sidebarLayout(
                                sidebarPanel(
                                  
                                    pickerInput("continents", "Type:",as.character(colnames(continents_corona)[2:4]),
                                                selected = c("Total Case"),
                                                multiple = FALSE),
                                    br(),
                                    tableOutput("continent_table"),width = 2
                                ),
                                
                                mainPanel(
                                  fluidRow(
                                    box(solidHeader = TRUE,plotlyOutput("treemap",height = "700px",width = "650px")),
                                    box(solidHeader = TRUE,plotlyOutput("barchart",height = "700px",width = "900px"))
                                  )
                                )
                            )
                   ),
                   tabPanel("DOWNLOAD DATA",
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                pickerInput("selected_data", "Data",data_to_download,
                                            selected = c("Worldometer Live"),
                                            multiple = FALSE),br(),
                                downloadButton("downloadData", "Download"),br(),
                                "Some of data may have been updated one or two days ago",width = 3
                              ),
                              
                              mainPanel(
                                DT::dataTableOutput("mytable")
                              )
                            )
                   )
                   
        )
    ),

    server=function(input, output) {
      
      output$group_first <- renderUI({  
        var.opts<-namel(colnames(df_chart))[2:ncol(df_chart)]
        pickerInput("group","Country:", var.opts,
                    selected = c("United Kingdom"))				 
      }) 
      
      output$group_second <- renderUI({  
        var.opts<-namel(colnames(df_chart))[2:ncol(df_chart)]
        pickerInput("group_two","Country:", var.opts,
                    selected = c("United Kingdom"))				 
      }) 
      
      world_data<-reactive({
        if(input$case_choice=="Total Cases"){
          dunya<-table1[1:8,1:2]
        }
        if(input$case_choice=="Total Recovered"){
          dunya<-table1[1:8,c(1,4)]
        }
        if(input$case_choice=="Total Deaths"){
          dunya<-table1[1:8,c(1,3)]
        }
        dunya<-dunya %>% rename(Continent = Country.Other)
        return(dunya)
      })
      
      continents_data<-reactive({
        if(input$continents=="Total Case"){
          conts<-continents_corona[,1:2]
        }
        if(input$continents=="Total Recovered"){
          conts<-continents_corona[,c(1,4)]
        }
        if(input$continents=="Total Deaths"){
          conts<-continents_corona[,c(1,3)]
        }
        return(conts)
      })
      

      datasetInput <- reactive({
        switch(input$selected_data,
               "Worldometer Live" = table1,
               "Daily Data" = corona,
               "Daily World" = worldwide,
               "China States" = China_states,
               "Canada States" = Canada_states,
               "Australia States" = Australia_states,
               "Germany States" = germany,
               "United Kingdom States" = uk,
               "Italy States" = italy,
               "USA States" = usa_table,
               "India States" = india)
      })
      
      output$continent_table <- renderTable({
        continents_data()
      })
    
      output$data1 <- renderTable({
        world_data()
      })
      
      output$mytable <- DT::renderDataTable({
        DT::datatable(datasetInput(), options = list(lengthMenu = c(10, 20, 30), pageLength = 15))
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$selected_data, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
      
      output$country_plot_cumulative <- renderPlotly({
        require(input$group)
        plot_ly(df_chart, x =~Date,y =~Turkey, name = 'Turkey', type = 'scatter', mode = 'lines+markers') %>% 
          add_trace(y = ~Canada, name = 'Canada', mode = 'lines+markers') %>% 
          add_trace(y = ~Italy, name = 'Italy', mode = 'lines+markers') %>%
          add_trace(y = ~China, name = 'China', mode = 'lines+markers') %>%
          add_trace(y = ~Spain, name = 'Spain', mode = 'lines+markers') %>%
          add_trace(y = ~France, name = 'France', mode = 'lines+markers') %>%
          add_trace(y = ~Germany, name = 'Germany', mode = 'lines+markers') %>%
          add_trace(y = ~get(as.character(input$group)), name = as.character(input$group), mode = 'lines+markers') %>%
          layout(title = "Case Comparison Over Countries",
                                yaxis = list (title = "Case Numbers"))

        
      })
      
      
      output$last_ten_days<-renderPlotly({
        require(input$group)
        my_df_ten<-corona %>% filter(Country %in% input$group) %>% .[,c("Date","Country","Confirmed","Recovered","Deaths")]
        my_df_ten<-my_df_ten[input$time[1]:input$time[2],]
      
        fig <- plot_ly(my_df_ten, x = ~Date, y = ~Confirmed, type = 'scatter',mode = 'lines+markers', name = 'Confirmed')
        fig <- fig %>% add_trace(y = ~Recovered,mode = 'lines+markers', name = 'Recovered')
        fig <- fig %>% add_trace(y = ~Deaths,mode = 'lines+markers', name = 'Deaths')
        fig <- fig %>% layout(title=paste("COVID-19 Timeline of",input$group),
                              yaxis = list(title = 'Case Numbers'), barmode = 'group')
        
        fig
      })
      
      output$gap_plot<-renderPlotly({
        my_counts<-corona %>% filter(Country %in% c("Turkey","Canada","Italy","China","Spain","France","Germany",input$group_two))
        my_counts<-spread(my_counts[1:3],Date,Confirmed)
        my_counts<-my_counts[,c(1,input$gaps[1]+1,input$gaps[2]+1)]
        first_date<-colnames(my_counts)[2]
        second_date<-colnames(my_counts)[3]
        colnames(my_counts)<-c("Country","first","second")
        my_counts$Country<-as.factor(as.character(my_counts$Country))
        
        fig <- plot_ly(my_counts, color = I("gray80"))
        fig <- fig %>% add_segments(x = ~first, xend = ~second, y = ~Country, yend = ~Country, showlegend = FALSE)
        fig <- fig %>% add_markers(x = ~first, y = ~Country, name = first_date, color = I("blue"))
        fig <- fig %>% add_markers(x = ~second, y = ~Country, name = second_date, color = I("red"))
        fig <- fig %>% layout(
          title = "COVID-19 Timeline of Two Different Date ",
          xaxis = list(title = "Total Cases"),
          margin = list(l = 65)
        )
        
        fig
        
      })
      
      new_tree_data<-reactive({
        if(input$continents =="Total Case"){
          df<-data_tree[,c(1,2,3)]
        }
        if(input$continents =="Total Deaths"){
          df<-data_tree[,c(1,2,5)]
        }
        if(input$continents =="Total Recovered"){
          df<-data_tree[,c(1,2,4)]
        }
        return(df)
      })
      
      
      output$treemap<-renderPlotly({
        fig <- plot_ly(
          type='treemap',
          values=new_tree_data()$values,
          labels=new_tree_data()$labels,
          parents=new_tree_data()$parents,
          textinfo="label+value+percent parent+percent")
        
        fig <- fig %>% layout(uniformtext=list(minsize=10, mode='hide'))
        fig
      })
      
      output$barchart<-renderPlotly({
        data_bar<-rbind(new_tree_data()[7:30,],new_tree_data()[nrow(new_tree_data()),])
        data_bar<- data_bar %>% arrange(desc(data_bar$values))
        data_bar<-data_bar[1:20,]
        fig <- plot_ly(
          x = reorder(data_bar$labels,data_bar[,3]),
          y = data_bar[,3],
          color = data_bar$parents,
          type = "bar"
        )
        
        fig <- fig %>% layout(title = 'Top 20 Countries with continents')
        fig
      })
      
      
      mapping_data <- reactive({
        switch(input$selected_country,
               "World"=world_corona,
               "USA" = usa_map,
               "China" = China_map,
               "Canada" = Canada_map,
               "Australia" = Australia_map,
               "Germany" = germany_map,
               "United Kingdom" = uk_map,
               "Italy" = italy_map,
               "India" = india_map)

        
      })
      
      map_url <- reactive({
        switch(input$selected_country,
               "World"="https://code.highcharts.com/mapdata/custom/world-palestine-highres.js",
               "USA" = "https://code.highcharts.com/mapdata/countries/us/us-all.js",
               "China" = "https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar.js",
               "Canada" = "https://code.highcharts.com/mapdata/countries/ca/ca-all.js",
               "Australia" = "https://code.highcharts.com/mapdata/countries/au/au-all.js",
               "Germany" = "https://code.highcharts.com/mapdata/countries/de/de-all.js",
               "United Kingdom" = "https://code.highcharts.com/mapdata/countries/gb/gb-all.js",
               "Italy" = "https://code.highcharts.com/mapdata/countries/it/it-all.js",
               "India" = "https://code.highcharts.com/mapdata/countries/in/custom/in-all-andaman-and-nicobar.js")
        
        
      })
      output$country_map<-renderHighchart({
        
        
        hcmap(map_url(), data = mapping_data(), value = "case",
              joinBy = c("name", "state"), name = "state",
              dataLabels = list(enabled = TRUE, format = '{point.name}'),
              borderColor = "#FAFAFA", borderWidth = 0.4,
              tooltip = list(valueDecimals = 0)) %>% 
          hc_colorAxis(dataClasses = color_classes(c(0,ceiling(as.integer(quantile(as.integer(mapping_data()$case), probs = c(0.1,0.4,0.75,0.95))[1])/10)*10,
                                                     ceiling(as.integer(quantile(as.integer(mapping_data()$case), probs = c(0.1,0.4,0.75,0.95))[2])/100)*100,
                                                     ceiling(as.integer(quantile(as.integer(mapping_data()$case), probs = c(0.1,0.4,0.75,0.97))[3])/100)*100,
                                                     ceiling(as.integer(quantile(as.integer(mapping_data()$case), probs = c(0.1,0.4,0.75,0.97))[4])/1000)*1000,
                                                     ceiling(as.integer(max(mapping_data()$case))/10000)*10000))) %>% 
          hc_mapNavigation(enabled = T)
        
        
        
      })
        
    })



