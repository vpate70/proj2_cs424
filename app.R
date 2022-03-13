library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(leaflet.providers)
options(scipen=10000)
#load in all data
#_ = ' '
#__ = '/'
#___ = '-'
# changed above to underlines so i could make them variable names and file names
s_Jefferson_Park <- readRDS(file = "rdata/Jefferson_Park.rds")
s_Cermak___Chinatown <- readRDS(file = "rdata/Cermak-Chinatown.rds")
s_Central___Lake <- readRDS(file = "rdata/Central-Lake.rds")
s_Dempster___Skokie <- readRDS(file = "rdata/Dempster-Skokie.rds")
s_Dempster <- readRDS(file = "rdata/Dempster.rds")
s_Lake__State <- readRDS(file = "rdata/Lake__State.rds")
s_Oak_Park___Forest_Park <- readRDS(file = "rdata/Oak_Park-Forest_Park.rds")
s_Kedzie___Homan___Forest_Park <- readRDS(file = "rdata/Kedzie-Homan-Forest_Park.rds")
s_35th__Archer <- readRDS(file = "rdata/35th__Archer.rds")
s_Addison___North_Main <- readRDS(file = "rdata/Addison-North_Main.rds")
s_Main <- readRDS(file = "rdata/Main.rds")
s_Chicago__State <- readRDS(file = "rdata/Chicago__State.rds")
s_Wellington <- readRDS(file = "rdata/Wellington.rds")
s_Austin___Forest_Park <- readRDS(file = "rdata/Austin-Forest_Park.rds")
s_Clinton___Lake <- readRDS(file = "rdata/Clinton-Lake.rds")
s_East_63rd___Cottage_Grove <- readRDS(file = "rdata/East_63rd-Cottage_Grove.rds")
s_Grand__State <- readRDS(file = "rdata/Grand__State.rds")
s_Wilson <- readRDS(file = "rdata/Wilson.rds")
s_Cicero___Cermak <- readRDS(file = "rdata/Cicero-Cermak.rds")
s_State__Lake <- readRDS(file = "rdata/State__Lake.rds")
s_51st <- readRDS(file = "rdata/51st.rds")
s_95th__Dan_Ryan <- readRDS(file = "rdata/95th__Dan_Ryan.rds")
s_Jackson__State <- readRDS(file = "rdata/Jackson__State.rds")
s_Randolph__Wabash <- readRDS(file = "rdata/Randolph__Wabash.rds")
s_Logan_Square <- readRDS(file = "rdata/Logan_Square.rds")
s_Morse <- readRDS(file = "rdata/Morse.rds")
s_Grand__Milwaukee <- readRDS(file = "rdata/Grand__Milwaukee.rds")
s_69th <- readRDS(file = "rdata/69th.rds")
s_Paulina <- readRDS(file = "rdata/Paulina.rds")
s_Damen___Brown <- readRDS(file = "rdata/Damen-Brown.rds")
s_Washington__Dearborn <- readRDS(file = "rdata/Washington__Dearborn.rds")
s_Kimball <- readRDS(file = "rdata/Kimball.rds")
s_Clark__Lake <- readRDS(file = "rdata/Clark__Lake.rds")
s_Lawrence <- readRDS(file = "rdata/Lawrence.rds")
s_Polk <- readRDS(file = "rdata/Polk.rds")
s_47th___Dan_Ryan <- readRDS(file = "rdata/47th-Dan_Ryan.rds")
s_Sedgwick <- readRDS(file = "rdata/Sedgwick.rds")
s_54th__Cermak <- readRDS(file = "rdata/54th__Cermak.rds")
s_Ashland__63rd <- readRDS(file = "rdata/Ashland__63rd.rds")
s_Morgan___Lake <- readRDS(file = "rdata/Morgan-Lake.rds")
s_Harrison <- readRDS(file = "rdata/Harrison.rds")
s_Sheridan <- readRDS(file = "rdata/Sheridan.rds")
s_Racine <- readRDS(file = "rdata/Racine.rds")
s_Washington__Wells <- readRDS(file = "rdata/Washington__Wells.rds")
s_Quincy__Wells <- readRDS(file = "rdata/Quincy__Wells.rds")
s_Foster <- readRDS(file = "rdata/Foster.rds")
s_California__Milwaukee <- readRDS(file = "rdata/California__Milwaukee.rds")
s_Cermak___McCormick_Place <- readRDS(file = "rdata/Cermak-McCormick_Place.rds")
s_Sox___35th___Dan_Ryan <- readRDS(file = "rdata/Sox-35th-Dan_Ryan.rds")
s_Chicago__Milwaukee <- readRDS(file = "rdata/Chicago__Milwaukee.rds")
s_OHare_Airport <- readRDS(file = "rdata/O'Hare_Airport.rds")
s_Kedzie___Lake <- readRDS(file = "rdata/Kedzie-Lake.rds")
s_Fullerton <- readRDS(file = "rdata/Fullerton.rds")
s_Irving_Park___Brown <- readRDS(file = "rdata/Irving_Park-Brown.rds")
s_LaSalle__Van_Buren <- readRDS(file = "rdata/LaSalle__Van_Buren.rds")
s_Belmont___North_Main <- readRDS(file = "rdata/Belmont-North_Main.rds")
s_79th <- readRDS(file = "rdata/79th.rds")
s_Adams__Wabash <- readRDS(file = "rdata/Adams__Wabash.rds")
s_Western___Orange <- readRDS(file = "rdata/Western-Orange.rds")
s_Clinton___Forest_Park <- readRDS(file = "rdata/Clinton-Forest_Park.rds")
s_UIC___Halsted <- readRDS(file = "rdata/UIC-Halsted.rds")
s_35___Bronzeville___IIT <- readRDS(file = "rdata/35-Bronzeville-IIT.rds")
s_87th <- readRDS(file = "rdata/87th.rds")
s_18th <- readRDS(file = "rdata/18th.rds")
s_Indiana <- readRDS(file = "rdata/Indiana.rds")
s_Monroe__State <- readRDS(file = "rdata/Monroe__State.rds")
s_Irving_Park___OHare <- readRDS(file = "rdata/Irving_Park-O'Hare.rds")
s_Cumberland <- readRDS(file = "rdata/Cumberland.rds")
s_Roosevelt <- readRDS(file = "rdata/Roosevelt.rds")
s_Damen__Milwaukee <- readRDS(file = "rdata/Damen__Milwaukee.rds")
s_Kedzie___Midway <- readRDS(file = "rdata/Kedzie-Midway.rds")
s_63rd___Dan_Ryan <- readRDS(file = "rdata/63rd-Dan_Ryan.rds")
s_Kedzie___Cermak <- readRDS(file = "rdata/Kedzie-Cermak.rds")
s_Addison___Brown <- readRDS(file = "rdata/Addison-Brown.rds")
s_Division__Milwaukee <- readRDS(file = "rdata/Division__Milwaukee.rds")
s_Damen___Cermak <- readRDS(file = "rdata/Damen-Cermak.rds")
s_Cicero___Lake <- readRDS(file = "rdata/Cicero-Lake.rds")
s_Madison__Wabash <- readRDS(file = "rdata/Madison__Wabash.rds")
s_Harlem___Lake <- readRDS(file = "rdata/Harlem-Lake.rds")
s_Pulaski___Cermak <- readRDS(file = "rdata/Pulaski-Cermak.rds")
s_Kedzie___Brown <- readRDS(file = "rdata/Kedzie-Brown.rds")
s_Central_Park <- readRDS(file = "rdata/Central_Park.rds")
s_Harlem___OHare <- readRDS(file = "rdata/Harlem-O'Hare.rds")
s_Chicago__Franklin <- readRDS(file = "rdata/Chicago__Franklin.rds")
s_North__Clybourn <- readRDS(file = "rdata/North__Clybourn.rds")
s_Berwyn <- readRDS(file = "rdata/Berwyn.rds")
s_Laramie <- readRDS(file = "rdata/Laramie.rds")
s_Howard <- readRDS(file = "rdata/Howard.rds")
s_Granville <- readRDS(file = "rdata/Granville.rds")
s_Western___Forest_Park <- readRDS(file = "rdata/Western-Forest_Park.rds")
s_California___Cermak <- readRDS(file = "rdata/California-Cermak.rds")
s_Ridgeland <- readRDS(file = "rdata/Ridgeland.rds")
s_Western___Cermak <- readRDS(file = "rdata/Western-Cermak.rds")
s_Halsted__63rd <- readRDS(file = "rdata/Halsted__63rd.rds")
s_Pulaski___Forest_Park <- readRDS(file = "rdata/Pulaski-Forest_Park.rds")
s_Montrose___Brown <- readRDS(file = "rdata/Montrose-Brown.rds")
s_Linden <- readRDS(file = "rdata/Linden.rds")
s_Pulaski___Lake <- readRDS(file = "rdata/Pulaski-Lake.rds")
s_Harlem___Forest_Park <- readRDS(file = "rdata/Harlem-Forest_Park.rds")
s_Ashland___Orange <- readRDS(file = "rdata/Ashland-Orange.rds")
s_Garfield___Dan_Ryan <- readRDS(file = "rdata/Garfield-Dan_Ryan.rds")
s_Halsted___Orange <- readRDS(file = "rdata/Halsted-Orange.rds")
s_Addison___OHare <- readRDS(file = "rdata/Addison-O'Hare.rds")
s_Pulaski___Orange <- readRDS(file = "rdata/Pulaski-Orange.rds")
s_Noyes <- readRDS(file = "rdata/Noyes.rds")
s_47th___South_Elevated <- readRDS(file = "rdata/47th-South_Elevated.rds")
s_Merchandise_Mart <- readRDS(file = "rdata/Merchandise_Mart.rds")
s_Midway_Airport <- readRDS(file = "rdata/Midway_Airport.rds")
s_43rd <- readRDS(file = "rdata/43rd.rds")
s_Western__Milwaukee <- readRDS(file = "rdata/Western__Milwaukee.rds")
s_Ashland___Lake <- readRDS(file = "rdata/Ashland-Lake.rds")
s_Belmont___OHare <- readRDS(file = "rdata/Belmont-O'Hare.rds")
s_Oak_Park___Lake <- readRDS(file = "rdata/Oak_Park-Lake.rds")
s_Conservatory <- readRDS(file = "rdata/Conservatory.rds")
s_Library <- readRDS(file = "rdata/Library.rds")
s_Loyola <- readRDS(file = "rdata/Loyola.rds")
s_Southport <- readRDS(file = "rdata/Southport.rds")
s_Montrose___OHare <- readRDS(file = "rdata/Montrose-O'Hare.rds")
s_Jarvis <- readRDS(file = "rdata/Jarvis.rds")
s_South_Boulevard <- readRDS(file = "rdata/South_Boulevard.rds")
s_Cicero___Forest_Park <- readRDS(file = "rdata/Cicero-Forest_Park.rds")
s_Medical_Center <- readRDS(file = "rdata/Medical_Center.rds")
s_Davis <- readRDS(file = "rdata/Davis.rds")
s_Clark__Division <- readRDS(file = "rdata/Clark__Division.rds")
s_Jackson__Dearborn <- readRDS(file = "rdata/Jackson__Dearborn.rds")
s_Washington__Wabash <- readRDS(file = "rdata/Washington__Wabash.rds")
s_Francisco <- readRDS(file = "rdata/Francisco.rds")
s_Central___Evanston <- readRDS(file = "rdata/Central-Evanston.rds")
s_Oakton___Skokie <- readRDS(file = "rdata/Oakton-Skokie.rds")
s_Austin___Lake <- readRDS(file = "rdata/Austin-Lake.rds")
s_Bryn_Mawr <- readRDS(file = "rdata/Bryn_Mawr.rds")
s_Kostner <- readRDS(file = "rdata/Kostner.rds")
s_Forest_Park <- readRDS(file = "rdata/Forest_Park.rds")
s_California___Lake <- readRDS(file = "rdata/California-Lake.rds")
s_Garfield___South_Elevated <- readRDS(file = "rdata/Garfield-South_Elevated.rds")
s_Rockwell <- readRDS(file = "rdata/Rockwell.rds")
s_Diversey <- readRDS(file = "rdata/Diversey.rds")
s_Argyle <- readRDS(file = "rdata/Argyle.rds")
s_LaSalle <- readRDS(file = "rdata/LaSalle.rds")
s_Monroe__Dearborn <- readRDS(file = "rdata/Monroe__Dearborn.rds")
s_Rosemont <- readRDS(file = "rdata/Rosemont.rds")
s_King_Drive <- readRDS(file = "rdata/King_Drive.rds")
s_Armitage <- readRDS(file = "rdata/Armitage.rds")
s_Thorndale <- readRDS(file = "rdata/Thorndale.rds")
s_Western___Brown <- readRDS(file = "rdata/Western-Brown.rds")
s_Skokie <- readRDS(file = "rdata/Skokie.rds")
s_Washington__State <- readRDS(file = "rdata/Washington__State.rds")
s_Homan <- readRDS(file = "rdata/Homan.rds")
allstationdf <-list(s_Jefferson_Park,s_Cermak___Chinatown,s_Central___Lake,s_Dempster___Skokie,s_Dempster,s_Lake__State,s_Oak_Park___Forest_Park,s_Kedzie___Homan___Forest_Park,s_35th__Archer,s_Addison___North_Main,s_Main,s_Chicago__State,s_Wellington,s_Austin___Forest_Park,s_Clinton___Lake,s_East_63rd___Cottage_Grove,s_Grand__State,s_Wilson,s_Cicero___Cermak,s_State__Lake,s_51st,s_95th__Dan_Ryan,s_Jackson__State,s_Randolph__Wabash,s_Logan_Square,s_Morse,s_Grand__Milwaukee,s_69th,s_Paulina,s_Damen___Brown,s_Washington__Dearborn,s_Kimball,s_Clark__Lake,s_Lawrence,s_Polk,s_47th___Dan_Ryan,s_Sedgwick,s_54th__Cermak,s_Ashland__63rd,s_Morgan___Lake,s_Harrison,s_Sheridan,s_Racine,s_Washington__Wells,s_Quincy__Wells,s_Foster,s_California__Milwaukee,s_Cermak___McCormick_Place,s_Sox___35th___Dan_Ryan,s_Chicago__Milwaukee,s_OHare_Airport,s_Kedzie___Lake,s_Fullerton,s_Irving_Park___Brown,s_LaSalle__Van_Buren,s_Belmont___North_Main,s_79th,s_Adams__Wabash,s_Western___Orange,s_Clinton___Forest_Park,s_UIC___Halsted,s_35___Bronzeville___IIT,s_87th,s_18th,s_Indiana,s_Monroe__State,s_Irving_Park___OHare,s_Cumberland,s_Roosevelt,s_Damen__Milwaukee,s_Kedzie___Midway,s_63rd___Dan_Ryan,s_Kedzie___Cermak,s_Addison___Brown,s_Division__Milwaukee,s_Damen___Cermak,s_Cicero___Lake,s_Madison__Wabash,s_Harlem___Lake,s_Pulaski___Cermak,s_Kedzie___Brown,s_Central_Park,s_Harlem___OHare,s_Chicago__Franklin,s_North__Clybourn,s_Berwyn,s_Laramie,s_Howard,s_Granville,s_Western___Forest_Park,s_California___Cermak,s_Ridgeland,s_Western___Cermak,s_Halsted__63rd,s_Pulaski___Forest_Park,s_Montrose___Brown,s_Linden,s_Pulaski___Lake,s_Harlem___Forest_Park,s_Ashland___Orange,s_Garfield___Dan_Ryan,s_Halsted___Orange,s_Addison___OHare,s_Pulaski___Orange,s_Noyes,s_47th___South_Elevated,s_Merchandise_Mart,s_Midway_Airport,s_43rd,s_Western__Milwaukee,s_Ashland___Lake,s_Belmont___OHare,s_Oak_Park___Lake,s_Conservatory,s_Library,s_Loyola,s_Southport,s_Montrose___OHare,s_Jarvis,s_South_Boulevard,s_Cicero___Forest_Park,s_Medical_Center,s_Davis,s_Clark__Division,s_Jackson__Dearborn,s_Washington__Wabash,s_Francisco,s_Central___Evanston,s_Oakton___Skokie,s_Austin___Lake,s_Bryn_Mawr,s_Kostner,s_Forest_Park,s_California___Lake,s_Garfield___South_Elevated,s_Rockwell,s_Diversey,s_Argyle,s_LaSalle,s_Monroe__Dearborn,s_Rosemont,s_King_Drive,s_Armitage,s_Thorndale,s_Western___Brown,s_Skokie,s_Washington__State,s_Homan)
dfMerge <- do.call("rbind", allstationdf)
dfMerge <- dfMerge[order(dfMerge$stationname),]

latlonstation <- read.csv(file = 'data/lonlat.csv')

x_con <- sort(c("Daily", "Monthly", "Week Day", "All For Year", "Default"))
tflist <- c("FALSE","TRUE")

minDate <- min(dfMerge$updated_date)
maxDate <- max(dfMerge$updated_date)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2022 Project 2"),
  #edit to make mini menu items for both
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                  
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)
                     ),
                   #Options for right graphs
                   menuItem("Right options",
                            selectInput("rstation_name", "Select the station name", unique(dfMerge$stationname), selected = "18th"),
                            selectInput("rtype_x", "Select the constraint", x_con, selected = "Default"),
                            selectInput("rtableCheck", "Show Table Values",tflist, selected = "FALSE"),
                            conditionalPanel(
                              condition = "input.rtype_x != 'Default'",
                              selectInput("rYear", "Select the year to visualize", unique(dfMerge[order(-dfMerge$the_year),]$the_year), selected = 2021)
                            )
                   ),
                   #Option to change page to about section
                   menuItem("Page options",
                            selectInput("pageOption", "Select page", c("Data","About"), selected = "Data")
                   ),
                   menuItem("Left Options",
                   actionButton("reset_button", "Reset Map View"),
                   selectInput("longGraphOrder", "Order By", c("Alphabetical","Minimum","Maximum"), selected = "Alphabetical"),
                   selectInput("datesChange", "Date Constraints",c("One Day", "Two Day Differ"), selected = "One Day"),
                   conditionalPanel(
                     condition = "input.datesChange == 'One Day'",
                     actionButton("prev_day", "Prev Day"),
                     actionButton("next_day", "Next Day")
                   ),
                   dateInput("date1",label = "date1", value = "2021-8-23", min = minDate, max = maxDate),
                   conditionalPanel(
                    condition = "input.datesChange == 'Two Day Differ'",
                    dateInput("date2",label = "date2",value = "2021-8-22", min = minDate, max = maxDate)
                   )
                   )
  ),
  
  dashboardBody(
    #Data page, only show when page option has data selected
    conditionalPanel(
        condition = "input.pageOption == 'Data'",
        fluidRow(
          column(6,
                 fluidRow(
                   conditionalPanel(
                     condition = "input.datesChange == 'Two Day Differ'",
                     plotOutput("main2",width="100%")
                   ),
                   conditionalPanel(
                     condition = "input.datesChange == 'One Day'",
                     plotOutput("main",width="100%")
                   )
                   
                 ),
                 fluidRow(
                   tags$style(type = "text/css", "#leaf {height: calc(65vh - 80px) !important;}"),
                   splitLayout(
                     cellWidths = c('80%','20%'),
                     leafletOutput("leaf"),
                     verticalLayout(
                       conditionalPanel(
                         condition = "input.datesChange == 'Two Day Differ'",
                         dataTableOutput("tab2")
                       ),
                       conditionalPanel(
                         condition = "input.datesChange == 'One Day'",
                         dataTableOutput("tab1")
                       )
                     )
                   )
                 ),
                 
          ),
          
          column(6,
                 uiOutput("rightUI")
          )
         
        )
    ),
    #About page, only show when page option about is selected
    conditionalPanel(
      condition = "input.pageOption == 'About'",
      fluidRow(
        h1("About Page"),
        p("The data is from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
        p("and https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
        p("Vivek Patel wrote this application."),
        p("Created: Spring 2022, March"),
        p("The application was created for Project 2 of Spring 2022 CS 424 with Dr. Johnson")
      )
    )
  )
)


# Define server logic
server <- function(input, output,session) {
  stationReactive <- reactive({
    # input$rstationname
    for(sdf in allstationdf){
      if(sdf$stationname[1] == input$rstation_name){
        return(sdf)
      }
    }
  })
  
  
  dateOneReactive <- reactive({
    return(input$date1)
  })
  
  
  orderByReactive <- reactive({
    df <- subset(dfMerge, updated_date == input$date1)
    if(input$longGraphOrder == "Alphabetical"){
      return(
        ggplot(df, aes(x=stationname,y=rides)) + geom_bar( stat='identity', fill='steelblue') + 
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())))) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
    else if(input$longGraphOrder == "Minimum"){
      return(
        ggplot(df, aes(reorder(stationname, rides),rides)) + geom_bar( stat='identity', fill='steelblue') + 
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())))) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
    else{
      return(
        ggplot(df, aes(reorder(stationname, -rides),rides)) + geom_bar( stat='identity', fill='steelblue') + 
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())))) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
  })
  
  diffReactive <- reactive({
    df <- changeDF()
    df <- df[!duplicated(df), ]
    df$colour <- ifelse(df$rides < 0,"negative","positive")
    
    if(input$longGraphOrder == "Alphabetical"){
      return(
        ggplot(df, aes(x=stationname,y=rides)) + geom_bar( stat='identity', aes(fill=colour)) + scale_fill_manual(values=c(positive="steelblue",negative="firebrick3"))+
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())),'and',wday(ymd(input$date2),label=TRUE),month(ymd(input$date2),label=TRUE),day(ymd(input$date2)),year(ymd(input$date2)) )) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
    else if(input$longGraphOrder == "Minimum"){
      return(
        ggplot(df, aes(reorder(stationname, rides),rides)) + geom_bar( stat='identity', aes(fill=colour)) + scale_fill_manual(values=c(positive="steelblue",negative="firebrick3"))+
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())),'and',wday(ymd(input$date2),label=TRUE),month(ymd(input$date2),label=TRUE),day(ymd(input$date2)),year(ymd(input$date2)) )) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
    else{
      return(
        ggplot(df, aes(reorder(stationname, -rides),rides)) + geom_bar( stat='identity', aes(fill=colour)) + scale_fill_manual(values=c(positive="steelblue",negative="firebrick3"))+
          labs(x="Station", y="Rides")+ scale_y_continuous(label=comma) +ggtitle(label = paste(wday(ymd(dateOneReactive()),label=TRUE),month(ymd(dateOneReactive()),label=TRUE),day(ymd(dateOneReactive())),year(ymd(dateOneReactive())),'and',wday(ymd(input$date2),label=TRUE),month(ymd(input$date2),label=TRUE),day(ymd(input$date2)),year(ymd(input$date2)) )) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
          )
      )
    }
  })
  
  mapReactive <- reactive({
    df<-subset(dfMerge, updated_date == input$date1)
    df2 <- data.frame(latlonstation$STATION_NAME,as.numeric(latlonstation$Lat), as.numeric(latlonstation$Long), as.numeric(latlonstation$MAP_ID))
    colnames(df2)<-c('stationname','Lat','Long', 'station_id')
    df <- merge(df, df2, by = "station_id")
    df<- data.frame(as.numeric(df$station_id), df$stationname.x, as.numeric(df$rides),as.numeric(df$Lat),as.numeric(df$Long))
    colnames(df) <- c('station_id','stationname','rides','Lat','Long')
    df <- df[!duplicated(df), ]
    return(df)
  })
  
  mapChangeDF <- reactive({
    df2 <- data.frame(latlonstation$STATION_NAME,as.numeric(latlonstation$Lat), as.numeric(latlonstation$Long), as.numeric(latlonstation$MAP_ID))
    colnames(df2)<-c('stationname','Lat','Long', 'station_id')
    df <- changeDF()
    df <- merge(df, df2, by = "station_id")
    return(df)
  })
  
  changeDF <- reactive({
    df1 <- subset(dfMerge, updated_date == input$date1)
    df1 <- data.frame(df1$rides,df1$stationname,df1$station_id)
    colnames(df1) = c("rides","stationname","station_id")
    df2 <- subset(dfMerge, updated_date == input$date2)
    df2 <- data.frame(df2$rides,df2$stationname,df2$station_id)
    
    colnames(df2) = c("rides","stationname","station_id")
    stn <- setdiff(df1$stationname,df2$stationname)
    for(p in stn){
      df2[nrow(df2) + 1,] <- c(0,p,dfMerge[dfMerge$stationname == p,]$station_id[1])
    }
    stn <- setdiff(df2$stationname,df1$stationname)
    for(x in stn){
      df1[nrow(df1) + 1,] <- c(0,x,dfMerge[dfMerge$stationname == x,]$station_id[1])
    }
    df <- merge(df1, df2, by = "station_id")
    
    df$rides <- as.numeric(df$rides.x) - as.numeric(df$rides.y)
    colnames(df) = c("station_id","ridex","stationname","ridey","stationname2","rides")
    return(df)
  })
  
  tableChange <- reactive({
    df <- negPos()
    df <- data.frame(df$stationname.x,as.numeric(df$rides))
    colnames(df) <- c('stationname','rides_difference')
    return(df)
  })
  
  negPos <- reactive({
    df3 <- data.frame(latlonstation$STATION_NAME,as.numeric(latlonstation$Lat), as.numeric(latlonstation$Long), as.numeric(latlonstation$MAP_ID))
    colnames(df3)<-c('stationname','Lat','Long', 'station_id')
    df1 <- subset(dfMerge, updated_date == input$date1)
    df1 <- data.frame(df1$rides,df1$stationname,df1$station_id)
    colnames(df1) = c("rides","stationname","station_id")
    df2 <- subset(dfMerge, updated_date == input$date2)
    df2 <- data.frame(df2$rides,df2$stationname,df2$station_id)
    
    colnames(df2) = c("rides","stationname","station_id")
    stn <- setdiff(df1$stationname,df2$stationname)
    for(p in stn){
      df2[nrow(df2) + 1,] <- c(0,p,dfMerge[dfMerge$stationname == p,]$station_id[1])
    }
    stn <- setdiff(df2$stationname,df1$stationname)
    for(x in stn){
      df1[nrow(df1) + 1,] <- c(0,x,dfMerge[dfMerge$stationname == x,]$station_id[1])
    }
    df <- merge(df1, df2, by = "station_id")
    
    df$rides <- as.numeric(df$rides.x) - as.numeric(df$rides.y)
    colnames(df) = c("station_id","ridex","stationname","ridey","stationname2","rides")
    df <- data.frame(as.numeric(df$station_id), df$stationname,as.numeric(df$rides))
    colnames(df) <- c("station_id","stationname","rides")
    
    df <- merge(df, df3, by = "station_id")
    df<-df[!duplicated(df), ]
    return(df)
  })
  
  output$tab1 <- DT::renderDataTable(
    subset(dfMerge, updated_date == dateOneReactive())[, c("stationname", "rides")], 
    options = list(searching = FALSE, pageLength = 20, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
  )
  
  output$tab2 <- DT::renderDataTable(
    tableChange(), 
    options = list(searching = FALSE, pageLength = 20, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
  )
  
    output$main <- renderPlot({
      orderByReactive()
    }
      
    )  
    
    output$main2 <- renderPlot({
      diffReactive()
    })
    
    rv <- reactiveValues()
    rv$m <- NULL
    rv$p <- NULL
    
    output$leaf <- renderLeaflet({
      if(input$datesChange == 'One Day'){
        df <- mapReactive()
        map <- leaflet()
        map <- addTiles(map)
        new_zoom <- 11.3
        if(!is.null(input$leaf_zoom)) new_zoom <- input$leaf_zoom
        map <- setView(map, lng = -87.683177, lat = 41.921832, zoom = new_zoom)
        map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname, layerId = df$stationname, icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = 'blue'
        ))
        
        removeMarker(map = leafletProxy(mapId = "leaf", session), layerId = input$rstation_name)
        addAwesomeMarkers(map = leafletProxy(mapId = "leaf", session),
                          lng = df[df$stationname == input$rstation_name,]$Long[1],
                          lat = df[df$stationname == input$rstation_name,]$Lat[1],
                          layerId = input$rstation_name,
                          icon = awesomeIcons(
                            icon = 'ios-close',
                            iconColor = 'white',
                            library = 'ion',
                            markerColor = 'red'
                          ))
        
        map <- addCircles(map,lng = df$Long, lat = df$Lat, weight = 1,
                   radius = sqrt(df$rides) * 25
        )
        map <- addTiles(map = map, urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}", group = "Default")
        map <- addTiles(map = map, urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png", group = "Dark")
        map <- addTiles(map = map, urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png", group = "Light")
        map <- addLayersControl(map = map,
          baseGroups = c("Default", "Dark", "Light"),
          options = layersControlOptions(collapsed = FALSE)
        )
        map <- addLegend(map = map,position = "bottomright", colors = c("red","blue"), labels = c("negative","positive"))
        rv$m <- map
        map
      }
      else{
        df <- negPos()
        df$colour <- ifelse(df$rides < 0,"red","blue")
        map <- leaflet()
        map <- addTiles(map)
        if(!is.null(input$leaf_zoom)) new_zoom <- input$leaf_zoom
        map <- setView(map, lng = -87.683177, lat = 41.921832, zoom = new_zoom)
        map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname.x, layerId = df$stationname.x, icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = 'blue'
        ))
        
        removeMarker(map = leafletProxy(mapId = "leaf", session), layerId = input$rstation_name)
        addAwesomeMarkers(map = leafletProxy(mapId = "leaf", session),
                          lng = df[df$stationname.x == input$rstation_name,]$Long[1],
                          lat = df[df$stationname.x == input$rstation_name,]$Lat[1],
                          layerId = input$rstation_name,
                          icon = awesomeIcons(
                            icon = 'ios-close',
                            iconColor = 'white',
                            library = 'ion',
                            markerColor = 'red'
                          ))
        
        map <- addCircles(map,lng = df$Long, lat = df$Lat, weight = 1,
                          radius = sqrt(abs(df$rides)) * 25, color = df$colour
        )
        map <- addTiles(map = map, urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}", group = "Default")
        map <- addTiles(map = map, urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png", group = "Dark")
        map <- addTiles(map = map, urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png", group = "Light")
        map <- addLayersControl(map = map,
                                baseGroups = c("Default", "Dark", "Light"),
                                options = layersControlOptions(collapsed = FALSE)
        )
        map <- addLegend(map = map,position = "bottomright", colors = c("red","blue"), labels = c("negative","positive"))
        rv$m <- map
        map
      }
    })
    # 
    # colorMarker <- reactive({
    #   map<-leafletProxy(mapId = "leaf",session)
    #   map <- clearMarkers(map)
    #   df <- data.frame()
    #   if(input$datesChange == 'One Day'){
    #     df <- mapReactive()
    #     map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname, layerId = df$stationname, icon = awesomeIcons(
    #       icon = 'ios-close',
    #       iconColor = 'white',
    #       library = 'ion',
    #       markerColor = 'blue'
    #     ))
    #   }
    #   else{
    #     df <- negPos()
    #     map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname.x, layerId = df$stationname.x, icon = awesomeIcons(
    #       icon = 'ios-close',
    #       iconColor = 'white',
    #       library = 'ion',
    #       markerColor = 'blue'
    #     ))
    #   }
    # })
    
    observeEvent(input$leaf_marker_click,{
      clicked_point <- input$leaf_marker_click
      
      map<-leafletProxy(mapId = "leaf",session)
      map <- clearMarkers(map)
      if(input$datesChange == 'One Day'){
        df <- mapReactive()
        map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname, layerId = df$stationname, icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = 'blue'
        ))
      }
      else{
        df <- negPos()
        map <- addAwesomeMarkers(map, lng = df$Long, lat = df$Lat, popup = df$stationname.x, layerId = df$stationname.x, icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = 'blue'
        ))
      }
      removeMarker(map = leafletProxy(mapId = "leaf", session), layerId = clicked_point$id)
      addAwesomeMarkers(map = leafletProxy(mapId = "leaf", session),
                        lng = clicked_point$lng,
                        lat = clicked_point$lat,
                        layerId = clicked_point$id,
                        icon = awesomeIcons(
                          icon = 'ios-close',
                          iconColor = 'white',
                          library = 'ion',
                          markerColor = 'red'
                        ))
      updateSelectInput(session, 'rstation_name', "Select the station name", unique(dfMerge$stationname),
                        selected = clicked_point$id)
    }
    )
    
    observeEvent(input$prev_day,
      {
        updateDateInput( session, "date1", label = "date1", value = (ymd(input$date1) - days(1)), min = minDate,max = maxDate)
      }
      )
    
    observeEvent(input$next_day,
      {
        updateDateInput( session, "date1", label = "date1", value = (ymd(input$date1) + days(1)), min = minDate,max = maxDate)
    })
    
    observeEvent(
      input$reset_button,
      {
      leafletProxy("leaf") %>% setView(lat = 41.921832, lng = -87.683177, zoom = 11.3)
    })
    
    
    output$rightUI <- renderUI({
      if(input$rtableCheck == "FALSE"){
        if(input$rtype_x == "Default")
        {
          verticalLayout(
            renderPlot({
              #Output plot based on what station is selected
              #Add all the ride totals based on entries at selected station for all years, then plot
              rdf <- stationReactive()
              df <- aggregate(rdf$rides, by=list(Category=rdf$the_year), FUN=sum)
              ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
            })
          )
        }
        else if(input$rtype_x == "Week Day"){
            rdf <- stationReactive()
            rdf <- subset(rdf, the_year == input$rYear)
            if(nrow(rdf)==0){
              verticalLayout(
                renderPlot({
                  ggplot(data.frame()) +
                    labs(x="Day", y="Rides") + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
                })
              )
            }
            else{
            df <- aggregate(rdf$rides, by=list(Category=rdf$weekday), FUN=sum)
            verticalLayout(
              renderPlot({
                ggplot(df, aes(x=Category, y=x)) + geom_bar( stat='identity', fill="steelblue") + 
                  labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
              })
            )
            }
        }
        else if(input$rtype_x == "Daily"){
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          df <- data.frame(rdf$updated_date, rdf$rides)
          colnames(df) = c("date","rides")
          verticalLayout(
            renderPlot({
              ggplot(df, aes(x=date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of",input$rYear))
            })
          )
        }
        else if(input$rtype_x == "Monthly"){
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          if(nrow(rdf)==0){
            verticalLayout(
              renderPlot({
                ggplot(data.frame()) +
                  labs(x="Month", y="Rides") + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
              })
            )
          }
         else{
          df <- aggregate(rdf$rides, by=list(Category=rdf$the_month), FUN=sum)
          colnames(df) = c("month","rides")
          verticalLayout(
            renderPlot({
              ggplot(df, aes(x=month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Month", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
            })
          )
        }
        }
        else{
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          if(nrow(rdf)==0){
            df <- data.frame(rdf$updated_date, rdf$rides)
            colnames(df) = c("date","rides")
            verticalLayout(
              renderPlot({
                ggplot(df, aes(x=date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                  labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of",input$rYear))
              }),
              renderPlot({
                ggplot(data.frame()) +
                  labs(x="Day", y="Rides") + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
              }),
              renderPlot({
                ggplot(data.frame()) +
                  labs(x="Month", y="Rides") + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
              })
              
            )
          }
          else {
          df <- data.frame(rdf$updated_date, rdf$rides)
          colnames(df) = c("date","rides")

          df2 <- aggregate(rdf$rides, by=list(Category=rdf$weekday), FUN=sum)
          colnames(df2) = c("weekday","rides")
          
          df3 <- aggregate(rdf$rides, by=list(Category=rdf$the_month), FUN=sum)
          colnames(df3) = c("month","rides")
          
          verticalLayout(
            renderPlot({
              ggplot(df, aes(x=date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of",input$rYear))
            }),
            renderPlot({
              ggplot(df2, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
            }),
            renderPlot({
              ggplot(df3, aes(x=month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Month", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
            })
            
            
          )
        }
        }
      }
      else{
        if(input$rtype_x == "Default")
        {
          rdf <- stationReactive()
          df <- aggregate(rdf$rides, by=list(Category=rdf$the_year), FUN=sum)
          colnames(df) = c("Year","Rides")
          splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              #Output plot based on what station is selected
              #Add all the ride totals based on entries at selected station for all years, then plot
              ggplot(df, aes(x=Year, y=Rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides")+ scale_y_continuous(label=comma)+ggtitle(paste("All time ridership for",input$rstation_name,"Station"))
            }),
            DT::renderDataTable(
              df,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
            
          )
        }
        else if(input$rtype_x == "Week Day"){
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          if(nrow(rdf)==0){
            renderText({paste("No data to show for",input$rstation_name,input$rYear)})
          }
          else{
          df <- aggregate(rdf$rides, by=list(Category=rdf$weekday), FUN=sum)
          colnames(df) = c("weekday","rides")
          splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
            }),
            DT::renderDataTable(
              df,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
          )
        }
        }
        else if(input$rtype_x == "Daily"){
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          df <- data.frame(rdf$updated_date, rdf$rides)
          colnames(df) = c("date","rides")
          splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df, aes(x=date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of",input$rYear))
            }),
            DT::renderDataTable(
              df,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
          )
        }
        else if(input$rtype_x == "Monthly"){
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          if(nrow(rdf)==0){
            renderText({paste("No data to show for",input$rstation_name,input$rYear)})
          }
          else{
          df <- aggregate(rdf$rides, by=list(Category=rdf$the_month), FUN=sum)
          colnames(df) = c("month","rides")
          splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df, aes(x=month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Month", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
            }),
            DT::renderDataTable(
              df,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
          )
        }
        }
        else{
          rdf <- stationReactive()
          rdf <- subset(rdf, the_year == input$rYear)
          if(nrow(rdf)==0){
            renderText({paste("No data to show for",input$rstation_name,input$rYear)})
          }
          else{
          df <- data.frame(rdf$updated_date, rdf$rides)
          colnames(df) = c("date","rides")
          
          df2 <- aggregate(rdf$rides, by=list(Category=rdf$weekday), FUN=sum)
          colnames(df2) = c("weekday","rides")
          
          df3 <- aggregate(rdf$rides, by=list(Category=rdf$the_month), FUN=sum)
          colnames(df3) = c("month","rides")
          
          verticalLayout(
            splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df, aes(x=date, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Date", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of",input$rYear))
            }),
            DT::renderDataTable(
              df,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
            ),
            
            splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df2, aes(x=weekday, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Day", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each day of the week for",input$rYear))
            }),
            DT::renderDataTable(
              df2,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
            ),
            splitLayout(cellWidths = c('80%','20%'),
            renderPlot({
              ggplot(df3, aes(x=month, y=rides)) + geom_bar( stat='identity', fill="steelblue") + 
                labs(x="Month", y="Rides") + scale_y_continuous(label=comma) + ggtitle(paste(input$rstation_name,"each month of",input$rYear))
            }),
            DT::renderDataTable(
              df3,
              options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
              ), rownames = FALSE
            )
            
           )
          )
        }
        }
      }
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
