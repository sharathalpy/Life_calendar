

library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(ggrepel)

# default values for events
# event_def = data.frame(text = c("a","b","c"), date = as.Date(10000:10002))

ui <- shinyUI(fluidPage(
  

  titlePanel("Your life in months"),

  # tags$h1("Revised"),
  "This app helps you to visualise your life in months. It is inspired by this ",
  tags$a(href="https://waitbutwhy.com/2014/05/life-weeks.html", 
         "post"),
  "from ",
  tags$a(href="https://waitbutwhy.com", "waitbutwhy.com."),
  tags$br(),
  "Enter your date of birth and probable life span in the input fields.
  The app shows the time you have lived so far and the remaining time you have 
  (based on the life span entered) in years, months, weeks and days.",
  # tags$br(),
  "Click on the ", tags$b("Plot"), " button to see a calendar-like figure in which each month of your life is shown as a circle. 
  You can also label the figure with events from your life. 
  Choose the number of events you want to add to the figure, and enter the date and description for each event.
  Click on the Plot button to redraw the figure every time you change the inputs.",
  tags$p(),
  
  sidebarLayout(
    sidebarPanel(
      
      dateInput(inputId = "date",
                label = "Enter your date of birth ",
                format = "dd-mm-yyyy",
                # value = as.Date.character("01-01-2000",format = "%d-%m-%Y"),
                value = as.Date.character("01-01-1990",format = "%d-%m-%Y"),
                max = today()),
      
      
      uiOutput(outputId = "age_live"),
      
      h4("Life events"),
      
      numericInput(inputId = "nevents",
                   label = "No. of events",
                   min = 1,
                   max = 15,
                   value = 3),
      
      # selectInput(inputId = "nevents",
      #             label = "No. of events",
      #             choices = 1:15,
      #             selected = 3),
      
      uiOutput(outputId = "events"),
    
      actionButton(inputId = "plot",label = "Plot")
      
    ),
    
    
    mainPanel(
      # textOutput("day"),
      fluidRow( align="left",
                splitLayout(cellWidths = c("49%", "49%"), 
                            verbatimTextOutput("lived"), 
                            verbatimTextOutput("left"))),
      # tags$style(type="text/css", "#day {white-space: pre-wrap;}"),
      p("Each circle in the figure represents a month of your life. Each row has 36 months = 3 years.",align = "center"),
      plotOutput("months_plot")
    )
  )
))



server <- shinyServer(function(input, output) {
  
  
  # no of days lived
  dmy_lived = reactive({
    
    t3 = list()
    # t3$nsecs = difftime(today(),input$date,units = "secs")
    # t3$nmins = difftime(today(),input$date,units = "mins")
    # t3$nhours = difftime(today(),input$date,units = "hours")
    t3$ndays = difftime(today(),input$date,units = "days")
    t3$nweeks = floor(difftime(today(),input$date,units = "weeks"))
    # t3$ndays = as.numeric(today() - input$date)
    t3$nmonths = floor((as.numeric(t3$ndays)/365.25)*12)
    t3$nyears = round(as.numeric(t3$ndays)/365.25,1)
    
    
    t3
    
  })
  
  
  # no of days left
  dmy_left = reactive({
    
    death = input$date + round(input$age_lived*365.25)
    
    t5 = list()
    t5$ndays = difftime(death,today(),units = "days")
    t5$nweeks = floor(difftime(death,today(),units = "weeks"))
    t5$nmonths = floor((as.numeric(t5$ndays)/365.25)*12)
    # t5$nyears = round(as.numeric(t5$ndays)/365.25,1)
    t5$nyears = input$age_lived - dmy_lived()$nyears
    
    
    t5
    
  })
  
  
  output$events = renderUI({
    
    
    # lapply(1:input$nevents,function(i){
    #   textInput(inputId = paste0("ev",i),label = "Enter",value = "hi")
    # })
    
    # input$nevents is character. have to convert to numeric
    
    lapply(1:input$nevents,function(i){
      
      shinydashboard::box(title = NULL,
                          width = 20,
                          splitLayout(cellWidths = c("60%","40%"),
                                      textInput(inputId = paste0("e",i,"_text"),
                                                label = if(i==1) "Enter description" else NULL,
                                                value = paste0("Event ",i)),
                                      dateInput(inputId = paste0("e",i,"_date"),
                                                label = if(i==1) "Enter date" else NULL,
                                                format = "dd-mm-yyyy",
                                                value =     seq.Date(input$date,
                                                                     today()+500 ,
                                                                     length.out = input$nevents+1)[i+1] )
                          ))
      
    })
    
    
  })
  

  output$lived = renderText({
    
    paste0( "Years lived:\t",dmy_lived()$nyears,
            "\nMonths lived:\t",dmy_lived()$nmonths,
            "\nWeeks lived:\t",dmy_lived()$nweeks,
            "\nDays lived:\t",dmy_lived()$ndays)
    
  })
  
  output$left = renderText({
    
    paste0( "Years left:\t",dmy_left()$nyears,
            "\nMonths left:\t",dmy_left()$nmonths,
            "\nWeeks left:\t",dmy_left()$nweeks,
            "\nDays left:\t",dmy_left()$ndays)
    
  })
  
  output$age_live = renderUI({
    
    
    sliderInput(inputId = "age_lived",
                label = "Assumed life span",
                min = max(ceiling(dmy_lived()$nyears),10) ,
                max = 120,
                value = 90)
    
  })
  
  
  dat_plot = eventReactive(eventExpr = input$plot,valueExpr = {
    
    div = 3
    
    yl = input$age_lived
    
    dat = data.frame(x = rep(1:(12*div),ceiling(yl/div)),
                     y =rep(0:-(ceiling(yl/div) - 1),each=12*div))
    
    dat$labx = NA
    dat$laby = NA
    
    dat$labx[ (dat$x == 1) & (dat$y %in% seq(0,min(dat$y),by = -5) ) ] = -seq(0,min(dat$y),by = -5)*div
    dat$laby[ dat$y == 0 ] = rep(substr(month.abb[c(month(input$date):12,1:month(input$date))[-13]],1,1),3)
    
    
    # month count
    dat$monthc = (-dat$y)*3*12 + dat$x
    
    dv3 = rep((year(input$date):(year(input$date)+dat$monthc[nrow(dat)]/12)),each=12)
    
    if(month(input$date)>1){
      dv3 = dv3[-(1:(month(input$date)-1))]
      dv3 = dv3[-((length(dv3) - (12-month(input$date))):length(dv3))]
    } else{
      dv3 = dv3[-((length(dv3) - (12-month(input$date))):length(dv3))]
    }
    
    # year
    dat$year = dv3
    dat$year[ !((dat$x == 36) & (dat$y %in% seq(0,min(dat$y),by = -5) )) ] = NA
    
    
    # dv2 = data.frame(date = do.call(c,input[paste0("e",1:input$nevents,"_date")]), 
    #                  text = do.call(c,input[paste0("e",1:input$nevents,"_text")]))
    
    dv2 = data.frame(date = do.call(c,lapply(1:input$nevents, function(i) input[[paste0("e",i,"_date")]] )), 
                     text = do.call(c,lapply(1:input$nevents, function(i) input[[paste0("e",i,"_text")]] )))
    
    
    # dv2 = data.frame(date = c(input$e1_date,input$e2_date,input$e3_date,input$e4_date,input$e5_date),
    #                  text = c(input$e1_text,input$e2_text,input$e3_text,input$e4_text,input$e5_text))
    
    
    dv2 = rbind(dv2,data.frame(date = c(input$date,
                                        today())
                               ,text = c("Birth",
                                         "This month")))
    
    if(input$age_lived > 30){
      dv2 = rbind(dv2,data.frame(date = input$date + 30*365.25
                                 ,text = "30th Birthday"))
    }
    
    if(input$age_lived > 60){
      dv2 = rbind(dv2,data.frame(date = input$date + 60*365.25
                                 ,text = "60th Birthday"))
    }
    
    
    dv2$monthc = sapply(1:nrow(dv2),function(i) {
      (year(dv2$date[i]) - year(input$date))*12 + (month(dv2$date[i]) - month(input$date)) + 1
    } )
    
    
    dat$event = NA
    dat$event[match(dv2$monthc,dat$monthc)] = dv2$text
    
    dat$event[nrow(dat)] = paste("Turning", dat$monthc[nrow(dat)]/12)
    
    
    dat
    
    
  })
  
  
  output$months_plot = renderPlot({
    
    
    dat = dat_plot()
    
    pl = ggplot(data = dat) + 
      geom_point(mapping = aes(x,y,col = event,shape = is.na(event)),size=5) + 
      scale_shape_manual(values = c(19,1)) + 
      geom_text_repel(data = subset(dat[!is.na(dat$event),],x %in% 1:17),
                      mapping = aes(x,y,label = event,col=event),
                      xlim = c(-10,-1),size = 5) +
      geom_text_repel(data = subset(dat[!is.na(dat$event),],x %in% 18:36),
                      mapping = aes(x,y,label = event,col=event),
                      xlim = c(37,46),size = 5) +
      xlim(c(-10,46)) +
      geom_text(data = dat[!is.na(dat$labx),],mapping = aes(x-1.2,y,label = labx)) + 
      geom_text(data = dat[!is.na(dat$year),],mapping = aes(x+2.2,y,label = year)) + 
      geom_text(data = dat[!is.na(dat$laby),],mapping = aes(x,y+1.2,label = laby)) + 
      # geom_text(mapping = aes(18.5,3),
      #           label = " Each circle represents a month. Each row has 36 months = 3 years ",size=5) + 
      
      geom_text(mapping = aes(18.5,2.5),
                label = " Month ",size=5) +
      geom_text(mapping = aes(-2+1,-2),
                label = " Age ",angle=90,size=5) + 
      geom_text(mapping = aes(36+2,-2),
                label = " Year ",angle=-90,size=5) + 
      # ggtitle("Your life in months") + 
      coord_fixed() +
      theme_bw() + theme(axis.title =  element_blank(),
                         line = element_blank(),
                         axis.text = element_blank(),
                         legend.position = "none",
                         plot.title = element_text(hjust = 0.5))
    
    pl
    
    
    
  },height = 600)
  
})

shinyApp(ui = ui, server = server)
