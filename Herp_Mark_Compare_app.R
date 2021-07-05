### Title: Shiny App for comparing differences in handling time between two amphibian marking methods
### Author: Abbey Feuka
### Date: 14 SEP 2020
### Notes: Formulas copied from 'Calculations_NumCaptures_Updated_Aug2020.xlsx'

#######################

library(shiny)
library(plotly)

# Define user interface
ui <- fluidPage(
    # Application title
    titlePanel("Calculation and comparison of cumulative handling time for two amphibian marking methods"),
    withMathJax(),
    h3("Compare handling times of multiple identification methods by changing the parameters below. 
      See calculations described in Roberts et al. (",tags$em("In review"),"). Study duration is number of periods (e.g. years, weeks)
      in the proposed study. Initial handling time refers to the time it takes to identify an individual 
      upon first capture. Subsequent handling time refers to the time spent identifying a recaptured individual."),
    # Sidebar with parameter controls
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId="p",
                        label="Capture probability (p)",
                        min = 0,
                        max = 1,
                        step=0.05,
                        value = 0.65
                        ),
            numericInput(inputId="phi",
                        label= HTML("Surival probabilty (&phi;)"),
                        min = 0,
                        max = 1,
                        value=0.90,
                        step = 0.01
                        ),
            numericInput(inputId="n",
                        label="Number of surveys per period",
                        min = 0,
                        max = 10,
                        value=3,
                        step = 1
                        ),
            numericInput(inputId="duration",
                        label="Study duration in periods (e.g., years, weeks)",
                        min = 1,
                        max = 40,
                        value=40,
                        step = 1
                        ),
            textInput(inputId = "name1",
                      label="Marking method name 1",
                      value="Photo"),
            textInput(inputId = "name2",
                      label="Marking method name 2",
                      value="PIT Tag"),
            radioButtons("time.unit", 
                         "Select Time Unit",
                         c("Seconds","Minutes")),
            numericInput(inputId="method1_init",
                             label="Initial handling time for method 1",
                             min = 0,
                             max = 60*5, #five minute max
                             value=53.94,
                             step = 0.1
                ),
            numericInput(inputId="method1_sub",
                             label="Subsequent handling time for method 1",
                             min = 0,
                             max = 60*5,
                             value=53.94,
                             step = 0.1
                ),
            numericInput(inputId="method2_init",
                             label="Initial handling time for method 2",
                             min = 0,
                             max = 60*5, #five minutes
                             value=106.46,
                             step = 0.1
                ),
            numericInput(inputId="method2_sub",
                             label="Subsequent handling time for method 2",
                             min = 0,
                             max = 60*5,
                             value=3.46,
                             step = 0.1
                )
        ),

        # Main panel with handling time v. age plot and table of pop. means
        mainPanel(
            br(),
            p(HTML(paste0("Expected cumulative handling times for an individual for the duration of the study (E[z",tags$sub("T"),"] in minutes:seconds)."))),
            tableOutput("mean_handling_times"),
            br(),
            p("Expected number of times each individual is captured during the entire study:"),
            textOutput("mean_captures"),
            br(),
            br(),
            p("Expected individual cumulative handling time over various study durations"),
            plotlyOutput("handling_plot"),
            br(),
            br(),
            fluidRow(
              column(width = 6, img(src = "toad_photo_lanier.jpg", height = 200, width = 250)), 
              column(width = 6, img(src = "toad_illustration.png", height = 200, width = 250)) 
            ),
            p("Photo credit: Wendy Lanier"),
            br(),
            p("Please cite as: Roberts, L.S., Hardy, B., Muths, E., Feuka, A.B., and Bailey, L.L.", tags$em("In review."), "Trade-offs in initial and long-term handling efficiency of PIT-tag and photographic identification methods.", tags$em("Ecological Indicators.")),
            align="center"
        )
    )
)

# Server logic
server <- function(input, output) {

    output$handling_plot <- renderPlotly({
      max_age <- 50
        #expected number of captures per year E[x] per individual
        cap.ex <- 0
        for(t in 1:(input$n+1)){
            caps <- t-1
            p.fx <- input$p^caps*(1-input$p)^(input$n-caps)
            combin <- factorial(input$n)/(factorial(caps)*factorial(input$n-caps))
            cap.p <- p.fx*combin
            P=(caps)*cap.p
            cap.ex=cap.ex+P
        }
        
        #plot handling time E[zt] over different study durations in minutes
        cap.avg.yr <- matrix(NA,input$duration,max_age) 
        for(i in 1:input$duration){
          for(t in 1:max_age){
            if(t <= i){
              #individuals that die during the study
              cap.avg.yr[i,t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*t            
            } else {
              #individual that die after the study
              cap.avg.yr[i,t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*i
            }
          }
        }
          if(sum(cap.avg.yr[input$duration,])<1){
            cap.avg.yr[,1] <- 1
          }
       
        method1_ht <- method2_ht <- numeric(input$duration)
        if(input$time.unit=="Seconds"){
            for(i in 1:input$duration){
                method1_ht[i]=input$method1_init/60+(input$method1_sub/60)*(sum(cap.avg.yr[i,])-1)
                method2_ht[i]=input$method2_init/60+(input$method2_sub/60)*(sum(cap.avg.yr[i,])-1)
            }
        } else if(input$time.unit=="Minutes"){
            for(i in 1:input$duration){
                method1_ht[i]=input$method1_init+(input$method1_sub)*(sum(cap.avg.yr[i,])-1)
                method2_ht[i]=input$method2_init+(input$method2_sub)*(sum(cap.avg.yr[i,])-1)
            }
        }
        
        df<-data.frame(duration=rep(1:input$duration,2),
                      ht=c(method1_ht,method2_ht),
                      typ=c(rep(input$name1,input$duration),
                                rep(input$name2,input$duration))
                            )
        
       fig<-plot_ly(data=df,x=~duration,y=~ht,type="scatter",
                color=~typ,mode="lines+markers")
          fig <- fig %>% layout(
              xaxis=list(title="Study Duration (in periods)"),
              yaxis=list(title="Cumulative Handling Time (minutes)"))
          fig
        })
    output$mean_captures <- renderText({
      max_age <- 50
      #expected number of captures per year E[x] per individual
      cap.ex <- 0
      for(t in 1:(input$n+1)){
        caps <- t-1
        p.fx <- input$p^caps*(1-input$p)^(input$n-caps)
        combin <- factorial(input$n)/(factorial(caps)*factorial(input$n-caps))
        cap.p <- p.fx*combin
        P=(caps)*cap.p
        cap.ex=cap.ex+P
      }
      #average number of expected captures for indiv that lives until period t and then dies (E[yt])
      cap.avg.yr <- numeric(input$duration) 
      for(t in 1:max_age){
        if(t <= input$duration){
          #individuals that die during the study
          cap.avg.yr[t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*t            
        } else {
          #individual that die after the study
          cap.avg.yr[t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*input$duration
        }
      }
      
      #ensure expected number of captures >1 for low-detection species
      if(sum(cap.avg.yr)<1){
        cap.avg.yr[1] <- 1
      }
      
      round(sum(cap.avg.yr),1)

    })
    output$mean_handling_times <- renderTable({
      max_age <- 50
        #expected number of captures E[x] per period per individual
        cap.ex <- 0
        for(t in 1:(input$n+1)){
            caps <- t-1
            p.fx <- input$p^caps*(1-input$p)^(input$n-caps)
            combin <- factorial(input$n)/(factorial(caps)*factorial(input$n-caps))
            cap.p <- p.fx*combin
            P=(caps)*cap.p
            cap.ex=cap.ex+P
        }
        #average number of expected captures for indiv that lives until period t and then dies (E[yt])
        cap.avg.yr <- numeric(input$duration) 
        for(t in 1:max_age){
          if(t <= input$duration){
            #individuals that die during the study
            cap.avg.yr[t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*t            
          } else {
            #individual that die after the study
            cap.avg.yr[t] <- input$phi^(t-1)*(1-input$phi)*cap.ex*input$duration
          }
        }
        
        if(sum(cap.avg.yr)<1){
          cap.avg.yr[1] <- 1
        }
        
        if(input$time.unit=="Seconds"){
            mn.mt.1.s <- input$method1_init+input$method1_sub*(sum(cap.avg.yr[1:max_age])-1) #seconds
            mn.mt.2.s <- input$method2_init+input$method2_sub*(sum(cap.avg.yr[1:max_age])-1) #seconds
            mn.mt.1.m <- mn.mt.1.s/60 #minutes
            mn.mt.2.m <- mn.mt.2.s/60 #minutes
        } else if(input$time.unit=="Minutes"){
            mn.mt.1.m <- input$method1_init+input$method1_sub*(sum(cap.avg.yr[1:max_age])-1) #seconds
            mn.mt.2.m <- input$method2_init+input$method2_sub*(sum(cap.avg.yr[1:max_age])-1) #seconds
            mn.mt.1.s <- mn.mt.1.m*60 #minutes
            mn.mt.2.s <- mn.mt.2.m*60 #minutes
        }
        ct1 <- as.POSIXct(mn.mt.1.s*60, origin = "1970-01-01", tz = "UTC") 
        ct2 <- as.POSIXct(mn.mt.2.s*60, origin = "1970-01-01", tz = "UTC")         
        df.table <- data.frame(
            x=c(substring(ct1,first=12,last=16)),
            y=c(substring(ct2,first=12,last=16)),
            row.names = ""
        )
       colnames(df.table)<-c(input$name1,input$name2)
        df.table
    },include.rownames=TRUE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
