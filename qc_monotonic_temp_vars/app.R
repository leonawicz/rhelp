library(shiny); library(reshape2); library(dplyr); library(ggplot2); library(DT)
sets <- c("South Central", "Alaska/Canada", "Raw world grids")
wsSwitch <- function(x) switch(x, "South Central"="monotonic_temp_vars.RData",
  "Alaska/Canada"="monotonic_temp_vars_akcan2km.RData", "Raw world grids"="monotonic_temp_vars_raw.RData")
xvars <- c("Scenario", "Model", "Month", "Year")
ggProps <- theme_gray(base_size=16) + theme(legend.position="top")

ui <- shinyUI(fluidPage(
  titlePanel(title=h4("Monotonically increasing minimum, mean and maximum temepratures"), windowTitle="QC Viewer"),
  br(),
  fluidRow(
    column(2,
      selectInput("workspace", "Data set", sets, selected=sets[1]),
      selectInput("x", "X axis", xvars, selected=xvars[1]),
      selectInput("months", "Filter months", c("", month.abb), selected=""),
      selectInput("stat", "Stat", c("Occurrences", "Cells"), selected="Occurrences"),
      checkboxInput("log", "Log scale")
    ),
    column(5, plotOutput("plot1")),
    column(5, plotOutput("plot2"))
  ),
  fluidRow(column(7, dataTableOutput('tbl')), column(5, plotOutput("plot3", height="500px"))),
  br()
))

server <- shinyServer(function(input, output, session) {
  env <- new.env()
  rv <- reactiveValues()
  updateDataFun <- function(file){ if(!is.null(file)){ vars <- load(file=wsSwitch(file), env); for(v in vars) rv[[v]] <- get(v, env) } }
  updateData <- reactive({ updateDataFun(file=input$workspace) })
  observe({ updateData() })

  isTs <- reactive({ input$x=="Year" })
  mos <- reactive({ if(input$months=="") month.abb else input$months })
  d1 <- reactive({ filter(rv$d, (Min_high > 0 | Max_low > 0) & Month %in% mos()) })

  output$tbl = renderDataTable({
    x <- d1() %>% mutate(Model=sapply(strsplit(Model, "\\."), "[", 1), Scenario=as.character(Scenario)) %>%
      mutate(Scenario=ifelse(Scenario=="historical", "hist", as.character(Scenario)))
    datatable(x, colnames = c('Scen.'=1, 'Mo.'=4, 'MinHi'=5, 'MaxLo'=6, 'T_ok'=8, 'T_x'=9, 'T_xx'=10), rownames=FALSE)},
    options=list(pageLength=20, lengthChange=FALSE))

  output$plot1 <- renderPlot({
    dat <- if(isTs()) group_by(d1(), Model, Year) else group_by_(d1(), .dots=input$x)
    dat <- if(input$stat=="Occurrences") summarise(dat, val=n()) else summarise(dat, val=sum(Min_high + Max_low))
    if(input$log) dat <- mutate(dat, val=log(val + 1))
    g <- ggplot(dat, aes_string(input$x, "val")) + ggProps + ggtitle("Temperature monotonicity failure")
    if(isTs()){
      g <- g + geom_point(aes(colour=Model)) + geom_line(aes(colour=Model)) + labs(x=input$x, y=paste0("Total ", tolower(input$stat), " by model"))
    } else {
      g <- g + geom_bar(stat="identity") + labs(x=input$x, y=paste0("Total ", tolower(input$stat), " (all years) by ", input$x))
    }
    g
  })

  output$plot2 <- renderPlot({
    dat <- if(isTs()) group_by(d1(), Model, Year) else group_by_(d1(), .dots=input$x)
    dat <- summarise_each(dat, funs(round(mean(100 * . / rv$ncells[1]), 1)), Min_high, Max_low, Inversion) %>%
      melt(measure.vars=c("Min_high", "Max_low", "Inversion"), variable.name="Condition", value.name="Pct")
    g <- ggplot(dat, aes_string(input$x, "Pct", fill="Condition")) + ggProps +
      ggtitle("Map area affected (when affected)") + geom_bar(stat="identity", position="dodge")
    if(isTs()){
      g <- g + facet_wrap(~Model) + labs(x=input$x, y=paste0("Mean percent area by model"))
    } else {
      g <- g + labs(x=input$x, y=paste0("Mean percent area (all years) by ", input$x))
    }
    g
  })

  output$plot3 <- renderPlot({
    dat <- if(isTs()) group_by(d1(), Model, Year) else group_by_(d1(), .dots=input$x)
    dat <- melt(dat, measure.vars=c("T_ok", "T_off", "T_inv"), variable.name="Condition", value.name="Temperature")
    g <- ggplot(dat, aes_string(x="Temperature", colour="Condition")) + ggProps + ggtitle("Temperature density (all data)") +
      geom_line(stat="density") + facet_wrap(~Model, scales="free") + labs(x="Temperature", y=paste0("Density"))
    g
  })

})

shinyApp(ui=ui, server=server)
