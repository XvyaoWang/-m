
library(shiny)
library(shinyWidgets)
library(CMplot)
library(colourpicker)
library(shinyjqui)
library(shinyBS)

ui <- fluidPage(
  
  navbarPage(
    title = "Shiny CMplot",
    tabPanel(
      "Drawing",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            inputId = "filein",
            h3("File input" ,
               bsButton(
                 "bs11" , 
                 label = "" , 
                 icon = icon("question") , 
                 style = "info" , 
                 size = "small" 
               ))
          ),
          bsPopover(
            "bs11" , 
            "There are at most three P values in the input file" , 
            trigger = "fouce"
          ),
          pickerInput(
            inputId = "chrcolsel",
            label = tags$div(
              HTML(' <font><h3>Chr color(s)</font>')
             
            ),
            choices = c("default", "custom")
          ),
          conditionalPanel(
            condition = "input.chrcolsel == 'custom'",
            textInput(
              inputId = "colorChr",
              label = NULL,
              value="#00EAFF,#FF0000"
            )
          ),
          sliderInput(
            inputId = "plot1",
            h4("The radius of the central circle"),
            min = 0 , max = 5,
            value = 2.5 , step = 0.1
          ),
          sliderInput(
            inputId = "plot2",
            h4("Circle the high"),
            min = 1 , max = 100,
            value = 50 , step = 5
          ),
          sliderInput(
            inputId = "plot3",
            h4("The spacing between the two circles "),
            min = 1 , max = 100,
            value = 50 , step = 10
          ),
          actionBttn(
            inputId = "go",
            label = "Submit"
          )
        ),
        mainPanel(
          downloadButton(
            "shinyCMplot.pdf" ,
            "Download pdf-file"),
          downloadButton(
            "shinyCMplot.svg",
            "Downlode svg-file"
          ),
          tabPanel(
            "Plot",
            jqui_resizable(
              imageOutput(
                outputId = "distPlot",
                width = "600px" ,
                height = "600px"
              ) 
            )
          )
        )
      )
    ),
    tabPanel(
      "Help",
      
      h4(""),
      includeMarkdown("Help.md")
    )
  )
)


server <- function(input, output,session) {
  observeEvent(input$go,{
    a <- input$filein
    plot1 <- input$plot1
    plot2 <- input$plot2
    plot3 <- input$plot3
    if(!is.null(a)){
      data<- data.frame(read.csv(a[1,4],header = T),stringsAsFactors = F)
      colortp <- input$chrcolsel
      color0 <- input$colorChr
      color <- unlist(strsplit(color0,split = ","))
      
      output$distPlot <- renderPlot({
        if(colortp == "custom"){
          CMplot(
            Pmap = data,
            plot.type= "c",
            col=color,
            cex.axis = 1,
            r = plot1,
            H = plot2,
            cir.band = plot3,
            box = FALSE,
            LOG10 = TRUE,
            multracks = TRUE,
            file.output = FALSE,
            cir.legend = TRUE,
            threshold = c(1e-6),
            threshold.lty = c(1),
            threshold.lwd = c(2),
            threshold.col = c("red"),
            dpi = 300,
            verbose = TRUE
          )
        }else(
          CMplot(
            Pmap = data,
            plot.type= "c",
            cex.axis = 1,
            r = plot1,
            H = plot2,
            cir.band = plot3,
            box = FALSE,
            LOG10 = TRUE,
            multracks = TRUE,
            file.output = FALSE,
            cir.legend = TRUE,
            threshold = c(1e-6),
            threshold.lty = c(1),
            threshold.lwd = c(2),
            threshold.col = c("red"),
            dpi = 300,
            verbose = TRUE
          )
        )
        tupian <<- recordPlot()
      })
    }
  })
  observe({
    output$shinyCMplot.pdf <- downloadHandler(
      filename <- function(){paste('shinyCMplot.pdf')},
      content <- function(file){
        pdf(file , width = 750/72 , height = 750/72)
        print(tupian)
        dev.off()
      },
      contentType = 'application/pdf'
    )
  })
  observe({
    output$shinyCMplot.svg <- downloadHandler(
      filename <- function(){paste0('shinyCMplot.svg')},
      content <- function(file){
        svg(file , width = 750/72 , height = 750/72)
        print(tupian)
        dev.off()
      },
      contentType = 'image/svg'
    )
  })
}

shinyApp(ui = ui, server = server)