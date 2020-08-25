################################################################################
## MultiFlow Shiny App
################################################################################

## required packages
library(shiny) 
library(shinyjs) 
library(shinythemes)
library(EBImage)
library(ShinyImage)
library(shinyFiles)
library(DT)
library(rmarkdown)
library(ggplot2)

### UI ###
ui <- fluidPage( 
  theme = shinytheme("sandstone"),
  shinyjs::useShinyjs(),
  
  titlePanel("MultiFlow Shiny App"),
  tags$style(type='text/css', "#stop { float:right; }"),
  actionButton("stop", "Quit App"), 
  tabsetPanel(id = "tabs", 
              ## Start of Tab Image Editor
              tabPanel("Cropping and Segmentation", value = "tab1",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons("radio", 
                                        label = ("Upload Image or Choose Sample"), 
                                        choices = list("Upload Image" = 1, 
                                                       "Sample Image" = 2), 
                                        selected = 1),
                           conditionalPanel(
                             condition = "input.radio == 1",
                             fileInput(inputId = 'file1',
                                       label = 'Upload Image',
                                       placeholder = 'JPEG, PNG, and TIFF are supported',
                                       accept = c(
                                         "image/jpeg",
                                         "image/x-png",
                                         "image/tiff",
                                         ".jpg",
                                         ".png",
                                         ".tiff"))
                           ),hr(style="border-color: black"),
                           h4("Set number of strips and number of bands per strip",
                              style="font-weight:bold"),
                           sliderInput("strips", "Number of strips:", 
                                       min = 1, max = 7, value = 1),
                           sliderInput("bands", "Number of bands:", 
                                       min = 2, max = 5, value = 2),
                         ),
                         mainPanel(
                           HTML(
                             paste(
                               h3('Cropping and Segmentation', align = "center"),
                               plotOutput("plot1",
                                          click = "plot_click",
                                          dblclick = "plot_dblclick",
                                          hover = "plot_hover",
                                          brush = "plot_brush"),
                               '<br/>',
                               column(6, shinyjs::hidden(
                                 actionButton("segmentation", label = "Apply Segmentation")
                               )),
                               tags$style(type='text/css', "#segmentation { display: block; width:60%; margin-left: auto; margin-right:auto;}"),
                               '<br/>','<br/>',
                               h3('Preview Crop', align = "center"),
                               h6('Click and drag where you would like to crop the photo. To keep the cropped version, press Apply Crop', align = "center"),
                               '<br/>',
                               plotOutput("plot2"),
                               tags$style(type='text/css', "#keep { display:block; width:30%; margin-left: auto; margin-right:auto;}"),
                               '<br/>',
                               shinyjs::hidden(
                                 actionButton("keep", label = "Apply Crop")
                               ),
                               '<br/>',
                               verbatimTextOutput("info")
                             )
                           ),
                           width = 8
                         )
                       )
              ), # END OF TAB PANEL
          ## Start of Tab Background Correction
          tabPanel("Background", value = "tab2",
                   sidebarLayout(
                     sidebarPanel(
                       h4("Select strip", style="font-weight:bold"),
                       numericInput(inputId = "selectStrip",
                                    label = "Select strip:",
                                    value = 1,
                                    min = 0,
                                    max = 7,
                                    step = 1,
                                    width = NULL
                       ),
                       h4("Select threshold method and apply it",
                          style="font-weight:bold"),
                       radioButtons("thresh", 
                                    label = ("Threshold Method"), 
                                    choices = list("Otsu" = 1, 
                                                   "Quantile" = 2), 
                                    selected = 1),
                       conditionalPanel(
                         condition = "input.thresh == 2",
                         numericInput(inputId = "quantile1",
                                      label = "Probability [%]:",
                                      value = 99,
                                      min = 0,
                                      max = 100,
                                      step = 0.1,
                                      width = NULL
                         )
                       ),
                       actionButton("threshold", label = "Apply Threshold"), br(), br(),
                       actionButton("data", label = "Add To Data"), br(), br(),
                       actionButton("showIntensData", label = "Switch To Intensity Data")
                     ),
                     mainPanel(
                       HTML(
                         paste(
                           h3('Background Correction', align = "center"),
                           verbatimTextOutput("thresh"),br(),
                           h4('Signal Intensity Above Background', align = "center"),
                           plotOutput("plot3"),
                           h4('Bands After Background Subtraction', align = "center"),
                           plotOutput("plot4"),
                           verbatimTextOutput("meanIntens"),
                           verbatimTextOutput("medianIntens"),
                           '<br/>','<br/>'
                         )
                       ),
                       width = 8
                     )
                   )
          ), # END OF TAB PANEL
          ## Start of Tab Data
          tabPanel("Intensity Data", value = "tab3",
            sidebarLayout(
              sidebarPanel(
                h4("Refresh, download or delete data", style="font-weight:bold"),
                downloadButton("downloadData", "Download Data"), br(), br(),
                actionButton("deleteData", label = "Delete Data"), br(), br(),
                actionButton("refreshData", label = "Refresh Data"), br(),
                hr(style="border-color: black"),
                fileInput("intensFile", "Upload existing data (CSV File)", 
                          multiple = FALSE,
                          accept = c("text/csv", 
                                     "text/comma-separated-values,text/plain",
                                     ".csv")), hr(style="border-color: black"),
                actionButton("expInfo", label = "Upload Experiment Info")
              ),
              mainPanel(
                DTOutput("intens")
              )
            )
          ), # END OF TAB PANEL
          tabPanel("Experiment Info", value = "tab4",
            sidebarLayout(
              sidebarPanel(
                fileInput("expFile", "Upload existing data (CSV File) (experiment info or merged data)", 
                          multiple = FALSE,
                          accept = c("text/csv", 
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),  hr(style="border-color: black"),
                h4("Select ID columns and merge datasets", style="font-weight:bold"),
                textInput("mergeIntens", label = "ID Column Intensity Data", value = "File"),
                textInput("mergeExp", label = "ID Column Experiment Info", value = "File"),
                actionButton("merge", label = "Merge With Intensity Data"), br(),
                hr(style="border-color: black"),
                downloadButton("downloadData2", "Download Data"), br(), br(),
                actionButton("deleteData2", label = "Delete Data"), br(), br(),
                actionButton("refreshData2", label = "Refresh Data"), br(),
                hr(style="border-color: black"),
                actionButton("prepare", label = "Prepare Calibration")
              ),
              mainPanel(
                DTOutput("experiment")
              )
            )
          ), # END OF TAB PANEL
          tabPanel("Calibration", value = "tab5",
                   sidebarLayout(
                     sidebarPanel(
                       h4("You MUST first select a folder for the analysis results!", style="font-weight:bold; color: red"),
                       shinyDirButton('folder', "Select Folder", "Please select a folder:"),
                       hr(style="border-color: black"),
                       h4("Average Technical Replicates", style="font-weight:bold"),
                       textInput("combRepsColSI", label = "Column with sample information:", value = "Sample"),
                       numericInput(inputId = "colorsBands",
                                    label = "Number of analytes/colors per band:",
                                    value = 1,
                                    min = 1,
                                    max = 5,
                                    step = 1,
                                    width = NULL
                       ),
                       conditionalPanel(
                         condition = "input.colorsBands > 1",
                         textInput("combRepsColCL", label = "Column with color information:", value = "Color"),
                       ),
                       radioButtons("radioReps", 
                                    label = ("Choose measure for averaging:"), 
                                    choices = list("Mean" = 1, 
                                                   "Median" = 2), 
                                    selected = 1), 
                       actionButton("combReps", label = "Average Technical Relplicates"), br(),
                       hr(style="border-color: black"),
                       h4("Optional: Reshape Data From Long To Wide", style="font-weight:bold"),
                       textInput("reshapeCol", label = "Column:", value = "Color"),
                       actionButton("reshapeWide", label = "Reshape"), br(),
                       hr(style="border-color: black"),
                       h4("Download Preprocessed Data", style="font-weight:bold"),
                       downloadButton("downloadData3", "Download Data"), br(), br(),
                       fileInput("prepFile", "Upload existing data (CSV File)", 
                                 multiple = FALSE,
                                 accept = c("text/csv", 
                                            "text/comma-separated-values,text/plain",
                                            ".csv")), 
                       hr(style="border-color: black"),
                       h4("Calibration By Linear Model", style="font-weight:bold"),
                       textAreaInput("formula", label = "Specify Full Model (R formula)"),
                       textAreaInput("subset", label = "Optional: specify subset (logical R expression)"),
                       actionButton("runCali", label = "Run Calibration Analysis")
                     ),
                     mainPanel(
                       verbatimTextOutput("folder"),
                       DTOutput("calibration")
                     )
                   )
          ), # END OF TAB PANEL
          tabPanel("Results", value = "tab6",
                   sidebarLayout(
                     sidebarPanel(
                       h4("Open analysis report"),
                       actionButton("openReport", label = "Open")
                     ),
                     mainPanel(
                       h3("Results of Calibration Analysis", style="font-weight:bold"), br(),
                       h4("Calibration model", style="font-weight:bold"),
                       verbatimTextOutput("modelSummary"), br(),
                       plotOutput("plot5"), br(),
                       verbatimTextOutput("LOB"),
                       verbatimTextOutput("LOD"),
                       verbatimTextOutput("LOQ")
                     )
                   )
                ) # END OF TAB PANEL
  ) # END OF TAB SET PANEL
) # END OF UI


### SERVER ###
server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 30 mb; default is 5 mb
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, Threshold = NULL)
  
  #checks radio for file input
  observe({
    #default: upload image
    if(input$radio == 1){
      reset('file1') #allows plot1 to be null when radio is clicked
      # creates a warning to upload correct file 
      # otherwise outputs image
      output$plot1 <- renderPlot({ 
        validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
        if(is.null(input$file1)) 
          return(NULL)
      })
    }
    if(input$radio == 2){
      # using sample image
      system.file("images", "sample.TIF", package="MultiFlow")
      shinyImageFile$shiny_img_origin <- 
        shinyimg$new(system.file("images", "sample.TIF", package="MultiFlow"))

      shinyImageFile$filename <- "sample.TIF"
      #outputs image to plot1 -- main plot
      output$plot1 <- renderPlot({ shinyImageFile$shiny_img_origin$render() })
    }
  }) # end of observe
  
  # second observe
  # looks at its first 3 lines and observes those inputs
  # resets the sliders
  observe({
    #if user uploads new file or 
    #the sliders will change
    #and the brush will default 
    input$file1
    input$radio
    input$thresh

    # and reset plot brush
    session$resetBrush("plot_brush")
  })
  
  #//////// CDOE FOR RADIO BUTTONS /////////////
  #when user uploads file
  #the datapath is different from the one needed to properly recognize photo
  #so this function renames the file 
  renameUpload <- function(inFile){
    if(is.null(inFile))
      return(NULL)
    
    oldNames <- inFile$datapath
    newNames <- file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames
    
    return(inFile$datapath)
  }
  
  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    # reseting plots and text messages
    output$plot3 <- renderPlot({})
    output$plot4 <- renderPlot({})
    if(!is.null(shinyImageFile$Threshold))
      shinyImageFile$Threshold <- NULL
    if(!is.null(shinyImageFile$Mean_Intensities))
      shinyImageFile$Mean_Intensities <- NULL
    if(!is.null(shinyImageFile$Median_Intensities))
      shinyImageFile$Median_Intensities <- NULL
    
    shinyImageFile$filename <- input$file1$name
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$file1))
    output$plot1 <- renderPlot({shinyImageFile$shiny_img_origin$render()})
  })
  
  #//////// END OF CODE FOR RADIO BUTTONS /////////////
  
  #//////// CODE FOR CROPPING AND PLOTS /////////////
  
  #prompts shiny to look at recursive crop
  observe({recursiveCrop()})
    
  #only executes when keep is clicked 
  recursiveCrop <- eventReactive(input$keep,{
    isolate({
      p <- input$plot_brush 
      shinyImageFile$shiny_img_origin$cropxy(p$xmin,p$xmax,p$ymin,p$ymax)
      output$plot1 <- renderPlot({
        shinyImageFile$shiny_img_origin$render()
      
        MAX <- shinyImageFile$shiny_img_origin$size()[1:2]
        colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
        rowcuts <- seq(1, MAX[2], length.out = 2*input$bands) # bands + spaces between bands
        
        for (x in colcuts) {
          lines(x = rep(x, 2), y = c(1, MAX[2]), col="red")
        }
        for (y in rowcuts) {
          lines(x = c(1, MAX[1]), y = rep(y, 2), col="red")
        }
      })
    })
    session$resetBrush("plot_brush")
  })
  
  #hides the keep button in the instance in which the user highlighted the plot
  #then clicks on the side so that the brush plot disappears
  #if user clicks keep without a valid brush, there will be errors
  #so we need to hide the button
  observeEvent(is.null(input$plot_brush), {
    shinyjs::hide("keep")
  })
  
  #creates a clone of the image in the main image viewer
  #shows the user a preview of the cropped image 
  #since shinyimg saves every image that is edited, we use a clone
  #so that we aren't saving any of the previews
  #till the user clicks keep
  croppedShiny <- function(image){
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= shinyImageFile$shiny_img_origin$size()[1], 
                  "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= shinyImageFile$shiny_img_origin$size()[2], 
                  "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0, 
                  "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0, 
                  "Highlighted portion is out of bounds on the y-axis of your image 2"))
    preview <- shinyImageFile$shiny_img_origin$copy()
    preview$cropxy(p$xmin,p$xmax,p$ymin,p$ymax)
    preview$render()

    MAX <- preview$size()[1:2]
    colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
    rowcuts <- seq(1, MAX[2], length.out = 2*input$bands)
    
    for (x in colcuts) {
      lines(x = rep(x, 2), y = c(1, MAX[2]), col="red")
    }
    for (y in rowcuts) {
      lines(x = c(1, MAX[1]), y = rep(y, 2), col="red")
    }
  }
  
  #shows a preview of the cropped function
  #shows the keep button (originally hiding) 
  output$plot2 <- renderPlot({
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= shinyImageFile$shiny_img_origin$size()[1], 
                  "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= shinyImageFile$shiny_img_origin$size()[2], 
                  "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0, 
                  "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0, 
                  "Highlighted portion is out of bounds on the y-axis of your image 2"))
    
    croppedShiny(shinyImageFile$shiny_img_origin)
    
    shinyjs::show("keep")
    shinyjs::show("segmentation")
    shinyjs::show("undo")
  })

  observe({recursiveSegmentation()})
  
  #only executes when Apply Segmentation is clicked
  recursiveSegmentation <- eventReactive(input$segmentation,{
    isolate({
      MAX <- shinyImageFile$shiny_img_origin$size()[1:2]
      colcuts <- seq(1, MAX[1], length.out = input$strips + 1)
      rowcuts <- seq(1, MAX[2], length.out = 2*input$bands)
      
      segmentation.list <- vector("list", length = input$strips)  
      count <- 0
      for(i in 1:input$strips){
        tmp.list <- vector("list", length = 2*input$bands-1)
        for(j in 1:(2*input$bands-1)){
          img <- shinyImageFile$shiny_img_origin$copy()
          img$cropxy(colcuts[i], colcuts[i+1], rowcuts[j], rowcuts[j+1])
          tmp.list[[j]] <- img
        }
        segmentation.list[[i]] <- tmp.list
      }
      shinyImageFile$cropping_grid <- list("columns" = colcuts, "rows" = rowcuts)
      shinyImageFile$segmentation_list <- segmentation.list
      updateTabsetPanel(session, "tabs", selected = "tab2")
    })
  })
  
  observe({input$thresh})
  
  observe({recursiveThreshold()})
  
  #only executes when Apply Segmentation is clicked
  recursiveThreshold <- eventReactive(input$threshold,{
    isolate({
      seg.list <- shinyImageFile$segmentation_list
      i <- input$selectStrip
      if(input$thresh == 2){
        Background <- vector(mode = "list", length = input$bands)
        for(j in 1:input$bands){
          tmp <- seg.list[[i]][[j]]
          Background[[j]] <- as.numeric(imageData(tmp$.__enclos_env__$private$current_image))
        }
        Background.Threshold <- quantile(unlist(Background), 
                                         probs = input$quantile1/100)
        shinyImageFile$Threshold <- Background.Threshold
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            tmp <- seg.list[[i]][[j]]
            img <- tmp$.__enclos_env__$private$current_image
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- signal
            plot(img)
            title(paste0("Band ", count))
          }
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count <- 0
          for(j in Bands){
            count <- count + 1
            tmp <- seg.list[[i]][[j]]
            img <- tmp$.__enclos_env__$private$current_image
            signal <- imageData(img) > Background.Threshold
            imageData(img) <- (imageData(img) - Background.Threshold)*signal
            shinyImageFile$Mean_Intensities[1,count] <- mean(imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count] <- median(imageData(img)[signal])
            plot(img)
            title(paste0("Band ", count))
          }
        })
      }
      if(input$thresh == 1){
        Background.Threshold <- numeric(input$bands)
        output$plot3 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            tmp <- seg.list[[i]][[j]]
            img <- tmp$.__enclos_env__$private$current_image
            Background.Threshold[count1] <- otsu(img)
            signal <- imageData(img) > Background.Threshold[count1]
            imageData(img) <- signal
            plot(img)
            title(paste0("Band ", count2))
          }
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        shinyImageFile$Median_Intensities <- matrix(0, nrow = 1, ncol = input$bands)
        output$plot4 <- renderPlot({
          par(mfcol = c(1, input$bands))
          count1 <- 0
          Bands <- seq(1, 2*input$bands-1, by = 2)
          count2 <- 0
          for(j in Bands){
            count1 <- count1 + 1
            count2 <- count2 + 1
            tmp <- seg.list[[i]][[j]]
            img <- tmp$.__enclos_env__$private$current_image
            thr <- otsu(img)
            signal <- imageData(img) > thr
            imageData(img) <- (imageData(img) - thr)*signal
            shinyImageFile$Mean_Intensities[1,count1] <- mean(imageData(img)[signal])
            shinyImageFile$Median_Intensities[1,count1] <- median(imageData(img)[signal])
            plot(img)
            title(paste0("Band ", count2))
          }
        })
      }
    })
  })

  observe({recursiveData()})
  
  recursiveData <- eventReactive(input$data,{
    isolate({
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:input$bands)
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:input$bands)
      if(input$thresh == 1){
        BG.method <- matrix(c("Otsu", NA), nrow = 1, 
                            ncol = input$bands, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == 2){ 
        BG.method <- matrix(c("quantile", input$quantile1), 
                            nrow = 1, 
                            ncol = input$bands, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      DF <- data.frame("File" = shinyImageFile$filename,
                       "Strip" = input$selectStrip,
                       BG.method, AM, Med, 
                       check.names = FALSE)
      if(inherits(try(IntensData, silent = TRUE), "try-error"))
        IntensData <<- DF
      else
        IntensData <<- rbind(IntensData, DF)
      
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
      output$plot3 <- renderPlot({})
      output$plot4 <- renderPlot({})
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
    })
  })
  
  observe({recursiveShowIntensData()})
  
  recursiveShowIntensData <- eventReactive(input$showIntensData,{
    isolate({
      updateTabsetPanel(session, "tabs", selected = "tab3")
    })
  })

  observe({recursiveDelete()})
  
  recursiveDelete <- eventReactive(input$deleteData,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
    })
  })

  observe({recursiveDelete2()})
  
  recursiveDelete2 <- eventReactive(input$deleteData2,{
    isolate({
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(CombinedData, pos = 1))
    })
  })
  
  observe({recursiveRefresh()})
  
  recursiveRefresh <- eventReactive(input$refreshData,{
    isolate({
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
    })
  })

  observe({recursiveRefresh2()})
  
  recursiveRefresh2 <- eventReactive(input$refreshData2,{
    isolate({
      output$experiment <- renderDT({
        DF <- CombindedData
        datatable(DF)
      })
    })
  })
  observeEvent(input$intensFile,{
    output$intens <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
  })
  observeEvent(input$expFile,{
    output$experiment <- renderDT({})
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(CombinedData, pos = 1))
  })
  observeEvent(input$prepFile,{
    output$experiment <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
    suppressWarnings(rm(ExpInfo, pos = 1))
    suppressWarnings(rm(CombinedData, pos = 1))
  })
  
  observe({recursiveExpInfo()})
  
  recursiveExpInfo <- eventReactive(input$expInfo,{
    updateTabsetPanel(session, "tabs", selected = "tab4")
  })

  observe({recursiveUploadIntens()})

  recursiveUploadIntens <- eventReactive(input$intensFile,{
    isolate({
      req(input$intensFile)
      tryCatch(
        DF <- read.csv(input$intensFile$datapath, header = TRUE, 
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      IntensData <<- DF
      output$intens <- renderDT({
        datatable(DF)
      })
    })
  })
  
  observe({recursiveUploadExpFile()})
  
  recursiveUploadExpFile <- eventReactive(input$expFile,{
    isolate({
      req(input$expFile)
      tryCatch(
        DF <- read.csv(input$expFile$datapath, header = TRUE, 
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      ExpInfo <<- DF
      CombinedData <<- DF
      suppressWarnings(rm(AveragedData, pos = 1))
      output$calibration <- renderDT({})
      
      output$experiment <- renderDT({
        datatable(DF)
      })
    })
  })

  observe({recursiveUploadPrepFile()})
  
  recursiveUploadPrepFile <- eventReactive(input$prepFile,{
    isolate({
      req(input$prepFile)
      tryCatch(
        DF <- read.csv(input$prepFile$datapath, header = TRUE, 
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      AveragedData <<- DF
      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })
  
  observe({recursiveMerge()})
  
  recursiveMerge <- eventReactive(input$merge,{
    isolate({
      DF <- merge(ExpInfo, IntensData,
                  by.x = input$mergeExp, 
                  by.y = input$mergeIntens, all = TRUE)
      
      CombinedData <<- DF
      
      output$experiment <- renderDT({
        datatable(DF)
      })
    })
  })
  
  observe({recursivePrepare()})
  
  recursivePrepare <- eventReactive(input$prepare,{
    updateTabsetPanel(session, "tabs", selected = "tab5")
  })
  
  observe({recursiveCombReps()})
  
  recursiveCombReps <- eventReactive(input$combReps,{
    isolate({
      Cols <- c(grep("Mean", colnames(CombinedData)), 
                grep("Median", colnames(CombinedData)))
      RES <- NULL
      if(input$colorsBands > 1){
        DF <- CombinedData[,c(input$combRepsColSI, input$combRepsColCL)]
        DFuni <- DF[!duplicated(DF),]
        for (i in 1:nrow(DFuni)) {
          sel <- DF[,1] == DFuni[i,1] & DF[,2] == DFuni[i,2]
          tmp <- CombinedData[sel, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }else{
        DF <- CombinedData[,input$combRepsColSI]
        for (spl in unique(CombinedData[, input$combRepsColSI])) {
          tmp <- CombinedData[DF == spl, ]
          tmp2 <- tmp[1, ]
          if (input$radioReps == 1) #mean
            tmp2[, Cols] <- colMeans(tmp[, Cols], na.rm = TRUE)
          if (input$radioReps == 2) #median
            tmp2[, Cols] <- apply(tmp[, Cols], 2, median, na.rm = TRUE)
          RES <- rbind(RES, tmp2)
        }
      }
      rownames(RES) <- 1:nrow(RES)
      RES <- RES[order(RES[,input$combRepsColSI]),]
      AveragedData <<- RES
      
      output$calibration <- renderDT({
        datatable(RES)
      })
    })
  })
  
  observe({recursiveReshapeWide()})
  
  recursiveReshapeWide <- eventReactive(input$reshapeWide,{
    isolate({
      rm.file <- (colnames(AveragedData) != colnames(CombinedData)[1] &
                    colnames(AveragedData) != input$reshapeCol)
      DF.split <- split(AveragedData[,rm.file], AveragedData[,input$reshapeCol])
      
      N <- length(unique(AveragedData[,input$reshapeCol]))
      if(N > 1){
        DF <- DF.split[[1]]
        Cols <- c(grep("Mean", colnames(DF)), 
                  grep("Median", colnames(DF)))
        Cols <- c(Cols, which(colnames(DF) == input$combRepsColSI))
        for(i in 2:N){
          DF <- merge(DF, DF.split[[i]][,Cols], by = input$combRepsColSI, 
                      suffixes = paste0(".", names(DF.split)[c(i-1,i)]))
        }
        AveragedData <<- DF
      }else{
        DF <- AveragedData
      }
      
      output$calibration <- renderDT({
        datatable(DF)
      })
    })
  })
  
  observe({recursiveRunCali()})
  
  recursiveRunCali <- eventReactive(input$runCali,{
    isolate({
      PATH.OUT <- parseDirPath(c(wd="."), input$folder)
      FORMULA <- input$formula
      SUBSET <- input$subset
      save(AveragedData, FORMULA, SUBSET, PATH.OUT, 
           file = paste0(PATH.OUT,"CalibrationData.RData"))
      
      file.copy(from = system.file("rmarkdown", "CalibrationAnalysis.Rmd", 
                                   package="MultiFlow"), 
                to = paste0(PATH.OUT, "/CalibrationAnalysis.Rmd"))
      
      rmarkdown::render(input = paste0(PATH.OUT, "/CalibrationAnalysis.Rmd"),
                        output_file = paste0(PATH.OUT, "/CalibrationAnalysis.html"))
      
      load(file = paste0(PATH.OUT, "/CalibrationResults.RData"))
      
      tmp <- strsplit(FORMULA, "~")[[1]]
      y.var <- names(unlist(sapply(colnames(AveragedData), 
                                   grep, x = tmp[1])))
      AIC.fit.sum <- summary(AIC.fit)
      adj.R2 <- round(AIC.fit.sum$adj.r.squared, 3)
      DF <- data.frame(Concentration = AIC.fit$model[,y.var], 
                       Fitted = fitted(AIC.fit))
      output$modelSummary <- renderPrint({ AIC.fit })
        
      output$plot5 <- renderPlot({
        ggplot(DF, aes(x = Concentration, y = Fitted)) +
          geom_point() + geom_abline(slope = 1, intercept = 0) +
          ylab("Fitted values") + 
          annotate("text",  x=min(DF$Concentration), y = max(DF$Fitted), 
                   label = substitute(paste("adj. ", R^2, " = ", adj.R2), 
                                      list(adj.R2 = adj.R2)), 
                   vjust=1, hjust=0, size = 5)
        
      })
      output$LOB <- renderText({
        paste0("Limit of Blank (LOB): ", signif(LOB, 3))
      })
      output$LOD <- renderText({
        paste0("Limit of Detection (LOD): ", signif(LOD, 3))
      })
      output$LOQ <- renderText({
        paste0("Limit of Quantification (LOQ): ", signif(LOQ, 3))
      })
      
      updateTabsetPanel(session, "tabs", selected = "tab6")
    })
  })
  
  observe({recursiveOpenReport()})
  
  recursiveOpenReport <- eventReactive(input$openReport,{
    isolate({
      PATH.OUT <- parseDirPath(c(wd="."), input$folder)
      browseURL(paste0(PATH.OUT, "/CalibrationAnalysis.html"),
                browser = getOption("browser"))
    })
  })

  #//////// CODE FOR HELPFUL TEXTS /////////////
  
  #textbox for user to see what the image ID is

  #creates the textbox below plot2 about the plot_brush details and etc
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  output$thresh <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Threshold(s): ", paste0(signif(shinyImageFile$Threshold, 4), collapse = ", "))
  })
  output$meanIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Mean intensities: ", paste0(signif(shinyImageFile$Mean_Intensities, 4), collapse = ", "))
  })
  output$medianIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Median intensities: ", paste0(signif(shinyImageFile$Median_Intensities, 4), collapse = ", "))
  })
  output$intens <- renderDT({
    DF <- IntensData
    datatable(DF)
  })
  output$folder <- renderPrint({
    paste0("Folder for Results: ", parseDirPath(c(wd="."), input$folder))
  })
  
  #//////// END OF CODE FOR HELPFUL TEXTS /////////////
  
  #//////// CODE FOR DOWNLOAD BUTTON /////////////
  
  #_________________DOWNLOAD ____________________
  
  #allows user to download data
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
       write.csv(IntensData, file, row.names = FALSE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = "CombinedData.csv",
    content = function(file) {
       write.csv(CombinedData, file, row.names = FALSE)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = "PreprocessedData.csv",
    content = function(file) {
      write.csv(AveragedData, file, row.names = FALSE)
    }
  )
  shinyDirChoose(input, 'folder', roots=c(wd='.'), filetypes=c(''))

  #//////// END OF CODE FOR DOWNLOAD BUTTON /////////////
  
  #//////// START OF CODE FOR STOP SHINY APP ///////////////
  #When user clicks the return to command line button
  #stops the shiny app
  # prevents user from quitting shiny using ^C on commandline 
  observe({recursiveStop()})
  
  recursiveStop <- eventReactive(input$stop,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(CombinedData, pos = 1))
      suppressWarnings(rm(AveragedData, pos = 1))
      stopApp()
    })
  })
  #//////// END OF CODE FOR STOP SHINY APP ///////////////
} #end of server

shinyApp(ui, server)

