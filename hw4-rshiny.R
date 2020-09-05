library(shinydashboard)
library(ggplot2)
library(dplyr)
library(stringr)
mobile_devices <- read.csv("devices.csv")


model1 <- mobile_devices %>%
  select(c("brand", "model", "price")) %>%
  filter(price > 0)


model1 <- na.omit(model1)

#---------------------------------------------------------------------------------------

model2 <- mobile_devices %>%
  select(c("brand", "model", "display_size", "battery_mah_size")) 

model2 <- na.omit(model2)

#---------------------------------------------------------------------------------------

model3 <- mobile_devices %>%
  mutate(ram = ram_gb_size) %>%
  mutate(ram_mb_size = ifelse(str_detect(ram,'GB$'),
                              as.numeric(str_extract(ram,'^[0-9]+'))*1024,
                              as.numeric(str_extract(ram,'^[0-9]+')))) %>%
  mutate(memory = memory_gb_size) %>%
  mutate(memory_mb_size = ifelse(str_detect(memory,'GB$'),
                                 as.numeric(str_extract(memory,'^[0-9]+'))*1024,
                                 as.numeric(str_extract(memory,'^[0-9]+')))) %>%
  select(c("brand", "model", "display_size", "price", "ram_mb_size", "memory_mb_size", "camera_megapixels"))

model3 <- na.omit(model3)

#to make it in graph look nicer
colnames(model3)[3:7] <- c("Display_size", "Price", "Ram_mb_size", "Memory_mb_size", "Camera_megapixels") 

#---------------------------------------------------------------------------------------


modelTable <- mobile_devices %>%
  mutate(ram = ram_gb_size) %>%
  mutate(ram_mb_size = ifelse(str_detect(ram,'GB$'),
                              as.numeric(str_extract(ram,'^[0-9]+'))*1024,
                              as.numeric(str_extract(ram,'^[0-9]+')))) %>%
  mutate(memory = memory_gb_size) %>%
  mutate(memory_mb_size = ifelse(str_detect(memory,'GB$'),
                                 as.numeric(str_extract(memory,'^[0-9]+'))*1024,
                                 as.numeric(str_extract(memory,'^[0-9]+')))) %>%
  filter(price <=1000 & price > 0)

modelTable <- modelTable %>%
  select(c(1,2,7,10,32,34,30))
modelTable <- na.omit(modelTable)

i <- c(3:6)
modelTable[ , i] <- apply(modelTable[ , i], 2,            
                          function(x) as.numeric(x))
#---------------------------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title ="Phone analysis"),
  dashboardSidebar(
                   selectInput(inputId = "brand", label="Input for Brand name for Top pricy phones histogram",
                               choices = sort(model1$brand),
                               selected = "-"),
                   selectInput(inputId = "brand1", label="Input for Brand name for Display size and Battery scatterplot",
                               choices = sort(model2$brand),
                               selected = "-"),
                   selectInput(inputId = "brand2", label="Input for Brand name for boxplot",
                               choices = sort(model3$brand),
                               selected = "-"),
                   selectInput(inputId = "boxChar", label="Input for characteristic variable for boxplot",
                               choices = colnames(model3)[3:7],
                               selected = "-"),
                   selectInput(inputId = "table", label="Input for Summary Table",
                               choices = colnames(modelTable)[3:6],
                               selected = "memory_mb_size")),
  
  dashboardBody(
    fluidRow(
      splitLayout(plotOutput("scatterplot1",  width = "95%"), 
                  plotOutput("scatterplot2",  width = "95%"))
    ),
    br(),
    fluidRow(
      splitLayout(cellWidths = c("50%"), plotOutput("boxplot"), align = "center")
    ),
    br(),
    fluidRow(
      splitLayout(cellWidths = c("95%"), dataTableOutput("summary"), align = "center")
    )
  )
    
)

server <- function(input, output){
  output$scatterplot1 <- renderPlot({
    modelTemp <- model1[model1$brand == input$brand,]
    modelTemp$price <- sort(modelTemp$price, decreasing = T)
    modelTemp <- head(modelTemp,10)
    ggplot(data=modelTemp, aes(x=model, y=price, fill=price))+
      geom_histogram(stat = "identity") + 
      labs(title = paste("Top (up to 10) pricy phones for the brand", input$brand),
           x = "Phone model", y = "Price") +
      theme(axis.text.x = element_text(angle = 90))
  })
  output$scatterplot2 <- renderPlot({
    modelTemp <- model2[model2$brand == input$brand1,]
    ggplot(data=modelTemp, aes(x=display_size, y=battery_mah_size))+
      geom_point(color = "navy") + 
      labs(title = paste("Relationship between Display size and Battery size for the phones of the brand", input$brand1),
           x = "Display size", y = "Battery mAh size") +
      theme(plot.title = element_text(size=12))
  })
  output$boxplot <- renderPlot({
    modelTemp <- model3[model3$brand == input$brand2,]
    ggplot(data=modelTemp, aes_string(y=input$boxChar))+
      geom_boxplot(fill = "lightsteelblue3") + 
      labs(title = paste("Range of the", str_replace_all(input$boxChar, "_", " "), "for the phones of the brand", input$brand2),
           y = str_replace_all(input$boxChar, "_", " ")) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  })
  ############################################
  output$summary <- renderDataTable({
    modelTable[,c("model", "brand", "price", input$table)]
  })
  ############################################
  
}

shinyApp(ui, server)
