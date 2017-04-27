library(dplyr)
library(ggvis)
library(shiny)
library(ggplot2)
library(tidyr)
library(googleCharts)
library(GGally)

fb <- read.table("dataset_Facebook.csv", sep = ";", header = TRUE)
fb <- fb %>% drop_na()

makePairs <- function(data) 
{
    grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
    grid <- subset(grid, x != y)
    all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
        xcol <- grid[i, "x"]
        ycol <- grid[i, "y"]
        data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
                   x = data[, xcol], y = data[, ycol], data)
    }))
    all$xvar <- factor(all$xvar, levels = names(data))
    all$yvar <- factor(all$yvar, levels = names(data))
    densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
        data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
    }))
    list(all=all, densities=densities)
}

ui <- navbarPage(
    titlePanel('HW3 Lin Chen'),
    tabPanel("Bubble chart",
    fluidRow(column(5, h4("Facebook Data"))),
    fluidRow(column(3, ggvisOutput("ggvis")),
    fluidRow(
            shiny::column(4, offset = 4,
                          sliderInput("month", 
                                      "Post.Month", 
                                      min = min(fb$Post.Month), 
                                      max = max(fb$Post.Month), 
                                      value = 1, 
                                      step=1,
                                      sep = "",
                                      animate = FALSE)
            )
        )
    )
    ),
    tabPanel("Scatterplot",
             fluidRow(column(8,
                             checkboxGroupInput("scatterCols", inline = T,
                                                label = 'Plot Columns',
                                                choices = list('Type' = 'Type',
                                                               'like' = 'like',
                                                               'comment' = 'comment',
                                                               'shares' = 'shares'),
                                                selected = c('like', 'comment', 'shares'))
             )
             ),
             fluidRow(column(12, plotOutput("scatter_plot")))
    )
)


server <- function(input, output, session) {
    fb.data <- reactive({
        
        df <- 
                fb %>% filter(Post.Month == input$month) %>%
                select(Type, like, Total.Interactions,
                       Post.Month, share) %>%
                arrange(Type)
            return(df)
        })
    
    fb.data %>% ggvis(~Total.Interactions, ~like, size := ~share * 10, fill = ~Type) %>%
        layer_points(fill = ~Type) %>%
        add_axis("x", title = 'Total.Interactions', orient = "bottom") %>%
        add_axis("y", title = 'like', orient = "left") %>%
        scale_numeric("x", domain = c(0, 7000), nice = T, clamp = F) %>%
        scale_numeric("y", domain = c(0, 5500), nice = T, clamp = F) %>%
        bind_shiny("ggvis")
    
    scatter_plot <- reactive({input$scatterCols})
    output$scatterplot <- renderPlot({
        fb %>%
            select_(.dots = scatterCols()) %>%
            ggpairs() +
            theme_bw()
    }, height = 550)
}

shinyApp(ui = ui, server = server)
