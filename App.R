library(dplyr)
library(ggvis)
library(shiny)
library(googleVis)
library(ggplot2)
library(GGally)
library(plotly)

#======================================== functions =============================================
fb <- read.table("dataset_Facebook.csv", sep = ";", header = TRUE)
fb <- na.omit(fb)

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

findUniqueCombine <- function(data, var1, var2){
    data[!duplicated(data[,c(var1,var2)]),]
}

#======================================== shiny UI =============================================
ui <- navbarPage(
    titlePanel('HW3 Lin Chen'),
    tabPanel("Bubble chart",
    h4("Facebook Data"),
    htmlOutput("bubble_plot")
    ),
    
    tabPanel("FB Scatter plot",
            checkboxGroupInput("scatterCols", inline = T,
                                                label = 'Scatter plot',
                                                choices = list('Total.Interactions' = 'Total.Interactions',
                                                               'like' = 'like',
                                                               'comment' = 'comment',
                                                               'share' = 'share'),
                                                selected = c('like', 'share')),
            plotlyOutput("scatter_plot")),
    
    tabPanel("FB Parallel plot",
             sidebarPanel(width = 3,
                          selectizeInput("parallel_vars", "Select parallel variables(>1)",
                                         colnames(fb), multiple = T,
                                         selected = c("Post.Month", 
                                                      "Type",
                                                      "Paid"))),
             plotlyOutput("parallel_plot"))
)

#======================================== shiny server =============================================
server <- function(input, output, session) {
    # bubble
    output$bubble_plot <- renderGvis({gvisMotionChart(findUniqueCombine(fb, "Total.Interactions", "Post.Month"), 
                                                      idvar="Total.Interactions", 
                                                      timevar="Post.Month")})
    
    # scatter
    output$scatter_plot <- renderPlotly({
        validate(
            need(length(input$scatterCols) >= 2, label = "At least 2 variables")
        )
    
        gg1 <- makePairs(fb[,input$scatterCols])
        
        mega_facebook <- data.frame(gg1$all, Type=rep(fb$Type, length=nrow(gg1$all)))
        
        # pairs 
        ggplot(mega_facebook, aes_string(x = "x", y = "y")) + 
            facet_grid(xvar ~ yvar, scales = "free") + 
            geom_point(aes(colour=Type), na.rm = TRUE, alpha=0.8) + 
            stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                         data = gg1$densities, position = "identity", 
                         colour = "grey20", geom = "line") +
            theme_bw() +
            xlab("") + ylab("")
    })
    
    # parallel
    output$parallel_plot <- renderPlotly({
        ggparcoord(fb, columns = which(colnames(fb) %in% input$parallel_vars),
                   groupColumn = 1,
                   scale = "uniminmax", 
                   scaleSummary = "center",
                   showPoints = T,
                   alphaLines = 0.5,
                   title= "Parallel Plot")
        
    })
}

shinyApp(ui = ui, server = server)
