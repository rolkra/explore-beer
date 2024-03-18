library(shiny)

#-----------------------------------------------------------------------------
# shinyUI
#-----------------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "tg_count", 
                   label = "Target Group Size:", 
                   value = 1000, 
                   min = 1, 
                   max = 10000000),
      
      numericInput(inputId = "tg_response", 
                   label = "Target Group Response:", 
                   value = 50, 
                   min = 0, 
                   max = 10000000),
      
      numericInput(inputId = "cg_count", 
                   label = "Control Group Size:", 
                   value = 1000, 
                   min = 1, 
                   max = 10000000),
      
      numericInput(inputId = "cg_response", 
                   label = "Control Group Response:", 
                   value = 10, 
                   min = 0, 
                   max = 10000000),
      
      radioButtons(inputId = "max_p_value_pct", label = "Max p-value", 
                   choices = c("1%" = "1", "5%" = "5", "10%" = "10", "15%" = "15", "20%" = "20"),
                   selected = "5")
    ),
    
    mainPanel(
      plotOutput("plot")
    ) # end mainPanel
  ) # end sidePanel
) # end fluidPage

#-----------------------------------------------------------------------------
# shinyServer
#-----------------------------------------------------------------------------

server <- function(input, output) {
  output$plot <- renderPlot({
    
    # get input values
    max_p_value <- as.integer(input$max_p_value_pct) / 100
    targetgroup_count <- input$tg_count
    controlgroup_count <- input$cg_count
    targetgroup_response <- input$tg_response
    controlgroup_response <- input$cg_response
    
    # calc percent
    targetgroup_response_pct <- targetgroup_response/targetgroup_count
    controlgroup_response_pct <- controlgroup_response/controlgroup_count
    
    # statistical test
    result <- chisq.test(matrix(c(targetgroup_response,controlgroup_response,targetgroup_count-targetgroup_response,controlgroup_count-controlgroup_response),2,2))                      
    
    
    # result text
    if (result$p.value <= max_p_value)  {
      result_txt <- paste ("significant, p value =", round(result$p.value,2))
      result_col <- "darkgreen"
    } else  {
      result_txt <- paste ("not significant, p value =", round(result$p.value,2))           
      result_col <- "red"
    }
    
    # define names
    tg_name <- paste0("target group\nsize=",targetgroup_count," ,resp=",targetgroup_response)
    cg_name <- paste0("control group\nsize=",controlgroup_count," ,resp=",controlgroup_response)
    title_name <- paste0("response in %\ntarget group vs control group")
    subtitle_name <- paste0("difference ", result_txt)
    
    # plot
    par(col.main = "black")
    par(col.sub = result_col)
    par(cex.sub = 1.5)
    b<-barplot(c(targetgroup_response/targetgroup_count*100,controlgroup_response/controlgroup_count*100), 
               names.arg = c(tg_name,cg_name),
               main = title_name,
               sub = subtitle_name, 
               col = "grey",
               border = FALSE,
               ylim = c(0, max(targetgroup_response_pct, controlgroup_response_pct)*100*1.3)
    )
    
    # x position of bars
    tg_x <- b[1,1]
    cg_x <- b[2,1]
    
    # plot lines
    abline(h=targetgroup_response_pct*100,col="darkgrey",lty="dotted")
    abline(h=controlgroup_response_pct*100,col="darkgrey",lty="dotted")
    
    # plot text on top of bars (response in %)
    text(tg_x, targetgroup_response_pct * 100, 
         paste0(round(targetgroup_response_pct*100,2),"%"), 
         pos = 3)
    
    text(cg_x, controlgroup_response_pct * 100, 
         paste0(round(controlgroup_response_pct*100,2),"%"), 
         pos = 3)
    
  })
}

#-----------------------------------------------------------------------------
# run APP
#-----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
