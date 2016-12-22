library(shiny)
function(input, output) {
  sliderValues <- reactive({
    data.frame(
      "Items" = c(
               "US Sales",
               "Foreign Sales", 
               "US Costs",
               "Foreign Costs",
               "Tax Base",
               "Taxes",
               "After Tax Profit"),
      "OTR" = as.character(c(
                                   input$c5,
                                   (1-input$c5/100)*100, 
                                   input$c6*input$c7/100,
                                   (1-input$c7/100)*input$c6,
                                   input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6,
                                   0.35*(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6),
                                   input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6 - 0.35*(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6))),
      
      "NTR" = as.character(c(round(input$c5,digits = 2),
                                   round((1-input$c5/100)*100,digits = 2),
                                   round(input$c6*input$c7/100,digits = 2),
                                   round((1-input$c7/100)*input$c6,digits = 2),
                                   round(input$c5,digits = 2)+
                                     round((1-input$c5/100)*100,digits = 2)-
                                     round(input$c6*input$c7/100,digits = 2)-
                                     round((1-input$c7/100)*input$c6,digits = 2),
                                   round(0.01*input$c8*(round(input$c5,digits = 2)+
                                                          round((1-input$c5/100)*100,digits = 2)-
                                                          round(input$c6*input$c7/100,digits = 2)-
                                                          round((1-input$c7/100)*input$c6,digits = 2)),digits = 2),
                                   round(input$c5,digits = 2)+
                                     round((1-input$c5/100)*100,digits = 2)-
                                     round(input$c6*input$c7/100,digits = 2)-
                                     round((1-input$c7/100)*input$c6,digits = 2)-round(0.01*input$c8*(round(input$c5,digits = 2)+
                                                                                                        round((1-input$c5/100)*100,digits = 2)-
                                                                                                        round(input$c6*input$c7/100,digits = 2)-
                                                                                                        round((1-input$c7/100)*input$c6,digits = 2)),digits = 2))),
                          
     "NTR.BA" = as.character(c(round(input$c5+
                                       0.4*(1/(1-0.4))*input$c10*(1-input$c9/100)*(round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                          round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2))/100
                                     ,digits = 2),
                                  round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2),
                                  round(input$c6*input$c7/100,digits = 2),
                                  round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2),
                               round(input$c5+
                                       0.4*(1/(1-0.4))*input$c10*(1-input$c9/100)*(round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                                                                     round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2))/100
                                     ,digits = 2) - round(input$c6*input$c7/100,digits = 2),
                                  round(0.01*input$c8*(round(input$c5+
                                                               0.4*(1/(1-0.4))*input$c10*(1-input$c9/100)*(round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                                                                                             round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2))/100
                                                             ,digits = 2) - round(input$c6*input$c7/100,digits = 2)),digits = 2),
                               round(input$c5+
                                       0.4*(1/(1-0.4))*input$c10*(1-input$c9/100)*(round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                       round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2))/100
                                     ,digits = 2)+round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2)-
                                 round(input$c6*input$c7/100,digits = 2)- round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                 round(0.01*input$c8*(round(input$c5+
                                 0.4*(1/(1-0.4))*input$c10*(1-input$c9/100)*(round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 2)-
                                       round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 2))/100
                                       ,digits = 2) - round(input$c6*input$c7/100,digits = 2)),digits = 2)  
                                  )), 
      stringsAsFactors=FALSE)
  }) 
  output$values <- renderTable({
    sliderValues()
  })
}