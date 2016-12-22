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
                                   round(input$c5,digits = 1),
                                   round((1-input$c5/100)*100,digits = 1), 
                                   round(input$c6*input$c7/100,digits = 1),
                                   round((1-input$c7/100)*input$c6,digits = 1),
                                   round(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6,digits = 1),
                                   round(0.35*(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6),digits = 1),
                                   round(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6 - 
                                           0.35*(input$c5+(1-input$c5/100)*100-input$c6*input$c7/100-(1-input$c7/100)*input$c6))),digits = 1),
      
      "NTR" = as.character(c(round(input$c5,digits = 1),
                                   round((1-input$c5/100)*100,digits = 1),
                                   round(input$c6*input$c7/100,digits = 1),
                                   round((1-input$c7/100)*input$c6,digits = 1),
                                   round(input$c5,digits = 1)+
                                     round((1-input$c5/100)*100,digits = 1)-
                                     round(input$c6*input$c7/100,digits = 1)-
                                     round((1-input$c7/100)*input$c6,digits = 1),
                                   round(0.01*input$c8*(round(input$c5,digits = 1)+
                                                          round((1-input$c5/100)*100,digits = 1)-
                                                          round(input$c6*input$c7/100,digits = 1)-
                                                          round((1-input$c7/100)*input$c6,digits = 1)),digits = 1),
                                   round(input$c5,digits = 1)+
                                     round((1-input$c5/100)*100,digits = 1)-
                                     round(input$c6*input$c7/100,digits = 1)-
                                     round((1-input$c7/100)*input$c6,digits = 1)-round(0.01*input$c8*(round(input$c5,digits = 1)+
                                                                                                        round((1-input$c5/100)*100,digits = 1)-
                                                                                                        round(input$c6*input$c7/100,digits = 1)-
                                                                                                        round((1-input$c7/100)*input$c6,digits = 1)),digits = 1))),
      
     "NTR.BA" = as.character(c(  round(input$c5 + max((((1-input$c7/100)*input$c6*(1-input$c9/100))-
                                                    ((1-input$c5/100)*100*(1-input$c9/100)))*input$c8*(1/(1-input$c8/100))*input$c10/10000,0),digits = 1),
                                  round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 1),
                                  round(input$c6*input$c7/100,digits = 1),
                                  round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 1),
                               round(round(input$c5 + max((((1-input$c7/100)*input$c6*(1-input$c9/100))-
                                                         ((1-input$c5/100)*100*(1-input$c9/100)))*input$c8*(1/(1-input$c8/100))*input$c10/10000,0),digits = 1) 
                                     - round(input$c6*input$c7/100,digits = 1),digits = 1),
                                  round(0.01*input$c8*(round(input$c5 + max((((1-input$c7/100)*input$c6*(1-input$c9/100))-
                                                                           ((1-input$c5/100)*100*(1-input$c9/100)))*input$c8*(1/(1-input$c8/100))*input$c10/10000,0),digits = 1) 
                                                       - round(input$c6*input$c7/100,digits = 1)),digits = 1),
                               round(round(input$c5 + max((((1-input$c7/100)*input$c6*(1-input$c9/100))-
                                              ((1-input$c5/100)*100*(1-input$c9/100)))*input$c8*(1/(1-input$c8/100))*input$c10/10000,0),digits = 1)+
                               round((1-input$c5/100)*100*(1-input$c8*input$c9/10000),digits = 1) -
                                 round(input$c6*input$c7/100,digits = 1)-
                                 round((1-input$c7/100)*input$c6*(1-input$c8*input$c9/10000),digits = 1) -
                                 round(0.01*input$c8*(round(input$c5 + max((((1-input$c7/100)*input$c6*(1-input$c9/100))-
                                      ((1-input$c5/100)*100*(1-input$c9/100)))*input$c8*(1/(1-input$c8/100))*input$c10/10000,0),digits =1) 
                                              - round(input$c6*input$c7/100,digits = 1)),digits = 1),digits=1)
                                  )), 
      stringsAsFactors=FALSE)
  }) 
  output$values <- renderTable({
    sliderValues()
  })
}