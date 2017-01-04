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
                round(input$dom_sales,digits = 1), # US Sales
                round((1-input$dom_sales/100)*100,digits = 1), # Foreign Sales
                round(input$c6*input$c7/100,digits = 1), # US Costs 
                round((1-input$c7/100)*input$c6,digits = 1), # Foreign Costs
                round(input$dom_sales+(1-input$dom_sales/100)*100  # Tax Base
                  -input$c6*input$c7/100-(1-input$c7/100)*input$c6,digits = 1),
                round(0.01*35*((input$dom_sales)+((1-input$dom_sales/100)*100) # Taxes
                  -(input$c6*input$c7/100)-((1-input$c7/100)*input$c6)),digits = 1),
                round((input$dom_sales+(1-input$dom_sales/100)*100 - # After Tax Profit
                  input$c6*input$c7/100-(1-input$c7/100)*input$c6- 
                    (0.01*35*(input$dom_sales+(1-input$dom_sales/100)*100-
                      input$c6*input$c7/100-(1-input$c7/100)*input$c6)))*10)/10
                )),
      
      "NTR" = as.character(c(
                round(input$dom_sales,digits = 1), # US Sales
                round((1-input$dom_sales/100)*100,digits = 1), # Foreign Sales
                round(input$c6*input$c7/100,digits = 1), # US Costs 
                round((1-input$c7/100)*input$c6,digits = 1), # Foreign Costs
                round(input$dom_sales + (1-input$dom_sales/100)*100 - # Tax Base
                  input$c6*input$c7/100- (1-input$c7/100)*input$c6,digits = 1),
                round(0.01*input$new_tax_rate*(input$dom_sales + (1-input$dom_sales/100)*100- # Taxes
                  input$c6*input$c7/100-(1-input$c7/100)*input$c6),digits = 1),
                round((input$dom_sales + (1-input$dom_sales/100)*100 - # After Tax Profit
                  input$c6*input$c7/100 -
                    (1-input$c7/100)*input$c6 - 0.01*input$new_tax_rate*(input$dom_sales+
                      (1-input$dom_sales/100)*100 - input$c6*input$c7/100-
                        (1-input$c7/100)*input$c6))*10)/10
                )),
      
     "NTR.BA" = as.character(c( 
                round(input$dom_sales + max((((1-input$c7/100)*input$c6*(1-input$currency_adj/100))- # US Sales
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0),digits = 1),
                round((1-input$dom_sales/100)*100*(1-input$new_tax_rate*input$currency_adj/10000),digits = 1), # Foreign Sales
                round(input$c6*input$c7/100,digits = 1), # US Costs 
                round((1-input$c7/100)*input$c6*(1-input$new_tax_rate*input$currency_adj/10000),digits = 1), # Foreign Costs
                round(input$dom_sales + max((((1-input$c7/100)*input$c6*(1-input$currency_adj/100))- # Tax Base
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0)
                    - input$c6*input$c7/100,digits = 1),
                round(0.01*input$new_tax_rate*(input$dom_sales + max((((1-input$c7/100)*input$c6*(1-input$currency_adj/100))- # Taxes
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) 
                    - input$c6*input$c7/100),digits = 1),               
                round(input$dom_sales + max((((1-input$c7/100)*input$c6*(1-input$currency_adj/100))- # After Tax Profit
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) +
                    (1-input$dom_sales/100)*100*(1-input$new_tax_rate*input$currency_adj/10000) - input$c6*input$c7/100 -
                      (1-input$c7/100)*input$c6*(1-input$new_tax_rate*input$currency_adj/10000) -
                        (0.01*input$new_tax_rate*(input$dom_sales + max((((1-input$c7/100)*input$c6*(1-input$currency_adj/100))-
                          ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) 
                            - input$c6*input$c7/100)),1)
                )), 
      stringsAsFactors=FALSE)
  }) 
  output$values <- renderTable({
    sliderValues()
  })
}