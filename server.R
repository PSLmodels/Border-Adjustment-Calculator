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
                # US Sales
                round(input$dom_sales,digits = 1),

                # Foreign Sales
                round((1-input$dom_sales/100)*100,digits = 1), 

                # US Costs 
                round(input$tot_sales*input$dom_costs/100,digits = 1), 

                # Foreign Costs
                round((1-input$dom_costs/100)*input$tot_sales,digits = 1),

                # Tax Base = (US Sales + Foreign Sales) - (US Costs + Foreign Costs)
                round(input$dom_sales+(1-input$dom_sales/100)*100 
                  -input$tot_sales*input$dom_costs/100-(1-input$dom_costs/100)*input$tot_sales,digits = 1),

                # Taxes
                round(0.01*35*((input$dom_sales)+((1-input$dom_sales/100)*100)
                  -(input$tot_sales*input$dom_costs/100)-((1-input$dom_costs/100)*input$tot_sales)),digits = 1),

                # For original tax rate, after Tax Profit = (Tax Base - Taxes)
                round((input$dom_sales+(1-input$dom_sales/100)*100 - 
                  input$tot_sales*input$dom_costs/100-(1-input$dom_costs/100)*input$tot_sales- 
                    (0.01*35*(input$dom_sales+(1-input$dom_sales/100)*100-
                      input$tot_sales*input$dom_costs/100-(1-input$dom_costs/100)*input$tot_sales)))*10)/10
                )),
      
      "NTR" = as.character(c(
                # US Sales
                round(input$dom_sales,digits = 1),

                # Foreign Sales
                round((1-input$dom_sales/100)*100,digits = 1),

                # US Costs 
                round(input$tot_sales*input$dom_costs/100,digits = 1),

                # Foreign Costs
                round((1-input$dom_costs/100)*input$tot_sales,digits = 1),

                # Tax Base = (US Sales + Foreign Sales) - (US Costs + Foreign Costs)
                round(input$dom_sales + (1-input$dom_sales/100)*100 -
                  input$tot_sales*input$dom_costs/100- (1-input$dom_costs/100)*input$tot_sales,digits = 1),

                # Taxes
                round(0.01*input$new_tax_rate*(input$dom_sales + (1-input$dom_sales/100)*100-
                  input$tot_sales*input$dom_costs/100-(1-input$dom_costs/100)*input$tot_sales),digits = 1),

                # For new tax rate , after Tax Profit = (Tax Base - Taxes)
                round((input$dom_sales + (1-input$dom_sales/100)*100 - 
                  input$tot_sales*input$dom_costs/100 -
                    (1-input$dom_costs/100)*input$tot_sales - 0.01*input$new_tax_rate*(input$dom_sales+
                      (1-input$dom_sales/100)*100 - input$tot_sales*input$dom_costs/100-
                        (1-input$dom_costs/100)*input$tot_sales))*10)/10
                )),
      
     "NTR.BA" = as.character(c( 
                # US Sales = Domestic Sales + Max(((1 - Domestic Costs) * 100 * Total Sales * (1 - Currency Adjustment) - 
                # (1 - Domestic Sales) * 100 * (1 - Currency Adjustment)) * New Tax Rate * (1/(1 - New Tax Rate)) * Import Cost , 0) 
                round(input$dom_sales + max((((1-input$dom_costs/100)*input$tot_sales*(1-input$currency_adj/100))-
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0),digits = 1),
                
                # Foreign Sales = (1 - Domestic Sales) * 100 * (1 - New Tax Rate * Currency Adjustment)
                round((1-input$dom_sales/100)*100*(1-input$new_tax_rate*input$currency_adj/10000),digits = 1),
                
                # US Costs = 100 * Total Sales * Domestic Costs
                round(input$tot_sales*input$dom_costs/100,digits = 1),
                
                # Foreign Costs = (1 - Domestic Costs) * 100 * Total Sales * (1 - New Tax Rate * Currency Adjustment)
                round((1-input$dom_costs/100)*input$tot_sales*(1-input$new_tax_rate*input$currency_adj/10000),digits = 1),
                
                # Tax Base = (US Sales - US Costs)
                round(input$dom_sales + max((((1-input$dom_costs/100)*input$tot_sales*(1-input$currency_adj/100))- 
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0)
                    - input$tot_sales*input$dom_costs/100,digits = 1),
                
                # Taxes = New Tax Rate * Tax Base
                round(0.01*input$new_tax_rate*(input$dom_sales + max((((1-input$dom_costs/100)*input$tot_sales*(1-input$currency_adj/100))-
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) 
                    - input$tot_sales*input$dom_costs/100),digits = 1), 
                
                # For new tax rate with border adjustment, after Tax Profit = (US Sales + Foreign Sales) - (US Costs + Foreign Costs) - Taxes       
                round(input$dom_sales + max((((1-input$dom_costs/100)*input$tot_sales*(1-input$currency_adj/100))-
                  ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) +
                    (1-input$dom_sales/100)*100*(1-input$new_tax_rate*input$currency_adj/10000) - input$tot_sales*input$dom_costs/100 -
                      (1-input$dom_costs/100)*input$tot_sales*(1-input$new_tax_rate*input$currency_adj/10000) -
                        (0.01*input$new_tax_rate*(input$dom_sales + max((((1-input$dom_costs/100)*input$tot_sales*(1-input$currency_adj/100))-
                          ((1-input$dom_sales/100)*100*(1-input$currency_adj/100)))*input$new_tax_rate*(1/(1-input$new_tax_rate/100))*input$import_cost/10000,0) 
                            - input$tot_sales*input$dom_costs/100)),1)
                )), 
      stringsAsFactors=FALSE)
  }) 
  output$values <- renderTable({
    sliderValues()
  })
}