OTR_TB <- function(dom_costs, dom_sales, tot_sales){
    return(round(dom_sales+(1-dom_sales/100)*100 
                  -tot_sales*dom_costs/100-(1-dom_costs/100)*tot_sales,digits = 1))
}

OTR_ATP <- function(dom_costs, dom_sales, tot_sales){
    return(round((dom_sales+(1-dom_sales/100)*100 - 
                  tot_sales*dom_costs/100-(1-dom_costs/100)*tot_sales- 
                    (0.01*35*(dom_sales+(1-dom_sales/100)*100-
                      tot_sales*dom_costs/100-(1-dom_costs/100)*tot_sales)))*10)/10
    )
}

NTR_TB <- function(dom_costs, dom_sales, tot_sales){
    return(round(dom_sales + (1-dom_sales/100)*100 -
                  tot_sales*dom_costs/100- (1-dom_costs/100)*tot_sales,digits = 1)
    )
}

NTR_ATP <- function(dom_costs, dom_sales, tot_sales, new_tax_rate){
    return(round((dom_sales + (1-dom_sales/100)*100 - 
                  tot_sales*dom_costs/100 -
                    (1-dom_costs/100)*tot_sales - 0.01*new_tax_rate*(dom_sales+
                      (1-dom_sales/100)*100 - tot_sales*dom_costs/100-
                        (1-dom_costs/100)*tot_sales))*10)/10
    )
}

NTR_BA_USS <- function(dom_costs, dom_sales, tot_sales, currency_adj, import_cost, new_tax_rate){
    return(round(dom_sales + max((((1-dom_costs/100)*tot_sales*(1-currency_adj/100))-
                  ((1-dom_sales/100)*100*(1-currency_adj/100)))*new_tax_rate*(1/(1-new_tax_rate/100))*import_cost/10000,0),digits = 1)
    )
}

NTR_BA_TB <- function(dom_costs, dom_sales, tot_sales, currency_adj, import_cost, new_tax_rate){
    return(round(dom_sales + max((((1-dom_costs/100)*tot_sales*(1-currency_adj/100))- 
                  ((1-dom_sales/100)*100*(1-currency_adj/100)))*new_tax_rate*(1/(1-new_tax_rate/100))*import_cost/10000,0)
                    - tot_sales*dom_costs/100,digits = 1)
    )
}

NTR_BA_ATP <- function(dom_costs, dom_sales, tot_sales, currency_adj, import_cost, new_tax_rate){
    return(round(dom_sales + max((((1-dom_costs/100)*tot_sales*(1-currency_adj/100))-
                               ((1-dom_sales/100)*100*(1-currency_adj/100)))*new_tax_rate*(1/(1-new_tax_rate/100))*import_cost/10000,0) +
        (1-dom_sales/100)*100*(1-new_tax_rate*currency_adj/10000) - tot_sales*dom_costs/100 -
        (1-dom_costs/100)*tot_sales*(1-new_tax_rate*currency_adj/10000) -
        (0.01*new_tax_rate*(dom_sales + max((((1-dom_costs/100)*tot_sales*(1-currency_adj/100))-
                                                           ((1-dom_sales/100)*100*(1-currency_adj/100)))*new_tax_rate*(1/(1-new_tax_rate/100))*import_cost/10000,0) 
                                  - tot_sales*dom_costs/100)),1))
}

test_that("NTR.BA after tax profit", {  
    results = NTR_BA_ATP(20,50,30,80,55,25)
    expect_that( results, is_a("numeric") )
    expect_that( length(results), equals(1) )
    expect_that( results == 53.8, is_true() )
})