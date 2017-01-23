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

test_that("Testing Original Tax Rate #1", {  
  result_TB = OTR_TB(7,0,25)
  result_ATP = OTR_ATP(7,0,25)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( result_TB == 75, is_true() )
  expect_that( result_ATP == 48.8, is_true() )
})

test_that("Testing New Tax Rate #2", {  
  result_TB = NTR_TB(71,27,95)
  result_ATP = NTR_ATP(71,27,95,41)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( result_TB == 5, is_true() )
  expect_that( result_ATP == 2.9, is_true() )
})

test_that("Testing New Tax Rate #3", {  
  result_TB = NTR_TB(93,23,37)
  result_ATP = NTR_ATP(93,23,37,87)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( result_TB == 63, is_true() )
  expect_that( result_ATP == 8.2, is_true() )
})

test_that("Testing New Tax Rate with Border Adjustment #4", {  
  result_USS = NTR_BA_USS(28,58,83,80,27,27)
  result_TB = NTR_BA_TB(28,58,83,80,27,27)
  result_ATP = NTR_BA_ATP(28,58,83,80,27,27)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( result_USS, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( length(result_USS), equals(1) )
  expect_that( result_USS == 58.4, is_true() )
  expect_that( result_TB == 35.1, is_true() )
  expect_that( result_ATP == 11.7, is_true() )
})

test_that("Testing New Tax Rate with Border Adjustment #5", {  
  result_USS = NTR_BA_USS(100,36,64,63,92,98)
  result_TB = NTR_BA_TB(100,36,64,63,92,98)
  result_ATP = NTR_BA_ATP(100,36,64,63,92,98)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( result_USS, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( length(result_USS), equals(1) )
  expect_that( result_USS == 36, is_true() )
  expect_that( result_TB == -28, is_true() )
  expect_that( result_ATP == 23.9, is_true() )
})

test_that("Testing New Tax Rate with Border Adjustment #6", {  
  result_USS = NTR_BA_USS(80,9,23,16,40,0)
  result_TB = NTR_BA_TB(80,9,23,16,40,0)
  result_ATP = NTR_BA_ATP(80,9,23,16,40,0)
  expect_that( result_TB, is_a("numeric") )
  expect_that( result_ATP, is_a("numeric") )
  expect_that( result_USS, is_a("numeric") )
  expect_that( length(result_TB), equals(1) )
  expect_that( length(result_ATP), equals(1) )
  expect_that( length(result_USS), equals(1) )
  expect_that( result_USS == 9, is_true() )
  expect_that( result_TB == -9.4, is_true() )
  expect_that( result_ATP == 77, is_true() )
})