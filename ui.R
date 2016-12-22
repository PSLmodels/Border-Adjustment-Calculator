library(shiny)

fluidPage(
  
  #  logo 
  headerPanel(
    list(tags$img(src="ospc.png", width="130px",height="33px"), "How Border Adjustments Work"),
    windowTitle="How Border Adjustments Work"
  ),
  # Slider 
  fluidRow(
    column(3,
      wellPanel(strong("This example illustrates how a border adjustment would affect a firm with $100 in sales 
                        and the other characteristics supplied by the user. Under full currency adjustment, the 
                        dollar appreciates by the same rate as the tax -- this effect is predicted by efficient 
                        market models. This simple model assumes higher import costs are not passed through to 
                        consumers when the currency adjustment is incomplete, and it assumes that firms 
                        can fully deduct losses. ")
      ),
      wellPanel(
        sliderInput("c8", "Tax Rate (%)", min=0, max=100, value=50),
        strong("Note the current law assumes a tax rate of 35%, with no Border adjustment.")),
      wellPanel( sliderInput("c9", "Full Currency Adjustment (%)", min=0, max=100, value=70))
      ),
    column(5,
      wellPanel(
        sliderInput("c5", "Domestic Sales / Total Sales (%)", min=0, max=100, value=100),
        sliderInput("c6", "Total Costs / Total Sales (%)", min=0, max=100, value=60),
        sliderInput("c7", "Domestic Costs / Total Costs (%)", min=0, max=100, value=90)
      ),
      # Table header abbreviations
      strong("Table Keys: OTR: old tax rate (35%); NTR: new tax rate;"),
      strong("NTR.BA: new tax rate and border adjustment"),
      wellPanel(ableOutput("values")
      ),
      br(),
      h5("Credits:"),
      h5("Concept: Kevin Hassett"),
      h5("Development: Sean Wang"),
      h5("Open source software used:"),
      tags$a(href="https://github.com/open-source-economics/border-adjustment-calculator","Border-Adjustment-Calculator for tax computations."),
      tags$a(href="https://shiny.rstudio.com/"," Shiny for visualization.")
      )
  )
)