library(shiny)
fluidPage(
  tags$img(src="ospc.png", width="130px",height="33px"),
  tags$h2("How Border Adjustments Work"),
  fluidRow(
    column(3,
           wellPanel(strong("This example illustrates how a border adjustment would affect a firm with $100 
                            in initial sales and other characteristics supplied by the user. Under full 
                            currency adjustment, the dollar appreciates by the same rate as the tax -- 
                            this effect is predicted by efficient market models. Under partial currency 
                            adjustments, firms may be able to pass on some of their increased import 
                            costs to consumers.  This model assumes that firms can fully deduct losses 
                            against past and future taxes.")),
      wellPanel(
        sliderInput("c8", "Tax Rate (%)",
                  min=0, max=100, value=20),
        strong("Note the current law assumes a tax rate of 35%, with no Border adjustment.")),
      wellPanel(
      sliderInput("c9", "Full Currency Adjustment (%)",
                  min=0, max=100, value=100),
      sliderInput("c10", "Import Costs Passed on to Consumers (%)",
                  min=0, max=100, value=0))
    ),
    column(5,
           wellPanel(
      sliderInput("c5", "Domestic Sales / Total Sales (%)",
                  min=0, max=100, value=50),
      sliderInput("c6", "Total Costs / Total Sales (%)",
                  min=0, max=100, value=50),
      sliderInput("c7", "Domestic Costs / Total Costs (%)",
                  min=0, max=100, value=50)
      ),
      strong("Table Keys:"),
      h5("OTR: old tax rate (35%)"),
      h5("NTR: new tax rate"),
      h5("NTR.BA: new tax rate and border adjustment"),
      wellPanel(
        tableOutput("values")),
      br(),
      h5("Credits:"),
      h5("Concept: Kevin Hassett"),
      h5("Development: Sean Wang"),
      h5("Open source software used:"),
      tags$div(
        tags$a(href="https://github.com/open-source-economics/border-adjustment-calculator","Border-Adjustment-Calculator"),
        "for tax computations."
      ),
      tags$div(
        tags$a(href="https://shiny.rstudio.com/"," Shiny"),
        "for visualization."
      )
    )
  )
)