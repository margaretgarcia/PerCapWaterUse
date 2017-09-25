# ui.R

shinyUI(fluidPage(
  titlePanel("Per Capita Demand Change in Las Vegas"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("Model", 
                  label = "Model Selection:",
                  choices = list("Price Only", "Price & Building Codes", 
                                 "Building Codes & Stress Response",
                                 "Price, Codes & Stress Response"),
                  selected = "Price Only"),
      br(),
      p("This RShiny application is built to explore the drivers of per capita 
        demand change in Las Vegas from 1997 to 2012. Price refers to the marginal
        water rate for the average residential customer. Building code changes refer
        modifications in the requirements for water efficient fixtures and landscapes
        required in new construction. Stress response refers to the set of
        demand management measures implemented in response to water stress or drought.")
      ),
    
    mainPanel(
      plotOutput(outputId = "Plot", width = "100%")
    )
      )
    ))