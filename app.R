library(shiny)
library(gapminder)
library(plotly)


ui <- fluidPage(
  
  tags$head(tags$style(HTML('* {font-family: "Courier"};'))),
  
  titlePanel("Life expectancy ver GDP per Capita: 1950s - 2000s"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("The dataset is retrieved from Gapminder package in CRAN. 
               Gapminder is an independent Swedish foundation with no political, religious, 
               or economic affiliations.",tags$br(),tags$br(), "The visualization is created by Sohyun Park."),
      
      selectInput("continent","Continent:",c("All","Asia","Americas","Africa","Europe","Oceania"))),
    
    mainPanel(
      plotlyOutput("plt"),
      textOutput("cont")
    ))
)


server <- function(input, output) {
  
  gapped <- reactive({
    if(input$continent == 'All'){
      gapminder
    } else {gapminder %>% filter(continent == input$continent)}
  })
  
  output$plt <- renderPlotly({
    
    plot_ly(gapped(), x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
            text = ~country, hoverinfo = "text") %>%
      layout(xaxis = list(type = "log")) %>%
      add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
      animation_opts(500, easing = "elastic", redraw = FALSE) %>%
      animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
      animation_slider(
        currentvalue = list(prefix = "YEAR ", font = list(color="red")))
    
  })
  
  output$cont <- renderText({paste("dflk;",input$continent)}) 
  
}

shinyApp(ui, server)
