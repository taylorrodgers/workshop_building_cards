library(shiny)
library(bslib)
library(tidyverse)
library(purrr)
library(ggthemes)

company_data <-  readRDS("./company_data.rds")
historical_prices <-  readRDS("./historical_prices.rds")

ui <- page(
  title = titlePanel("Custom Card"),
  theme = bs_theme(
    version = 5,
    base_font = font_google("Lato", wght = "200..900", local = FALSE)),
  div(class = "row justify-content-center align-items-center", 
      div(class = "col-11 col-sm-7 col-md-5 col-lg-4 col-xl-3 col-xxl-2",
          h2("Our Custom Card"),
          selectInput("filter_company", 
                      label = strong("Select a company:"), 
                      choices = company_data$Symbol, 
                      width = "100%"),
          div(class = "card shadow-sm",
              style = "min-height: 300px; border-top: solid black;",
              div(class = "card-body",
                  div(style = "height: 200px",
                      plotOutput("card_financials_trend")),
                  br(),
                  tableOutput("card_financials_table")
              )
          )
      )
  )
)

server <- function(input, output, session) {
  
  output$card_financials_trend <- renderPlot({
    
    ggplot(historical_prices |> 
             filter(Ticker == input$filter_company),
           aes(x = Date,
               y = Close)) +
      geom_line(linewidth = 1.03, color = "#36454F") +
      ggthemes::theme_pander() +
      theme(text = element_text(
        family = "Lato"
      ),
      axis.title.y = element_blank(),
      axis.title.x = element_blank())
    
  }, height = 190)
  
  output$card_financials_table <- renderTable({
    
    company_data |> filter(Symbol == input$filter_company) |> 
      mutate(across(everything(), as.character)) |>
      transmute(
        "Market Cap" = Market.Cap,
        "52 Week Low" = X52.Week.High,
        "52 Week High" = X52.Week.Low,
        "Price-to-Earnings" = Price.Earnings,
        "Price-to-Book" = Price.Book,
        "Price-to-Sales" = Price.Sales,
        "Dividend Yield" = Dividend.Yield
      ) |>
      pivot_longer(everything())
    
  }, colnames = FALSE, align = "lr", width = "100%")
  
}

shinyApp(ui, server)