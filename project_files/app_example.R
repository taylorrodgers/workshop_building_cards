library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

company_data <-  readRDS("./company_data.rds")
historical_prices <-  readRDS("./historical_prices.rds")

font_to_link <- font_link("Lato", href = "https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&display=swap")

ticker_summary <- function(ticker = NULL, 
                           name_label = NULL, 
                           price_data = NULL) {
  
  change_color <- ifelse(price_data$change < 0,  "red",
                         ifelse(price_data$change == 0, "black", 
                                "green"))
  
  change_symbol <- ifelse(price_data$change < 0, "caret-down",
                          ifelse(price_data$change == 0, "", 
                                 "caret-up"))
  
  tag_to_return <- div(
    class = "row px-1",
    div(class = "col-5",
        h4(class = "my-0",
           ticker),
        p(class = "lead my-0",
          style = "font-size: .85em; white-space: nowrap;",
          name_label)
    ),
    div(class = "col-7",
        style = "text-align: right;",
        h4(class = "my-0",
           paste0("$", round(price_data$Adjusted, 2))),
        p(class = "lead my-0",
          style = paste0("font-size: .85em; color:", change_color),
          icon(change_symbol), 
          paste0(round(price_data$change, 2), " (",
                 round(price_data$percentage_change,2) , "%)"))
    )
  )
  
  return(tag_to_return)
  
}


ui <- page(
  title = titlePanel("Custom Card"),
  theme = bs_theme(
    version = 5,
    base_font = font_to_link
  ),
  div(class = "row justify-content-center align-items-center", 
      div(class = "col-11 col-sm-7 col-md-5 col-lg-4 col-xl-3 col-xxl-2",
          selectInput("filter_company", 
                      label = strong("Select a company:"), 
                      choices = company_data$Symbol, 
                      width = "100%"),
          div(class = "card shadow-sm",
              style = "min-height: 300px; border-top: solid black;",
              div(class = "card-body",
                  uiOutput("card_financials"),
                  br(),
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
  
  output$card_financials <- renderUI({
    
    name_label <- company_data |> 
      filter(Symbol == input$filter_company) |> 
      select("Name")
    name_label <- name_label$Name
    
    price_data <- historical_prices |> 
      filter(Ticker == input$filter_company, 
             Date == max(historical_prices$Date))
    
    ticker_summary(
      ticker = input$filter_company, 
      price_data = price_data,
      name_label = name_label)
    
  })
  
  output$card_financials_trend <- renderPlot({
    
    ggplot(historical_prices |> 
             filter(Ticker == input$filter_company),
           aes(x = Date,
               y = Close)) +
      geom_line(linewidth = 1.03, color = "#36454F") +
      ggthemes::theme_pander() +
      theme(
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


