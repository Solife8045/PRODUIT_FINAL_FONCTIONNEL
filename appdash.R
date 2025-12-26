

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(viridis)
library(shinydashboard)


load("MY WORKSPACE_RAPPORTPROJET.RData")


## ==============================
## App Shiny – Dashboard COMPLET
## ==============================



# ===============================
# UI
# ===============================
ui <- fluidPage(
  
  titlePanel("Dashboard KPI – Performance E-commerce"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "dates",
        "Sélectionner la période :",
        start = min(AmazonSaleReport$Date),
        end   = max(AmazonSaleReport$Date)
      )
    ),
    
    mainPanel(
      fluidRow(
        valueBoxOutput("ca_total"),
        valueBoxOutput("quantite_totale"),
        valueBoxOutput("panier_moyen")
      ),
      
      hr(),
      
      plotOutput("ca_categorie"),
      plotOutput("ca_temps"),
      
      hr(),
      
      tableOutput("stock_table")
    )
  )
)

# ==============================
# SERVER
# ==============================
server <- function(input, output) {
  
  data_filtre <- reactive({
    AmazonSaleReport %>%
      filter(
        Status == "Shipped",
        Date >= input$dates[1],
        Date <= input$dates[2]
      )
  })
  
  # KPI globaux
  output$ca_total <- renderValueBox({
    valueBox(
      comma(sum(data_filtre()$Amount, na.rm = TRUE)),
      "Chiffre d’affaires total (INR)",
      color = "blue"
    )
  })
  
  output$quantite_totale <- renderValueBox({
    valueBox(
      comma(sum(data_filtre()$Qty, na.rm = TRUE)),
      "Quantité totale vendue",
      color = "green"
    )
  })
  
  output$panier_moyen <- renderValueBox({
    valueBox(
      round(mean(data_filtre()$Amount, na.rm = TRUE), 2),
      "Panier moyen (INR)",
      color = "orange"
    )
  })
  
  # CA par catégorie
  output$ca_categorie <- renderPlot({
    data_filtre() %>%
      group_by(Category) %>%
      summarise(CA = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(CA)) %>%
      ggplot(aes(reorder(Category, CA), CA)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Chiffre d’affaires par catégorie",
        x = "Catégorie",
        y = "CA (INR)"
      ) +
      theme_minimal()
  })
  
  # Série temporelle
  output$ca_temps <- renderPlot({
    data_filtre() %>%
      group_by(Date) %>%
      summarise(CA = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(Date, CA)) +
      geom_line(color = "black") +
      labs(
        title = "Évolution temporelle du chiffre d’affaires",
        x = "Date",
        y = "CA (INR)"
      ) +
      theme_minimal()
  })
  
  # Table Stock
  output$stock_table <- renderTable({
    SaleReport_clean %>%
      group_by(Category, Size) %>%
      summarise(
        Stock_total = sum(Stock, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Stock_total))
  })
}

# =================================
shinyApp(ui = ui, server = server)