# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(DT)
library(tidyr)

# Load the dataset
gemstone_data <- read.csv("C:/Users/supul/OneDrive/Desktop/16177/16177/cubic_zirconia.csv")

# Data Preprocessing Steps

# Removing duplicate records
# The original dataset had 34 duplicates which were removed
gemstone_data <- gemstone_data %>% distinct()

# Removing physically impossible dimensions
# Filter out records where length, width, or height is zero
gemstone_data <- gemstone_data %>%
  filter(x > 0, y > 0, z > 0)

# Removing missing values
# There were 697 observations missing in the Depth column
gemstone_data <- gemstone_data %>% drop_na()

# Removing outliers using the IQR method (alternative to Rosner's Test)
# Calculate IQR for price
Q1 <- quantile(gemstone_data$price, 0.25)
Q3 <- quantile(gemstone_data$price, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtering out price outliers
gemstone_data <- gemstone_data %>% 
  filter(price >= lower_bound & price <= upper_bound)

# Creating Volume variable from x, y, z dimensions and drop original dimension columns
gemstone_data <- gemstone_data %>%
  mutate(Volume = x * y * z) %>%
  select(-x, -y, -z)  # Drop original x, y, z columns

# Define UI for the Shiny app
ui <- dashboardPage(
  # Set the dashboard header with title
  dashboardHeader(title = "Gemstone Price Dashboard"),
  
  # Sidebar with menu items and filters
  dashboardSidebar(
    # Main menu navigation
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Price Analysis", tabName = "price_analysis", icon = icon("dollar-sign")),
      menuItem("Volume Analysis", tabName = "volume_analysis", icon = icon("cubes")),
      menuItem("Color Analysis", tabName = "color_analysis", icon = icon("palette")),
      menuItem("Data Summary", tabName = "data_summary", icon = icon("table"))
    ),
    
    # Filter controls for interactive data exploration
    h4("Filters"),
    # Carat range slider
    sliderInput("carat_range", "Carat Range:",
                min = min(gemstone_data$carat), max = max(gemstone_data$carat),
                value = c(min(gemstone_data$carat), max(gemstone_data$carat))),
    
    # Price range slider
    sliderInput("price_range", "Price Range:",
                min = min(gemstone_data$price), max = max(gemstone_data$price),
                value = c(min(gemstone_data$price), max(gemstone_data$price))),
    
    # Cut quality selection with checkboxes
    checkboxGroupInput("cut_quality", "Cut Quality:", 
                       choices = unique(gemstone_data$cut),
                       selected = "Ideal"),  # Changed to only select "Ideal" as default
    
    # Color selection with multi-select picker
    pickerInput("color_filter", "Color:", 
                choices = unique(gemstone_data$color), multiple = TRUE, 
                options = list(`actions-box` = TRUE), 
                selected = unique(gemstone_data$color)),  # All colors selected by default
    
    # Clarity selection with multi-select picker
    pickerInput("clarity_filter", "Clarity:", 
                choices = unique(gemstone_data$clarity), multiple = TRUE, 
                options = list(`actions-box` = TRUE), 
                selected = "SI1")
  ),
  
  # Main dashboard body containing all visualizations and information
  dashboardBody(
    tabItems(
      # Overview Tab - general information and key metrics
      tabItem(tabName = "overview",
              # Add diamond image at the top centered
              fluidRow(
                column(width = 12, align = "center",
                       tags$img(src = "Diamond-PNG-Photos.png", height = "200px")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Introduction",
                    status = "primary",
                    solidHeader = TRUE,
                    p("Diamonds have long symbolized elegance and rarity, being one of the hardest naturally occurring substances composed of carbon. 
                      Traditionally sold in jewelry stores, the rise of e-commerce has expanded their market reach. 
                      Diamond prices are influenced by physical attributes such as ", strong("Carat, Cut, Color, and Clarity"), 
                      ", along with factors like brand reputation, craftsmanship, and market demand. 
                      Prestigious brands often impose high markups, further affecting pricing. 
                      Given their value, diamonds have been traded globally for centuries. 
                      This report aims to lay the groundwork for understanding diamond pricing through exploratory data analysis of key features in the dataset.")
                )
              ),
              # First row of value boxes
              fluidRow(
                valueBoxOutput("total_diamonds"),
                valueBoxOutput("avg_price"),
                valueBoxOutput("common_cut")
              ),
              # Second row of value boxes
              fluidRow(
                valueBoxOutput("most_expensive"),
                valueBoxOutput("avg_carat"),
                valueBoxOutput("common_color")
              ),
              # Third row with centered value box
              fluidRow(
                column(width = 4), # For centering
                valueBoxOutput("median_volume", width = 4),
                column(width = 4) # For centering
              )),
      
      # Price Analysis Tab - visualizations related to price
      tabItem(tabName = "price_analysis",
              h3("Price Distribution"),
              plotOutput("price_distribution"),
              # Description text for Price Distribution
              box(width = 12,
                  p("A histogram of diamond prices shows the distribution of price values, often with a right-skewed distribution, indicating that most diamonds are priced lower, with fewer expensive ones.")),
              
              h3("Carat vs Price"),
              plotOutput("carat_vs_price"),
              # Description text for Carat vs Price
              box(width = 12,
                  p("A scatter plot of carat weight against price shows a strong positive correlation, indicating that larger diamonds tend to have higher prices.")),
              
              h3("Price by Cut"),
              plotOutput("price_by_cut"),
              # Description text for Price by Cut
              box(width = 12,
                  p("The boxplot demonstrates how diamond prices vary across different cut qualities, showing that Premium and Fair cuts generally have higher median prices compared to Ideal and Good cuts. Additionally, Premium and Very Good cuts exhibit a wider price range with more outliers above $10,000.")),
              
              h3("Price by Clarity"),
              plotOutput("price_by_clarity"),
              # Description text for Price by Clarity
              box(width = 12,
                  p("The boxplot illustrates how diamond prices vary by clarity, showing that SI1, SI2, and l1 clarity grades tend to have higher median prices. The IF (Internally Flawless) and VVS1/VVS2 grades, despite their superior clarity, generally have lower median prices, possibly due to other influencing factors like cut and carat weight."))),
      
      # Volume Analysis Tab - relationships between volume and other attributes
      tabItem(tabName = "volume_analysis",
              h3("Price vs Volume"),
              plotOutput("price_vs_volume"),
              # Description text for Price vs Volume
              box(width = 12,
                  p("The scatter plot shows the relationship between diamond volume (mm³) and price ($). There is a positive correlation, meaning that as the diamond volume increases, the price tends to rise. However, the spread becomes wider at higher volumes, indicating greater variability in pricing for larger diamonds. Some outliers can also be observed at high volumes."))),
      
      # Color Analysis Tab - visualizations related to color
      tabItem(tabName = "color_analysis",
              h3("Price by Color"),
              plotOutput("price_by_color"),
              # Description text for Price by Color
              box(width = 12,
                  p("The boxplot shows that diamonds with higher color grades (J, I) tend to have higher median prices, but there is significant variation within each category.")),
              
              h3("Color Distribution"),
              plotOutput("color_distribution"),
              # Description text for Color Distribution
              box(width = 12,
                  p("The bar chart indicates that G is the most common diamond color, while I and J are the least frequent in the dataset."))),
      
      # Data Summary Tab - detailed data view
      tabItem(tabName = "data_summary",
              DT::dataTableOutput("data_table"))
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Create reactive filtered dataset based on user input selections
  filtered_data <- reactive({
    gemstone_data %>%
      filter(carat >= input$carat_range[1],
             carat <= input$carat_range[2],
             price >= input$price_range[1],
             price <= input$price_range[2],
             (is.null(input$cut_quality) | cut %in% input$cut_quality),
             (is.null(input$color_filter) | color %in% input$color_filter),
             (is.null(input$clarity_filter) | clarity %in% input$clarity_filter))
  })
  
  # Value Boxes - Display key metrics and summaries
  # Total number of diamonds in filtered data
  output$total_diamonds <- renderValueBox({
    shinydashboard::valueBox(nrow(filtered_data()), "Total Diamonds", icon = icon("gem"), color = "blue")
  })
  
  # Average price of diamonds in filtered data
  output$avg_price <- renderValueBox({
    shinydashboard::valueBox(round(mean(filtered_data()$price), 2), "Avg. Price ($)", icon = icon("dollar-sign"), color = "green")
  })
  
  # Most common cut type in filtered data
  output$common_cut <- renderValueBox({
    shinydashboard::valueBox(names(sort(table(filtered_data()$cut), decreasing = TRUE)[1]), "Most Common Cut", icon = icon("cut"), color = "purple")
  })
  
  # Most expensive diamond in filtered data
  output$most_expensive <- renderValueBox({
    shinydashboard::valueBox(max(filtered_data()$price), "Most Expensive ($)", icon = icon("money-bill"), color = "red")
  })
  
  # Average carat weight in filtered data
  output$avg_carat <- renderValueBox({
    shinydashboard::valueBox(round(mean(filtered_data()$carat), 2), "Avg. Carat", icon = icon("balance-scale"), color = "yellow")
  })
  
  # Most common color in filtered data
  output$common_color <- renderValueBox({
    shinydashboard::valueBox(names(sort(table(filtered_data()$color), decreasing = TRUE)[1]), "Most Common Color", icon = icon("palette"), color = "orange")
  })
  
  # Median volume in filtered data
  output$median_volume <- renderValueBox({
    shinydashboard::valueBox(round(median(filtered_data()$Volume), 2), "Median Volume (mm³)", icon = icon("cube"), color = "teal")
  })
  
  # Visualizations
  # Price distribution histogram
  output$price_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = price)) +
      geom_histogram(bins = 30, fill = "blue", color = "white") +
      labs(title = "Price Distribution", x = "Price", y = "Count")
  })
  
  # Scatter plot of carat vs price
  output$carat_vs_price <- renderPlot({
    ggplot(filtered_data(), aes(x = carat, y = price)) +
      geom_point(color = "purple", alpha = 0.5) +
      labs(title = "Carat vs Price", x = "Carat", y = "Price ($)")
  })
  
  # Boxplot of price by cut quality
  output$price_by_cut <- renderPlot({
    ggplot(filtered_data(), aes(x = cut, y = price, fill = cut)) +
      geom_boxplot() +
      labs(title = "Price by Cut", x = "Cut", y = "Price ($)")
  })
  
  # Boxplot of price by clarity
  output$price_by_clarity <- renderPlot({
    # Angled x-axis labels for better readability
    ggplot(filtered_data(), aes(x = clarity, y = price, fill = clarity)) +
      geom_boxplot() +
      labs(title = "Price by Clarity", x = "Clarity", y = "Price ($)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve readability of x-axis labels
  })
  
  # Scatter plot of volume vs price
  output$price_vs_volume <- renderPlot({
    ggplot(filtered_data(), aes(x = Volume, y = price)) +
      geom_point(color = "red", alpha = 0.5) +
      labs(title = "Price vs Volume", x = "Volume (mm³)", y = "Price ($)") +
      xlim(0, 500)  # Set maximum value for volume axis to 500
  })
  
  # Color Analysis Plots
  # Boxplot of price by color
  output$price_by_color <- renderPlot({
    ggplot(filtered_data(), aes(x = color, y = price, fill = color)) +
      geom_boxplot() +
      labs(title = "Price by Color", x = "Color", y = "Price ($)") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Bar chart of color distribution
  output$color_distribution <- renderPlot({
    # Creating a data frame with color counts for labels
    color_counts <- filtered_data() %>%
      count(color) %>%
      arrange(color)
    
    ggplot(color_counts, aes(x = color, y = n, fill = color)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Add data labels
      labs(title = "Color Distribution", x = "Color", y = "Count") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Data Summary Table - full interactive data table
  output$data_table <- DT::renderDataTable({
    datatable(filtered_data())
  })
}

# Run the Shiny app
shinyApp(ui, server)