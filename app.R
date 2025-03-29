library(shiny)
library(SECFC)
library(dplyr)
library(tibble)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel("\U0001F30D SECFC Global Carbon Footprint Calculator"),
  
  tags$head(
    tags$style(HTML(".dark-mode {
      background-color: #121212 !important;
      color: #ffffff !important;
    }
    .dark-mode .well {
      background-color: #1e1e1e !important;
    }
    .dark-mode .form-control {
      background-color: #2a2a2a !important;
      color: #ffffff !important;
    }
    .dark-mode .btn {
      background-color: #444 !important;
      color: #fff !important;
    }
    ul {
      list-style-type: circle;
      font-size: 16px;
      margin-left: 20px;
    }
    .tab-content img {
      border-radius: 12px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      margin-top: 10px;
    }"))
  ),
  
  switchInput("darkmode", "\u262F\ufe0f Dark Mode", value = FALSE),
  
  sidebarLayout(
    sidebarPanel(
      h4("\U0001F331 Participation & Location"),
      checkboxInput("consent", "Yes, I accept to proceed to the study.", value = TRUE),
      selectInput("country", "Which country are you from?", choices = c("United States", "China", "European Union")),
      conditionalPanel(
        condition = "input.country == 'United States'",
        textInput("zipcode", "What is your ZIP code?", placeholder = "Enter your ZIP code")
      ),
      h4("\U0001F357 Food Consumption"),
      sliderInput("meat", "Meat-based meals per week (0–14)", 0, 14, 5),
      sliderInput("vegan", "Vegan meals per week (0–14)", 0, 14, 3),
      sliderInput("vegetarian", "Vegetarian meals per week (0–14)", 0, 14, 2),
      sliderInput("dairy", "Dairy product meals per week (0–14)", 0, 14, 4),
      h4("\U0001F6D2 Monthly Spending (USD)"),
      selectInput("clothing_freq", "How frequently do you purchase new clothing?",
                  choices = c("More than 12 times (At least once a month)", "7-12 times (About every 1-2 months)",
                              "4-6 times (About every 2-3 months)", "1-3 times (Less than every 3 months)", "Rarely")),
      numericInput("food_delivery", "Food delivery (only fee)", 50),
      numericInput("dining_out", "Dining out", 80),
      numericInput("hotels", "Hotel stays", 40),
      numericInput("tobacco", "Tobacco products", 10),
      numericInput("alcohol", "Alcohol and other beverages", 30),
      numericInput("entertainment", "Entertainment (movies, concerts, events)", 60),
      numericInput("healthcare", "Healthcare expenses", 100),
      h4("\U0001F697 Transportation"),
      selectInput("car_use", "On average, how many days a week do you use a car?",
                  choices = c("0 days (I do not use a car)", "1-2 days", "3-4 days", "5-6 days", "Everyday")),
      selectInput("car_type", "What type of car do you primarily use?",
                  choices = c("Gasoline Vehicle", "Diesel Vehicle", "Hybrid Vehicle", "Electric Vehicle", "Natural Gas Vehicle")),
      numericInput("car_km", "How far do you drive per day you use a car? (km)", 15, min = 0, max = 200),
      selectInput("public_transport_use", "How often do you use inner-city public transportation?",
                  choices = c("Daily", "Weekly", "Monthly", "Rarely", "Never")),
      numericInput("public_transport_km", "How far do you commute via public transport? (km)", 10, min = 0, max = 200),
      selectInput("flight_long", "Long-distance flights (>1,000 mi) per year",
                  choices = c("None", "1-3 flights", "4-6 flights", "7-10 flights", "More than 10 flights")),
      selectInput("flight_short", "Short-distance flights (<1,000 mi) per year",
                  choices = c("None", "1-3 flights", "4-6 flights", "7-10 flights", "More than 10 flights")),
      selectInput("long_transport", "How often do you use long-distance transport (rail/bus)?",
                  choices = c("Daily", "Weekly", "Monthly", "Rarely", "Never")),
      h4("\U0001F3E1 Housing & Energy"),
      sliderInput("electricity", "Monthly electricity bill", 0, 300, 100, step = 30),
      sliderInput("electricity_alt", "Alternative annual electricity bill (optional)", 0, 300, 0, step = 30),
      sliderInput("gas", "Monthly natural gas bill", 0, 300, 50, step = 30),
      sliderInput("gas_alt", "Alternative annual natural gas bill (optional)", 0, 300, 0, step = 30),
      h4("\U0001F468‍\U0001F467‍\U0001F467 Household"),
      numericInput("adults", "How many adults (18–64)?", 2),
      numericInput("children", "How many children (<18)?", 1),
      numericInput("seniors", "How many seniors (65+)?", 0),
      h4("\U0001F43E Pets"),
      numericInput("dogs", "Number of dogs", 1),
      numericInput("cats", "Number of cats", 0),
      actionButton("calc", "\U0001F4CA Calculate My Carbon Footprint", class = "btn-primary"),
      br(), br(),
      downloadButton("downloadData", "\U0001F4C4 Download Results")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Emissions Chart",
                 h4("📈 Carbon Emissions by Category (kg CO₂e/year)"),
                 tableOutput("detail_output"),
                 br(),
                 plotOutput("emission_plot", height = "400px")
        ),
        tabPanel("🌿 Tips to Reduce Emissions",
                 h4("💡 Simple Tips to Lower Your Carbon Footprint"),
                 tags$ul(
                   tags$li("🚴‍♂️ Choose biking or public transit over driving."),
                   tags$li("🥦 Eat more plant-based meals and reduce red meat."),
                   tags$li("🔌 Turn off unused electronics and lights."),
                   tags$li("🌡️ Adjust thermostat settings for energy efficiency."),
                   tags$li("🧥 Buy fewer, more durable clothes."),
                   tags$li("✈️ Avoid unnecessary flights; consider train or video calls."),
                   tags$li("🔄 Reuse, reduce, and recycle whenever possible."),
                   tags$li("💡 Switch to LED lighting and energy-efficient appliances."),
                   tags$li("🚿 Take shorter showers and conserve water."),
                   tags$li("⚡ Consider green energy providers or solar panels.")
                 ),
                 br(),
                 h4("🎨 Posters"),
                 fluidRow(
                   column(6, img(src = "post1.png", width = "100%")),
                   column(6, img(src = "post2.png", width = "100%"))
                 )
        )
      )
    )
    
  )
)

server <- function(input, output, session) {
  observe({
    if (input$darkmode) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  observeEvent(input$zipcode, {
    user_zip <- as.numeric(input$zipcode)
    # Add any validation or fallback logic here
    if (is.na(user_zip)) {
      showNotification("Please enter a valid ZIP code", type = "error")
    } else {
      # Continue with your calculation
      print(paste("User ZIP code is", user_zip))
    }
  })
  
  data_input <- eventReactive(input$calc, {
    tibble(
      SD_07_Country = input$country,
      SD_08_ZipCode = input$zipcode,
      F_01_DietaryHabits_5 = input$meat,
      F_01_DietaryHabits_6 = input$vegan,
      F_01_DietaryHabits_7 = input$vegetarian,
      F_01_DietaryHabits_4 = input$dairy,
      CL_01_ClothingPurcha = input$clothing_freq,
      CL_03_MonthlyEx_9 = input$food_delivery,
      CL_03_MonthlyEx_10 = input$dining_out,
      CL_03_MonthlyEx_11 = input$hotels,
      CL_03_MonthlyEx_12 = input$tobacco,
      CL_03_MonthlyEx_13 = input$alcohol,
      CL_03_MonthlyEx_14 = input$entertainment,
      CL_03_MonthlyEx_15 = input$healthcare,
      T_01_CarUsage = input$car_use,
      T_02_CarType = input$car_type,
      T_03_CarDistance = paste0(input$car_km, " km or ", round(input$car_km * 0.621371, 1), " miles"),
      T_04_PublicTransport = input$public_transport_use,
      T_05_PublicTransport = paste0(input$public_transport_km, " km or ", round(input$public_transport_km * 0.621371, 1), " miles"),
      T_06_AirTravelLong = input$flight_long,
      T_07_AirTravelShort = input$flight_short,
      T_08_LongDistanceTra = input$long_transport,
      EH_02_ElectricityBil_1 = input$electricity,
      EH_03_ElectricityBil_1 = input$electricity_alt,
      EH_05_NaturalGasBill_1 = input$gas,
      EH_06_NaturalGasBill_1 = input$gas_alt,
      PETS_5 = input$dogs,
      PETS_4 = input$cats
    )
  })
  
  emissions <- reactive({
    df <- data_input()
    tryCatch({
      df <- calc_food_emissions(df)
      df <- calc_cons_emissions(df)
      df <- calc_transport_emissions(df)
      df <- calc_housing_emissions(df)
      df <- calc_pet_emissions(df)
      df <- df %>% mutate(TotalEmissions = FoodEmissions + ConsEmissions + TransportEmissions + HousingEmissions + PetEmissions)
      df
    }, error = function(e) {
      print(paste("\U0001F6A8 Calculation Error:", e$message))
      tibble()
    })
  })
  
  output$detail_output <- renderTable({
    df <- emissions()
    if (nrow(df) == 0) return(NULL)
    tibble(
      Category = c("Food", "Consumption", "Transport", "Housing", "Pets", "Total"),
      Emissions = c(df$FoodEmissions, df$ConsEmissions, df$TransportEmissions, df$HousingEmissions, df$PetEmissions, df$TotalEmissions)
    )
  })
  
  output$emission_plot <- renderPlot({
    df <- emissions()
    if (nrow(df) == 0) return(NULL)
    data <- tibble(
      Category = c("Food", "Consumption", "Transport", "Housing", "Pets"),
      Emissions = c(df$FoodEmissions, df$ConsEmissions, df$TransportEmissions, df$HousingEmissions, df$PetEmissions)
    ) %>%
      mutate(
        Percentage = Emissions / sum(Emissions) * 100,
        Label = paste0(round(Emissions, 1), " kg\n", round(Percentage, 1), "%"),
        hjust_value = ifelse(Category == "Housing", 1.05, -0.1)
      )
    
    ggplot(data, aes(x = reorder(Category, Emissions), y = Emissions, fill = Category)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = Label, hjust = hjust_value), size = 5, color = "black") +
      scale_fill_manual(values = c(
        "Food" = "#fbb4ae",
        "Consumption" = "#b3cde3",
        "Transport" = "#ccebc5",
        "Housing" = "#decbe4",
        "Pets" = "#fed9a6"
      )) +
      coord_flip() +
      labs(x = NULL, y = "Emissions (kg CO₂e/year)", title = "Carbon Emissions by Category") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
