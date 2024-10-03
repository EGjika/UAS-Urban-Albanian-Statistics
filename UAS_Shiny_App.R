library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(stringi)

# Predefined list of Albanian cities with latitude and longitude
city_coords <- data.frame(
  city = c("Tirana", "Durrës", "Shkodër", "Vlorë", "Fier", "Berat", "Korçë", 
           "Gjirokastër", "Elbasan", "Lushnjë", "Përmet", "Sarandë", 
           "Vau i Dejës", "Kamëz", "Kavajë", "Peqin", "Mirditë", "Sukth"),
  lat = c(41.3275, 41.3231, 42.0683, 40.4667, 40.7239, 40.7058, 40.6186, 
          40.0833, 41.1139, 40.9920, 40.2372, 39.8739, 
          39.8643, 41.1833, 41.3133, 41.0750, 41.5281, 41.3075),
  lon = c(19.8189, 19.4414, 19.5126, 19.4908, 19.5561, 19.9520, 20.7808, 
          20.1431, 20.0833, 19.7275, 19.5742, 20.0032, 
          20.0013, 19.6639, 19.5636, 19.7972, 19.6544, 19.6896)
)


# Normalize city names by removing accents and other special characters
normalize_city_names <- function(city_names) {
  city_names <- stri_trans_general(city_names, "Latin-ASCII")  # Convert to ASCII
  city_names <- tolower(city_names)  # Convert to lowercase for consistent matching
  return(city_names)
}

# Define the UI
ui <- fluidPage(
  titlePanel("UAS-Urban Albanian Statistics"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = c(".csv")),
      uiOutput("variable_select"),
      actionButton("update", "Update Map"),
      helpText("WARNING!!!"),
      helpText("Ensure your CSV file has a 'city' column (case-sensitive) and other columns for data."),
      helpText("NOTE: "),
      helpText("Predefined Cities:tirana,durres,shkoder,vlore,fier,berat,korce,gjirokaster"),    
      helpText("elbasan,lushnje,permet,sarande,kamez,kavaje,peqin,mirdite,sukth"),
      helpText(
        tags$a(href = "https://www.linkedin.com/in/eralda-gjika-71879128/", "Creator: Eralda Gjika (Dhamo)", target = "_blank"),
        " - Click for more information."
      ),
      helpText(
        tags$a(href = "https://github.com/EGjika/UAS-Urban-Albanian-Statistics/tree/main", "GitHub UAS", target = "_blank"),
        
      ) 
         ),
    mainPanel(
      leafletOutput("map", height = 700)  # Increase the height of the map
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to read and clean the uploaded data
  uploaded_data <- reactive({
    req(input$file1)  # Require the file input
    
    # Read CSV file using readr with proper encoding
    df <- read_csv(input$file1$datapath, locale = locale(encoding = "UTF-8"))
    
    # Check if the 'city' column exists
    if (!"city" %in% colnames(df)) {
      stop("The uploaded file must contain a 'city' column.")
    }
    
    # Normalize city names in the uploaded file
    df$city <- normalize_city_names(df$city)
    
    # Normalize city names in the predefined coordinates list
    city_coords$city <- normalize_city_names(city_coords$city)
    
    # Debug: Print normalized city names
    print("Uploaded Cities:")
    print(unique(df$city))
    print("Predefined Cities:")
    print(unique(city_coords$city))
    
    # Match city names with predefined coordinates
    df <- df %>%
      inner_join(city_coords, by = "city")  # Merge uploaded data with city coordinates
    
    # Debug: Check the number of matched rows
    print(paste("Matched Rows:", nrow(df)))
    
    return(df)
  })
  
  # Output UI for variable selection
  output$variable_select <- renderUI({
    req(uploaded_data())  # Require the uploaded data
    selectInput("variables", "Select Variables to Show:", 
                choices = colnames(uploaded_data())[!(colnames(uploaded_data()) %in% c("city", "lat", "lon"))], 
                multiple = TRUE)
  })
  
  # Observe the update button to redraw the map
  observeEvent(input$update, {
    req(uploaded_data())  # Require the uploaded data
    req(input$variables)  # Require the selected variables
    
    # Generate popup content for each city row
    df_with_popup <- uploaded_data() %>%
      rowwise() %>%
      mutate(popup_info = paste(
        "<strong>City:</strong>", city,
        "<br>", paste(input$variables, ": ", sapply(input$variables, function(v) get(v)), collapse = "<br>")
      ))
    
    # Create leaflet map
    output$map <- renderLeaflet({
      leaflet(df_with_popup) %>%
        addTiles() %>%
        addMarkers(
          lng = ~lon,
          lat = ~lat,
          popup = ~popup_info  # Use the pre-calculated popup content
        )
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
