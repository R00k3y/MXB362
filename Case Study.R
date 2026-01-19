+install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)
install.packages("magick")
library("magick")
install.packages("gganimate")
library(gganimate)
install.packages(c("readr", "sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages("leaflet")
library(leaflet) # package for interactive maps
library(tidyr)
install.packages("ggrepel")
library(ggrepel)
install.packages("shiny")
library(shiny)
remove.packages("fastmap")
install.packages("fastmap")
library(fastmap)
packageVersion("fastmap")
library(fastmap)
install.packages("bslib")
library(bslib)
install.packages("Rcolorbewer")
library(RColorBrewer)
install.packages("viridis")
library(viridis)

#.rs.restartR()


data <- read.csv("C:/Users/colin/OneDrive/Documents/QUT/SEM 4/MXB362/Case study/Datasets/Cleaned_Database.csv")
#data <- read.csv("C:/Users/n11382678/OneDrive - Queensland University of Technology/MXB362/Cleaned_Database.csv")


# coordinates of each region 
nz_regions <- data.frame(
  Region = c("Taitokerau", "Waitematā", "Auckland", "South Auckland", 
            "Waikato", "Bay of Plenty", "Waiariki", "East Coast", 
            "Taranaki/Whanganui", "Manawatū/Wairarapa", "Northern Wellington", 
            "Wellington", "Nelson/Marlborough/West Coast", "Canterbury", 
            "Otago", "Southland"),
  lon = c(174.29, 174.76, 174.76, 174.89, 
          175.28, 176.17, 176.25, 178.35, 
          174.37, 175.61, 175.02, 174.78, 
          173.28, 172.63, 170.50, 168.35),
  lat = c(-35.43, -36.85, -36.85, -37.00, 
          -37.78, -37.68, -38.14, -38.31, 
          -39.30, -40.35, -41.13, -41.29, 
          -41.27, -43.53, -45.87, -46.41)
)


# map data for New Zealand
nz_map <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data = nz_map) +
  geom_sf() +
  coord_sf(xlim = c(165.31, 180.0), ylim = c(-49.00,-34.03), expand = FALSE)+
  geom_text_repel(data = nz_regions, aes(x = lon, y = lat, label = Region), size = 3)



## create different data frames for the different categories ##
## data frame for year and crime by type
long_data_total <- data %>%
  pivot_longer(cols = starts_with("X"),  # Replace if column names are not prefixed with "X"
               names_to = "year",
               values_to = "Crime_Count") %>%
  mutate(year = as.integer(gsub("X", "", year)),  
         Crime_Count = as.numeric(Crime_Count))   

# clean Region names in long_data_total by removing leading/trailing spaces and fixing capitalization
long_data_total <- long_data_total %>%
  mutate(Region = trimws(Region),  # Remove leading/trailing spaces
         Region = case_when(
           Region == "Waitemata\u0081" ~ "Waitematā",  # fix special character issue
           Region == "South Auckland " ~ "South Auckland",  
           Region == "Bay of plenty " ~ "Bay of Plenty",  
           Region == "Bay of plenty" ~ "Bay of Plenty",
           Region == "East Coast " ~ "East Coast",  
           Region == "Manawatu/Wairarapa" ~ "Manawatū/Wairarapa",  # fix missing macron or spelling
           TRUE ~ Region  # Keep other regions unchanged
         ))

# clean Region names in nz_regions in a similar way
nz_regions <- nz_regions %>%
  mutate(Region = trimws(Region))

## check if there are still irregularities in data frame 
# unmatched_regions <- setdiff(long_data_total$Region, nz_regions$Region)
# print(unmatched_regions) # should be 0 


# merge with coordinates dataframe 
crime_region_category_merged <- merge(long_data_total, nz_regions, by = "Region")

print(unique(crime_region_category_merged$Category))
print(unique(crime_region_category_merged$year))

# Create an interactive map #
# define years and categories
years <- unique(crime_region_category_merged$year) %>%
  sort()
print(years)

# trim whitespace from Category names
crime_region_category_merged <- crime_region_category_merged %>%
  mutate(Category = trimws(Category))
categories <- sort(unique(crime_region_category_merged$Category))

print(categories)

# create an empty list to store the layers
crime_data_list <- list()


# loop through each year and category to create filtered data frames
for (category in categories) {
  for (year_value in years) {
    # Filter data for the specific year and category
    filtered_data <- crime_region_category_merged %>%
      filter(year == year_value & Category == category)
    
    # Only store if there is data
    if (nrow(filtered_data) > 0) {
      # Create a unique name for the list element
      df_name <- paste0("crime_region_", year_value, "_", gsub(" ", "_", tolower(category)))
      crime_data_list[[df_name]] <- filtered_data
    }
  }
}

# check if for loop work, result should be 50 
length(crime_data_list)


# check for violent offence in 2014
crime_region_2014_assault <- crime_data_list[["crime_region_2014_violent_offence"]]
crime_region_2014_drug_offences <- crime_data_list[["crime_region_2014_Drug_Offences"]]
head(crime_region_2014_assault)


# define a color palette for different crime categories
palette <- colorFactor(
  palette = brewer.pal(n = length(categories), name = "Set1"),
  domain = categories
)



# initialize the map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 174.76, lat = -41.29, zoom = 5)

# add labels for each region on map 
map <- map %>%
  addLabelOnlyMarkers(
    data = nz_regions,  # Use your region coordinates data
    lng = ~lon, lat = ~lat,
    label = ~Region,  # Display region names
    labelOptions = labelOptions(noHide = TRUE,  
                                direction = "right",  # position the label above the point
                                textsize = "13px",  
                                style = list("font-weight" = "bold", "color" = "black"))  
  )

# loop through years and categories to add layers dynamically
for (year in years) {
  for (category in categories) {
    # Filter data for the specific year and category
    filtered_data <- crime_region_category_merged %>%
      filter(year == year & Category == category)
    
    # Check if the filtered data has any rows
    if(nrow(filtered_data) > 0) {
      # Add markers for the filtered data
      map <- map %>%
        addCircleMarkers(
          data = filtered_data,
          lng = ~lon, lat = ~lat,
          color = ~palette(Category),
          radius = ~sqrt(Crime_Count) * 0.5,
          fillOpacity = 0.7,
          group = paste(category, year),  # Create group combining year and category
          popup = ~paste0("<h4>", Region, "</h4>", 
                          "<strong>Year:</strong> ", year, "<br>",
                          "<strong>Crime Count:</strong> ", Crime_Count)
        )
    }
  }
}

# add layer control to toggle between categories and years
map <- map %>%
  addLayersControl(
    overlayGroups = paste(rep(categories, each = length(years)), years), # Create the groups dynamically
    options = layersControlOptions(collapsed = FALSE)
  )

# Display the map
map

## shiny interaction ##
ui <- fluidPage(
  titlePanel("New Zealand Crime Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_years", "Select Years:", choices = years, selected = years),
      checkboxGroupInput("selected_categories", "Select Crime Categories:", choices = categories, selected = categories)
    ),
    mainPanel(
      leafletOutput("crime_map")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtering of data based on user input
  filtered_data <- reactive({
    req(input$selected_years, input$selected_categories)
    crime_region_category_merged %>%
      filter(year %in% input$selected_years & Category %in% input$selected_categories)
  })
  
  # Initial map rendering
  output$crime_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 174.76, lat = -41.29, zoom = 5) %>%
      
      # Add a legend for the crime categories and radius scale
      addLegend(
        position = "bottomright", 
        pal = palette, 
        values = unique(categories),  
        title = "Crime Categories",
        opacity = 1,
        labFormat = labelFormat(),  
        group = "Crime Categories"
      )
  })
  
  # Observe changes and update the map
  observe({
    data <- filtered_data()
    
    leafletProxy("crime_map", data = data) %>%
      clearMarkers() %>%
      # Add dynamic circle markers for the filtered data
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        color = ~palette(Category),
        radius = ~sqrt(Crime_Count) * 0.5,
        fillOpacity = 0.7,
        popup = ~paste("<h4>", Region, "</h4>",
                       "<strong>Year:</strong> ", year, "<br>",
                       "<strong>Crime Count:</strong> ", Crime_Count)
      ) %>%
      
      # Add region labels
      addLabelOnlyMarkers(
        data = nz_regions,
        lng = ~lon, lat = ~lat,
        label = ~Region,
        labelOptions = labelOptions(noHide = TRUE, 
                                    direction = "right", 
                                    textsize = "13px", 
                                    style = list("font-weight" = "bold", "color" = "black"))
      )
    
  })
}

shinyApp(ui, server)

### animation visualisation

# Create the animated plot showing all years #
crime_animation <- ggplot(data = nz_map) +
  geom_sf(fill = "lightgrey", color = "black") +
  coord_sf(xlim = c(165.31, 180.0), ylim = c(-49.00, -34.03), expand = FALSE) +
  
  geom_point(data = crime_region_category_merged, aes(x = lon, y = lat, size = Crime_Count, color = Category), alpha = 0.7) +
  
  # Better label positioning to reduce overlap
  geom_text_repel(data = nz_regions, aes(x = lon, y = lat, label = Region), 
                  nudge_y = 0.2, segment.color = "grey50", size = 3) +
  
  scale_size_continuous(range = c(2, 10)) +
  scale_color_viridis_d() +
  
  labs(title = 'Crimes Across New Zealand: Year {frame_time}', 
       x = 'Longitude', 
       y = 'Latitude', 
       size = 'Number of Thefts') +
  
  theme_minimal() +
  
  transition_time(year) +  
  ease_aes('linear') +
  labs(subtitle = "Year: {frame_time}")

# Render the animation
animate(crime_animation, nframes = 100, duration = 20, width = 800, height = 600)






