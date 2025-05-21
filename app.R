# Carregar les llibreries necessàries per a realitzar la PAC
library(shiny)
library(tidyverse)
library(countrycode)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Carregar el dataset
df <- read.csv("hotel_bookings.csv")

# Preprocessar les dades agrupant els bookings per estació de l'any
df <- df %>%
    season = case_when(
      arrival_date_month %in% c("December", "January", "February") ~ "Hivern",
      arrival_date_month %in% c("March", "April", "May") ~ "Primavera",
      arrival_date_month %in% c("June", "July", "August") ~ "Estiu",
      TRUE ~ "Tardor"
    )

# Calcular el ADR mitjà per país i per estació de l'any
aggregated <- df %>%
  group_by(country, season) %>%
  summarise(
    adr = mean(adr, na.rm = TRUE),
    .groups = 'drop'
  ) 

# Convertir els noms dels països abreviats als noms complerts
aggregated <- aggregated %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name"))

# Seleccionar el mapa d'Europa per mostrar els valors d'ADR mitjà per cada país europeu
europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe")

# Calcular la mitjana total dels valors ADR per país 
total_adr <- df %>%
  group_by(country) %>%
  summarise(adr_total = mean(adr, na.rm = TRUE)) %>%
  mutate(country_name = countrycode(country, "iso3c", "country.name")) %>%
  filter(!is.na(country_name))

# Unir el el valor mitjà totsal per país al df de dades del mapa europeu
europe_map_data <- europe_map %>%
  left_join(total_adr, by = c("name" = "country_name"))

# Crear la User Interface
ui <- fluidPage(
  # Títol general de la visualització
  titlePanel("ADR mitjà per països i estacions: Existeix una associació entre l'ADR i la temporada turística principal de cada país?"),
  sidebarLayout(
    sidebarPanel(
      # Panell de l'esquerra amunt amb les indicacions de com utilitzar la visualització interactiva
      wellPanel(
        h4("Instruccions"),
        p("1. Observa el mapa amb el preu mitjà per nit (ADR) per país."),
        p("2. Fes clic sobre un país per veure el detall per estació."),
        p("3. Pots fer clic en més d’un país per comparar."),
        actionButton("reset_selection", "Reinicia selecció de països")
      ),
      br(),
      # Panell d'abaix a l'esquerra amb punts claus sobre la metodologia i la narrativa visual de la visualització
      wellPanel(
        h4("Objectiu"),
        p("Explorar com varia el preu mitjà per nit (ADR) a Europa segons el país i l’estació de l’any."),
        h4("Metodologia"),
        tags$ul(
          tags$li("Dades: hotel_bookings.csv (Nuno Antonio, Ana de Almeida, Luis Nunes. Hotel booking demand datasets. Data in Brief, 22 (2019)"),
          tags$li("Transformacions: integració mapa, assignació d’estacions, càlcul ADR mitjà per país i estació"),
          tags$li("Eines: R - Shiny, dplyr, leaflet, ggplot2"),
          tags$li("Codi per a crear la Shiny App: https://github.com/Gerard-Deuner/DataVis_PAC3/")
        ),
        h4("Narrativa Visual"),
        tags$ul(
          tags$li("Hipòtesi: el preu mitjà per nit (ADR) per països es correlaciona amb la temporada d’activitat turística."),
          tags$li("Nus: L'anàlisi inidica que en tots els països hi ha un augment dels preus a l’estiu, independentment de si la seva activitat turística principal és estiuenca o hivernal."),
          tags$li("Desenllaç i Conclusions: La dinàmica del mercat turístic europeu ve fortament marcada pel calendari vacacional estival.")
        ),
        
      )
    ),
    # Preparar panell general on es mostrarn le sdues visualitzacions: el mapa i el barplot
    mainPanel(
      h3("Mapa interactiu: ADR mitjà per país"),
      leafletOutput("map", height = 500),
      h3("Comparativa estacional dels països seleccionats"),
      plotOutput("barplot")
    )
  )
)

# Configurar el server
server <- function(input, output, session) {
  
  # Funcionalitat per seleccionar països
  selected_countries <- reactiveVal(character())
  
  # Funcionalitat per seleccionar països clicant sobre ells per posteriorment comparar els valors mitjans d'ADR per estacions
  observeEvent(input$map_shape_click, {
    clicked_country <- input$map_shape_click$id
    current <- selected_countries()
    if (clicked_country %in% current) {
      selected_countries(setdiff(current, clicked_country))
    } else {
      selected_countries(c(current, clicked_country))
    }
  })
  
  observeEvent(input$reset_selection, {
    selected_countries(character())
  })
  
  # Plot del mapa europeu amb els valors mitjans d'ADR i amb colors que representen aquests valors de menor a major
  output$map <- renderLeaflet({
    adr_vals <- europe_map_data$adr_total
    adr_vals <- adr_vals[!is.na(adr_vals)]  
    pal <- colorNumeric("YlOrRd", domain = c(min(adr_vals), 140), na.color = "#f0f0f0")
    
    leaflet(europe_map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(adr_total),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        layerId = ~name,
        label = ~paste0(name, ": ", round(adr_total, 2), " € per nit"),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(pal = pal, values = seq(min(adr_vals), 140, by = 5), title = "ADR mitjà (€)", position = "bottomright")
  })
  
  # Barplot comparatiu de valor ADR mitjà per país (color) i estació de l'any (eix x)
  output$barplot <- renderPlot({
    countries <- selected_countries()
    if (length(countries) == 0) return(NULL)
    
    plot_data <- aggregated %>%
      filter(country_name %in% countries)
    
    ggplot(plot_data, aes(x = season, y = adr, fill = country_name)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Estació", y = "ADR (€)", fill = "País") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui = ui, server = server)
