# note: packages should be installed from data-prep

# color hacking 

human_col <- colorNumeric("YlOrRd", domain = human_map$Total, reverse = FALSE)
livestock_col <- colorNumeric("YlOrRd", domain = livestock_map$Count_Cases, reverse = FALSE)

category_colors1 <- c(
  "Other Mammal" = "green", "Rodent" = "orange", "Big Cat" = "red",
  "Water Mammal" = "lightblue", "Canid" = "pink", "Bear" = "brown", "Bird" = "purple"
)

category_colors2 <- c(
  "2022" = "#90D5FF", "2023" = "#90EE90", "2024" = "#BF77F6", "2025" = "#FFCCCB"
)

please_work_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#d4883a",  
  secondary = "#88b04b",
  base_font = font_google("Roboto"),
  heading_font = font_google("Poppins")
)

# onto app

ui <- navbarPage(
  title = div(class = "navbar-brand d-flex align-items-center gap-2",
              icon("virus"), "H5N1 Mapping Project"),
  theme = please_work_theme,
  
  header = tags$head(
    tags$style(HTML("
      /* big tabs */
      .navbar {
        background-color: #b85c1a !important;  
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
        padding: 0.5rem 2rem;
      }

      .navbar-brand {
        font-weight: 700;
        font-size: 26px;
        color: white !important;
        font-family: 'Poppins', sans-serif;
        letter-spacing: 1px;
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .nav-link {
        background-color: #b85c1a !important;
        color: white !important;
        font-weight: 600;
        padding: 0.5rem 1.2rem !important;
        border-radius: 6px 6px 0 0;
        margin-right: 6px;
        transition: background-color 0.3s ease;
      }

      .nav-link:hover {
        background-color: #d4883a !important; 
        color: white !important;
      }

      .nav-link.active {
        background-color: #884d12 !important; 
        color: white !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }

      /* dash instructions */
      .card-header.bg-primary {
        background-color: #d4883a !important;
        color: white !important;
        font-weight: 700;
        font-size: 1.2rem;
        border-radius: 6px 6px 0 0;
      }

      /* animal filter bttns */
      .animal-btns {
        display: flex;
        justify-content: center;
        flex-wrap: wrap;
        margin-bottom: 1.5rem;
        gap: 0.75rem;
      }

      .animal-btns .btn {
        min-width: 140px;
        font-weight: 600;
        color: white !important;
        border: none;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        transition: filter 0.2s ease;
      }
      .animal-btns .btn:hover {
        filter: brightness(1.15);
      }

      .btn-human { background-color: #d97826; }     
      .btn-poultry { background-color: #c76e2e; }  
      .btn-livestock { background-color: #8f5522; }
      .btn-wild { background-color: #a65f3b; }
      .btn-pets { background-color: #e0a96d; } 

      /* map! */
      .map-card {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        padding: 1rem;
        margin-bottom: 1.5rem;
      }

      /* cards down at bottom */
      .summary-box {
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        margin-bottom: 1rem;
      }

      .summary-title {
        font-weight: 600;
        font-size: 1rem;
        color: #555;
        margin-bottom: 0.25rem;
      }

      .summary-number {
        font-weight: 700;
      }
      
     .summary-wild-birds .summary-number { color: #a65f3b !important; }
     .summary-wild-mammals .summary-number { color: #8f5522 !important; }
     .summary-pets .summary-number { color: #e0a96d !important; }
     
    "))
  ),
  
  tabPanel("Dashboard",
           fluidRow(
             column(12,
                    div(class = "card mb-4 shadow-sm",
                        div(class = "card-header bg-primary text-white",
                            tags$h5("Instructions", style = "margin:0;")
                        ),
                        div(class = "card-body",
                            p("Use the buttons below to explore cases by group. Click on markers or regions for more information.")
                        )
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    div(class = "animal-btns",
                        actionButton("btn_humans", "Human Cases", class = "btn btn-human"),
                        actionButton("btn_pets", "Pets", class = "btn btn-pets"),
                        actionButton("btn_wild", "Wild Animals", class = "btn btn-wild"),
                        actionButton("btn_poultry", "Poultry", class = "btn btn-poultry"),
                        actionButton("btn_livestock", "Livestock", class = "btn btn-livestock")
                    )
             )
           ),
           uiOutput("poultry_date_filter"),
           
           fluidRow(
             column(12,
                    div(class = "map-card",
                        uiOutput("map_ui")
                    )
             )
           ),
           
           fluidRow(
             column(4,
                    div(class = "card summary-box p-3 text-center summary-wild-birds",
                        div(class = "summary-title", "Total Cases in Wild Birds"),
                        div(class = "summary-number h4", textOutput("wild_birds_count"))
                    )
             ),
             column(4,
                    div(class = "card summary-box p-3 text-center summary-wild-mammals",
                        div(class = "summary-title", "Total Cases in Wild Mammals"),
                        div(class = "summary-number h4", textOutput("wild_mammals_count"))
                    )
             ),
             column(4,
                    div(class = "card summary-box p-3 text-center summary-pets",
                        div(class = "summary-title", "Total Cases in Pets"),
                        div(class = "summary-number h4", textOutput("pets_count"))
                    )
             )
           )
  ),
  tabPanel("FAQ",
           div(class = "container",
               br(),
               h2("Frequently Asked Questions"),
               uiOutput("faq_ui"),
               br(),
               div(
                 style = "text-align:center; margin-top:0; padding-top:5px;",
                 tags$p(
                   "More questions?",
                   style = "font-weight:600; font-size:18px; color:#555; margin-bottom:20px;"
                 ),
                 tags$a(
                   href = "mailto:h5n1mapping@gmail.com",
                   "Email us here",
                   style = "
      display:inline-block;
      max-width:180px;
      background-color:#d4883a;
      color:white;
      font-weight:700;
      text-decoration:none;
      padding:6px 14px;
      border-radius:6px;
      font-size:15px;
      box-shadow:0 2px 5px rgba(0,0,0,0.15);
      transition:background-color 0.2s ease;
    ",
                   onmouseover = "this.style.backgroundColor='#e0a96d';",
                   onmouseout = "this.style.backgroundColor='#d4883a';"
                 )
               )
           )
  ),
  
  tabPanel("About",
           div(class = "container",
               br(),
               h2("About This Dashboard", style = "color:#d4883a;"),
               p("We are a group of infectious disease epidemiologists concerned about the spread of H5N1 in the United States. We aim to provide real-time, accurate information regarding the current H5N1 situation in the United States."),
               br(),
               h4("Credits"),
               tags$ul(
                 tags$li("Dashboard was built and is currently maintained by members of the H5N1 Mapping Project."),
                 tags$li("Data sources: CDC, USDA, ProMED, etc."),
                 tags$li("Built using Shiny, Leaflet, Plotly, and Bootstrap 5"),
                 br()
               )
           )
  )
)


server <- function(input, output, session) {
  
  selected_animal <- reactiveVal("Humans") 
  
  faq_data <- reactiveVal(NULL)
  
  observe({
    sheet_url <- "https://docs.google.com/spreadsheets/d/12SDONQ5wyP1k2Rcheww_K2HY4lq5n-u_HNm0XCyXHc8/edit?usp=sharing"
    
    faq <- read_sheet(sheet_url)
    
    faq_data(faq)
  })
  
  observeEvent(input$btn_humans, { selected_animal("Humans") })
  observeEvent(input$btn_pets, { selected_animal("Pets") })
  observeEvent(input$btn_wild, { selected_animal("Wild") })
  observeEvent(input$btn_poultry, { selected_animal("Poultry") })
  observeEvent(input$btn_livestock, { selected_animal("Livestock") })
  
  filtered_poultry <- reactive({
    req(poultry_shp)
    
    if (!is.null(input$poultry_date_range)) {
      poultry_shp |>
        filter(outbreak_date >= input$poultry_date_range[1],
               outbreak_date <= input$poultry_date_range[2])
    } else {
      poultry_shp
    }
  })
  
  output$map_ui <- renderUI({
    switch(selected_animal(),
           "Humans" = leafletOutput("human_map"),
           "Pets" = leafletOutput("map_pets"),
           "Wild" = leafletOutput("map_wild"),
           "Poultry" = leafletOutput("map_poultry"),
           "Livestock" = leafletOutput("livestock_map"))
  })
  
  output$human_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.58, lat = 39.83, zoom = 3) |>
      addPolygons(data = human_map, color = "black", weight = 1,
                  fillColor = ~human_col(Total), fillOpacity = 0.7,
                  label = ~NAME,
                  popup = ~paste0(
                    "<div style='text-align:center; font-weight:bold; font-size:16px;'>Total Cases: ", Total, "</div><br>",
                    "<span style='color:red; font-weight:bold;'>Case count by exposure type</span><br>",
                    "<b>Dairy:</b> ", Dairy, "<br>",
                    "<b>Poultry:</b> ", Poultry, "<br>",
                    "<b>Other/Unknown:</b> ", Other + Unknown)) |>
      addLegend(pal = human_col, values = human_map$Total, title = "Human Cases", position = "bottomright")
  })
  
  output$map_pets <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.58, lat = 39.83, zoom = 3) |>
      addCircleMarkers(
        data = cat_data,
        lng = ~lng,
        lat = ~lat,
        radius = 7,
        color = "#8f5522",        
        fillColor = "#d8954c",    
        fillOpacity = 0.9,
        weight = 2,
        popup = ~paste(
          "<b>County:</b>", County.x, "<br>",
          "<b>Species:</b>", Species, "<br>",
          "<b>Date Collected:</b>", Date_Collected)
      ) |>
      addLegend(
        position = "bottomright",
        colors = "#d8954c",
        labels = "Cases in Pets",
        title = "Legend",
        opacity = 1
      )
  })
  
  output$map_wild <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.58, lat = 39.83, zoom = 3) |>
      addAwesomeMarkers(data = mammal_data, lng = ~lng, lat = ~lat,
                        icon = awesomeIcons(icon = "exclamation-sign", iconColor = "white", markerColor = ~markerColor),
                        popup = ~paste("Species:", Species, "<br>Date Collected:", Date_Collected),
                        clusterOptions = markerClusterOptions()) |>
      addAwesomeMarkers(data = bird_data, lng = ~lng, lat = ~lat,
                        icon = awesomeIcons(icon = "exclamation-sign", iconColor = "white", markerColor = "navy"),
                        popup = ~paste("Species:", Bird_Species, "<br>Date Collected:", Date_Collected),
                        clusterOptions = markerClusterOptions()) |>
      addLegend(position = "bottomright", title = "Animal Types",
                colors = unname(category_colors1), labels = names(category_colors1), opacity = 1)
  })
  
  output$map_poultry <- renderLeaflet({
    data <- filtered_poultry()
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.58, lat = 39.83, zoom = 3) |>
      addAwesomeMarkers(data = data, lng = ~lng, lat = ~lat,
                        icon = awesomeIcons(icon = "feather", library = "fa", iconColor = "white", markerColor = ~markerColor),
                        popup = ~paste("County:", county, "<br>State:", state, "<br>Date:", outbreak_date),
                        clusterOptions = markerClusterOptions()) |>
      addLegend(position = "bottomright", title = "Year",
                colors = unname(category_colors2), labels = names(category_colors2), opacity = 1)
  })
  
  output$poultry_date_filter <- renderUI({
    if (selected_animal() == "Poultry") {
      dateRangeInput("poultry_date_range",
                     label = "Filter by Outbreak Date",
                     start = min(poultry_shp$outbreak_date, na.rm = TRUE),
                     end = max(poultry_shp$outbreak_date, na.rm = TRUE),
                     format = "yyyy-mm-dd",
                     startview = "year")
    }
  })
  
  output$livestock_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.58, lat = 39.83, zoom = 3) |>
      addPolygons(data = livestock_map, color = "black", weight = 1,
                  fillColor = ~livestock_col(Count_Cases), fillOpacity = 0.7,
                  label = ~NAME,
                  popup = ~paste0("<b>Total Cases:</b> ", Count_Cases)) |>
      addLegend(pal = livestock_col, values = livestock_map$Count_Cases, title = "Livestock Cases", position = "bottomright")
  })
  
  output$wild_birds_count <- renderText({ nrow(bird_data) })
  output$wild_mammals_count <- renderText({ nrow(mammal_data) })
  output$pets_count <- renderText({ nrow(cat_data) })
  
  output$faq_ui <- renderUI({
    req(faq_data())
    faq <- faq_data()
    
    faq_list <- lapply(seq_len(nrow(faq)), function(i) {
      question <- faq$Question[i]
      answer <- faq$Answer[i]
      
      tags$div(
        tags$h4(question),
        tags$p(answer),
        tags$hr()
      )
    })
    
    do.call(tagList, faq_list)
  })
}

shinyApp(ui, server)
