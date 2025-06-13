library(shiny)
library(readxl)
library(readr)
library(shinydashboard)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(DT)
library(ggplot2)
library(rlang)  
library(tidyr)
library(lmtest)
library(nortest) 

dataDir = "C:/Users/acer/Downloads/Data/"
setwd(dataDir)
# UI

calculate_ahh <- function(X1, X2, X3, X4) {
  # Create positive truncation function (x - k)_+
  trunc_pos <- function(x, k) {
    pmax(0, x - k)
  }
  
  # Calculate each component of the model
  yhat <- 26.595 + 
    0.558 * X1 - 
    4.02 * trunc_pos(X1, 5.961) + 
    4.408 * trunc_pos(X1, 6.686) - 
    1.037 * trunc_pos(X1, 7.774) + 
    0.267 * X2 - 
    4.197 * trunc_pos(X2, 18.973) + 
    7.692 * trunc_pos(X2, 19.407) - 
    3.887 * trunc_pos(X2, 20.057) + 
    3.027 * X3 - 
    12.454 * trunc_pos(X3, 12.602) + 
    11.106 * trunc_pos(X3, 12.757) - 
    1.215 * trunc_pos(X3, 12.990) + 
    2.336 * X4 - 
    19.207 * trunc_pos(X4, 2.364) + 
    21.517 * trunc_pos(X4, 2.566) - 
    4.363 * trunc_pos(X4, 2.867)
  
  return(yhat)
}

parameter_data <- data.frame(
  Parameter = c("Intercept", "X1", "X1 (knot 5.961)", "X1 (knot 6.686)", "X1 (knot 7.774)",
                "X2", "X2 (knot 18.973)", "X2 (knot 19.407)", "X2 (knot 20.057)",
                "X3", "X3 (knot 12.602)", "X3 (knot 12.757)", "X3 (knot 12.990)",
                "X4", "X4 (knot 2.364)", "X4 (knot 2.566)", "X4 (knot 2.867)"),
  Koefisien = c(26.595, 0.558, -4.02, 4.408, -1.037, 0.267, -4.197, 7.692, -3.887,
                3.027, -12.454, 11.106, -1.215, 2.336, -19.207, 21.517, -4.363),
  t_value = c(1.731, 2.18, -2.714, 2.799, -2.669, 0.604, -1.872, 3.228, -4.278,
              3.064, -2.8, 1.94, -0.603, 2.655, -2.546, 2.196, -1.355),
  p_value = c(0.097, 0.0407, 0.0129, 0.0107, 0.0143, 0.5522, 0.075, 0.0040, 0.0003,
              0.0058, 0.0107, 0.065, 0.552, 0.0147, 0.0188, 0.0394, 0.189),
  Signifikansi = ifelse(c(0.097, 0.0407, 0.0129, 0.0107, 0.0143, 0.5522, 0.075, 0.0040, 0.0003,
                          0.0058, 0.0107, 0.065, 0.552, 0.0147, 0.0188, 0.0394, 0.189) < 0.05, 
                        "Signifikan", "Tidak Signifikan")
)


ui <- dashboardPage(
  dashboardHeader(title = "Proyeksi AHH 2025"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu", # Perbaikan: Tambahkan ID eksplisit untuk sidebarMenu
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Analisis", tabName = "analysis", icon = icon("chart-bar"))
    ),
    
    # Only show variable selection when on Dashboard tab
    conditionalPanel(
      condition = "input.sidebarMenu == 'dashboard'",
      # Select variable to display on map
      selectInput("display_var", "Variabel untuk Ditampilkan:", 
                  choices = c("Angka Harapan Hidup (Y)" = "Y", 
                              "Persentase Penduduk Miskin" = "X1", 
                              "Persentase Penduduk Perokok >5 Tahun" = "X2", 
                              "Harapan Lama Sekolah" = "X3", 
                              "Tingkat Pengangguran Terbuka" = "X4",
                              "Proyeksi AHH" = "yhat"),
                  selected = "Y"),
      
      # Slider for selecting value range
      
      # Color scheme selection
      selectInput("color_scheme", "Skema Warna Peta:", 
                  choices = c("YlOrRd", "YlGnBu", "RdYlGn", "Spectral", "Blues", "Greens", "Reds"),
                  selected = "YlOrRd"),
      
      # Model info
      tags$div(
        tags$h4("Info Model", style = "margin-top: 20px; padding-left: 10px;"),
        tags$p("Pemodelan berdasarkan pemilihan titik knot terbaik menggunakan mGCV dengan jumlah 3 knot pada setiap variabel ", 
               style = "padding-left: 10px; padding-right: 10px; font-size: 11px;")
      )
    )
  ),
  
  dashboardBody(
    # Perbaikan: Tambahkan CSS untuk memastikan semua UI elements terlihat
    tags$head(
      tags$style(HTML("
        .sidebar-menu {z-index: 1000;}
        #dynamic_slider_container {margin-bottom: 15px;}
        .form-group {margin-bottom: 15px;}
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Peta Visualisasi Data di Jawa Timur", width = 12, status = "primary",
                    leafletOutput("map", height = 600)
                )
              ),
              fluidRow(
                valueBoxOutput("min_box", width = 3),
                valueBoxOutput("mean_box", width = 3),
                valueBoxOutput("median_box", width = 3),
                valueBoxOutput("max_box", width = 3)
              ),
              fluidRow(
                box(title = "Detail Kabupaten/Kota yang Dipilih", width = 12, status = "info",
                    "Klik pada peta untuk melihat detail kabupaten/kota",
                    verbatimTextOutput("click_info")
                )
              )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Upload Data Excel", width = 12, status = "primary",
                    fileInput("file_input", "Pilih File Excel (.xlsx):",
                              accept = c(".xlsx")),
                    helpText("Upload file Excel dengan kolom: Kabupaten/Kota, Y, X1, X2, X3, X4"),
                    actionButton("compute_predictions", "Hitung Proyeksi AHH", 
                                 icon = icon("calculator"), 
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              ),
              fluidRow(
                box(title = "Data yang Diupload", width = 12, status = "primary",
                    downloadButton("download_data", "Unduh Data CSV", icon = icon("download")),
                    tags$br(), tags$br(),
                    DTOutput("table")
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Korelasi antar Variabel", width = 5, status = "primary",
                    plotOutput("correlation_plot", height = 400)
                ),
                box(title = "Perbandingan Nilai Aktual dan Prediksi AHH", width = 4, status = "primary",
                    plotOutput("prediction_comparison_plot", height = 331),
                    tags$div(
                      tags$p("Garis putus-putus merah menunjukkan nilai ideal di mana prediksi = aktual.", 
                             style = "font-style: italic; color: #666;")
                    )
                ),
                box(title = "Ringkasan Statistik Proyeksi AHH 2025", width = 3, status = "primary",
                    tableOutput("prediction_summary"),
                    tags$div(
                      tags$p("Catatan: Proyeksi AHH dihitung dengan model regresi nonparametrik spline truncated.", 
                             style = "font-style: italic; color: #666;")
                    )
                )
              ),
              fluidRow(
                box(title = "Parameter Model Spline Truncated", width = 12, status = "primary",
                    DTOutput("parameter_table"),
                    tags$div(
                      tags$p("Catatan: Parameter dengan p-value < 0.05 dianggap signifikan pada α = 5%.", 
                             style = "font-style: italic; color: #666; margin-top: 10px;")
                    )
                )
              ),
              fluidRow(
                box(title = "Uji Asumsi Residual", width = 12, status = "info",
                    fluidRow(
                      column(4,
                             h4("Uji Homoskedastisitas (Glejser)", style = "color: #2c3e50;"),
                             verbatimTextOutput("glejser_test"),
                             tags$div(
                               tags$p("H0: Residual homoskedastis (varians konstan)", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("H1: Residual heteroskedastis (varians tidak konstan)", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("Keputusan: Tolak H0 jika p-value < 0.05", 
                                      style = "font-size: 11px; color: #666;")
                             )
                      ),
                      column(4,
                             h4("Uji Independensi (Durbin-Watson)", style = "color: #2c3e50;"),
                             verbatimTextOutput("durbin_watson_test"),
                             tags$div(
                               tags$p("H0: Tidak ada autokorelasi", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("H1: Ada autokorelasi", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("Keputusan: Tolak H0 jika p-value < 0.05", 
                                      style = "font-size: 11px; color: #666;")
                             )
                      ),
                      column(4,
                             h4("Uji Normalitas (Kolmogorov-Smirnov)", style = "color: #2c3e50;"),
                             verbatimTextOutput("ks_test"),
                             tags$div(
                               tags$p("H0: Residual berdistribusi normal", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("H1: Residual tidak berdistribusi normal", 
                                      style = "font-size: 11px; color: #666; margin-bottom: 2px;"),
                               tags$p("Keputusan: Tolak H0 jika p-value < 0.05", 
                                      style = "font-size: 11px; color: #666;")
                             )
                      )
                    ),
                    tags$hr(),
                    fluidRow(
                      column(6,
                             h4("Q-Q Plot Normalitas Residual", style = "color: #2c3e50;"),
                             plotOutput("qq_plot", height = 300)
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Hasil Proyeksi AHH 2025 per Kabupaten/Kota", width = 12, status = "primary",
                    DTOutput("projection_table"),
                    tags$br(),
                    downloadButton("download_projection", "Unduh Hasil Proyeksi CSV", icon = icon("download"))
                )
              ),
              fluidRow(
                box(title = "Perbandingan Kabupaten/Kota", width = 12, status = "primary",
                    fluidRow(
                      column(6,
                             selectInput("compare_variable", "Pilih Variabel untuk Dibandingkan:",
                                         choices = c("Angka Harapan Hidup (Y)" = "Y", 
                                                     "Persentase Penduduk Miskin" = "X1", 
                                                     "Persentase Penduduk >5 Tahun Merokok" = "X2", 
                                                     "Harapan Lama Sekolah" = "X3", 
                                                     "Tingkat Pengangguran Terbuka" = "X4",
                                                     "Proyeksi AHH" = "yhat"),
                                         selected = "Y")
                      ),
                      column(6,
                             selectInput("compare_kabupaten", "Pilih Kabupaten/Kota untuk Dibandingkan:",
                                         choices = NULL, multiple = TRUE, selected = NULL)
                      )
                    ),
                    plotOutput("comparison_plot", height = 500)
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Separate data storage for shapefile data and uploaded data
  data_values <- reactiveValues(
    sf_data = NULL,          # For shapefile data - used in dashboard
    excel_data = NULL        # For uploaded Excel data - used in data panel
  )
  
  
  # Read shapefile for Jawa Timur
  observe({
    # Try to read the shapefile (without requiring a specific working directory)
    sf_data <- tryCatch({
      st_read("Jawa_Timur_ADMIN_BPS.shp", quiet = TRUE)
    }, error = function(e) {
      # Return NULL if file not found
      return(NULL)
    })
    
    # Store in reactive values
    data_values$sf_data <- sf_data
  })
  
  
  # Handle Excel file upload
  observeEvent(input$file_input, {
    req(input$file_input)
    
    # Read Excel file
    tryCatch({
      excel_data <- read_excel(input$file_input$datapath)
      
      # Rename columns if needed to match expected format
      if("Kabupaten" %in% names(excel_data)) {
        excel_data <- excel_data %>% rename(Kabupaten = `Kabupaten`)
      }
      
      # Store in reactive values
      data_values$excel_data <- excel_data
      
      # Update kabupaten choices for comparison if data exists
      if(!is.null(excel_data)) {
        kabupaten_choices <- sort(unique(excel_data$Kabupaten))
        updateSelectInput(session, "compare_kabupaten", 
                          choices = kabupaten_choices,
                          selected = kabupaten_choices[1:min(5, length(kabupaten_choices))])
      }
      
      # Show notification when file is loaded successfully
      showNotification(paste("File berhasil diupload:", input$file_input$name), type = "default")
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  

  observeEvent(input$compute_predictions, {
    req(data_values$excel_data)
    
    # Show a notification that calculation is starting
    id <- showNotification("Menghitung proyeksi...", type = "default", duration = NULL)
    on.exit(removeNotification(id), add = TRUE)
    
    result <- tryCatch({
      # Print data structure to diagnose issues (can be removed in production)
      print("Imported column names:")
      print(names(data_values$excel_data))
      
      # Check if required columns exist (case-insensitive check)
      required_cols <- c("X1", "X2", "X3", "X4")
      cols_exist <- sapply(required_cols, function(col) {
        any(toupper(names(data_values$excel_data)) == toupper(col))
      })
      
      if(!all(cols_exist)) {
        missing_cols <- required_cols[!cols_exist]
        return(list(success = FALSE, 
                    message = paste("Kolom tidak lengkap:", paste(missing_cols, collapse=", ")), 
                    data = NULL))
      }
      
      # Ensure column names match exactly what the function expects
      for(col in required_cols) {
        col_idx <- which(toupper(names(data_values$excel_data)) == toupper(col))
        if(length(col_idx) > 0 && names(data_values$excel_data)[col_idx] != col) {
          names(data_values$excel_data)[col_idx] <- col
        }
      }
      
      # Cek tipe data
      data_valid <- all(sapply(required_cols, function(col) {
        is.numeric(data_values$excel_data[[col]]) && !any(is.na(data_values$excel_data[[col]]))
      }))
      
      if(!data_valid) {
        return(list(success = FALSE, message = "Data tidak valid atau mengandung NA di kolom X1, X2, X3, atau X4", data = NULL))
      }
      
      # Hitung proyeksi
      data_with_pred <- data_values$excel_data %>%
        rowwise() %>%
        mutate(yhat = calculate_ahh(X1, X2, X3, X4)) %>%
        ungroup()
      
      list(success = TRUE, message = "Perhitungan proyeksi berhasil", data = data_with_pred)
    }, error = function(e) {
      list(success = FALSE, message = paste("Error:", e$message), data = NULL)
    })
    
    # Proses hasil
    if(result$success) {
      # Update data dengan hasil proyeksi
      data_values$excel_data <- result$data
      
      # Tampilkan notifikasi sukses
      showNotification(result$message, type = "message")
    } else {
      # Tampilkan notifikasi error
      showNotification(result$message, type = "error")
    }
  })
  
  
  # Combined data for map - merges shapefile with Excel data if both exist
  peta_jatim <- reactive({
    req(data_values$sf_data)
    return(data_values$sf_data)
  })
  
  # Get data for analysis and tables - prioritize Excel data if available
  analysis_data <- reactive({
    if(!is.null(data_values$excel_data)) {
      return(data_values$excel_data)
    } else if(!is.null(data_values$sf_data)) {
      return(data_values$sf_data %>% st_drop_geometry())
    } else {
      return(NULL)
    }
  })
  
  # Selected variable for display
  selected_var <- reactive({
    input$display_var
  })
  
  # Range of values for selected variable
  var_range <- reactive({
    req(peta_jatim())
    var <- selected_var()
    
    if(var %in% names(peta_jatim())) {
      data_vector <- peta_jatim()[[var]]
      c(min(data_vector, na.rm = TRUE), max(data_vector, na.rm = TRUE))
    } else {
      c(0, 1)  # Default range if variable not found
    }
  })
  
  # Dynamic slider based on selected variable
  output$range_slider <- renderUI({
    req(peta_jatim())
    var <- selected_var()
    range <- var_range()
    
    # Format step size based on value range
    step_size <- (range[2] - range[1]) / 50
    if (step_size < 0.01) step_size <- 0.01
    
    var_name <- switch(var,
                       "Y" = "Angka Harapan Hidup 2024",
                       "X1" = "Persentase Penduduk Miskin",
                       "X2" = "Persentase Penduduk >5 Tahun Merokok",
                       "X3" = "Harapan Lama Sekolah",
                       "X4" = "Tingkat Pengangguran Terbuka",
                       "yhat" = "Proyeksi AHH")
    
    sliderInput("var_range", paste0("Rentang Nilai ", var_name, ":"),
                min = range[1], max = range[2], 
                value = range, step = step_size)
  })
  
  # Update kabupaten choices when shapefile data changes
  observe({
    req(peta_jatim())
    kabupaten_choices <- sort(unique(peta_jatim()$Kabupaten))
    updateSelectInput(session, "compare_kabupaten", 
                      choices = kabupaten_choices,
                      selected = kabupaten_choices[1:min(5, length(kabupaten_choices))])
  })
  
  # Filter data based on selected range
  filtered_data <- reactive({
    req(peta_jatim())
    var <- selected_var()
    
    range_to_use <- if(!is.null(input$var_range)) input$var_range else input$var_range_static
    
    if(var %in% names(peta_jatim()) && !is.null(range_to_use)) {
      # Filter data
      data_temp <- peta_jatim()
      filtered <- data_temp[data_temp[[var]] >= range_to_use[1] & 
                              data_temp[[var]] <= range_to_use[2], ]
      
      return(filtered)
    } else {
      return(peta_jatim())
    }
  })
  
  # Render map
  output$map <- renderLeaflet({
    req(filtered_data())
    var <- selected_var()
    
    # Check if selected variable exists in data
    if(!(var %in% names(filtered_data()))) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = 112.7, lat = -7.8, zoom = 8) %>%
               addControl("Variable not found in data", position = "topright"))
    }
    
    color_scheme <- if(!is.null(input$color_scheme)) input$color_scheme else "YlOrRd"
    
    # Create color palette based on selected variable
    pal <- colorNumeric(
      palette = input$color_scheme,
      domain = peta_jatim()[[var]]
    )
    
    # Title for legend
    legend_title <- switch(var,
                           "Y" = "Angka Harapan Hidup 2024",
                           "X1" = "Persentase Penduduk Miskin",
                           "X2" = "Persentase Penduduk >5 Tahun Merokok",
                           "X3" = "Harapan Lama Sekolah",
                           "X4" = "Tingkat Pengangguran Terbuka",
                           "yhat" = "Proyeksi AHH")
    
    # Create leaflet map
    leaflet() %>%
      # Add basemap options
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      setView(lng = 112.7, lat = -7.8, zoom = 8) %>%
      
      # Add polygons for kabupaten/kota
      addPolygons(
        data = filtered_data(),
        fillColor = ~pal(get(var)),  # Use get() to access var dynamically
        fillOpacity = 0.7,
        color = "#666",
        weight = 1,
        highlight = highlightOptions(
          weight = 3,
          color = "#000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(Kabupaten, ": ", round(get(var), 2)),  # Use get() for var
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~Kabupaten
      ) %>%
      
      # Add legend
      addLegend(
        position = "bottomright",
        pal = pal,
        values = peta_jatim()[[var]],
        title = legend_title,
        opacity = 0.7
      ) %>%
      
      # Add layer controls
      addLayersControl(
        baseGroups = c("OpenStreetMap", "CartoDB Light", "CartoDB Dark", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Capture clicks on map
  observeEvent(input$map_shape_click, {
    click_data <- input$map_shape_click
    
    # Find clicked kabupaten
    selected_kabupaten <- click_data$id
    
    if(!is.null(selected_kabupaten)) {
      # Get data for selected kabupaten
      selected_data <- peta_jatim() %>%
        filter(Kabupaten == selected_kabupaten) %>%
        st_drop_geometry()
      
      # Update click info output
      output$click_info <- renderPrint({
        cat("Kabupaten/Kota:", selected_data$Kabupaten, "\n")
        if("Y" %in% names(selected_data)) cat("Angka Harapan Hidup 2024:", round(selected_data$Y, 2), "\n")
        if("X1" %in% names(selected_data)) cat("Persentase Penduduk Miskin:", round(selected_data$X1, 2), "\n")
        if("X2" %in% names(selected_data)) cat("Persentase Penduduk >5 Tahun Merokok:", round(selected_data$X2, 2), "\n")
        if("X3" %in% names(selected_data)) cat("Harapan Lama Sekolah:", round(selected_data$X3, 2), "\n")
        if("X4" %in% names(selected_data)) cat("Tingkat Pengangguran Terbuka:", round(selected_data$X4, 2), "\n")
      })
    }
  })
  
  # Value boxes
  output$min_box <- renderValueBox({
    req(peta_jatim())
    var <- selected_var()
    
    if(var %in% names(peta_jatim())) {
      min_val <- min(peta_jatim()[[var]], na.rm = TRUE)
      
      # Alternative way to filter without using .data
      data_temp <- peta_jatim()
      min_kab <- data_temp[data_temp[[var]] == min_val, "Kabupaten", drop = TRUE][1]
      
      var_name <- switch(var,
                         "Y" = "Angka Harapan Hidup",
                         "X1" = "Persentase Penduduk Miskin",
                         "X2" = "Persentase Penduduk >5 Tahun Merokok",
                         "X3" = "Harapan Lama Sekolah",
                         "X4" = "Tingkat Pengangguran Terbuka",
                         "yhat" = "Proyeksi AHH")
      
      box_title <- paste0("Minimum ", var_name, ": ", min_kab)
      
      valueBox(
        round(min_val, 2),
        box_title,
        icon = icon("arrow-down"),
        color = "red"
      )
    } else {
      valueBox(
        "N/A",
        "Data tidak tersedia",
        icon = icon("warning"),
        color = "red"
      )
    }
  })
  
  output$mean_box <- renderValueBox({
    req(peta_jatim())
    var <- selected_var()
    
    if(var %in% names(peta_jatim())) {
      var_name <- switch(var,
                         "Y" = "Angka Harapan Hidup",
                         "X1" = "Persentase Penduduk Miskin",
                         "X2" = "Persentase Penduduk >5 Tahun Merokok",
                         "X3" = "Harapan Lama Sekolah",
                         "X4" = "Tingkat Pengangguran Terbuka",
                         "yhat" = "Proyeksi AHH")
      
      valueBox(
        round(mean(peta_jatim()[[var]], na.rm = TRUE), 2),
        paste0("Rata-rata ", var_name),
        icon = icon("calculator"),
        color = "purple"
      )
    } else {
      valueBox(
        "N/A",
        "Data tidak tersedia",
        icon = icon("warning"),
        color = "purple"
      )
    }
  })
  
  output$median_box <- renderValueBox({
    req(peta_jatim())
    var <- selected_var()
    
    if(var %in% names(peta_jatim())) {
      var_name <- switch(var,
                         "Y" = "Angka Harapan Hidup",
                         "X1" = "Persentase Penduduk Miskin",
                         "X2" = "Persentase Penduduk >5 Tahun Merokok",
                         "X3" = "Harapan Lama Sekolah",
                         "X4" = "Tingkat Pengangguran Terbuka",
                         "yhat" = "Proyeksi AHH")
      
      valueBox(
        round(median(peta_jatim()[[var]], na.rm = TRUE), 2),
        paste0("Median ", var_name),
        icon = icon("percentage"),
        color = "blue"
      )
    } else {
      valueBox(
        "N/A",
        "Data tidak tersedia",
        icon = icon("warning"),
        color = "blue"
      )
    }
  })
  
  output$max_box <- renderValueBox({
    req(peta_jatim())
    var <- selected_var()
    
    if(var %in% names(peta_jatim())) {
      max_val <- max(peta_jatim()[[var]], na.rm = TRUE)
      
      # Alternative way to filter without using .data
      data_temp <- peta_jatim()
      max_kab <- data_temp[data_temp[[var]] == max_val, "Kabupaten", drop = TRUE][1]
      
      var_name <- switch(var,
                         "Y" = "Angka Harapan Hidup",
                         "X1" = "Persentase Penduduk Miskin",
                         "X2" = "Persentase Penduduk >5 Tahun Merokok",
                         "X3" = "Harapan Lama Sekolah",
                         "X4" = "Tingkat Pengangguran Terbuka",
                         "yhat" = "Proyeksi AHH")
      
      box_title <- paste0("Maksimum ", var_name, ": ", max_kab)
      
      valueBox(
        round(max_val, 2),
        box_title,
        icon = icon("arrow-up"),
        color = "green"
      )
    } else {
      valueBox(
        "N/A",
        "Data tidak tersedia",
        icon = icon("warning"),
        color = "green"
      )
    }
  })
  
  # Render data table
  output$table <- renderDT({
    # Only show data if a file has been uploaded
    if(is.null(data_values$excel_data)) {
      # Return empty table with a message when no file is uploaded
      return(datatable(
        data.frame(Message = "Silakan upload file data terlebih dahulu."),
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        )
      ))
    }
    
    # Pastikan analysis_data() mengembalikan data yang valid
    current_data <- analysis_data()
    if(is.null(current_data)) {
      return(datatable(
        data.frame(Message = "Data tidak tersedia."),
        options = list(dom = 't', ordering = FALSE, paging = FALSE, searching = FALSE, info = FALSE)
      ))
    }
    
    # Gunakan intersect untuk memastikan kolom ada
    available_cols <- names(current_data)
    desired_cols <- c("Kabupaten", "Y", "X1", "X2", "X3", "X4", "yhat")
    selected_cols <- intersect(available_cols, desired_cols)
    
    if(length(selected_cols) > 0) {
      data_to_show <- current_data[, selected_cols, drop = FALSE]
      
      # Arrange data if Y column exists
      if("Y" %in% selected_cols) {
        data_to_show <- data_to_show %>% arrange(desc(Y))
      }
      
      # Round numeric columns
      numeric_cols <- sapply(data_to_show, is.numeric)
      data_to_show[numeric_cols] <- lapply(data_to_show[numeric_cols], round, 2)
      
      datatable(data_to_show, options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 38),
        searching = TRUE
      ))
    } else {
      datatable(data.frame(Message = "Kolom data tidak ditemukan"), options = list(dom = 't'))
    }
  })
  
  residual_data <- reactive({
    req(analysis_data())
    
    # Cek apakah Y dan yhat ada dalam data
    if(all(c("Y", "yhat") %in% names(analysis_data()))) {
      data <- analysis_data()
      
      # Filter data yang tidak NA
      data <- data[!is.na(data$Y) & !is.na(data$yhat), ]
      
      # Hitung residual
      data$residual <- data$Y - data$yhat
      
      return(data)
    } else {
      return(NULL)
    }
  })
  
  output$parameter_table <- renderDT({
    datatable(parameter_data, 
              options = list(
                pageLength = 17,
                lengthMenu = c(10, 17, 25),
                searching = TRUE,
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel'),
                columnDefs = list(
                  list(className = 'dt-center', targets = c(0, 2, 3, 4)),
                  list(className = 'dt-right', targets = 1)
                )
              ),
              rownames = FALSE) %>%
      formatStyle(
        'p_value',
        backgroundColor = styleInterval(0.05, c('#d4edda', '#f8d7da')),
        color = styleInterval(0.05, c('#155724', '#721c24'))
      ) %>%
      formatRound(columns = c('Koefisien', 't_value', 'p_value'), digits = 4)
  })
  
  output$prediction_summary <- renderTable({
    req(analysis_data())
    
    # Cek apakah yhat ada dalam data
    if("yhat" %in% names(analysis_data())) {
      data <- analysis_data()
      
      # Filter data yang tidak NA
      yhat_data <- data$yhat[!is.na(data$yhat)]
      
      if(length(yhat_data) > 0) {
        # Hitung statistik ringkasan
        summary_stats <- data.frame(
          Statistik = c("Minimum", "Q1", "Median", "Mean", "Q3", "Maximum", "Std Dev"),
          Nilai = c(
            round(min(yhat_data), 2),
            round(quantile(yhat_data, 0.25), 2),
            round(median(yhat_data), 2),
            round(mean(yhat_data), 2),
            round(quantile(yhat_data, 0.75), 2),
            round(max(yhat_data), 2),
            round(sd(yhat_data), 2)
          )
        )
        
        return(summary_stats)
      } else {
        return(data.frame(Statistik = "Error", Nilai = "Data tidak valid"))
      }
    } else {
      return(data.frame(
        Statistik = "Info", 
        Nilai = "Belum ada proyeksi"
      ))
    }
  }, striped = TRUE, hover = TRUE)
  
  output$glejser_test <- renderPrint({
    req(residual_data())
    
    tryCatch({
      data <- residual_data()
      
      # Uji Glejser: regresikan |residual| terhadap fitted values
      abs_residual <- abs(data$residual)
      fitted_values <- data$yhat
      
      # Lakukan regresi linear
      glejser_model <- lm(abs_residual ~ fitted_values)
      
      # Uji signifikansi koefisien fitted_values
      summary_model <- summary(glejser_model)
      coef_fitted <- summary_model$coefficients["fitted_values", ]
      
      cat("=== UJI GLEJSER ===\n")
      cat("p-value:", round(coef_fitted["Pr(>|t|)"], 4), "\n")
      
      if(coef_fitted["Pr(>|t|)"] < 0.05) {
        cat("Kesimpulan: Heteroskedastis (p < 0.05)\n")
        cat("Asumsi homoskedastisitas TIDAK terpenuhi\n")
      } else {
        cat("Kesimpulan: Homoskedastis (p >= 0.05)\n")
        cat("Asumsi homoskedastisitas TERPENUHI\n")
      }
      
    }, error = function(e) {
      cat("Error dalam Uji Glejser:", e$message, "\n")
      cat("Pastikan data tersedia dan valid.\n")
    })
  })
  
  # Uji Durbin-Watson untuk independensi
  output$durbin_watson_test <- renderPrint({
    req(residual_data())
    
    tryCatch({
      data <- residual_data()
      
      # Buat model linear sederhana untuk uji Durbin-Watson
      # Karena kita sudah punya residual, kita buat model dummy
      dummy_model <- lm(Y ~ yhat, data = data)
      
      # Lakukan uji Durbin-Watson
      dw_test <- dwtest(dummy_model)
      
      cat("=== UJI DURBIN-WATSON ===\n")
      cat("DW Statistic:", round(dw_test$statistic, 4), "\n")
      cat("p-value:", round(dw_test$p.value, 4), "\n")
      
      if(dw_test$p.value < 0.05) {
        cat("Kesimpulan: Ada autokorelasi (p < 0.05)\n")
        cat("Asumsi independensi TIDAK terpenuhi\n")
      } else {
        cat("Kesimpulan: Tidak ada autokorelasi (p >= 0.05)\n")
        cat("Asumsi independensi TERPENUHI\n")
      }
      
      # Interpretasi nilai DW
      if(dw_test$statistic > 1.5 && dw_test$statistic < 2.5) {
        cat("Nilai DW mendekati 2: Indikasi tidak ada autokorelasi\n")
      } else if(dw_test$statistic < 1.5) {
        cat("Nilai DW < 1.5: Indikasi autokorelasi positif\n")
      } else {
        cat("Nilai DW > 2.5: Indikasi autokorelasi negatif\n")
      }
      
    }, error = function(e) {
      cat("Error dalam Uji Durbin-Watson:", e$message, "\n")
      cat("Pastikan data tersedia dan valid.\n")
    })
  })
  
  # Uji Kolmogorov-Smirnov untuk normalitas
  output$ks_test <- renderPrint({
    req(residual_data())
    
    tryCatch({
      data <- residual_data()
      residuals <- data$residual
      
      # Lakukan uji Kolmogorov-Smirnov seperti script asli
      ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
      
      cat("=== UJI KOLMOGOROV-SMIRNOV ===\n")
      cat("KS Statistic:", round(ks_test$statistic, 4), "\n")
      cat("p-value:", round(ks_test$p.value, 4), "\n")
      cat("Mean:", round(mean(residuals), 4), "\n")
      cat("Std Dev:", round(sd(residuals), 4), "\n")
      
      if(ks_test$p.value < 0.05) {
        cat("Kesimpulan: Residual tidak normal (p < 0.05)\n")
        cat("Asumsi normalitas TIDAK terpenuhi\n")
      } else {
        cat("Kesimpulan: Residual berdistribusi normal (p >= 0.05)\n")
        cat("Asumsi normalitas TERPENUHI\n")
      }
    }, error = function(e) {
      cat("Error dalam Uji Kolmogorov-Smirnov:", e$message, "\n")
      cat("Pastikan data tersedia dan valid.\n")
    })
  })
  
  # Q-Q plot untuk normalitas
  output$qq_plot <- renderPlot({
    req(residual_data())
    
    data <- residual_data()
    
    ggplot(data, aes(sample = residual)) +
      stat_qq(color = "blue", size = 2) +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(title = "Q-Q Plot Normalitas Residual",
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  
  output$projection_table <- renderDT({
    req(analysis_data())
    
    # Cek apakah yhat ada dalam data
    if("yhat" %in% names(analysis_data())) {
      data <- analysis_data()
      
      # Tentukan kolom yang tersedia
      available_cols <- names(data)
      desired_cols <- c("Kabupaten", "Y", "yhat")
      selected_cols <- desired_cols[desired_cols %in% available_cols]
      
      # Filter data yang tidak NA dan pilih kolom yang dibutuhkan menggunakan base R
      projection_data <- data[!is.na(data$yhat), selected_cols]
      
      # Urutkan berdasarkan yhat (descending)
      projection_data <- projection_data[order(-projection_data$yhat), ]
      
      # Tambahkan ranking dan bulatkan angka
      projection_data$Ranking <- 1:nrow(projection_data)
      projection_data$yhat <- round(projection_data$yhat, 2)
      
      if("Y" %in% names(projection_data)) {
        projection_data$Y <- round(projection_data$Y, 2)
      }
      
      # Reorder kolom dengan Ranking di depan
      if("Y" %in% names(projection_data)) {
        projection_data <- projection_data[, c("Ranking", "Kabupaten", "Y", "yhat")]
      } else {
        projection_data <- projection_data[, c("Ranking", "Kabupaten", "yhat")]
      }
      
      # Rename columns for display
      names(projection_data)[names(projection_data) == "Kabupaten"] <- "Kabupaten/Kota"
      names(projection_data)[names(projection_data) == "Y"] <- "AHH Aktual 2024"
      names(projection_data)[names(projection_data) == "yhat"] <- "Proyeksi AHH 2025"
      
      # Tambahkan kolom selisih jika data Y tersedia
      if("AHH Aktual 2024" %in% names(projection_data) && "Proyeksi AHH 2025" %in% names(projection_data)) {
        projection_data$`Selisih (Proyeksi - Aktual)` <- round(projection_data$`Proyeksi AHH 2025` - projection_data$`AHH Aktual 2024`, 2)
      }
      
      datatable(projection_data, 
                options = list(
                  pageLength = 15,
                  lengthMenu = c(10, 15, 25, 38),
                  searching = TRUE,
                  dom = 'Blfrtip',
                  buttons = c('copy', 'csv', 'excel'),
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE) %>%
        formatStyle(
          'Proyeksi AHH 2025',
          backgroundColor = styleInterval(c(70, 75), c('#ffcccc', '#ffffcc', '#ccffcc'))
        )
    } else {
      # Return empty table with message when no projection data available
      datatable(
        data.frame(Message = "Belum ada hasil proyeksi AHH. Silakan klik tombol 'Hitung Proyeksi AHH' pada panel Data terlebih dahulu."),
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        ),
        rownames = FALSE
      )
    }
  })
  
  output$comparison_plot <- renderPlot({
    req(input$compare_variable, input$compare_kabupaten)
    
    # Gunakan data Excel jika tersedia, jika tidak gunakan data shapefile
    data_source <- if(!is.null(data_values$excel_data)) {
      data_values$excel_data
    } else if(!is.null(data_values$sf_data)) {
      data_values$sf_data %>% st_drop_geometry()
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Data tidak tersedia.\nSilakan upload file Excel terlebih dahulu.") +
               theme_void())
    }
    
    # Filter data untuk kabupaten yang dipilih
    filtered_data <- data_source %>%
      filter(Kabupaten %in% input$compare_kabupaten)
    
    # Cek apakah variabel yang dipilih ada dalam data
    if(!(input$compare_variable %in% names(filtered_data))) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Variabel", input$compare_variable, "tidak ditemukan dalam data.")) +
               theme_void())
    }
    
    # Cek apakah ada data yang tersedia
    if(nrow(filtered_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Tidak ada data untuk kabupaten yang dipilih.") +
               theme_void())
    }
    
    # Buat label untuk variabel
    var_label <- switch(input$compare_variable,
                        "Y" = "Angka Harapan Hidup",
                        "X1" = "Persentase Penduduk Miskin (%)",
                        "X2" = "Persentase Penduduk >5 Tahun Merokok (%)",
                        "X3" = "Harapan Lama Sekolah (Tahun)",
                        "X4" = "Tingkat Pengangguran Terbuka (%)",
                        "yhat" = "Proyeksi AHH 2025")
    
    # Siapkan data untuk plotting
    plot_data <- filtered_data
    var_col <- input$compare_variable
    
    # Urutkan data berdasarkan nilai variabel (descending)
    plot_data <- plot_data[order(-plot_data[[var_col]]), ]
    
    # Buat plot bar
    ggplot(plot_data, aes(x = reorder(Kabupaten, get(var_col)), 
                          y = get(var_col))) +
      geom_col(aes(fill = get(var_col)), 
               alpha = 0.8, 
               color = "black", 
               size = 0.3) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = var_label) +
      coord_flip() +
      labs(title = paste("Perbandingan", var_label, "antar Kabupaten/Kota"),
           subtitle = paste("Berdasarkan data", ifelse(!is.null(data_values$excel_data), "Excel yang diupload", "shapefile")),
           x = "Kabupaten/Kota",
           y = var_label) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.position = "right"
      ) +
      # Tambahkan label nilai pada setiap bar
      geom_text(aes(label = round(get(var_col), 2)), 
                hjust = -0.1, 
                size = 3)
  })
  
  # Render comparison plot
  output$prediction_comparison_plot <- renderPlot({
    req(analysis_data())
    
    # Cek apakah Y dan yhat ada dalam data
    if(all(c("Y", "yhat") %in% names(analysis_data()))) {
      data <- analysis_data()
      
      # Filter data yang tidak NA
      data <- data[!is.na(data$Y) & !is.na(data$yhat), ]
      
      # Urutkan data berdasarkan Angka Harapan Hidup (Y)
      data <- data[order(data$Y), ]
      
      # Buat plot perbandingan
      ggplot(data) +
        geom_point(aes(x = Y, y = yhat), color = "blue", size = 3) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = "Perbandingan Angka Harapan Hidup Aktual vs Proyeksi",
             x = "Angka Harapan Hidup Aktual",
             y = "Proyeksi AHH") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Plot kosong jika data tidak tersedia
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Data tidak tersedia.\nSilakan klik tombol 'Hitung Proyeksi AHH' pada panel Data terlebih dahulu.") +
        theme_void()
    }
  })
  
  output$correlation_plot <- renderPlot({
    req(analysis_data())
    
    # Check if all required variables exist in data
    required_vars <- c("Y", "X1", "X2", "X3", "X4")
    if(all(required_vars %in% names(analysis_data()))) {
      # Gunakan base R indexing untuk menghindari masalah dengan select()
      data <- analysis_data()[, required_vars]
      
      # Convert from wide to long format for visualization
      data_long <- tidyr::pivot_longer(
        data,
        cols = c(X1, X2, X3, X4),
        names_to = "Variable",
        values_to = "Value"
      )
      
      # Add descriptive labels for variables
      data_long$Variable <- factor(data_long$Variable,
                                   levels = c("X1", "X2", "X3", "X4"),
                                   labels = c("Penduduk Miskin", 
                                              "Penduduk Merokok", 
                                              "Harapan Lama Sekolah", 
                                              "Pengangguran Terbuka"))
      
      # Create scatterplot with facet for each predictor variable
      ggplot(data_long, aes(x = Value, y = Y)) +
        geom_point(aes(color = Variable), size = 3, alpha = 0.7) +
        facet_wrap(~ Variable, scales = "free_x", ncol = 2) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Hubungan Angka Harapan Hidup dengan Variabel Prediktor",
             y = "Angka Harapan Hidup",
             x = "Nilai Variabel") +
        theme_minimal() +
        theme(legend.position = "none",
              strip.background = element_rect(fill = "lightblue", color = "black"),
              strip.text = element_text(face = "bold"),
              plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Empty plot if data not available
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Data tidak tersedia") +
        theme_void()
    }
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-jawa-timur-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use the appropriate data source based on which tab we're on
      if (input$sidebarMenu == "dashboard") {
        # For dashboard tab, use shapefile data only
        data <- data_values$sf_data %>%
          st_drop_geometry() %>%
          select(any_of(c("Kabupaten", "Y", "X1", "X2", "X3", "X4"))) %>%
          arrange(desc(if("Y" %in% names(.)) Y else 1))
      } else {
        # For other tabs, use the analysis data (excel if available)  
        data <- analysis_data() %>%
          select(any_of(c("Kabupaten", "Y", "X1", "X2", "X3", "X4"))) %>%
          arrange(desc(if("Y" %in% names(.)) Y else 1))
      }
      
      # Try to rename columns if they exist
      if("Kabupaten" %in% names(data)) {
        data <- data %>% rename(`Kabupaten/Kota` = Kabupaten)
      }
      if("Y" %in% names(data)) {
        data <- data %>% rename(`Angka Harapan Hidup` = Y)
      }
      if("X1" %in% names(data)) {
        data <- data %>% rename(`Persentase Penduduk Miskin` = X1)
      }
      if("X2" %in% names(data)) {
        data <- data %>% rename(`Persentase Penduduk >5 Tahun Merokok` = X2)
      }
      if("X3" %in% names(data)) {
        data <- data %>% rename(`Harapan Lama Sekolah` = X3)
      }
      if("X4" %in% names(data)) {
        data <- data %>% rename(`Tingkat Pengangguran Terbuka` = X4)
      }
      
      write.csv(data, file, row.names = FALSE)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
