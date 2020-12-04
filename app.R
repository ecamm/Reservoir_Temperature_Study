## GM Temperature Study app.R ##
## Shiny dashboard to display temperature/depth data, rainfall, reservoir volume, air temp, and stratification process##

## libraries####
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate) ## datetime
library(here) ## local directory
library(janitor) ## clean names
library(fs) ## file management
library(plotly) ## interactive plots
library(mapview) ## mapping package
mapviewOptions(legend.pos = "bottomright") ## move default legend position
library(glue) ## string manipulation
library(gt) ## tables
library(viridis)
library(shinycssloaders) ## loading animations

## function to debug shinydashboard problem with displaying the dashboardbody
convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs["data-toggle"] <- "tab"
  mi$children[[1]]$attribs["data-value"] <- tabName
  mi
}

## load files ####
## use regexp to look for files with csv format
fs::dir_ls("data\\tempproject")
csv_files <- fs::dir_ls("data\\tempproject", regexp = "\\.csv$")
colnames <- c("measurement", "date_time", "temperature")

read_and_add <- function(file) {
  dat <- read_csv(file, skip = 2, col_names = colnames)
  dat
}

## purrr loops over each file and applies function + cleaning steps in pipeline
depth_temp <- map_dfr(
  .x = csv_files,
  .id = "source",
  .f = read_and_add
) %>%
  separate(source, c("first", "second", "depth", sep = "/")) %>%
  select(-first, -second, -`/`) %>%
  mutate(depth = as.numeric(str_replace_all(depth, "M", ""))) %>%
  mutate(date_time = mdy_hms(date_time)) %>%
  arrange(depth) %>% 
  filter(temperature <= 100) #data cleaning - 42 erroneous temperature datum of 300-500deg

## reservoir level data ####
pi_data <- read_csv(here("data", "reservoir_pi_data.csv"))
pi_data$date_time <- mdy_hm(pi_data$date_time)
pi_data$`GM RWPS LAKE LVL (GEODECTIC)(m)` <- as.numeric(pi_data$`GM RWPS LAKE LVL (GEODECTIC)(m)`)

pi_data <- pivot_longer(pi_data,
  cols = c(-date_time), names_to = "pi_tag",
  values_to = "pi_value"
)

## rain data - manual import
# rain <- read.csv(here("data", "lincolnpark.csv"), skip = 8) %>%
#   rename(Date = yyyy.mm.dd.hh.mm.ss, cumulative_mm = mm, mm = mm.1) %>%
#   mutate(rain = mm - lag(mm, default = first(mm))) %>%
#   mutate(rain = if_else(rain < 0, 0, rain)) %>%
#   select(Date, cumulative_mm, mm, rain) %>%
#   mutate(day = mdy_hm(Date)) %>%
#   mutate(date_parsed = parse_date_time(day, "%Y-%m-%d %H:%M %s", tz = "America/Edmonton")) %>%
#   arrange(date_parsed)
#
#
# rain_hourly <- rain %>%
#   group_by(date_time = floor_date(day, "1 hour")) %>%
#   summarize(rain = sum(rain))

## querying rain data from Socrata Open Data Portal
## rain data ####
library(RSocrata)
## uncomment below when saving new rain data
# url <- "https://data.calgary.ca/api/odata/v4/d9kv-swk3"
# rain_socrata <- read.socrata(url)
# saveRDS(rain_socrata, "rain_query")

# use to avoid querying each time
rain_socrata <- readRDS("rain_query")

rain_socrata <- rain_socrata %>%
  rename("date_time" = "timestamp")

rain_socrata$date_time <- ymd_hms(rain_socrata$date_time)
rain_socrata <- rain_socrata %>%
  select(channel:rainfall)

rain_socrata_hourly <- rain_socrata %>%
  mutate(date_time = floor_date(date_time, "1 hour")) %>%
  group_by(channel, name, year, date_time) %>%
  summarize(rain = sum(rainfall))

## air temperature data ####
## weathercan package pulls Environment Canada data
library(weathercan)
calgary <- stations_search("Calgary") ## find desired station_id. Calgary Itn'l and Springbank
airtemp_query <- weather_dl(
  station_ids = c(27211, 52200), start = "2020-01-01",
  end = Sys.Date()
)
airtemp_query <- airtemp_query %>%
  rename("date_time" = "time") %>%
  rename("air_temp" = "temp")


## map data ####
library(sf)
locations <- read_csv(here("data", "shiny_map.csv")) %>%
  clean_names()

locations <- locations %>%
  st_as_sf(coords = c("easting", "northing")) %>%
  st_set_crs(4326)

locations %>%
  mapview(zcol = "type")

## joining dataframes ####
df_joined <- left_join(depth_temp, pi_data, by = "date_time")
df_joined <- left_join(df_joined, airtemp_query, by = "date_time")
df_joined <- left_join(df_joined, rain_socrata_hourly, by = "date_time")
df_joined <- df_joined %>%
  clean_names() %>%
  filter(date_time >= "2020-05-29") %>% 
  filter(temperature <= 100) #data cleaning - 42 erroneous temperature datum of 300-500deg

## scales
min_date <- min(df_joined$date_time)
max_date <- max(df_joined$date_time)

## summary table ####
## gt table
gt_depth <- depth_temp %>%
  filter(date_time >= "2020-05-29") %>%
  mutate(
    date_time = floor_date(date_time, "1 day"),
    month = month.abb[(month(date_time))]
  ) %>%
  group_by(month, depth) %>%
  summarize(
    min = min(temperature),
    mean = mean(temperature),
    max = max(temperature)
  ) %>%
  ungroup() %>%
  arrange(desc(month)) %>%
  gt() %>%
  tab_spanner(
    label = "temperature (°C)",
    columns = vars(min, mean, max)
  )

## gt table formatting
gt_depth <- gt_depth %>%
  data_color(
    columns = vars(depth),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::blue_material",
        n = 10
      )),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(depth)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )
gt_depth

## stratification plots ####
##source Dewey Dunnington
##https://fishandwhistle.net/post/2019/depth-time-heatmaps/
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- depth_temp %>%
    filter(date_time == target_date) %>%
    arrange(depth)

  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temperature, xout = target_depth)$y
}

estimate_temp_by_date(ymd("2020-07-13"), c(0, 1, 1.5, 2))

temp_interp_depth <- crossing(
  # the same dates as depth_temp
  tibble(date_time = unique(depth_temp$date_time)),
  # depths can now be any value
  tibble(depth = seq(1, 18, length.out = 100))
) %>%
  group_by(date_time) %>%
  mutate(temperature = estimate_temp_by_date(date_time[1], depth))

estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date_time)
  approx(data_for_depth$date_time, data_for_depth$temperature, xout = target_date)$y
}

estimate_temp_by_depth(
  target_depth = 1,
  target_date = seq(ymd_hms("2020-06-12 06:00:00"), ymd_hms("2020-06-15 06:00:00"), by = 1)
)

sonde_tbl_yearless <- depth_temp %>%
  mutate(
    day_in_year = as.numeric(date_time - floor_date(date_time, unit = "years"), unit = "days"),
    date_label = ymd("2020-01-01") + day_in_year
  )

smooth_fit <- loess(
  temperature ~ day_in_year + depth,
  data = sonde_tbl_yearless,
  span = 0.2
)

temp_raster_smooth <- crossing(
  tibble(date = seq(ymd("2020-05-30"), ymd("2020-07-30"), by = 1)),
  tibble(depth = seq(1, 18, length.out = 100))
) %>%
  mutate(
    day_in_year = as.numeric(date - floor_date(date, unit = "years"), unit = "days"),
    temperature = predict(
      smooth_fit,
      newdata = tibble(day_in_year = day_in_year, depth = depth)
    )
  )

#########################################
## shinydashboard ui ####

dashboard_sidebar <- dashboardSidebar(
  sidebarMenu(
    convertMenuItem(menuItem("Overview", tabName = "gm_overview", icon = icon("file-alt")),
      tabName = "gm_overview"
    ),
    convertMenuItem(menuItem("Data",
      tabName = "gm_data", icon = icon("database"),
      sliderInput("dates_merge",
        "Date Range:",
        min = as.Date(min_date, "%Y-%m-%d"),
        max = as.Date(max_date, "%Y-%m-%d"),
        value = c(
          as.Date(min_date, "%Y-%m-%d"),
          as.Date(max_date, "%Y-%m-%d")
        )
      ),
      selectInput(
        inputId = "v_depth",
        label = "Depth",
        selected = depth_temp$depth,
        selectize = FALSE,
        multiple = TRUE,
        choices = df_joined %>%
          select(depth) %>%
          distinct() %>%
          arrange(depth)
      ),
      selectInput(
        inputId = "v_pi",
        label = "Pi Tag",
        selected = "GM RWPS LAKE LVL (GEODECTIC)(m)",
        choices = df_joined %>%
          select(pi_tag) %>%
          distinct() %>%
          arrange(pi_tag) %>%
          drop_na()
      ),
      selectInput(
        inputId = "v_rain",
        label = "Rain gauge",
        selected = "Lincoln Park",
        choices = df_joined %>%
          select(name) %>%
          distinct() %>%
          arrange(name) %>%
          drop_na()
      ),
      selectInput(
        inputId = "v_air",
        label = "Air Temperature",
        choices = airtemp_query %>%
          select(station_name) %>%
          distinct() %>%
          arrange(station_name) %>%
          drop_na()
      )
    ),
    tabName = "gm_data"
    ),
    convertMenuItem(menuItem("Plot", tabName = "gm_plot", icon = icon("chart-bar")),
      tabName = "gm_plot"
    ),
    convertMenuItem(menuItem("Table", tabName = "gm_table", icon = icon("table")),
      tabName = "gm_table"
    ),
    convertMenuItem(menuItem("Map", tabName = "gm_map", icon = icon("map")),
      tabName = "gm_map"
    ),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    downloadButton('downloadReport')
  )
)



dashboard_body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "gm_overview",
      fluidRow(
        column(
          p("The purpose of the GM Temperature Project is to gain a more complete understanding
            of reservoir dynamics to better assist operational decisions for Water Treatment. 
            Water treatment processes can potentially be optimized by selecting from different 
            depths of the reservoir in response to varying water quality conditions or stratification processes."),
          div(),
          p("The temperature project is the first step in understanding if there are significant physio-chemical 
            differences throughout the reservoir profile.  Additional water quality parameters may be added in 
            the future based on this pilot project.  The use of in-situ monitoring sensors allows for a dense 
            temporal dataset with measurements being collected every hour.  This data will give a clearer insight 
            into daily to seasonal temperature fluxes throughout the raw water intake area of the reservoir."),
          br(),
          tags$img(src = "bob_buoy_4.jpg", width = "320px", height = "400px"),
          width = 8
        )
      )
    ),
    tabItem(
      tabName = "gm_data",
      fluidRow(
        box(
          plotlyOutput("reservoir_temp")
        ),
        box(
          plotlyOutput("pi_data")
        ),
        fluidRow(
          box(
            plotlyOutput("rain")
          ),
          box(plotlyOutput("air_temp"))
        )
      )
    ),
    tabItem(
      tabName = "gm_plot",
      fluidRow(
        box(
          plotOutput("stratification_point") %>%
            withSpinner()
        ),
        box(
          plotOutput("stratification_viridis") %>%
            withSpinner()
        )
      ),
      fluidRow(
        box(
          plotOutput("stratification") %>%
            withSpinner()
        ),
        box(
          plotOutput("stratification_smooth") %>%
            withSpinner()
        )
      )
    ),
    tabItem(
      tabName = "gm_table",
      gt_output(outputId = "depth_table")
    ),
    tabItem(
      tabName = "gm_map",
      mapviewOutput("calgarymap", width = "100%", height = 700)
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(
    title = "GM Temp Study",
    tags$li(img(
      src = "coc_logo.png",
      height = "50px"
    ),
    class = "dropdown"
    )
  ),
  dashboard_sidebar,
  dashboard_body
)

## server ####
server <- function(input, output) {

  ## reservoir temperature plot
  output$reservoir_temp <- renderPlotly({
    dates_merge <- as.Date(input$dates_merge, format = "%Y-%m-%d")
    depth_temp %>%
      filter(depth %in% input$v_depth) %>%
      subset(date_time >= dates_merge[1] & date_time <= dates_merge[2]) %>%
      ggplot(aes(x = date_time, y = temperature, group = depth)) +
      geom_line(aes(color = desc(depth))) +
      theme_bw() +
      labs(
        color = "Depth (m)",
        subtitle = "Glenmore Reservoir Temperature Study",
        x = "",
        y = "Water Temperature (°C)"
      ) +
      theme(
        legend.position = "none",
        axis.title.y = element_text(size = 7)
      )
  })
  ## Pi data
  output$pi_data <- renderPlotly({
    dates_merge <- as.Date(input$dates_merge, format = "%Y-%m-%d")
    df_joined %>%
      filter(pi_tag %in% input$v_pi) %>%
      subset(date_time >= dates_merge[1] & date_time <= dates_merge[2]) %>%
      distinct(date_time, .keep_all = T) %>%
      ggplot(aes(x = date_time, y = pi_value)) +
      geom_line() +
      theme_bw() +
      labs(
        x = "",
        y = {
          input$v_pi
        }
      ) +
      theme(axis.title.y = element_text(size = 7))
  })


  ## rain gauge plot
  output$rain <- renderPlotly({
    dates_merge <- as.Date(input$dates_merge, format = "%Y-%m-%d")
    df_joined %>%
      filter(name %in% input$v_rain) %>%
      subset(date_time >= dates_merge[1] & date_time <= dates_merge[2]) %>%
      distinct(date_time, .keep_all = T) %>%
      ggplot(aes(x = date_time, y = rain)) +
      geom_col(color = "steelblue3") +
      # lims(x = c(as.Date(input$dates_merge[1]), as.Date(input$dates_merge[2]))) +
      theme_bw() +
      labs(
        title = glue("Rain gauge: {input$v_rain}"),
        x = "",
        y = "Rain (mm)"
      ) +
      scale_y_reverse(limits = c(45, 0)) +
      theme(
        title = element_text(size = 7),
        axis.text.y = element_text(size = 7)
      )
  })

  # air temperature plot
  output$air_temp <- renderPlotly({
    dates_merge <- as.Date(input$dates_merge, format = "%Y-%m-%d")
    df_joined %>%
      subset(date_time >= dates_merge[1] & date_time <= dates_merge[2]) %>%
      mutate(air_day = floor_date(date(date_time), unit = "days")) %>%
      group_by(station_name, air_day) %>%
      summarize(min_air = min(air_temp, na.rm = T), max_air = max(air_temp, na.rm = T)) %>%
      ungroup() %>%
      filter(station_name %in% input$v_air) %>%
      ggplot() +
      geom_line(aes(x = air_day, y = max_air), color = "orange") +
      geom_line(aes(x = air_day, y = min_air), color = "lightblue") +
      scale_y_continuous(limits = c(0, 30)) +
      labs(
        title = glue("{input$v_air}"),
        y = "Air Temperature (°C)",
        x = ""
      ) +
      theme_bw() +
      theme(
        title = element_text(size = 7),
        axis.text.y = element_text(size = 7)
      )
  })


  ## mapview
  output$calgarymap <- renderMapview({
    mapview::mapview(locations,
      zcol = "type",
      layer.name = ""
    )
  })

  output$depth_table <- render_gt(
    expr = gt_depth
    # height = px(600),
    # width = px(600)
  )

  output$stratification_point <- renderPlot(
    ggplot(depth_temp, aes(x = date_time, y = depth, color = temperature)) +
      geom_point() +
      scale_y_reverse() +
      scale_color_viridis(name = "Temp (\u00B0C)") +
      # scale_colour_gradient2(
      #   midpoint = 15,
      #   high = scales::muted("red"),
      #   low = scales::muted("blue"),
      #   name = "Temp (\u00B0C)"
      theme_bw() +
      labs(title = "Temperature Profile", subtitle = "Discrete point data", x = "", y = "Depth (m)") +
      coord_cartesian(expand = FALSE)
  )

  output$stratification_viridis <- renderPlot(
    ggplot(temp_interp_depth, aes(x = date_time, y = depth, color = temperature)) +
      geom_point() +
      scale_y_reverse() +
      scale_color_viridis(name = "Temp (\u00B0C)") +
      labs(title = "Temperature Profile", subtitle = "Depth linear interpolation", x = "", y = "Depth (m)") +
      coord_cartesian(expand = FALSE)
  )

  output$stratification <- renderPlot(
    ggplot(temp_interp_depth, aes(x = date_time, y = depth, color = temperature)) +
      geom_point() +
      scale_y_reverse() +
      scale_colour_gradient2(
        midpoint = 15,
        high = scales::muted("red"),
        low = scales::muted("blue"),
        name = "Temp (\u00B0C)"
      ) +
      labs(title = "Temperature Profile", subtitle = "Depth linear interpolation", x = "", y = "Depth (m)") +
      coord_cartesian(expand = FALSE)
  )

  output$stratification_smooth <- renderPlot(
    ggplot(temp_raster_smooth, aes(x = date, y = depth, fill = temperature)) +
      geom_raster() +
      scale_y_reverse() +
      scale_fill_gradient2(
        midpoint = 15,
        high = scales::muted("red"),
        low = scales::muted("blue"),
        name = "Temp (\u00B0C)"
      ) +
      labs(
        title = "Temperature Profile",
        subtitle = "2D smoothed -  Depth linear interpolation",
        x = "",
        y = "Depth (m)",
        caption = "loess span = 0.2"
      ) +
      coord_cartesian(expand = FALSE)
  )
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "shiny_report_draft.Rmd")
      file.copy("shiny_report_draft.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
