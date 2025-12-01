# app.R -----------------------------------------------------------
library(shiny)

# 数据处理 & 画图相关包
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(usmap)
library(FactoMineR)
library(factoextra)
library(usdata)

#--------------------------
# 0. 读入 & 清洗数据（你的原始代码）
#--------------------------
df <- read_csv("pollution_2000_2023.csv") %>%
  clean_names()

df <- df %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date, label = TRUE)
  )

df <- df %>%
  filter(
    o3_mean  >= 0,
    no2_mean >= 0,
    so2_mean >= 0,
    co_mean  >= 0
  )

df <- df %>%
  mutate(
    O3_valid  = o3_mean  != 0,
    NO2_valid = no2_mean != 0
  )

coverage_days <- df %>%
  group_by(state, year) %>%
  summarise(
    O3_days  = sum(O3_valid,  na.rm = TRUE),
    NO2_days = sum(NO2_valid, na.rm = TRUE),
    .groups = "drop"
  )

coverage_days <- coverage_days %>%
  mutate(
    O3_full  = O3_days  >= 250,
    NO2_full = NO2_days >= 250
  )

state_full_years <- coverage_days %>%
  group_by(state) %>%
  summarise(
    O3_years  = sum(O3_full),
    NO2_years = sum(NO2_full),
    .groups = "drop"
  )

good_states <- state_full_years %>%
  filter(O3_years >= 10, NO2_years >= 10) %>%
  pull(state)

df_clean <- df %>%
  filter(state %in% good_states) %>%
  filter(year != 2023)   # 去掉数据不完整的 2023

# 公用颜色
cols <- c(
  "CO"  = "#E64B35FF",
  "NO2" = "#4DBBD5FF",
  "O3"  = "#00A087FF",
  "SO2" = "#3C5488FF"
)

#--------------------------
# 1. 时间趋势：年度 + 月度 AQI
#--------------------------
yearly_aqi <- df_clean %>%
  group_by(year) %>%
  summarise(
    o3_aqi  = mean(o3_aqi,  na.rm = TRUE),
    co_aqi  = mean(co_aqi,  na.rm = TRUE),
    so2_aqi = mean(so2_aqi, na.rm = TRUE),
    no2_aqi = mean(no2_aqi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(o3_aqi, co_aqi, so2_aqi, no2_aqi),
    names_to  = "pollutant",
    values_to = "aqi"
  ) %>%
  mutate(
    pollutant = recode(pollutant,
                       o3_aqi  = "O3",
                       co_aqi  = "CO",
                       so2_aqi = "SO2",
                       no2_aqi = "NO2")
  )

monthly_aqi <- df_clean %>%
  mutate(mon = month(as.Date(date))) %>%
  group_by(mon) %>%
  summarise(
    o3_aqi  = mean(o3_aqi,  na.rm = TRUE),
    co_aqi  = mean(co_aqi,  na.rm = TRUE),
    so2_aqi = mean(so2_aqi, na.rm = TRUE),
    no2_aqi = mean(no2_aqi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = c(o3_aqi, co_aqi, so2_aqi, no2_aqi),
    names_to  = "pollutant",
    values_to = "aqi"
  ) %>%
  mutate(
    pollutant = recode(pollutant,
                       o3_aqi  = "O3",
                       co_aqi  = "CO",
                       so2_aqi = "SO2",
                       no2_aqi = "NO2")
  )

yearly_long <- yearly_aqi %>%
  transmute(
    panel     = "Yearly trend (2000–2022)",
    x         = factor(year),
    aqi       = aqi,
    pollutant = pollutant
  )

monthly_long <- monthly_aqi %>%
  transmute(
    panel     = "Seasonal pattern (monthly)",
    x         = factor(mon, levels = 1:12,
                       labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec")),
    aqi       = aqi,
    pollutant = pollutant
  )

trend_all <- bind_rows(yearly_long, monthly_long)

trend_plot <- ggplot(trend_all,
                     aes(x = x, y = aqi, color = pollutant, group = pollutant)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_color_manual(values = cols, name = "Pollutant") +
  facet_wrap(~ panel, nrow = 1, scales = "free_x") +
  scale_x_discrete(
    breaks = function(x) {
      if (all(x %in% c("Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"))) {
        c("Jan","Apr","Jul","Oct")
      } else {
        c("2000","2005","2010","2015","2020")
      }
    }
  ) +
  labs(x = NULL, y = "Average AQI") +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 12, face = "bold"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

# heatmap for four pollutants
monthly_heat_all <- df_clean %>%
  group_by(year, month) %>%
  summarise(
    O3  = mean(o3_aqi, na.rm = TRUE),
    NO2 = mean(no2_aqi, na.rm = TRUE),
    SO2 = mean(so2_aqi, na.rm = TRUE),
    CO  = mean(co_aqi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month = month.abb[as.integer(month)])

#--------------------------
# 2. Spatial：state map + region boxplot
#--------------------------
state_lookup <- data.frame(
  state = state.name,
  abbr  = state.abb,
  stringsAsFactors = FALSE
) |>
  bind_rows(data.frame(state = "District Of Columbia", abbr = "DC"))

# recent 10 years 平均 AQI
state_pollutants <- df_clean %>%
  filter(year >= 2010) %>%   
  group_by(state) %>%
  summarise(
    o3_aqi  = mean(o3_aqi,  na.rm = TRUE),
    no2_aqi = mean(no2_aqi, na.rm = TRUE),
    so2_aqi = mean(so2_aqi, na.rm = TRUE),
    co_aqi  = mean(co_aqi,  na.rm = TRUE),
    .groups = "drop"
  )

# 用于 region boxplot
region_lookup <- data.frame(
  state = state.name,
  region = state.region,
  stringsAsFactors = FALSE
)

state_pollutants_region <- state_pollutants %>%
  left_join(region_lookup, by = "state")

state_pollutants_long <- state_pollutants_region %>%
  pivot_longer(
    cols = c(o3_aqi, co_aqi, so2_aqi, no2_aqi),
    names_to = "pollutant",
    values_to = "aqi_value"
  ) %>%
  mutate(
    pollutant = recode(
      pollutant,
      "o3_aqi"  = "O3",
      "co_aqi"  = "CO",
      "so2_aqi" = "SO2",
      "no2_aqi" = "NO2"
    )
  )

make_region_boxplot <- function(poll) {
  ggplot(
    state_pollutants_long %>% filter(pollutant == poll),
    aes(x = region, y = aqi_value, fill = region)
  ) +
    geom_boxplot() +
    labs(
      title = paste("Regional Distribution of", poll, "(2010–2022)"),
      x = "Region",
      y = paste(poll, "AQI")
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}


# usmap 按 AQI，给一个函数根据 pollutant 画图
make_usmap <- function(poll) {
  value_col <- paste0(tolower(poll), "_aqi")
  plot_usmap(data = state_pollutants,
             regions = "states",
             values  = value_col) +
    scale_fill_continuous(
      name  = paste("Avg", poll, "(2010–2022)"),
      label = scales::label_number(accuracy = 0.1)
    ) +
    labs(title = paste("Average", poll, "Levels by State (2010–2022)")) +
    theme(legend.position = "right")
}

#--------------------------
# 3. Pollutant correlation
#--------------------------
corr_vars <- df_clean %>%
  select(o3_aqi, co_aqi, so2_aqi, no2_aqi)

corr_mat <- cor(corr_vars, use = "pairwise.complete.obs",
                method = "spearman")

corr_df <- as.data.frame(corr_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr")

corr_plot <- ggplot(corr_df, aes(x = var1, y = var2, fill = corr)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = "brown",
                       mid = "white",
                       high = "blue",
                       midpoint = 0) +
  geom_text(aes(label = sprintf("%.2f", corr)), size = 3) +
  labs(
    title = "Spearman Correlation among Pollutants (AQI)",
    x = "", y = "", fill = "ρ"
  ) +
  theme_minimal(base_size = 12)

#--------------------------
# 4. PCA maps（PC1 / PC2）
#--------------------------
state_pollutants_AQI <- df_clean %>%
  group_by(state) %>%
  summarise(
    o3_aqi  = mean(o3_aqi,  na.rm = TRUE),
    no2_aqi = mean(no2_aqi, na.rm = TRUE),
    so2_aqi = mean(so2_aqi, na.rm = TRUE),
    co_aqi  = mean(co_aqi,  na.rm = TRUE),
    .groups = "drop"
  )

pca_data <- state_pollutants_AQI %>%
  select(o3_aqi, no2_aqi, so2_aqi, co_aqi)

state_pollutants_AQI$abbr <- state2abbr(state_pollutants_AQI$state)
row.names(pca_data) <- state_pollutants_AQI$abbr

pca <- prcomp(pca_data, scale. = TRUE)
pca_scores <- as.data.frame(pca$x) %>%
  mutate(state = state_pollutants_AQI$state)

pca_map <- function(pc_col, title, subtitle) {
  plot_usmap(
    data    = pca_scores,
    values  = pc_col,
    regions = "states"
  ) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, name = paste(pc_col, "Score")
    ) +
    labs(title = title, subtitle = subtitle) +
    theme(legend.position = "right")
}

pc1_plot <- pca_map(
  "PC1",
  "PC1: Overall Pollution Burden (AQI-based)",
  "High = more polluted overall | Low = cleaner states"
)

pc2_plot <- pca_map(
  "PC2",
  "PC2: Pollution Composition (Ozone vs NO2/CO/SO2)",
  "High = ozone-dominated | Low = combustion-dominated"
)

#--------------------------
# 5. STL decomposition (O3)
#--------------------------

pollutant_list <- c("o3_aqi", "no2_aqi", "so2_aqi", "co_aqi")

ts_list <- list()

for (poll in pollutant_list) {
  temp <- df_clean %>%
    group_by(date) %>%
    summarise(value = mean(.data[[poll]], na.rm = TRUE)) %>%
    arrange(date)
  
  ts_list[[poll]] <- ts(temp$value, frequency = 365)
}

make_stl_plot <- function(ts_obj, poll_label) {
  stl_res <- stl(ts_obj, s.window = "periodic")
  
  plot(stl_res, main = paste("STL Decomposition -", poll_label))
}


#--------------------------
# 6. （可选）O3 动态 plotly map – 直接用你之前的代码
#--------------------------
state_year_long <- df_clean %>%
  group_by(state, year) %>%
  summarise(
    o3  = mean(o3_mean,  na.rm = TRUE),
    no2 = mean(no2_mean, na.rm = TRUE),
    so2 = mean(so2_mean, na.rm = TRUE),
    co  = mean(co_mean,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(o3, no2, so2, co),
    names_to = "pollutant",
    values_to = "value"
  ) %>%
  left_join(state_lookup, by = "state")

all_years <- sort(unique(state_year_long$year))
all_pollutants <- c("no2", "o3", "so2", "co")

complete_grid <- expand.grid(
  year = all_years,
  state = state_lookup$state,
  pollutant = all_pollutants,
  stringsAsFactors = FALSE
) %>%
  left_join(state_lookup, by = "state")

state_year_complete <- complete_grid %>%
  left_join(
    state_year_long %>% select(year, state, pollutant, value), 
    by = c("year", "state", "pollutant")
  )

pollutant_ranges <- state_year_complete %>%
  group_by(pollutant) %>%
  summarise(
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE),
    .groups = "drop"
  )

data_o3 <- state_year_complete %>% 
  filter(pollutant == "o3") %>%
  arrange(year, abbr)

fig_o3 <- plot_ly(
  data = data_o3,
  type = "choropleth",
  locations = ~abbr,
  locationmode = "USA-states",
  z = ~value,
  frame = ~year,
  text = ~paste0(
    state, "<br>",
    "Year: ", year, "<br>",
    "O\u2083: ", ifelse(is.na(value), "No data", round(value, 3))
  ),
  hoverinfo = "text",
  colorscale = "Reds",
  showscale = TRUE,
  zmin = pollutant_ranges$min_val[pollutant_ranges$pollutant == "o3"],
  zmax = pollutant_ranges$max_val[pollutant_ranges$pollutant == "o3"]
) %>%
  layout(
    title = "Annual O\u2083 Pollution Levels by State (animated)",
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showlakes = TRUE,
      lakecolor = toRGB("white")
    )
  )

make_animated_map <- function(poll) {
  
  dat <- state_year_complete %>%
    filter(pollutant == poll) %>%
    arrange(year, abbr)
  
  plot_ly(
    data = dat,
    type = "choropleth",
    locations = ~abbr,
    locationmode = "USA-states",
    z = ~value,
    frame = ~year,
    text = ~paste0(
      state, "<br>",
      "Year: ", year, "<br>",
      toupper(poll), ": ", ifelse(is.na(value), "No data", round(value, 3))
    ),
    hoverinfo = "text",
    colorscale = "Reds",
    showscale = TRUE,
    zmin = pollutant_ranges$min_val[pollutant_ranges$pollutant == poll],
    zmax = pollutant_ranges$max_val[pollutant_ranges$pollutant == poll]
  ) %>%
    layout(
      title = paste("Annual", toupper(poll), "Pollution Levels by State (animated)"),
      geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        showlakes = TRUE,
        lakecolor = toRGB("white")
      )
    )
}

#==========================
# UI
#==========================
ui <- navbarPage(
  "U.S. Air Quality Dashboard (2000–2022)",
  
  tabPanel(
    "Time Trends",
    sidebarLayout(
      sidebarPanel(
        helpText("Overall trends & seasonality of AQI for four pollutants."),
        selectInput(
          "heat_poll",
          "Heatmap pollutant:",
          choices = c("O3", "NO2", "SO2", "CO"),
          selected = "O3"
        )
      ),
      mainPanel(
        plotOutput("trendPlot", height = "300px"),   # ← 上图（季节+趋势）
        br(),
        plotOutput("heatmapPlot", height = "350px")  # ← 下图（heatmap）
      )
    )
  ),
  
  tabPanel(
    "Spatial Patterns",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "map_pollutant",
          "Choose pollutant:",
          choices = c("O3","NO2","SO2","CO"),
          selected = "O3"
        ),
        helpText("Top: State-level average AQI map (2010–2022)."),
        helpText("Bottom: Regional distribution for the same pollutant.")
      ),
      mainPanel(
        plotOutput("stateMap", height = "350px"),
        br(),
        plotOutput("regionBox", height = "350px")
      )
    )
  ),
  
  tabPanel(
    "Animated Map",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "ani_pollutant",
          "Choose pollutant:",
          choices = c("O3", "NO2", "SO2", "CO"),
          selected = "O3"
        ),
        helpText("Animated annual AQI map for selected pollutant.")
      ),
      mainPanel(
        plotlyOutput("animatedMap", height = "550px")
      )
    )
  ),
  
  tabPanel(
    "Correlation",
    fluidPage(
      br(),
      plotOutput("corrPlot", height = "450px")
    )
  ),
  
  tabPanel(
    "PCA Overview",
    fluidPage(
      br(),
      plotOutput("pcaMaps", height = "450px")
    )
  ),
  
  tabPanel(
    "STL Decomposition",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "stl_poll",
          "Choose pollutant:",
          choices = c("O3"="o3_aqi", "NO2"="no2_aqi", 
                      "SO2"="so2_aqi", "CO"="co_aqi"),
          selected = "o3_aqi"
        )
      ),
      mainPanel(
        plotOutput("stlPlot", height = "650px")
      )
    )
  )
)

#==========================
# Server
#==========================
server <- function(input, output, session) {
  
  # 时间趋势
  output$trendPlot <- renderPlot({
    trend_plot
  })
  
  output$heatmapPlot <- renderPlot({
    req(input$heat_poll)   # 确保选择器有值
    
    ggplot(
      monthly_heat_all,
      aes(x = month, y = year, fill = .data[[input$heat_poll]])
    ) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(
        title = paste("Seasonal Heatmap of", input$heat_poll, "AQI (2000–2022)"),
        x = "Month",
        y = "Year",
        fill = paste(input$heat_poll, "AQI")
      ) +
      theme_minimal()
  })
  
  
  # 空间分布
  output$stateMap <- renderPlot({
    make_usmap(input$map_pollutant)
  })
  
  output$regionBox <- renderPlot({
    make_region_boxplot(input$map_pollutant)
  })
  
  # O3 动态地图
  output$animatedMap <- renderPlotly({
    req(input$ani_pollutant)
    
    poll <- tolower(input$ani_pollutant)   # "O3" → "o3"
    
    make_animated_map(poll)
  })
  
  # 相关性
  output$corrPlot <- renderPlot({
    corr_plot
  })
  
  # PCA
  output$pcaMaps <- renderPlot({
    pc1_plot | pc2_plot
  })
  
  # STL
  output$stlPlot <- renderPlot({
    req(input$stl_poll)
    
    ts_obj <- ts_list[[input$stl_poll]]
    
    poll_label <- toupper(gsub("_aqi", "", input$stl_poll))
    
    make_stl_plot(ts_obj, poll_label)
  })
}

# 运行 app
shinyApp(ui, server)
