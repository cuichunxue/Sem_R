# =============================================================================
# lavaan SEM Analysis Web Application
# 構造方程式モデリング解析Webアプリケーション
# =============================================================================

# --- パッケージ読み込み ---
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
  library(lavaan)
  library(semPlot)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(haven)
  library(corrplot)
  library(knitr)
  library(kableExtra)
  library(htmltools)
  library(waiter)
})

# --- ソースファイル読み込み ---
source("R/utils.R")
source("R/ui_modules.R")
source("R/server_modules.R")

# --- カスタムテーマ ---
app_theme <- bs_theme(

  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  secondary = "#95a5a6",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Noto Sans JP"),
  heading_font = font_google("Noto Sans JP"),
  code_font = font_google("Source Code Pro"),
  "navbar-bg" = "#2c3e50",
  "body-bg" = "#ecf0f1",
  "card-bg" = "#ffffff"
)

# --- UI定義 ---
ui <- page_navbar(
  title = tags$span(
    tags$i(class = "fas fa-project-diagram me-2"),
    "SEM Analysis Tool"
  ),
  id = "main_nav",
  theme = app_theme,
  fillable = TRUE,

  # Head要素
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML(custom_css())),
    useShinyjs(),
    useWaiter()
  ),

  # --- データタブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-database me-1"), "データ"),
    value = "data_tab",
    data_ui("data")
  ),

  # --- モデル定義タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-code me-1"), "モデル定義"),
    value = "model_tab",
    model_ui("model")
  ),

  # --- 推定設定タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-cogs me-1"), "推定設定"),
    value = "estimation_tab",
    estimation_ui("estimation")
  ),

  # --- 結果タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-chart-bar me-1"), "結果"),
    value = "results_tab",
    results_ui("results")
  ),

  # --- パス図タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-diagram-project me-1"), "パス図"),
    value = "diagram_tab",
    diagram_ui("diagram")
  ),

  # --- モデル比較タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-balance-scale me-1"), "モデル比較"),
    value = "comparison_tab",
    comparison_ui("comparison")
  ),

  # --- ヘルプタブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-question-circle me-1"), "ヘルプ"),
    value = "help_tab",
    help_ui("help")
  ),

  # ナビゲーション右側
  nav_spacer(),
  nav_item(
    tags$span(
      class = "navbar-text text-light",
      tags$small("lavaan ", packageVersion("lavaan"))
    )
  )
)

# --- サーバー定義 ---
server <- function(input, output, session) {

  # --- リアクティブ値 ---
  rv <- reactiveValues(
    data = NULL,
    data_name = NULL,
    model_syntax = NULL,
    fit = NULL,
    fit_summary = NULL,
    estimation_complete = FALSE,
    error_message = NULL,
    saved_models = list()
  )

  # --- モジュールサーバー呼び出し ---
  data_result <- data_server("data", rv)
  model_result <- model_server("model", rv)
  estimation_result <- estimation_server("estimation", rv)
  results_server("results", rv)
  diagram_server("diagram", rv)
  comparison_server("comparison", rv)
  help_server("help")

  # --- グローバルエラーハンドリング ---
  observe({
    if (!is.null(rv$error_message)) {
      showNotification(
        rv$error_message,
        type = "error",
        duration = 8
      )
      rv$error_message <- NULL
    }
  })
}

# --- アプリケーション起動 ---
shinyApp(ui = ui, server = server)
