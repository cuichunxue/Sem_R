# =============================================================================
# lavaan SEM Analysis Web Application
# 構造方程式モデリング解析Webアプリケーション
# Production Version 1.0
# =============================================================================

# --- 設定 ---
options(
  shiny.maxRequestSize = 50 * 1024^2,  # 最大50MBのファイルアップロード
  shiny.sanitize.errors = FALSE,        # デバッグ中: FALSE / 本番: TRUE に変更
  warn = 1                               # 警告を即座に表示
)
# digits と scipen は global.R で設定済み

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
  library(colourpicker)
  library(htmltools)
  library(waiter)
  library(jsonlite)
})

# --- ソースファイル読み込み ---
source("R/utils.R")
source("R/ui_modules.R")
source("R/server_modules.R")

# --- アプリケーション設定 ---
APP_CONFIG <- list(
  name = "SEM Analysis Tool",
  version = "1.0.0",
  max_variables = 200,        # 最大変数数

  max_observations = 100000,  # 最大観測数
  session_timeout = 30,       # セッションタイムアウト（分）
  enable_logging = TRUE
)

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
    APP_CONFIG$name
  ),
  id = "main_nav",
  theme = app_theme,
  fillable = TRUE,

  # Head要素
  header = tagList(
    # スキップリンク（アクセシビリティ）
    tags$a(
      href = "#main-content",
      class = "skip-link",
      "メインコンテンツへスキップ"
    ),

    tags$head(
      # メタタグ
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$meta(name = "description", content = "構造方程式モデリング(SEM)解析ツール - lavaan"),
      tags$meta(name = "robots", content = "noindex, nofollow"),
      tags$meta(name = "theme-color", content = "#2c3e50"),

      # ファビコン
      tags$link(rel = "icon", href = "data:image/x-icon;,"),

      # Font Awesome
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
        crossorigin = "anonymous"
      ),

      # カスタムCSS
      tags$style(HTML(custom_css())),
      tags$link(rel = "stylesheet", href = "custom.css"),

      # JavaScript
      tags$script(HTML(sprintf("
      // アプリケーション設定
      var APP_CONFIG = %s;

      // クリップボードコピー
      Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function() {
            console.log('Copied to clipboard');
          }).catch(function(err) {
            fallbackCopy(text);
          });
        } else {
          fallbackCopy(text);
        }
      });

      function fallbackCopy(text) {
        var textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.style.position = 'fixed';
        textarea.style.opacity = '0';
        document.body.appendChild(textarea);
        textarea.select();
        try {
          document.execCommand('copy');
        } catch (err) {
          console.error('Copy failed:', err);
        }
        document.body.removeChild(textarea);
      }

      // キーボードショートカット
      document.addEventListener('keydown', function(e) {
        // Ctrl+Enter: 分析実行
        if (e.ctrlKey && e.key === 'Enter') {
          var runBtn = document.querySelector('#estimation-run_analysis');
          if (runBtn) runBtn.click();
        }
        // Ctrl+G: 構文生成
        if (e.ctrlKey && e.key === 'g') {
          e.preventDefault();
          var genBtn = document.querySelector('#model-generate_syntax');
          if (genBtn) genBtn.click();
        }
      });

      // セッション監視
      var lastActivity = Date.now();
      document.addEventListener('mousemove', function() { lastActivity = Date.now(); });
      document.addEventListener('keypress', function() { lastActivity = Date.now(); });

      setInterval(function() {
        var inactive = (Date.now() - lastActivity) / 1000 / 60;
        if (inactive > APP_CONFIG.session_timeout) {
          Shiny.setInputValue('session_timeout', true, {priority: 'event'});
        }
      }, 60000);

      // ページ離脱警告
      window.addEventListener('beforeunload', function(e) {
        if (Shiny.shinyapp.$inputValues['data-data_loaded']) {
          e.preventDefault();
          e.returnValue = '';
        }
      });

      // エラーハンドリング
      window.onerror = function(msg, url, lineNo, columnNo, error) {
        console.error('Error:', msg, 'at', url, lineNo);
        return false;
      };

      // アクセシビリティ: ライブリージョン通知
      window.announceToSR = function(message) {
        var region = document.getElementById('sr-announcements');
        if (region) {
          region.textContent = message;
          setTimeout(function() { region.textContent = ''; }, 1000);
        }
      };

      // 分析完了時のスクリーンリーダー通知
      Shiny.addCustomMessageHandler('announceMessage', function(message) {
        announceToSR(message);
      });
    ", jsonlite::toJSON(APP_CONFIG, auto_unbox = TRUE)))),

      # タブ遷移ハンドラ（別scriptブロックでsprintfの影響を排除）
      tags$script(HTML("
        Shiny.addCustomMessageHandler('navigateTab', function(tabValue) {
          Shiny.setInputValue('main_nav', tabValue, {priority: 'event'});
          var links = document.querySelectorAll('[data-value]');
          for (var i = 0; i < links.length; i++) {
            if (links[i].getAttribute('data-value') === tabValue) {
              links[i].click();
              break;
            }
          }
        });
      ")),

      useShinyjs(),
      useWaiter(),

      # スクリーンリーダー用ライブリージョン
      tags$div(
        id = "sr-announcements",
        class = "sr-only",
        `aria-live` = "polite",
        `aria-atomic` = "true"
      )
    )
  ),

  # フッター
  footer = tags$footer(
    class = "bg-dark text-light py-2 mt-auto",
    style = "font-size: 0.8rem;",
    div(
      class = "container-fluid d-flex justify-content-between",
      tags$span(
        paste0(APP_CONFIG$name, " v", APP_CONFIG$version),
        " | Powered by ",
        tags$a(href = "https://lavaan.ugent.be/", target = "_blank", class = "text-info", "lavaan"),
        " ", as.character(packageVersion("lavaan"))
      ),
      tags$span(
        tags$kbd("Ctrl+Enter"), " 分析実行 | ",
        tags$kbd("Ctrl+G"), " 構文生成"
      )
    )
  ),

  # --- データタブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-database me-1", `aria-hidden` = "true"), "データ"),
    value = "data_tab",
    tags$main(
      id = "main-content",
      role = "main",
      `aria-label` = "メインコンテンツ",
      data_ui("data")
    )
  ),

  # --- モデル定義タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-code me-1", `aria-hidden` = "true"), "モデル定義"),
    value = "model_tab",
    model_ui("model")
  ),

  # --- 推定設定タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-cogs me-1", `aria-hidden` = "true"), "推定設定"),
    value = "estimation_tab",
    estimation_ui("estimation")
  ),

  # --- 結果タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-chart-bar me-1", `aria-hidden` = "true"), "結果"),
    value = "results_tab",
    results_ui("results")
  ),

  # --- パス図タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-diagram-project me-1", `aria-hidden` = "true"), "パス図"),
    value = "diagram_tab",
    diagram_ui("diagram")
  ),

  # --- モデル比較タブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-balance-scale me-1", `aria-hidden` = "true"), "モデル比較"),
    value = "comparison_tab",
    comparison_ui("comparison")
  ),

  # --- ヘルプタブ ---
  nav_panel(
    title = tags$span(tags$i(class = "fas fa-question-circle me-1", `aria-hidden` = "true"), "ヘルプ"),
    value = "help_tab",
    help_ui("help")
  ),

  # ナビゲーション右側
  nav_spacer(),
  nav_item(
    tags$span(
      class = "navbar-text text-light",
      id = "session_status",
      tags$i(class = "fas fa-circle text-success me-1", style = "font-size: 0.6rem;"),
      tags$small("接続中")
    )
  )
)

# --- サーバー定義 ---
server <- function(input, output, session) {

  # --- ログ関数 ---
  log_event <- function(event, details = NULL) {
    if (APP_CONFIG$enable_logging) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      msg <- paste0("[", timestamp, "] ", event)
      if (!is.null(details)) {
        msg <- paste0(msg, " - ", details)
      }
      message(msg)
    }
  }

  log_event("Session started", session$token)

  # --- リアクティブ値 ---
  rv <- reactiveValues(
    data = NULL,
    data_name = NULL,
    model_syntax = NULL,
    fit = NULL,
    fit_summary = NULL,
    estimation_complete = FALSE,
    error_message = NULL,
    error_help = NULL,
    saved_models = list(),
    navigate_to = NULL,
    session_start = Sys.time()
  )

  # --- セッションタイムアウト処理 ---
  observeEvent(input$session_timeout, {
    showModal(modalDialog(
      title = tags$span(tags$i(class = "fas fa-clock me-2"), "セッションタイムアウト"),
      tags$p("長時間操作がなかったため、セッションがタイムアウトしました。"),
      tags$p("ページを再読み込みして続行してください。"),
      footer = actionButton("reload_page", "再読み込み", class = "btn-primary",
                           onclick = "location.reload();"),
      easyClose = FALSE
    ))
  })

  # --- セッション終了時のクリーンアップ ---
  session$onSessionEnded(function() {
    log_event("Session ended", session$token)
    # メモリ解放
    gc()
  })

  # --- モジュールサーバー呼び出し ---
  data_result <- data_server("data", rv)
  model_result <- model_server("model", rv)
  estimation_result <- estimation_server("estimation", rv)
  results_server("results", rv)
  diagram_server("diagram", rv)
  comparison_server("comparison", rv)
  help_server("help")

  # --- データサイズチェック ---
  observe({
    req(rv$data)

    n_obs <- nrow(rv$data)
    n_vars <- ncol(rv$data)

    if (n_obs > APP_CONFIG$max_observations) {
      showNotification(
        paste0("データの観測数(", format(n_obs, big.mark = ","),
               ")が上限(", format(APP_CONFIG$max_observations, big.mark = ","),
               ")を超えています。処理が遅くなる可能性があります。"),
        type = "warning",
        duration = 10
      )
    }

    if (n_vars > APP_CONFIG$max_variables) {
      showNotification(
        paste0("変数数(", n_vars, ")が上限(", APP_CONFIG$max_variables,
               ")を超えています。変数を選択してください。"),
        type = "warning",
        duration = 10
      )
    }

    log_event("Data loaded", paste0(n_obs, " obs x ", n_vars, " vars"))
  })

  # --- グローバルエラーハンドリング ---
  observe({
    if (!is.null(rv$error_message) && rv$error_message != "") {
      log_event("Error", rv$error_message)
    }
  })

  # --- タブ遷移（モジュール内からの要求を親セッションで実行）---
  observeEvent(rv$navigate_to, {
    req(rv$navigate_to)
    session$sendCustomMessage("navigateTab", rv$navigate_to)
    rv$navigate_to <- NULL
  })

  # --- 分析完了時のスクリーンリーダー通知 ---
  observe({
    if (isTRUE(rv$estimation_complete) && !is.null(rv$fit)) {
      fm <- tryCatch(
        fitMeasures(rv$fit, c("cfi", "rmsea")),
        error = function(e) c(cfi = NA, rmsea = NA)
      )
      msg <- sprintf(
        "分析が完了しました。CFI: %.3f, RMSEA: %.3f",
        fm["cfi"], fm["rmsea"]
      )
      session$sendCustomMessage("announceMessage", msg)
    }
  })
}

# --- アプリケーション起動 ---
shinyApp(
  ui = ui,
  server = server,
  options = list(
    launch.browser = FALSE,
    host = "0.0.0.0",
    port = 3838
  )
)
