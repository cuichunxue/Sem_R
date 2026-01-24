#!/usr/bin/env Rscript
# =============================================================================
# SEM Analysis Web App - 起動スクリプト
# =============================================================================

# 作業ディレクトリを設定
if (!interactive()) {
  setwd(dirname(sys.frame(1)$ofile))
}

# アプリを起動
cat("Starting SEM Analysis Web App...\n")
cat("Access the app at: http://127.0.0.1:3838\n\n")

shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)
