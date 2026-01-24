# =============================================================================
# SEM Analysis Web App - パッケージインストールスクリプト
# =============================================================================

# 必要なパッケージのリスト
required_packages <- c(
  # Shiny関連
  "shiny",
  "bslib",
  "shinyWidgets",
  "shinyjs",
  "waiter",

  # データ処理
  "DT",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "haven",

  # SEM解析
  "lavaan",
  "semPlot",

  # 可視化
  "ggplot2",
  "corrplot",
  "colourpicker",

  # レポート生成
  "knitr",
  "kableExtra",
  "htmltools"
)

# インストール関数
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

  if (length(new_packages) > 0) {
    cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# インストール実行
cat("=== SEM Analysis Web App Package Installer ===\n\n")
cat("Checking and installing required packages...\n\n")

install_if_missing(required_packages)

# バージョン確認
cat("\n=== Installed Package Versions ===\n")
for (pkg in required_packages) {
  if (pkg %in% installed.packages()[, "Package"]) {
    version <- packageVersion(pkg)
    cat(sprintf("  %-15s : %s\n", pkg, version))
  } else {
    cat(sprintf("  %-15s : NOT INSTALLED\n", pkg))
  }
}

cat("\n=== Installation Complete ===\n")
cat("You can now run the app with: shiny::runApp()\n")
