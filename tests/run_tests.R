#!/usr/bin/env Rscript
# =============================================================================
# SEM Analysis Web App - テスト実行スクリプト
# =============================================================================

# 必要なパッケージ
required_packages <- c("testthat", "lavaan", "dplyr")

# パッケージインストール
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# 作業ディレクトリを設定
if (!interactive()) {
  script_dir <- dirname(sys.frame(1)$ofile)
  setwd(script_dir)
}

cat("\n")
cat("=============================================================================\n")
cat("  SEM Analysis Web App - Test Suite\n")
cat("=============================================================================\n")
cat("\n")

# ソースファイルを読み込み
cat("Loading source files...\n")
source("../R/utils.R")

# テスト実行
cat("\nRunning tests...\n\n")

# testthatでテスト実行
test_results <- testthat::test_dir(
  "testthat",
  reporter = testthat::SummaryReporter$new()
)

# 結果サマリー
cat("\n")
cat("=============================================================================\n")
cat("  Test Results Summary\n")
cat("=============================================================================\n")

# 結果を集計
n_tests <- length(test_results)
n_passed <- sum(sapply(test_results, function(x) {
  if (is.null(x$results)) return(0)
  sum(sapply(x$results, function(r) inherits(r, "expectation_success")))
}))
n_failed <- sum(sapply(test_results, function(x) {
  if (is.null(x$results)) return(0)
  sum(sapply(x$results, function(r) inherits(r, "expectation_failure")))
}))
n_errors <- sum(sapply(test_results, function(x) {
  if (is.null(x$results)) return(0)
  sum(sapply(x$results, function(r) inherits(r, "expectation_error")))
}))

cat(sprintf("\nTest files: %d\n", n_tests))
cat(sprintf("Passed:     %d\n", n_passed))
cat(sprintf("Failed:     %d\n", n_failed))
cat(sprintf("Errors:     %d\n", n_errors))

if (n_failed == 0 && n_errors == 0) {
  cat("\n✓ All tests passed!\n")
  quit(status = 0)
} else {
  cat("\n✗ Some tests failed.\n")
  quit(status = 1)
}
