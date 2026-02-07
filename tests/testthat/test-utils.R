# =============================================================================
# ユーティリティ関数のテスト
# Production Version 2.0
# =============================================================================

library(testthat)
library(lavaan)
library(dplyr)

# テスト用にutils.Rを読み込み
source("../../R/utils.R")

# -----------------------------------------------------------------------------
# evaluate_fit_index のテスト
# -----------------------------------------------------------------------------
describe("evaluate_fit_index", {

  it("CFI >= 0.95 を良好と判定する", {
    result <- evaluate_fit_index("cfi", 0.96)
    expect_equal(result$judgment, "\u826f\u597d")
    expect_equal(result$class, "fit-good")
  })

  it("CFI 0.90-0.95 を許容と判定する", {
    result <- evaluate_fit_index("cfi", 0.92)
    expect_equal(result$judgment, "\u8a31\u5bb9")
    expect_equal(result$class, "fit-acceptable")
  })

  it("CFI < 0.90 を不良と判定する", {
    result <- evaluate_fit_index("cfi", 0.85)
    expect_equal(result$judgment, "\u4e0d\u826f")
    expect_equal(result$class, "fit-poor")
  })

  it("RMSEA <= 0.05 を良好と判定する", {
    result <- evaluate_fit_index("rmsea", 0.04)
    expect_equal(result$judgment, "\u826f\u597d")
    expect_equal(result$class, "fit-good")
  })

  it("RMSEA 0.05-0.08 を許容と判定する", {
    result <- evaluate_fit_index("rmsea", 0.07)
    expect_equal(result$judgment, "\u8a31\u5bb9")
    expect_equal(result$class, "fit-acceptable")
  })

  it("RMSEA > 0.08 を不良と判定する", {
    result <- evaluate_fit_index("rmsea", 0.10)
    expect_equal(result$judgment, "\u4e0d\u826f")
    expect_equal(result$class, "fit-poor")
  })

  it("TLI >= 0.95 を良好と判定する", {
    result <- evaluate_fit_index("tli", 0.97)
    expect_equal(result$judgment, "\u826f\u597d")
  })

  it("SRMR <= 0.05 を良好と判定する", {
    result <- evaluate_fit_index("srmr", 0.03)
    expect_equal(result$judgment, "\u826f\u597d")
  })

  it("NA値に対して'-'を返す", {
    result <- evaluate_fit_index("cfi", NA)
    expect_equal(result$judgment, "-")
    expect_equal(result$class, "")
  })

  it("未知の指標に対して'-'を返す", {
    result <- evaluate_fit_index("unknown_index", 0.5)
    expect_equal(result$judgment, "-")
    expect_equal(result$class, "")
  })

})

# -----------------------------------------------------------------------------
# validate_model_syntax のテスト
# -----------------------------------------------------------------------------
describe("validate_model_syntax", {

  it("有効なCFA構文を検証する", {
    syntax <- "F1 =~ x1 + x2 + x3"
    result <- validate_model_syntax(syntax)
    expect_true(result$valid)
    expect_match(result$message, "\u6709\u52b9")
  })

  it("有効なSEM構文を検証する", {
    syntax <- "
      F1 =~ x1 + x2 + x3
      F2 =~ x4 + x5 + x6
      F2 ~ F1
    "
    result <- validate_model_syntax(syntax)
    expect_true(result$valid)
  })

  it("有効な媒介モデル構文を検証する", {
    syntax <- "
      M ~ a*X
      Y ~ b*M + c*X
      indirect := a*b
    "
    result <- validate_model_syntax(syntax)
    expect_true(result$valid)
  })

  it("構文の詳細情報を返す", {
    syntax <- "
      F1 =~ x1 + x2 + x3
      F2 ~ F1
    "
    result <- validate_model_syntax(syntax)
    expect_true(result$valid)
    expect_match(result$message, "\u6e2c\u5b9a")
    expect_match(result$message, "\u56de\u5e30")
  })

  it("空の構文を無効と判定する", {
    result <- validate_model_syntax("")
    expect_false(result$valid)
    expect_match(result$message, "\u5165\u529b\u3055\u308c\u3066\u3044\u307e\u305b\u3093")
  })

  it("NULL構文を無効と判定する", {
    result <- validate_model_syntax(NULL)
    expect_false(result$valid)
  })

  it("空白のみの構文を無効と判定する", {
    result <- validate_model_syntax("   \n\t   ")
    expect_false(result$valid)
  })

  it("コメントのみの構文を無効と判定する", {
    result <- validate_model_syntax("# This is just a comment")
    expect_false(result$valid)
  })

})

# -----------------------------------------------------------------------------
# calculate_descriptives のテスト
# -----------------------------------------------------------------------------
describe("calculate_descriptives", {

  it("基本統計量を正しく計算する", {
    data <- data.frame(
      x = c(1, 2, 3, 4, 5),
      y = c(2, 4, 6, 8, 10)
    )
    result <- calculate_descriptives(data)

    expect_equal(nrow(result), 2)
    expect_equal(result[["\u5909\u6570"]][1], "x")
    expect_equal(result[["N"]][1], 5)
    expect_equal(result[["\u5e73\u5747"]][1], 3)
    expect_equal(result[["\u6b20\u640d"]][1], 0)
  })

  it("欠損値を正しくカウントする", {
    data <- data.frame(
      x = c(1, 2, NA, 4, 5),
      y = c(2, NA, NA, 8, 10)
    )
    result <- calculate_descriptives(data)

    expect_equal(result[["\u6b20\u640d"]][1], 1)  # x has 1 NA
    expect_equal(result[["\u6b20\u640d"]][2], 2)  # y has 2 NAs
    expect_equal(result[["N"]][1], 4)     # x has 4 valid values
    expect_equal(result[["N"]][2], 3)     # y has 3 valid values
  })

  it("数値変数のみを処理する", {
    data <- data.frame(
      x = c(1, 2, 3),
      y = c("a", "b", "c"),
      z = c(4, 5, 6)
    )
    result <- calculate_descriptives(data)

    expect_equal(nrow(result), 2)
    expect_true("x" %in% result[["\u5909\u6570"]])
    expect_true("z" %in% result[["\u5909\u6570"]])
    expect_false("y" %in% result[["\u5909\u6570"]])
  })

  it("歪度と尖度を計算する", {
    set.seed(123)
    data <- data.frame(x = rnorm(100))
    result <- calculate_descriptives(data)

    expect_true("\u6b6a\u5ea6" %in% names(result))
    expect_true("\u5c16\u5ea6" %in% names(result))
    expect_true(abs(result[["\u6b6a\u5ea6"]][1]) < 1)
    expect_true(abs(result[["\u5c16\u5ea6"]][1]) < 1)
  })

})

# -----------------------------------------------------------------------------
# create_fit_table のテスト
# -----------------------------------------------------------------------------
describe("create_fit_table", {

  setup_fit <- function() {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '
    fit <- cfa(model, data = HolzingerSwineford1939)
    return(fit)
  }

  it("適合度テーブルを正しく作成する", {
    fit <- setup_fit()
    result <- create_fit_table(fit)

    expect_true(is.data.frame(result))
    expect_true("\u6307\u6a19" %in% names(result))
    expect_true("\u5024" %in% names(result))
    expect_true("\u5224\u5b9a" %in% names(result))
  })

  it("主要な適合度指標を含む", {
    fit <- setup_fit()
    result <- create_fit_table(fit)

    indices <- result[["\u6307\u6a19"]]
    expect_true("CFI" %in% indices)
    expect_true("TLI" %in% indices)
    expect_true("RMSEA" %in% indices)
    expect_true("SRMR" %in% indices)
    expect_true("AIC" %in% indices)
    expect_true("BIC" %in% indices)
  })

  it("NULLフィットに対してNULLを返す", {
    result <- create_fit_table(NULL)
    expect_null(result)
  })

})

# -----------------------------------------------------------------------------
# create_parameter_table のテスト
# -----------------------------------------------------------------------------
describe("create_parameter_table", {

  setup_fit <- function() {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '
    fit <- cfa(model, data = HolzingerSwineford1939)
    return(fit)
  }

  it("パラメータテーブルを正しく作成する（標準化あり）", {
    fit <- setup_fit()
    result <- create_parameter_table(fit, standardized = TRUE)

    expect_true(is.data.frame(result))
    expect_true("\u30d1\u30b9" %in% names(result))
    expect_true("\u63a8\u5b9a\u5024" %in% names(result))
    expect_true("\u6a19\u6e96\u5316" %in% names(result))
  })

  it("パラメータテーブルを正しく作成する（標準化なし）", {
    fit <- setup_fit()
    result <- create_parameter_table(fit, standardized = FALSE)

    expect_true(is.data.frame(result))
    expect_true("\u30d1\u30b9" %in% names(result))
    expect_true("\u63a8\u5b9a\u5024" %in% names(result))
    expect_false("\u6a19\u6e96\u5316" %in% names(result))
  })

  it("NULLフィットに対してNULLを返す", {
    result <- create_parameter_table(NULL)
    expect_null(result)
  })

})

# -----------------------------------------------------------------------------
# model_templates のテスト
# -----------------------------------------------------------------------------
describe("model_templates", {

  it("全てのテンプレートが存在する", {
    expect_true("cfa_1factor" %in% names(model_templates))
    expect_true("cfa_2factor" %in% names(model_templates))
    expect_true("cfa_3factor" %in% names(model_templates))
    expect_true("sem_basic" %in% names(model_templates))
    expect_true("sem_mediation" %in% names(model_templates))
    expect_true("path_analysis" %in% names(model_templates))
    expect_true("higher_order" %in% names(model_templates))
    expect_true("bifactor" %in% names(model_templates))
    expect_true("mimic" %in% names(model_templates))
  })

  it("各テンプレートに必要な要素がある", {
    for (name in names(model_templates)) {
      template <- model_templates[[name]]
      expect_true("name" %in% names(template), info = paste("Template", name, "missing 'name'"))
      expect_true("description" %in% names(template), info = paste("Template", name, "missing 'description'"))
      expect_true("syntax" %in% names(template), info = paste("Template", name, "missing 'syntax'"))
    }
  })

  it("各テンプレートの構文が有効である", {
    for (name in names(model_templates)) {
      template <- model_templates[[name]]
      result <- validate_model_syntax(template$syntax)
      expect_true(result$valid, info = paste("Template", name, "has invalid syntax"))
    }
  })

})

# -----------------------------------------------------------------------------
# generate_summary_text のテスト
# -----------------------------------------------------------------------------
describe("generate_summary_text", {

  setup_fit <- function() {
    model <- 'visual =~ x1 + x2 + x3'
    fit <- cfa(model, data = HolzingerSwineford1939)
    return(fit)
  }

  it("サマリーテキストを生成する", {
    fit <- setup_fit()
    result <- generate_summary_text(fit)

    expect_true(is.character(result))
    expect_true(nchar(result) > 0)
    expect_match(result, "CFI")
    expect_match(result, "RMSEA")
    expect_match(result, "SRMR")
  })

  it("R\u00b2情報を含む", {
    fit <- setup_fit()
    result <- generate_summary_text(fit)
    expect_match(result, "R")
  })

  it("NULLフィットに対して空文字を返す", {
    result <- generate_summary_text(NULL)
    expect_equal(result, "")
  })

})

# -----------------------------------------------------------------------------
# escape_html のテスト
# -----------------------------------------------------------------------------
describe("escape_html", {

  it("HTML特殊文字をエスケープする", {
    expect_equal(escape_html("<script>"), "&lt;script&gt;")
    expect_equal(escape_html("a&b"), "a&amp;b")
    expect_equal(escape_html('test"value'), "test&quot;value")
  })

  it("NULLに対して空文字を返す", {
    expect_equal(escape_html(NULL), "")
    expect_equal(escape_html(NA), "")
  })

  it("通常の文字列をそのまま返す", {
    expect_equal(escape_html("normal text"), "normal text")
  })

})

# -----------------------------------------------------------------------------
# sanitize_variable_names のテスト
# -----------------------------------------------------------------------------
describe("sanitize_variable_names", {

  it("特殊文字を除去する", {
    result <- sanitize_variable_names(c("a b", "c-d", "e.f"))
    expect_equal(result, c("a_b", "c_d", "e.f"))
  })

  it("数字で始まる名前にプレフィックスを追加する", {
    result <- sanitize_variable_names(c("1var", "2var"))
    expect_true(all(grepl("^V_", result)))
  })

  it("重複名を解消する", {
    result <- sanitize_variable_names(c("a b", "a-b"))
    expect_equal(length(unique(result)), 2)
  })

})

# -----------------------------------------------------------------------------
# validate_dataframe のテスト
# -----------------------------------------------------------------------------
describe("validate_dataframe", {

  it("有効なデータフレームを受け入れる", {
    df <- data.frame(x = 1:10, y = 11:20)
    result <- validate_dataframe(df)
    expect_true(result$valid)
  })

  it("NULLデータを拒否する", {
    result <- validate_dataframe(NULL)
    expect_false(result$valid)
  })

  it("空のデータフレームを拒否する", {
    result <- validate_dataframe(data.frame())
    expect_false(result$valid)
  })

  it("数値変数不足を検出する", {
    df <- data.frame(x = letters[1:5])
    result <- validate_dataframe(df)
    expect_false(result$valid)
  })

  it("欠損値の警告を生成する", {
    df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
    result <- validate_dataframe(df)
    expect_true(result$valid)
    expect_true(length(result$warnings) > 0)
    expect_true(any(grepl("\u6b20\u640d", result$warnings)))
  })

})

# -----------------------------------------------------------------------------
# calculate_cronbach_alpha のテスト
# -----------------------------------------------------------------------------
describe("calculate_cronbach_alpha", {

  it("正しくalphaを計算する", {
    set.seed(42)
    n <- 100
    true_score <- rnorm(n)
    data <- data.frame(
      item1 = true_score + rnorm(n, sd = 0.5),
      item2 = true_score + rnorm(n, sd = 0.5),
      item3 = true_score + rnorm(n, sd = 0.5),
      item4 = true_score + rnorm(n, sd = 0.5)
    )

    result <- calculate_cronbach_alpha(data, c("item1", "item2", "item3", "item4"))
    expect_true(!is.na(result$alpha))
    expect_true(result$alpha > 0.5)
    expect_true(result$alpha < 1.0)
    expect_equal(result$n, 100)
    expect_equal(result$k, 4)
  })

  it("項目統計を返す", {
    set.seed(42)
    n <- 50
    true_score <- rnorm(n)
    data <- data.frame(
      a = true_score + rnorm(n, sd = 0.3),
      b = true_score + rnorm(n, sd = 0.3),
      c = true_score + rnorm(n, sd = 0.3)
    )

    result <- calculate_cronbach_alpha(data, c("a", "b", "c"))
    expect_true(is.data.frame(result$item_stats))
    expect_equal(nrow(result$item_stats), 3)
  })

  it("2項目未満でNAを返す", {
    data <- data.frame(x = 1:10)
    result <- calculate_cronbach_alpha(data, "x")
    expect_true(is.na(result$alpha))
  })

})

# -----------------------------------------------------------------------------
# test_normality のテスト
# -----------------------------------------------------------------------------
describe("test_normality", {

  it("正規分布データの検定結果を返す", {
    set.seed(123)
    data <- data.frame(
      x = rnorm(100),
      y = rnorm(100)
    )

    result <- test_normality(data)
    expect_true(!is.null(result$univariate))
    expect_true(nrow(result$univariate) == 2)
    expect_true("SW_p\u5024" %in% names(result$univariate) ||
                "SW\u7d71\u8a08\u91cf" %in% names(result$univariate))
  })

  it("少なすぎるデータでメッセージを返す", {
    data <- data.frame(x = c(1, 2), y = c(3, 4))
    result <- test_normality(data)
    expect_true(!is.null(result$message))
  })

})

# -----------------------------------------------------------------------------
# create_rsquare_table のテスト
# -----------------------------------------------------------------------------
describe("create_rsquare_table", {

  it("R\u00b2テーブルを正しく作成する", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '
    fit <- cfa(model, data = HolzingerSwineford1939)
    result <- create_rsquare_table(fit)

    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("\u5909\u6570" %in% names(result))
    expect_true("R\u00b2" %in% names(result))
    expect_true("\u89e3\u91c8" %in% names(result))
  })

  it("NULLフィットに対してNULLを返す", {
    result <- create_rsquare_table(NULL)
    expect_null(result)
  })

})

# -----------------------------------------------------------------------------
# translate_lavaan_error のテスト
# -----------------------------------------------------------------------------
describe("translate_lavaan_error", {

  it("共分散行列エラーを翻訳する", {
    result <- translate_lavaan_error("covariance matrix is not positive definite")
    expect_true(nchar(result$help) > 0)
  })

  it("収束エラーを翻訳する", {
    result <- translate_lavaan_error("model did not convergence")
    expect_true(nchar(result$help) > 0)
  })

  it("未知のエラーにも対応する", {
    result <- translate_lavaan_error("some unknown error")
    expect_equal(result$help, "")
  })

})

# -----------------------------------------------------------------------------
# detect_outliers_mahalanobis のテスト
# -----------------------------------------------------------------------------
describe("detect_outliers_mahalanobis", {

  it("外れ値を検出する", {
    set.seed(42)
    data <- data.frame(
      x = c(rnorm(99), 100),   # 1つの明確な外れ値
      y = c(rnorm(99), 100)
    )

    result <- detect_outliers_mahalanobis(data)
    expect_true(result$n_outliers >= 1)
    expect_true(100 %in% result$outlier_indices)
  })

  it("正常データでは外れ値が少ない", {
    set.seed(42)
    data <- data.frame(
      x = rnorm(200),
      y = rnorm(200)
    )

    result <- detect_outliers_mahalanobis(data)
    expect_true(result$n_outliers < 10)  # 0.1%の閾値で200件中10未満
  })

})

# -----------------------------------------------------------------------------
# check_model_identification のテスト
# -----------------------------------------------------------------------------
describe("check_model_identification", {

  it("過剰識別モデルを正しく判定する", {
    result <- check_model_identification(
      n_factors = 3,
      n_indicators = c(3, 3, 3),
      n_structural = 0
    )
    expect_true(result$identified)
    expect_true(result$df > 0)
  })

  it("識別不能モデルを正しく判定する", {
    result <- check_model_identification(
      n_factors = 3,
      n_indicators = c(1, 1, 1),
      n_structural = 3
    )
    expect_false(result$identified)
    expect_true(result$df < 0)
  })

})
