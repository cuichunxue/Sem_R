# =============================================================================
# ユーティリティ関数のテスト
# =============================================================================

library(testthat)
library(lavaan)

# テスト用にutils.Rを読み込み
source("../../R/utils.R")

# -----------------------------------------------------------------------------
# evaluate_fit_index のテスト
# -----------------------------------------------------------------------------
describe("evaluate_fit_index", {

  it("CFI >= 0.95 を良好と判定する", {
    result <- evaluate_fit_index("cfi", 0.96)
    expect_equal(result$judgment, "良好")
    expect_equal(result$class, "fit-good")
  })

  it("CFI 0.90-0.95 を許容と判定する", {
    result <- evaluate_fit_index("cfi", 0.92)
    expect_equal(result$judgment, "許容")
    expect_equal(result$class, "fit-acceptable")
  })

  it("CFI < 0.90 を不良と判定する", {
    result <- evaluate_fit_index("cfi", 0.85)
    expect_equal(result$judgment, "不良")
    expect_equal(result$class, "fit-poor")
  })

  it("RMSEA <= 0.05 を良好と判定する", {
    result <- evaluate_fit_index("rmsea", 0.04)
    expect_equal(result$judgment, "良好")
    expect_equal(result$class, "fit-good")
  })

  it("RMSEA 0.05-0.08 を許容と判定する", {
    result <- evaluate_fit_index("rmsea", 0.07)
    expect_equal(result$judgment, "許容")
    expect_equal(result$class, "fit-acceptable")
  })

  it("RMSEA > 0.08 を不良と判定する", {
    result <- evaluate_fit_index("rmsea", 0.10)
    expect_equal(result$judgment, "不良")
    expect_equal(result$class, "fit-poor")
  })

  it("TLI >= 0.95 を良好と判定する", {
    result <- evaluate_fit_index("tli", 0.97)
    expect_equal(result$judgment, "良好")
  })

  it("SRMR <= 0.05 を良好と判定する", {
    result <- evaluate_fit_index("srmr", 0.03)
    expect_equal(result$judgment, "良好")
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
    expect_equal(result$message, "構文は有効です")
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

  it("空の構文を無効と判定する", {
    result <- validate_model_syntax("")
    expect_false(result$valid)
    expect_match(result$message, "入力されていません")
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
    expect_equal(result$変数[1], "x")
    expect_equal(result$N[1], 5)
    expect_equal(result$平均[1], 3)
    expect_equal(result$欠損[1], 0)
  })

  it("欠損値を正しくカウントする", {
    data <- data.frame(
      x = c(1, 2, NA, 4, 5),
      y = c(2, NA, NA, 8, 10)
    )
    result <- calculate_descriptives(data)

    expect_equal(result$欠損[1], 1)  # x has 1 NA
    expect_equal(result$欠損[2], 2)  # y has 2 NAs
    expect_equal(result$N[1], 4)     # x has 4 valid values
    expect_equal(result$N[2], 3)     # y has 3 valid values
  })

  it("数値変数のみを処理する", {
    data <- data.frame(
      x = c(1, 2, 3),
      y = c("a", "b", "c"),
      z = c(4, 5, 6)
    )
    result <- calculate_descriptives(data)

    expect_equal(nrow(result), 2)
    expect_true("x" %in% result$変数)
    expect_true("z" %in% result$変数)
    expect_false("y" %in% result$変数)
  })

  it("歪度と尖度を計算する", {
    set.seed(123)
    data <- data.frame(x = rnorm(100))
    result <- calculate_descriptives(data)

    expect_true("歪度" %in% names(result))
    expect_true("尖度" %in% names(result))
    # 正規分布の歪度は約0、尖度は約0（超過尖度）
    expect_true(abs(result$歪度[1]) < 1)
    expect_true(abs(result$尖度[1]) < 1)
  })

})

# -----------------------------------------------------------------------------
# create_fit_table のテスト
# -----------------------------------------------------------------------------
describe("create_fit_table", {

  # テスト用のフィットオブジェクトを作成
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
    expect_true("指標" %in% names(result))
    expect_true("値" %in% names(result))
    expect_true("判定" %in% names(result))
  })

  it("主要な適合度指標を含む", {
    fit <- setup_fit()
    result <- create_fit_table(fit)

    indices <- result$指標
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
    expect_true("パス" %in% names(result))
    expect_true("推定値" %in% names(result))
    expect_true("標準化" %in% names(result))
  })

  it("パラメータテーブルを正しく作成する（標準化なし）", {
    fit <- setup_fit()
    result <- create_parameter_table(fit, standardized = FALSE)

    expect_true(is.data.frame(result))
    expect_true("パス" %in% names(result))
    expect_true("推定値" %in% names(result))
    expect_false("標準化" %in% names(result))
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

  it("NULLフィットに対して空文字を返す", {
    result <- generate_summary_text(NULL)
    expect_equal(result, "")
  })

})
