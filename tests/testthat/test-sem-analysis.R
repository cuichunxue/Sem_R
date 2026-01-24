# =============================================================================
# SEM解析機能のテスト
# =============================================================================

library(testthat)
library(lavaan)

# -----------------------------------------------------------------------------
# CFA解析のテスト
# -----------------------------------------------------------------------------
describe("CFA Analysis", {

  it("1因子CFAモデルを正しく推定する", {
    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = HolzingerSwineford1939)

    expect_true(lavInspect(fit, "converged"))
    expect_equal(lavInspect(fit, "npar"), 6)  # 3 loadings + 3 residual variances

    fm <- fitMeasures(fit)
    expect_true(!is.na(fm["cfi"]))
    expect_true(!is.na(fm["rmsea"]))
  })

  it("3因子CFAモデルを正しく推定する", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '
    fit <- cfa(model, data = HolzingerSwineford1939)

    expect_true(lavInspect(fit, "converged"))

    fm <- fitMeasures(fit)
    expect_true(fm["cfi"] > 0.9)  # Should have acceptable fit
  })

  it("因子負荷量が正しく推定される", {
    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = HolzingerSwineford1939)

    params <- parameterEstimates(fit)
    loadings <- params[params$op == "=~", ]

    expect_equal(nrow(loadings), 3)
    expect_true(all(loadings$est > 0))  # All loadings should be positive
  })

})

# -----------------------------------------------------------------------------
# SEM解析のテスト
# -----------------------------------------------------------------------------
describe("SEM Analysis", {

  it("構造モデルを正しく推定する", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
      speed ~ visual + textual
    '
    fit <- sem(model, data = HolzingerSwineford1939)

    expect_true(lavInspect(fit, "converged"))

    params <- parameterEstimates(fit)
    regressions <- params[params$op == "~", ]
    expect_equal(nrow(regressions), 2)
  })

  it("媒介モデルの間接効果を計算する", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9

      textual ~ a*visual
      speed ~ b*textual + c*visual

      indirect := a*b
      total := c + a*b
    '
    fit <- sem(model, data = HolzingerSwineford1939)

    expect_true(lavInspect(fit, "converged"))

    params <- parameterEstimates(fit)
    defined <- params[params$op == ":=", ]

    expect_equal(nrow(defined), 2)
    expect_true("indirect" %in% defined$lhs)
    expect_true("total" %in% defined$lhs)
  })

})

# -----------------------------------------------------------------------------
# 推定オプションのテスト
# -----------------------------------------------------------------------------
describe("Estimation Options", {

  setup_model <- function() {
    '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '
  }

  it("ML推定が正しく動作する", {
    fit <- cfa(setup_model(), data = HolzingerSwineford1939, estimator = "ML")
    expect_true(lavInspect(fit, "converged"))
    expect_equal(lavInspect(fit, "options")$estimator, "ML")
  })

  it("MLR推定が正しく動作する", {
    fit <- cfa(setup_model(), data = HolzingerSwineford1939, estimator = "MLR")
    expect_true(lavInspect(fit, "converged"))
  })

  it("std.lv=TRUEで潜在変数の分散が1に固定される", {
    fit <- cfa(setup_model(), data = HolzingerSwineford1939, std.lv = TRUE)
    expect_true(lavInspect(fit, "converged"))

    params <- parameterEstimates(fit)
    lv_variances <- params[params$op == "~~" & params$lhs %in% c("visual", "textual") & params$lhs == params$rhs, ]

    # std.lv=TRUE の場合、分散は1に固定される
    expect_true(all(lv_variances$est == 1))
  })

})

# -----------------------------------------------------------------------------
# 適合度指標のテスト
# -----------------------------------------------------------------------------
describe("Fit Measures", {

  setup_fit <- function() {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '
    cfa(model, data = HolzingerSwineford1939)
  }

  it("全ての主要適合度指標が計算される", {
    fit <- setup_fit()
    fm <- fitMeasures(fit)

    required_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                          "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic")

    for (idx in required_indices) {
      expect_true(idx %in% names(fm), info = paste("Missing index:", idx))
      expect_false(is.na(fm[idx]), info = paste("NA value for:", idx))
    }
  })

  it("CFIとTLIが0から1の範囲にある", {
    fit <- setup_fit()
    fm <- fitMeasures(fit)

    expect_true(fm["cfi"] >= 0 && fm["cfi"] <= 1)
    expect_true(fm["tli"] >= 0 && fm["tli"] <= 1.1)  # TLI can exceed 1
  })

  it("RMSEAとSRMRが非負である", {
    fit <- setup_fit()
    fm <- fitMeasures(fit)

    expect_true(fm["rmsea"] >= 0)
    expect_true(fm["srmr"] >= 0)
  })

  it("自由度が非負整数である", {
    fit <- setup_fit()
    fm <- fitMeasures(fit)

    expect_true(fm["df"] >= 0)
    expect_equal(fm["df"], floor(fm["df"]))  # Should be integer
  })

})

# -----------------------------------------------------------------------------
# モデル比較のテスト
# -----------------------------------------------------------------------------
describe("Model Comparison", {

  it("ネストモデルのカイ二乗差検定が正しく計算される", {
    # より制約的なモデル（因子相関を0に固定）
    model_constrained <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      visual ~~ 0*textual
    '

    # より自由なモデル
    model_free <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '

    fit_constrained <- cfa(model_constrained, data = HolzingerSwineford1939)
    fit_free <- cfa(model_free, data = HolzingerSwineford1939)

    # カイ二乗差検定
    fm_c <- fitMeasures(fit_constrained)
    fm_f <- fitMeasures(fit_free)

    chisq_diff <- fm_c["chisq"] - fm_f["chisq"]
    df_diff <- fm_c["df"] - fm_f["df"]

    expect_true(chisq_diff >= 0)  # Constrained model should have higher chi-sq
    expect_equal(df_diff, 1)      # One constraint added

    p_value <- pchisq(chisq_diff, df_diff, lower.tail = FALSE)
    expect_true(p_value >= 0 && p_value <= 1)
  })

  it("AICとBICでモデル比較ができる", {
    model_1 <- '
      F1 =~ x1 + x2 + x3 + x4 + x5 + x6
    '

    model_2 <- '
      F1 =~ x1 + x2 + x3
      F2 =~ x4 + x5 + x6
    '

    fit_1 <- cfa(model_1, data = HolzingerSwineford1939)
    fit_2 <- cfa(model_2, data = HolzingerSwineford1939)

    fm_1 <- fitMeasures(fit_1)
    fm_2 <- fitMeasures(fit_2)

    # 2因子モデルの方がAIC/BICが低いはず（適合が良い）
    expect_true(fm_2["aic"] < fm_1["aic"])
  })

})

# -----------------------------------------------------------------------------
# 修正指標のテスト
# -----------------------------------------------------------------------------
describe("Modification Indices", {

  it("修正指標が計算される", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '
    fit <- cfa(model, data = HolzingerSwineford1939)

    mi <- modificationIndices(fit)

    expect_true(is.data.frame(mi))
    expect_true("mi" %in% names(mi))
    expect_true("epc" %in% names(mi))
    expect_true(nrow(mi) > 0)
  })

  it("修正指標の値が非負である", {
    model <- 'F1 =~ x1 + x2 + x3 + x4'
    fit <- cfa(model, data = HolzingerSwineford1939)

    mi <- modificationIndices(fit)

    expect_true(all(mi$mi >= 0, na.rm = TRUE))
  })

})

# -----------------------------------------------------------------------------
# パラメータ推定値のテスト
# -----------------------------------------------------------------------------
describe("Parameter Estimates", {

  it("標準化係数が正しく計算される", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '
    fit <- cfa(model, data = HolzingerSwineford1939)

    params <- parameterEstimates(fit, standardized = TRUE)

    expect_true("std.all" %in% names(params))
    expect_true("std.lv" %in% names(params))
    expect_true("std.nox" %in% names(params))

    # 標準化因子負荷は通常-1から1の範囲
    loadings <- params[params$op == "=~", ]
    expect_true(all(abs(loadings$std.all) <= 1.5))  # Allow some flexibility
  })

  it("標準誤差とz値が計算される", {
    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = HolzingerSwineford1939)

    params <- parameterEstimates(fit)

    expect_true("se" %in% names(params))
    expect_true("z" %in% names(params))
    expect_true("pvalue" %in% names(params))

    # 自由に推定されたパラメータはSEが正
    free_params <- params[params$se > 0, ]
    expect_true(nrow(free_params) > 0)
  })

  it("信頼区間が計算される", {
    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = HolzingerSwineford1939)

    params <- parameterEstimates(fit, ci = TRUE)

    expect_true("ci.lower" %in% names(params))
    expect_true("ci.upper" %in% names(params))

    # 信頼区間が推定値を含む
    free_params <- params[params$se > 0, ]
    expect_true(all(free_params$ci.lower <= free_params$est))
    expect_true(all(free_params$ci.upper >= free_params$est))
  })

})

# -----------------------------------------------------------------------------
# エッジケースのテスト
# -----------------------------------------------------------------------------
describe("Edge Cases", {

  it("収束しないモデルを検出する", {
    # 意図的に収束困難なモデル
    model <- '
      F1 =~ x1
    '

    # 1指標では識別不能
    expect_error(
      cfa(model, data = HolzingerSwineford1939),
      regexp = NULL  # Some error expected
    )
  })

  it("欠損値を含むデータで分析できる（listwise）", {
    data_with_na <- HolzingerSwineford1939
    data_with_na$x1[1:10] <- NA

    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = data_with_na, missing = "listwise")

    expect_true(lavInspect(fit, "converged"))
    expect_true(lavInspect(fit, "nobs") < nrow(HolzingerSwineford1939))
  })

  it("欠損値を含むデータで分析できる（FIML）", {
    data_with_na <- HolzingerSwineford1939
    data_with_na$x1[1:10] <- NA

    model <- 'F1 =~ x1 + x2 + x3'
    fit <- cfa(model, data = data_with_na, missing = "fiml")

    expect_true(lavInspect(fit, "converged"))
    # FIML uses all available data
  })

})
