# =============================================================================
# モデル比較機能のテスト
# =============================================================================

library(testthat)
library(lavaan)

# -----------------------------------------------------------------------------
# モデル比較ユーティリティのテスト
# -----------------------------------------------------------------------------
describe("Model Comparison Utilities", {

  # テスト用のフィットオブジェクトを作成
  setup_models <- function() {
    model_1 <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '

    model_2 <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
      visual ~~ 0*textual
    '

    model_3 <- '
      F1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
    '

    list(
      model_1 = cfa(model_1, data = HolzingerSwineford1939),
      model_2 = cfa(model_2, data = HolzingerSwineford1939),
      model_3 = cfa(model_3, data = HolzingerSwineford1939)
    )
  }

  it("複数モデルの適合度指標を比較できる", {
    models <- setup_models()

    # 各モデルの適合度を取得
    fit_measures <- lapply(models, fitMeasures)

    # CFI比較
    cfis <- sapply(fit_measures, function(x) x["cfi"])
    expect_equal(length(cfis), 3)
    expect_true(all(!is.na(cfis)))

    # 3因子モデルが1因子モデルより良いはず
    expect_true(cfis["model_1"] > cfis["model_3"])
  })

  it("AICによるモデル選択が正しく機能する", {
    models <- setup_models()

    aics <- sapply(models, function(fit) fitMeasures(fit)["aic"])

    # 最良モデルの特定
    best_model <- names(which.min(aics))
    expect_true(best_model %in% names(models))

    # 3因子モデルが1因子モデルよりAICが低いはず
    expect_true(aics["model_1"] < aics["model_3"])
  })

  it("BICによるモデル選択が正しく機能する", {
    models <- setup_models()

    bics <- sapply(models, function(fit) fitMeasures(fit)["bic"])

    # 最良モデルの特定
    best_model <- names(which.min(bics))
    expect_true(best_model %in% names(models))
  })

})

# -----------------------------------------------------------------------------
# カイ二乗差検定のテスト
# -----------------------------------------------------------------------------
describe("Chi-square Difference Test", {

  it("ネストモデルのカイ二乗差を計算する", {
    # ベースモデル（自由）
    model_free <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '

    # 制約モデル（因子相関を0に）
    model_constrained <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      visual ~~ 0*textual
    '

    fit_free <- cfa(model_free, data = HolzingerSwineford1939)
    fit_constrained <- cfa(model_constrained, data = HolzingerSwineford1939)

    fm_free <- fitMeasures(fit_free)
    fm_constrained <- fitMeasures(fit_constrained)

    # カイ二乗差
    chisq_diff <- fm_constrained["chisq"] - fm_free["chisq"]
    df_diff <- fm_constrained["df"] - fm_free["df"]

    expect_true(chisq_diff > 0)  # 制約モデルの方がカイ二乗が大きい
    expect_equal(as.numeric(df_diff), 1)  # 制約が1つ追加された

    # p値計算
    p_value <- pchisq(chisq_diff, df_diff, lower.tail = FALSE)
    expect_true(p_value >= 0 && p_value <= 1)
  })

  it("同一モデル間のカイ二乗差は0", {
    model <- 'F1 =~ x1 + x2 + x3'

    fit_1 <- cfa(model, data = HolzingerSwineford1939)
    fit_2 <- cfa(model, data = HolzingerSwineford1939)

    fm_1 <- fitMeasures(fit_1)
    fm_2 <- fitMeasures(fit_2)

    chisq_diff <- abs(fm_1["chisq"] - fm_2["chisq"])
    df_diff <- abs(fm_1["df"] - fm_2["df"])

    expect_equal(as.numeric(chisq_diff), 0, tolerance = 0.001)
    expect_equal(as.numeric(df_diff), 0)
  })

  it("lavaan::anova関数との整合性", {
    model_free <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '

    model_constrained <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      visual ~~ 0*textual
    '

    fit_free <- cfa(model_free, data = HolzingerSwineford1939)
    fit_constrained <- cfa(model_constrained, data = HolzingerSwineford1939)

    # lavaan::anovaを使用
    comparison <- anova(fit_constrained, fit_free)

    expect_true(is.data.frame(comparison) || is.matrix(comparison))
    expect_true(nrow(comparison) == 2)
  })

})

# -----------------------------------------------------------------------------
# パラメータ比較のテスト
# -----------------------------------------------------------------------------
describe("Parameter Comparison", {

  it("同じモデルのパラメータは一致する", {
    model <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '

    fit_1 <- cfa(model, data = HolzingerSwineford1939)
    fit_2 <- cfa(model, data = HolzingerSwineford1939)

    params_1 <- parameterEstimates(fit_1)
    params_2 <- parameterEstimates(fit_2)

    # 推定値が一致
    expect_equal(params_1$est, params_2$est, tolerance = 0.001)
  })

  it("異なるモデル間でパラメータを比較できる", {
    model_1 <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    '

    model_2 <- '
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    '

    fit_1 <- cfa(model_1, data = HolzingerSwineford1939)
    fit_2 <- cfa(model_2, data = HolzingerSwineford1939)

    params_1 <- parameterEstimates(fit_1)
    params_2 <- parameterEstimates(fit_2)

    # 共通パラメータの比較
    params_1$path <- paste(params_1$lhs, params_1$op, params_1$rhs)
    params_2$path <- paste(params_2$lhs, params_2$op, params_2$rhs)

    common_paths <- intersect(params_1$path, params_2$path)
    expect_true(length(common_paths) > 0)

    # 共通パスの推定値は似ているはず（同じデータなので）
    for (path in common_paths[1:min(3, length(common_paths))]) {
      est_1 <- params_1$est[params_1$path == path]
      est_2 <- params_2$est[params_2$path == path]
      if (length(est_1) == 1 && length(est_2) == 1) {
        expect_equal(est_1, est_2, tolerance = 0.5)  # Some tolerance due to model differences
      }
    }
  })

})

# -----------------------------------------------------------------------------
# モデル選択基準のテスト
# -----------------------------------------------------------------------------
describe("Model Selection Criteria", {

  setup_competing_models <- function() {
    # 1因子モデル
    model_1f <- 'F1 =~ x1 + x2 + x3 + x4 + x5 + x6'

    # 2因子モデル
    model_2f <- '
      F1 =~ x1 + x2 + x3
      F2 =~ x4 + x5 + x6
    '

    # 2因子直交モデル
    model_2f_orth <- '
      F1 =~ x1 + x2 + x3
      F2 =~ x4 + x5 + x6
      F1 ~~ 0*F2
    '

    list(
      one_factor = cfa(model_1f, data = HolzingerSwineford1939),
      two_factor = cfa(model_2f, data = HolzingerSwineford1939),
      two_factor_orth = cfa(model_2f_orth, data = HolzingerSwineford1939)
    )
  }

  it("適合度に基づいて最良モデルを特定できる", {
    models <- setup_competing_models()

    # CFIで比較
    cfis <- sapply(models, function(fit) fitMeasures(fit)["cfi"])
    best_by_cfi <- names(which.max(cfis))

    # RMSEAで比較（低いほど良い）
    rmseas <- sapply(models, function(fit) fitMeasures(fit)["rmsea"])
    best_by_rmsea <- names(which.min(rmseas))

    expect_true(best_by_cfi %in% names(models))
    expect_true(best_by_rmsea %in% names(models))

    # 2因子モデルが1因子より良いはず
    expect_true(cfis["two_factor"] > cfis["one_factor"])
  })

  it("情報量基準で比較できる", {
    models <- setup_competing_models()

    # AIC
    aics <- sapply(models, function(fit) fitMeasures(fit)["aic"])
    expect_true(all(!is.na(aics)))

    # BIC
    bics <- sapply(models, function(fit) fitMeasures(fit)["bic"])
    expect_true(all(!is.na(bics)))

    # AICとBICの順序は必ずしも一致しない
    # しかし、両方とも有限値であるべき
    expect_true(all(is.finite(aics)))
    expect_true(all(is.finite(bics)))
  })

  it("節約性を考慮したモデル選択ができる", {
    models <- setup_competing_models()

    # パラメータ数を取得
    npars <- sapply(models, function(fit) lavInspect(fit, "npar"))

    # 自由度を取得
    dfs <- sapply(models, function(fit) fitMeasures(fit)["df"])

    expect_true(all(npars > 0))
    expect_true(all(dfs >= 0))

    # 1因子モデルはパラメータ数が少ない（より節約的）
    expect_true(npars["one_factor"] < npars["two_factor"])
  })

})
