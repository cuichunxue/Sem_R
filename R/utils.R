# =============================================================================
# ユーティリティ関数
# =============================================================================

#' カスタムCSS
custom_css <- function() {
  '
  /* 全体スタイル */
  body {
    font-size: 14px;
  }

  /* ナビゲーション */
  .navbar-brand {
    font-weight: 600;
    font-size: 1.2rem;
  }

  /* カード */
  .card {
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    border: none;
    border-radius: 8px;
    margin-bottom: 1rem;
  }

  .card-header {
    background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
    color: white;
    font-weight: 600;
    border-radius: 8px 8px 0 0 !important;
    padding: 0.75rem 1rem;
  }

  .card-header-secondary {
    background: linear-gradient(135deg, #95a5a6 0%, #bdc3c7 100%);
  }

  .card-header-success {
    background: linear-gradient(135deg, #18bc9c 0%, #1abc9c 100%);
  }

  /* コードエディタ */
  .syntax-editor {
    font-family: "Source Code Pro", monospace;
    font-size: 13px;
    line-height: 1.5;
    background-color: #2d2d2d;
    color: #f8f8f2;
    border-radius: 6px;
    padding: 1rem;
    min-height: 300px;
    resize: vertical;
  }

  /* ステータスバッジ */
  .status-badge {
    display: inline-flex;
    align-items: center;
    padding: 0.35rem 0.75rem;
    border-radius: 50px;
    font-size: 0.85rem;
    font-weight: 500;
  }

  .status-success {
    background-color: #d4edda;
    color: #155724;
  }

  .status-warning {
    background-color: #fff3cd;
    color: #856404;
  }

  .status-danger {
    background-color: #f8d7da;
    color: #721c24;
  }

  .status-info {
    background-color: #d1ecf1;
    color: #0c5460;
  }

  /* テーブル */
  .dataTables_wrapper {
    font-size: 13px;
  }

  .table-fit-indices th {
    background-color: #2c3e50;
    color: white;
  }

  .fit-good {
    background-color: #d4edda !important;
    color: #155724;
    font-weight: 600;
  }

  .fit-acceptable {
    background-color: #fff3cd !important;
    color: #856404;
    font-weight: 600;
  }

  .fit-poor {
    background-color: #f8d7da !important;
    color: #721c24;
    font-weight: 600;
  }

  /* ファイルアップロード */
  .file-upload-area {
    border: 2px dashed #bdc3c7;
    border-radius: 8px;
    padding: 2rem;
    text-align: center;
    background-color: #f8f9fa;
    transition: all 0.3s ease;
    cursor: pointer;
  }

  .file-upload-area:hover {
    border-color: #3498db;
    background-color: #e8f4fc;
  }

  .file-upload-area.dragover {
    border-color: #18bc9c;
    background-color: #e8f8f5;
  }

  /* プログレスバー */
  .analysis-progress {
    height: 8px;
    border-radius: 4px;
  }

  /* ヘルプテキスト */
  .help-text {
    font-size: 0.85rem;
    color: #6c757d;
    margin-top: 0.25rem;
  }

  /* 結果パネル */
  .result-panel {
    background: white;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 1rem;
    box-shadow: 0 1px 4px rgba(0,0,0,0.08);
  }

  .result-panel h5 {
    color: #2c3e50;
    border-bottom: 2px solid #3498db;
    padding-bottom: 0.5rem;
    margin-bottom: 1rem;
  }

  /* アラート改善 */
  .alert {
    border-radius: 8px;
    border: none;
  }

  /* ボタン改善 */
  .btn {
    border-radius: 6px;
    font-weight: 500;
    transition: all 0.2s ease;
  }

  .btn-lg {
    padding: 0.75rem 2rem;
  }

  .btn-primary:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(44, 62, 80, 0.3);
  }

  /* セクション見出し */
  .section-title {
    font-size: 1.1rem;
    font-weight: 600;
    color: #2c3e50;
    margin-bottom: 1rem;
    display: flex;
    align-items: center;
  }

  .section-title i {
    margin-right: 0.5rem;
    color: #3498db;
  }

  /* タブコンテンツ */
  .tab-content {
    padding: 1.5rem 0;
  }

  /* 値ハイライト */
  .value-highlight {
    font-size: 1.5rem;
    font-weight: 700;
    color: #2c3e50;
  }

  .value-label {
    font-size: 0.85rem;
    color: #6c757d;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* パス図コンテナ */
  .diagram-container {
    background: white;
    border-radius: 8px;
    padding: 1rem;
    min-height: 500px;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  /* テンプレートカード */
  .template-card {
    cursor: pointer;
    transition: all 0.2s ease;
    border: 2px solid transparent;
  }

  .template-card:hover {
    border-color: #3498db;
    transform: translateY(-2px);
  }

  .template-card.selected {
    border-color: #18bc9c;
    background-color: #e8f8f5;
  }

  /* スクロールバー */
  ::-webkit-scrollbar {
    width: 8px;
    height: 8px;
  }

  ::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 4px;
  }

  ::-webkit-scrollbar-thumb {
    background: #bdc3c7;
    border-radius: 4px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #95a5a6;
  }

  /* ========================================================
     ステップインジケーター（初心者向けガイド）
     ======================================================== */
  .step-indicator {
    display: flex;
    justify-content: center;
    align-items: center;
    padding: 0.75rem 1rem;
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 12px;
    margin-bottom: 1.5rem;
    gap: 0;
  }

  .step-item {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.5rem 1rem;
    border-radius: 8px;
    font-size: 0.85rem;
    font-weight: 500;
    color: #95a5a6;
    position: relative;
  }

  .step-item.active {
    background-color: #3498db;
    color: white;
    box-shadow: 0 2px 8px rgba(52, 152, 219, 0.3);
  }

  .step-item.completed {
    background-color: #18bc9c;
    color: white;
  }

  .step-number {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    border-radius: 50%;
    background: rgba(255,255,255,0.2);
    font-size: 0.75rem;
    font-weight: 700;
  }

  .step-item:not(.active):not(.completed) .step-number {
    background: #ddd;
    color: #999;
  }

  .step-connector {
    width: 30px;
    height: 2px;
    background-color: #ddd;
    flex-shrink: 0;
  }

  .step-connector.completed {
    background-color: #18bc9c;
  }

  /* ========================================================
     ウェルカムカード
     ======================================================== */
  .welcome-card {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 16px;
    padding: 2rem;
    margin-bottom: 1.5rem;
    position: relative;
    overflow: hidden;
  }

  .welcome-card::before {
    content: "";
    position: absolute;
    top: -50%;
    right: -20%;
    width: 300px;
    height: 300px;
    background: rgba(255,255,255,0.1);
    border-radius: 50%;
  }

  .welcome-card h2 {
    font-size: 1.5rem;
    font-weight: 700;
    margin-bottom: 0.5rem;
  }

  .welcome-card p {
    opacity: 0.9;
    margin-bottom: 1rem;
    font-size: 0.95rem;
  }

  .demo-btn {
    background: white;
    color: #667eea;
    border: none;
    padding: 0.75rem 1.5rem;
    border-radius: 8px;
    font-weight: 700;
    font-size: 1rem;
    cursor: pointer;
    transition: all 0.2s;
    box-shadow: 0 4px 15px rgba(0,0,0,0.2);
  }

  .demo-btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(0,0,0,0.3);
    color: #667eea;
    background: white;
  }

  /* ========================================================
     結果解釈カード
     ======================================================== */
  .interpretation-card {
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
    border-radius: 12px;
    padding: 1.25rem;
    margin-bottom: 1rem;
  }

  .interpretation-card.good {
    background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
    border-left: 4px solid #18bc9c;
  }

  .interpretation-card.acceptable {
    background: linear-gradient(135deg, #fff3cd 0%, #ffeeba 100%);
    border-left: 4px solid #f39c12;
  }

  .interpretation-card.poor {
    background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%);
    border-left: 4px solid #e74c3c;
  }

  .interpretation-title {
    font-weight: 700;
    font-size: 1rem;
    margin-bottom: 0.25rem;
  }

  .interpretation-text {
    font-size: 0.9rem;
    line-height: 1.5;
    color: #2c3e50;
  }

  /* ========================================================
     次のステップボタン
     ======================================================== */
  .next-step-banner {
    background: linear-gradient(135deg, #e8f4fc 0%, #d1ecf1 100%);
    border: 2px solid #3498db;
    border-radius: 12px;
    padding: 1rem 1.5rem;
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-top: 1rem;
  }

  .next-step-banner .next-step-text {
    font-weight: 600;
    color: #2c3e50;
  }

  .next-step-banner .next-step-hint {
    font-size: 0.85rem;
    color: #6c757d;
  }

  /* ========================================================
     ヒントツールチップ
     ======================================================== */
  .beginner-tip {
    background: #e8f4fc;
    border-left: 3px solid #3498db;
    border-radius: 0 8px 8px 0;
    padding: 0.75rem 1rem;
    margin-bottom: 1rem;
    font-size: 0.85rem;
    color: #2c3e50;
  }

  .beginner-tip strong {
    color: #3498db;
  }

  /* ========================================================
     サンプルデータカード
     ======================================================== */
  .sample-card {
    border: 2px solid #e9ecef;
    border-radius: 10px;
    padding: 1rem;
    cursor: pointer;
    transition: all 0.2s;
    background: white;
  }

  .sample-card:hover {
    border-color: #3498db;
    box-shadow: 0 4px 12px rgba(52, 152, 219, 0.15);
    transform: translateY(-2px);
  }

  .sample-card.selected {
    border-color: #18bc9c;
    background-color: #f0faf7;
  }

  .sample-card h6 {
    font-weight: 700;
    color: #2c3e50;
    margin-bottom: 0.25rem;
  }

  .sample-card .text-muted {
    font-size: 0.8rem;
  }

  /* ========================================================
     推奨バッジ
     ======================================================== */
  .badge-recommended {
    background: linear-gradient(135deg, #18bc9c 0%, #1abc9c 100%);
    color: white;
    font-size: 0.7rem;
    padding: 0.2rem 0.5rem;
    border-radius: 4px;
    margin-left: 0.5rem;
    font-weight: 600;
  }

  /* ========================================================
     適合度ゲージ
     ======================================================== */
  .fit-gauge {
    text-align: center;
    padding: 1rem;
  }

  .fit-gauge .gauge-value {
    font-size: 2rem;
    font-weight: 800;
    line-height: 1;
  }

  .fit-gauge .gauge-label {
    font-size: 0.8rem;
    color: #6c757d;
    text-transform: uppercase;
    letter-spacing: 1px;
    margin-top: 0.25rem;
  }

  .fit-gauge .gauge-bar {
    height: 6px;
    border-radius: 3px;
    background: #e9ecef;
    margin-top: 0.5rem;
    overflow: hidden;
  }

  .fit-gauge .gauge-bar-fill {
    height: 100%;
    border-radius: 3px;
    transition: width 0.5s ease;
  }

  .fit-gauge.good .gauge-value { color: #18bc9c; }
  .fit-gauge.good .gauge-bar-fill { background: #18bc9c; }
  .fit-gauge.acceptable .gauge-value { color: #f39c12; }
  .fit-gauge.acceptable .gauge-bar-fill { background: #f39c12; }
  .fit-gauge.poor .gauge-value { color: #e74c3c; }
  .fit-gauge.poor .gauge-bar-fill { background: #e74c3c; }

  /* レスポンシブ調整 */
  @media (max-width: 768px) {
    .card-header {
      font-size: 0.9rem;
    }

    .syntax-editor {
      min-height: 200px;
    }

    .step-indicator {
      flex-wrap: wrap;
      gap: 0.5rem;
    }

    .step-connector {
      display: none;
    }

    .welcome-card {
      padding: 1.5rem;
    }
  }
  '
}

#' データファイルを読み込む
#' @param file ファイルオブジェクト
#' @return データフレーム
read_data_file <- function(file) {
  ext <- tools::file_ext(file$name)

  tryCatch({
    data <- switch(
      tolower(ext),
      "csv" = read_csv(file$datapath, show_col_types = FALSE),
      "tsv" = read_tsv(file$datapath, show_col_types = FALSE),
      "txt" = read_delim(file$datapath, delim = "\t", show_col_types = FALSE),
      "xlsx" = read_excel(file$datapath),
      "xls" = read_excel(file$datapath),
      "sav" = read_sav(file$datapath),
      "sas7bdat" = read_sas(file$datapath),
      "dta" = read_dta(file$datapath),
      "rds" = readRDS(file$datapath),
      stop("サポートされていないファイル形式です: ", ext)
    )

    # 因子をダミー変換しない（lavaan で処理）
    as.data.frame(data)

  }, error = function(e) {
    stop("ファイル読み込みエラー: ", e$message)
  })
}

#' 適合度指標の判定
#' @param index 指標名
#' @param value 値
#' @return リスト（判定、クラス）
evaluate_fit_index <- function(index, value) {
  if (is.na(value) || is.null(value)) {
    return(list(judgment = "-", class = ""))
  }

  # 基準値
  criteria <- list(
    cfi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    tli = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    nnfi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    rmsea = list(good = 0.05, acceptable = 0.08, direction = "lower"),
    srmr = list(good = 0.05, acceptable = 0.08, direction = "lower"),
    gfi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    agfi = list(good = 0.90, acceptable = 0.85, direction = "higher"),
    nfi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    ifi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
    rfi = list(good = 0.95, acceptable = 0.90, direction = "higher")
  )

  idx_lower <- tolower(index)
  if (!(idx_lower %in% names(criteria))) {
    return(list(judgment = "-", class = ""))
  }

  crit <- criteria[[idx_lower]]

  if (crit$direction == "higher") {
    if (value >= crit$good) {
      return(list(judgment = "良好", class = "fit-good"))
    } else if (value >= crit$acceptable) {
      return(list(judgment = "許容", class = "fit-acceptable"))
    } else {
      return(list(judgment = "不良", class = "fit-poor"))
    }
  } else {
    if (value <= crit$good) {
      return(list(judgment = "良好", class = "fit-good"))
    } else if (value <= crit$acceptable) {
      return(list(judgment = "許容", class = "fit-acceptable"))
    } else {
      return(list(judgment = "不良", class = "fit-poor"))
    }
  }
}

#' 適合度指標テーブルを作成
#' @param fit lavaanオブジェクト
#' @return HTMLテーブル
create_fit_table <- function(fit) {
  if (is.null(fit)) return(NULL)

  fm <- fitMeasures(fit)

  indices <- data.frame(
    指標 = c("χ²", "df", "p値", "CFI", "TLI", "RMSEA", "RMSEA 90% CI", "SRMR", "AIC", "BIC"),
    値 = c(
      sprintf("%.3f", fm["chisq"]),
      sprintf("%.0f", fm["df"]),
      sprintf("%.4f", fm["pvalue"]),
      sprintf("%.3f", fm["cfi"]),
      sprintf("%.3f", fm["tli"]),
      sprintf("%.3f", fm["rmsea"]),
      sprintf("[%.3f, %.3f]", fm["rmsea.ci.lower"], fm["rmsea.ci.upper"]),
      sprintf("%.3f", fm["srmr"]),
      sprintf("%.1f", fm["aic"]),
      sprintf("%.1f", fm["bic"])
    ),
    stringsAsFactors = FALSE
  )

  # 判定を追加
  judgments <- sapply(c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci", "srmr", "aic", "bic"), function(idx) {
    val <- fm[idx]
    eval_result <- evaluate_fit_index(idx, val)
    eval_result$judgment
  })

  indices$判定 <- judgments

  indices
}

#' パラメータ推定値テーブルを作成
#' @param fit lavaanオブジェクト
#' @param standardized 標準化するか
#' @return データフレーム
create_parameter_table <- function(fit, standardized = TRUE) {
  if (is.null(fit)) return(NULL)

  params <- parameterEstimates(fit, standardized = TRUE)

  if (standardized) {
    result <- params %>%
      select(
        lhs, op, rhs,
        推定値 = est,
        標準化 = std.all,
        標準誤差 = se,
        z値 = z,
        p値 = pvalue
      ) %>%
      mutate(
        パス = paste(lhs, op, rhs),
        across(where(is.numeric), ~round(., 3))
      ) %>%
      select(パス, 推定値, 標準化, 標準誤差, z値, p値)
  } else {
    result <- params %>%
      select(
        lhs, op, rhs,
        推定値 = est,
        標準誤差 = se,
        z値 = z,
        p値 = pvalue
      ) %>%
      mutate(
        パス = paste(lhs, op, rhs),
        across(where(is.numeric), ~round(., 3))
      ) %>%
      select(パス, 推定値, 標準誤差, z値, p値)
  }

  result
}

#' モデル構文テンプレート
model_templates <- list(
  cfa_1factor = list(
    name = "1因子確認的因子分析",
    description = "単一の潜在変数を複数の観測変数で測定",
    syntax = '# 1因子確認的因子分析モデル
# =~ は「〜によって測定される」を意味します

Factor1 =~ x1 + x2 + x3 + x4
'
  ),

  cfa_2factor = list(
    name = "2因子確認的因子分析",
    description = "2つの相関する潜在変数",
    syntax = '# 2因子確認的因子分析モデル
# 因子間の相関は自動的に推定されます

Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
'
  ),

  cfa_3factor = list(
    name = "3因子確認的因子分析",
    description = "3つの相関する潜在変数",
    syntax = '# 3因子確認的因子分析モデル

Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
Factor3 =~ x7 + x8 + x9
'
  ),

  sem_basic = list(
    name = "基本的なSEM",
    description = "因子間の回帰パスを含むモデル",
    syntax = '# 基本的なSEMモデル
# ~ は回帰関係を表します

# 測定モデル
Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
Factor3 =~ x7 + x8 + x9

# 構造モデル（因子間の回帰）
Factor3 ~ Factor1 + Factor2
'
  ),

  sem_mediation = list(
    name = "媒介モデル",
    description = "間接効果を含む媒介分析",
    syntax = '# 媒介モデル
# 間接効果の検定が可能です

# 測定モデル
X =~ x1 + x2 + x3
M =~ m1 + m2 + m3
Y =~ y1 + y2 + y3

# 構造モデル
M ~ a*X          # X → M のパス (a)
Y ~ b*M + c*X    # M → Y のパス (b), X → Y の直接効果 (c)

# 間接効果と総合効果の定義
indirect := a*b      # 間接効果
total := c + a*b     # 総合効果
'
  ),

  path_analysis = list(
    name = "パス解析",
    description = "観測変数のみを使用したパス解析",
    syntax = '# パス解析モデル
# 観測変数間の直接的な関係を分析

y1 ~ x1 + x2
y2 ~ x1 + x2 + y1
'
  ),

  higher_order = list(
    name = "高次因子モデル",
    description = "下位因子と上位因子を含む階層モデル",
    syntax = '# 高次因子モデル

# 下位因子（一次因子）
F1 =~ x1 + x2 + x3
F2 =~ x4 + x5 + x6
F3 =~ x7 + x8 + x9

# 上位因子（二次因子）
General =~ F1 + F2 + F3
'
  ),

  bifactor = list(
    name = "バイファクターモデル",
    description = "一般因子と特殊因子を同時に推定",
    syntax = '# バイファクターモデル
# orthogonal = TRUE を使用して因子を直交させる場合が多い

# 一般因子
G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9

# 特殊因子（グループ因子）
S1 =~ x1 + x2 + x3
S2 =~ x4 + x5 + x6
S3 =~ x7 + x8 + x9

# 因子間の相関を0に固定（直交）
G ~~ 0*S1
G ~~ 0*S2
G ~~ 0*S3
S1 ~~ 0*S2
S1 ~~ 0*S3
S2 ~~ 0*S3
'
  )
)

#' 基本統計量を計算
#' @param data データフレーム
#' @return データフレーム
calculate_descriptives <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols, drop = FALSE]

  if (ncol(data_numeric) == 0) {
    return(data.frame(message = "数値変数がありません"))
  }

  result <- data.frame(
    変数 = names(data_numeric),
    N = sapply(data_numeric, function(x) sum(!is.na(x))),
    欠損 = sapply(data_numeric, function(x) sum(is.na(x))),
    平均 = sapply(data_numeric, mean, na.rm = TRUE),
    標準偏差 = sapply(data_numeric, sd, na.rm = TRUE),
    最小値 = sapply(data_numeric, min, na.rm = TRUE),
    最大値 = sapply(data_numeric, max, na.rm = TRUE),
    歪度 = sapply(data_numeric, function(x) {
      x <- na.omit(x)
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      sum((x - m)^3) / (n * s^3)
    }),
    尖度 = sapply(data_numeric, function(x) {
      x <- na.omit(x)
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      sum((x - m)^4) / (n * s^4) - 3
    }),
    row.names = NULL
  )

  result[, -1] <- lapply(result[, -1], function(x) round(x, 3))
  result
}

#' モデル構文のバリデーション
#' @param syntax モデル構文
#' @return リスト（valid, message）
validate_model_syntax <- function(syntax) {
  if (is.null(syntax) || trimws(syntax) == "") {
    return(list(valid = FALSE, message = "モデル構文が入力されていません"))
  }

  tryCatch({
    # 構文解析を試みる
    parsed <- lavParseModelString(syntax)

    if (nrow(parsed) == 0) {
      return(list(valid = FALSE, message = "有効なモデル定義が見つかりません"))
    }

    return(list(valid = TRUE, message = "構文は有効です"))

  }, error = function(e) {
    return(list(valid = FALSE, message = paste("構文エラー:", e$message)))
  })
}

#' 結果のサマリーテキストを生成
#' @param fit lavaanオブジェクト
#' @return 文字列
generate_summary_text <- function(fit) {
  if (is.null(fit)) return("")

  fm <- fitMeasures(fit)

  text <- paste0(
    "=== モデル適合度サマリー ===\n\n",
    sprintf("χ² = %.3f, df = %.0f, p = %.4f\n", fm["chisq"], fm["df"], fm["pvalue"]),
    sprintf("CFI = %.3f, TLI = %.3f\n", fm["cfi"], fm["tli"]),
    sprintf("RMSEA = %.3f [%.3f, %.3f]\n", fm["rmsea"], fm["rmsea.ci.lower"], fm["rmsea.ci.upper"]),
    sprintf("SRMR = %.3f\n", fm["srmr"]),
    sprintf("AIC = %.1f, BIC = %.1f\n", fm["aic"], fm["bic"])
  )

  text
}

# =============================================================================
# 入力バリデーション関数（製品版）
# =============================================================================

#' 変数名のサニタイズ
#' @param names 変数名ベクトル
#' @return サニタイズされた変数名
sanitize_variable_names <- function(names) {
  # 特殊文字を除去
  sanitized <- gsub("[^a-zA-Z0-9_.]", "_", names)
  # 数字で始まる場合はプレフィックスを追加

  sanitized <- ifelse(grepl("^[0-9]", sanitized), paste0("V_", sanitized), sanitized)
  # 空の名前を置換
  sanitized <- ifelse(sanitized == "" | is.na(sanitized), paste0("var_", seq_along(sanitized)), sanitized)
  # 重複を解消
  make.unique(sanitized, sep = "_")
}

#' データフレームの検証
#' @param data データフレーム
#' @param max_rows 最大行数
#' @param max_cols 最大列数
#' @return リスト（valid, message, warnings）
validate_dataframe <- function(data, max_rows = 100000, max_cols = 200) {
  warnings <- character(0)

  if (is.null(data)) {
    return(list(valid = FALSE, message = "データがNULLです", warnings = warnings))
  }

  if (!is.data.frame(data)) {
    return(list(valid = FALSE, message = "データフレーム形式ではありません", warnings = warnings))
  }

  if (nrow(data) == 0) {
    return(list(valid = FALSE, message = "データが空です", warnings = warnings))
  }

  if (ncol(data) == 0) {
    return(list(valid = FALSE, message = "変数がありません", warnings = warnings))
  }

  if (nrow(data) > max_rows) {
    warnings <- c(warnings, paste0("行数が", format(max_rows, big.mark = ","), "を超えています"))
  }

  if (ncol(data) > max_cols) {
    warnings <- c(warnings, paste0("列数が", max_cols, "を超えています"))
  }

  # 数値変数の確認
  n_numeric <- sum(sapply(data, is.numeric))
  if (n_numeric < 2) {
    return(list(valid = FALSE, message = "SEM分析には最低2つの数値変数が必要です", warnings = warnings))
  }

  # 欠損値チェック
  n_missing <- sum(is.na(data))
  if (n_missing > 0) {
    pct_missing <- round(n_missing / (nrow(data) * ncol(data)) * 100, 1)
    warnings <- c(warnings, paste0("欠損値が", format(n_missing, big.mark = ","), "個 (", pct_missing, "%) あります"))
  }

  list(valid = TRUE, message = "データは有効です", warnings = warnings)
}

#' モデル識別性のチェック
#' @param n_factors 因子数
#' @param n_indicators 各因子の指標数ベクトル
#' @param n_structural 構造パス数
#' @return リスト（identified, df, message）
check_model_identification <- function(n_factors, n_indicators, n_structural = 0) {
  # 観測変数の総数
  p <- sum(n_indicators)

  # 観測される共分散/分散の数
  n_observed <- p * (p + 1) / 2

  # 推定パラメータ数の概算
  # 因子負荷量（最初の指標は1に固定と仮定）
  n_loadings <- sum(n_indicators) - n_factors
  # 因子分散
  n_factor_var <- n_factors
  # 因子間共分散（構造パスがない場合）
  n_factor_cov <- if (n_structural == 0) n_factors * (n_factors - 1) / 2 else 0
  # 残差分散
  n_residual_var <- p
  # 構造パス
  n_structural_params <- n_structural

  n_estimated <- n_loadings + n_factor_var + n_factor_cov + n_residual_var + n_structural_params

  df <- n_observed - n_estimated

  if (df < 0) {
    return(list(
      identified = FALSE,
      df = df,
      message = paste0("モデルが識別不能です（自由度: ", df, "）。指標変数を追加するか、制約を追加してください。")
    ))
  } else if (df == 0) {
    return(list(
      identified = TRUE,
      df = df,
      message = "モデルはちょうど識別されています（飽和モデル）。適合度検定はできません。"
    ))
  } else {
    return(list(
      identified = TRUE,
      df = df,
      message = paste0("モデルは過剰識別されています（自由度: ", df, "）。")
    ))
  }
}

#' 安全なファイル読み込み
#' @param file ファイルオブジェクト
#' @param max_size_mb 最大ファイルサイズ（MB）
#' @return データフレームまたはエラー
safe_read_file <- function(file, max_size_mb = 50) {
  # ファイルサイズチェック
  file_size_mb <- file$size / 1024^2
  if (file_size_mb > max_size_mb) {
    stop(paste0("ファイルサイズ（", round(file_size_mb, 1), "MB）が上限（", max_size_mb, "MB）を超えています"))
  }

  # ファイル拡張子チェック
  ext <- tolower(tools::file_ext(file$name))
  allowed_extensions <- c("csv", "tsv", "txt", "xlsx", "xls", "sav", "sas7bdat", "dta", "rds")

  if (!(ext %in% allowed_extensions)) {
    stop(paste0("サポートされていないファイル形式です: .", ext,
                "\n対応形式: ", paste(allowed_extensions, collapse = ", ")))
  }

  # データ読み込み
  read_data_file(file)
}

#' 適合度の総合解釈を生成（初心者向け）
#' @param fit lavaanオブジェクト
#' @return リスト（overall_judgment, overall_class, summary_text, details）
interpret_fit <- function(fit) {
  if (is.null(fit)) return(NULL)

  fm <- tryCatch(fitMeasures(fit), error = function(e) NULL)
  if (is.null(fm)) return(NULL)

  cfi <- fm["cfi"]
  tli <- fm["tli"]
  rmsea <- fm["rmsea"]
  srmr <- fm["srmr"]

  # 各指標の判定
  scores <- c(
    cfi = if (!is.na(cfi)) { if (cfi >= 0.95) 2 else if (cfi >= 0.90) 1 else 0 } else NA,
    tli = if (!is.na(tli)) { if (tli >= 0.95) 2 else if (tli >= 0.90) 1 else 0 } else NA,
    rmsea = if (!is.na(rmsea)) { if (rmsea <= 0.05) 2 else if (rmsea <= 0.08) 1 else 0 } else NA,
    srmr = if (!is.na(srmr)) { if (srmr <= 0.05) 2 else if (srmr <= 0.08) 1 else 0 } else NA
  )

  valid_scores <- scores[!is.na(scores)]
  if (length(valid_scores) == 0) {
    return(list(
      overall_judgment = "判定不能",
      overall_class = "",
      summary_text = "適合度指標を計算できません。",
      details = list()
    ))
  }

  avg_score <- mean(valid_scores)

  if (avg_score >= 1.5) {
    overall <- "good"
    judgment <- "良好"
    summary <- "モデルはデータに良く適合しています。分析結果は信頼できます。"
  } else if (avg_score >= 0.75) {
    overall <- "acceptable"
    judgment <- "許容範囲"
    summary <- "モデルの適合度は許容範囲内です。結果は参考にできますが、モデルの改善余地があります。"
  } else {
    overall <- "poor"
    judgment <- "要改善"
    summary <- "モデルの適合度が不十分です。修正指標を参考にモデルを改善してください。"
  }

  # 各指標の詳細解釈
  details <- list()
  if (!is.na(cfi)) {
    details$cfi <- list(
      value = sprintf("%.3f", cfi),
      eval = evaluate_fit_index("cfi", cfi),
      text = if (cfi >= 0.95) "CFI(比較適合度指標)は0.95以上で良好です。"
             else if (cfi >= 0.90) "CFI(比較適合度指標)は0.90以上で許容範囲です。0.95以上を目指しましょう。"
             else "CFI(比較適合度指標)が0.90未満です。モデルの改善が必要です。"
    )
  }
  if (!is.na(tli)) {
    details$tli <- list(
      value = sprintf("%.3f", tli),
      eval = evaluate_fit_index("tli", tli),
      text = if (tli >= 0.95) "TLI(Tucker-Lewis指標)は0.95以上で良好です。"
             else if (tli >= 0.90) "TLI(Tucker-Lewis指標)は0.90以上で許容範囲です。0.95以上を目指しましょう。"
             else "TLI(Tucker-Lewis指標)が0.90未満です。モデルの改善が必要です。"
    )
  }
  if (!is.na(rmsea)) {
    details$rmsea <- list(
      value = sprintf("%.3f", rmsea),
      eval = evaluate_fit_index("rmsea", rmsea),
      text = if (rmsea <= 0.05) "RMSEA(近似誤差平均二乗根)は0.05以下で良好です。"
             else if (rmsea <= 0.08) "RMSEA(近似誤差平均二乗根)は0.08以下で許容範囲です。0.05以下を目指しましょう。"
             else "RMSEA(近似誤差平均二乗根)が0.08を超えています。モデルの改善が必要です。"
    )
  }
  if (!is.na(srmr)) {
    details$srmr <- list(
      value = sprintf("%.3f", srmr),
      eval = evaluate_fit_index("srmr", srmr),
      text = if (srmr <= 0.05) "SRMR(標準化残差平均二乗根)は0.05以下で良好です。"
             else if (srmr <= 0.08) "SRMR(標準化残差平均二乗根)は0.08以下で許容範囲です。"
             else "SRMR(標準化残差平均二乗根)が0.08を超えています。モデルの改善が必要です。"
    )
  }

  list(
    overall_judgment = judgment,
    overall_class = overall,
    summary_text = summary,
    details = details
  )
}

#' 結果のエクスポート用フォーマット
#' @param fit lavaanオブジェクト
#' @param format 出力形式 ("html", "csv", "txt")
#' @return フォーマットされた結果
format_results_for_export <- function(fit, format = "txt") {
  if (is.null(fit)) return(NULL)

  fm <- fitMeasures(fit)
  params <- parameterEstimates(fit, standardized = TRUE)

  if (format == "txt") {
    result <- capture.output(summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))
    paste(result, collapse = "\n")
  } else if (format == "csv") {
    params
  } else {
    # HTML形式はgenerate_html_reportを使用
    NULL
  }
}
