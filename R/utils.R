# =============================================================================
# ユーティリティ関数
# Production Version 2.0
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

  /* 信頼性分析結果 */
  .reliability-result {
    background: #f8f9fa;
    border-radius: 8px;
    padding: 1.25rem;
    margin-bottom: 1rem;
    border-left: 4px solid #3498db;
  }

  .reliability-result .metric-value {
    font-size: 2rem;
    font-weight: 700;
    color: #2c3e50;
    line-height: 1;
  }

  .reliability-result .metric-label {
    font-size: 0.85rem;
    color: #6c757d;
    text-transform: uppercase;
  }

  /* 正規性テスト結果 */
  .normality-pass {
    color: #155724;
    background-color: #d4edda;
    padding: 0.2rem 0.5rem;
    border-radius: 4px;
    font-weight: 500;
  }

  .normality-fail {
    color: #721c24;
    background-color: #f8d7da;
    padding: 0.2rem 0.5rem;
    border-radius: 4px;
    font-weight: 500;
  }

  /* ワークフローステッパー */
  .workflow-stepper {
    display: flex;
    justify-content: space-between;
    padding: 1rem 0;
    margin-bottom: 1rem;
  }

  .workflow-step {
    display: flex;
    align-items: center;
    flex: 1;
    position: relative;
  }

  .workflow-step::after {
    content: "";
    flex: 1;
    height: 2px;
    background: #dee2e6;
    margin: 0 0.5rem;
  }

  .workflow-step:last-child::after {
    display: none;
  }

  .workflow-step.completed .step-circle {
    background: #18bc9c;
    color: white;
  }

  .workflow-step.active .step-circle {
    background: #3498db;
    color: white;
    box-shadow: 0 0 0 4px rgba(52, 152, 219, 0.2);
  }

  .step-circle {
    width: 36px;
    height: 36px;
    border-radius: 50%;
    background: #dee2e6;
    color: #6c757d;
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 600;
    font-size: 0.85rem;
    flex-shrink: 0;
  }

  .step-label {
    font-size: 0.75rem;
    color: #6c757d;
    margin-left: 0.5rem;
    white-space: nowrap;
  }

  /* レスポンシブ調整 */
  @media (max-width: 768px) {
    .card-header {
      font-size: 0.9rem;
    }

    .syntax-editor {
      min-height: 200px;
    }

    .workflow-stepper {
      flex-direction: column;
      gap: 0.5rem;
    }

    .workflow-step::after {
      display: none;
    }
  }
  '
}

# =============================================================================
# HTMLエスケープユーティリティ
# =============================================================================

#' HTML特殊文字のエスケープ
#' @param text エスケープする文字列
#' @return エスケープされた文字列
escape_html <- function(text) {
  if (is.null(text) || is.na(text)) return("")
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}

# =============================================================================
# データ読み込み関数
# =============================================================================

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

    as.data.frame(data)

  }, error = function(e) {
    stop("ファイル読み込みエラー: ", e$message)
  })
}

#' 安全なファイル読み込み（バリデーション付き）
#' @param file ファイルオブジェクト
#' @param max_size_mb 最大ファイルサイズ（MB）
#' @return データフレームまたはエラー
safe_read_file <- function(file, max_size_mb = 100) {
  # ファイルサイズチェック
  file_size_mb <- file$size / 1024^2
  if (file_size_mb > max_size_mb) {
    stop(paste0("ファイルサイズ（", round(file_size_mb, 1), "MB）が上限（", max_size_mb, "MB）を超えています"))
  }

  # ファイル拡張子チェック
  ext <- tolower(tools::file_ext(file$name))
  allowed_extensions <- c("csv", "tsv", "txt", "xlsx", "xls", "sav", "sas7bdat", "dta", "rds")

  if (!(ext %in% allowed_extensions)) {
    stop(paste0(
      "サポートされていないファイル形式です: .", ext,
      "\n対応形式: ", paste(paste0(".", allowed_extensions), collapse = ", ")
    ))
  }

  # データ読み込み
  data <- read_data_file(file)

  # 変数名のサニタイズ
  original_names <- names(data)
  sanitized <- sanitize_variable_names(original_names)
  if (!identical(original_names, sanitized)) {
    names(data) <- sanitized
  }

  # データフレーム検証
  validation <- validate_dataframe(data)
  if (!validation$valid) {
    stop(validation$message)
  }

  attr(data, "validation_warnings") <- validation$warnings
  data
}

# =============================================================================
# 適合度指標関数
# =============================================================================

#' 適合度指標の判定
#' @param index 指標名
#' @param value 値
#' @return リスト（判定、クラス）
evaluate_fit_index <- function(index, value) {
  if (is.na(value) || is.null(value)) {
    return(list(judgment = "-", class = ""))
  }

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
      return(list(judgment = "\u826f\u597d", class = "fit-good"))
    } else if (value >= crit$acceptable) {
      return(list(judgment = "\u8a31\u5bb9", class = "fit-acceptable"))
    } else {
      return(list(judgment = "\u4e0d\u826f", class = "fit-poor"))
    }
  } else {
    if (value <= crit$good) {
      return(list(judgment = "\u826f\u597d", class = "fit-good"))
    } else if (value <= crit$acceptable) {
      return(list(judgment = "\u8a31\u5bb9", class = "fit-acceptable"))
    } else {
      return(list(judgment = "\u4e0d\u826f", class = "fit-poor"))
    }
  }
}

#' 適合度指標テーブルを作成（R²含む）
#' @param fit lavaanオブジェクト
#' @return データフレーム
create_fit_table <- function(fit) {
  if (is.null(fit)) return(NULL)

  fm <- fitMeasures(fit)

  indices <- data.frame(
    "\u6307\u6a19" = c("\u03c7\u00b2", "df", "p\u5024",
                       "CFI", "TLI", "RMSEA", "RMSEA 90% CI",
                       "SRMR", "AIC", "BIC"),
    "\u5024" = c(
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
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # 判定を追加
  index_names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea", "srmr", "aic", "bic")
  judgments <- sapply(index_names, function(idx) {
    val <- fm[idx]
    eval_result <- evaluate_fit_index(idx, val)
    eval_result$judgment
  })

  indices[["\u5224\u5b9a"]] <- judgments
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
        est, std.all, se, z, pvalue
      ) %>%
      mutate(
        path = paste(lhs, op, rhs),
        across(where(is.numeric), ~round(., 3))
      ) %>%
      select(path, est, std.all, se, z, pvalue)
    names(result) <- c("\u30d1\u30b9", "\u63a8\u5b9a\u5024", "\u6a19\u6e96\u5316",
                       "\u6a19\u6e96\u8aa4\u5dee", "z\u5024", "p\u5024")
  } else {
    result <- params %>%
      select(
        lhs, op, rhs,
        est, se, z, pvalue
      ) %>%
      mutate(
        path = paste(lhs, op, rhs),
        across(where(is.numeric), ~round(., 3))
      ) %>%
      select(path, est, se, z, pvalue)
    names(result) <- c("\u30d1\u30b9", "\u63a8\u5b9a\u5024",
                       "\u6a19\u6e96\u8aa4\u5dee", "z\u5024", "p\u5024")
  }

  result
}

# =============================================================================
# 信頼性分析関数（新機能）
# =============================================================================

#' Cronbach's alpha を計算
#' @param data データフレーム
#' @param items 項目名のベクトル
#' @return リスト（alpha, item_stats, n）
calculate_cronbach_alpha <- function(data, items) {
  if (length(items) < 2) {
    return(list(alpha = NA, n = 0,
                message = "2つ以上の項目が必要です"))
  }

  item_data <- data[, items, drop = FALSE]
  item_data <- item_data[complete.cases(item_data), , drop = FALSE]
  n <- nrow(item_data)

  if (n < 3) {
    return(list(alpha = NA, n = n,
                message = "有効なケースが3未満です"))
  }

  k <- ncol(item_data)
  item_vars <- apply(item_data, 2, var)
  total_var <- var(rowSums(item_data))

  alpha <- (k / (k - 1)) * (1 - sum(item_vars) / total_var)

  # 項目除外時のアルファ
  alpha_if_deleted <- sapply(1:k, function(i) {
    remaining <- item_data[, -i, drop = FALSE]
    k2 <- ncol(remaining)
    item_vars2 <- apply(remaining, 2, var)
    total_var2 <- var(rowSums(remaining))
    (k2 / (k2 - 1)) * (1 - sum(item_vars2) / total_var2)
  })

  # 項目-合計相関
  item_total_cor <- sapply(1:k, function(i) {
    corrected_total <- rowSums(item_data[, -i, drop = FALSE])
    cor(item_data[, i], corrected_total)
  })

  item_stats <- data.frame(
    "\u9805\u76ee" = items,
    "\u5e73\u5747" = round(colMeans(item_data), 3),
    "\u6a19\u6e96\u504f\u5dee" = round(apply(item_data, 2, sd), 3),
    "\u9805\u76ee-\u5408\u8a08\u76f8\u95a2" = round(item_total_cor, 3),
    "\u9664\u5916\u6642\u03b1" = round(alpha_if_deleted, 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  list(
    alpha = round(alpha, 3),
    n = n,
    k = k,
    item_stats = item_stats,
    message = interpret_alpha(alpha)
  )
}

#' Alpha値の解釈
#' @param alpha アルファ値
#' @return 解釈テキスト
interpret_alpha <- function(alpha) {
  if (is.na(alpha)) return("-")
  if (alpha >= 0.9) return("\u512a\u79c0 (\u03b1 \u2265 .90)")
  if (alpha >= 0.8) return("\u826f\u597d (\u03b1 \u2265 .80)")
  if (alpha >= 0.7) return("\u8a31\u5bb9 (\u03b1 \u2265 .70)")
  if (alpha >= 0.6) return("\u7591\u554f (\u03b1 \u2265 .60)")
  return("\u4e0d\u5341\u5206 (\u03b1 < .60)")
}

#' McDonald's omega を計算（lavaan CFA ベース）
#' @param fit lavaan CFA フィットオブジェクト
#' @return omega値
calculate_omega <- function(fit) {
  tryCatch({
    params <- parameterEstimates(fit, standardized = TRUE)
    loadings <- params[params$op == "=~", ]

    if (nrow(loadings) == 0) return(NA)

    # 因子ごとにオメガを計算
    factors <- unique(loadings$lhs)
    omegas <- list()

    for (f in factors) {
      f_loadings <- loadings[loadings$lhs == f, ]
      lambda <- f_loadings$std.all
      residuals_df <- params[params$op == "~~" & params$lhs == params$rhs &
                               params$lhs %in% f_loadings$rhs, ]

      if (nrow(residuals_df) > 0) {
        theta <- 1 - lambda^2  # 標準化残差分散
        omega <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))
        omegas[[f]] <- round(omega, 3)
      }
    }

    omegas
  }, error = function(e) {
    list(error = e$message)
  })
}

# =============================================================================
# 正規性検定関数（新機能）
# =============================================================================

#' 多変量正規性検定
#' @param data データフレーム（数値のみ）
#' @return リスト（univariate, multivariate）
test_normality <- function(data) {
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  numeric_data <- numeric_data[complete.cases(numeric_data), , drop = FALSE]

  if (ncol(numeric_data) == 0 || nrow(numeric_data) < 4) {
    return(list(
      univariate = NULL,
      multivariate = NULL,
      message = "\u6b63\u898f\u6027\u691c\u5b9a\u306b\u306f4\u4ef6\u4ee5\u4e0a\u306e\u6570\u5024\u30c7\u30fc\u30bf\u304c\u5fc5\u8981\u3067\u3059"
    ))
  }

  # 単変量正規性検定 (Shapiro-Wilk)
  n <- nrow(numeric_data)
  max_n_sw <- min(n, 5000)  # Shapiro-Wilkは5000まで

  univariate <- data.frame(
    "\u5909\u6570" = names(numeric_data),
    "\u6b6a\u5ea6" = sapply(numeric_data, function(x) {
      x <- na.omit(x)
      n_x <- length(x)
      m <- mean(x)
      s <- sd(x)
      round(sum((x - m)^3) / (n_x * s^3), 3)
    }),
    "\u5c16\u5ea6" = sapply(numeric_data, function(x) {
      x <- na.omit(x)
      n_x <- length(x)
      m <- mean(x)
      s <- sd(x)
      round(sum((x - m)^4) / (n_x * s^4) - 3, 3)
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Shapiro-Wilk検定（サンプルサイズ制限あり）
  if (max_n_sw >= 3) {
    sw_results <- sapply(numeric_data, function(x) {
      x <- na.omit(x)
      if (length(x) > 5000) x <- x[1:5000]
      if (length(x) < 3) return(c(W = NA, p = NA))
      test <- shapiro.test(x)
      c(W = round(test$statistic, 4), p = round(test$p.value, 4))
    })
    univariate[["SW\u7d71\u8a08\u91cf"]] <- sw_results["W", ]
    univariate[["SW_p\u5024"]] <- sw_results["p.W", ]
    univariate[["\u6b63\u898f\u6027"]] <- ifelse(
      is.na(sw_results["p.W", ]), "-",
      ifelse(sw_results["p.W", ] >= 0.05, "\u25cb", "\u00d7")
    )
  }

  # Mardia's多変量正規性検定（簡易版）
  multivariate <- tryCatch({
    if (ncol(numeric_data) > 50 || nrow(numeric_data) < ncol(numeric_data) + 1) {
      list(message = "\u591a\u5909\u91cf\u691c\u5b9a\u306f\u5909\u6570\u6570\u304c50\u4ee5\u4e0b\u304b\u3064\u30b5\u30f3\u30d7\u30eb > \u5909\u6570\u6570\u306e\u5834\u5408\u306b\u5b9f\u884c\u53ef\u80fd\u3067\u3059")
    } else {
      p <- ncol(numeric_data)
      n_mv <- nrow(numeric_data)
      S <- cov(numeric_data)
      S_inv <- solve(S)
      centered <- as.matrix(numeric_data) - matrix(colMeans(numeric_data),
                                                    nrow = n_mv, ncol = p, byrow = TRUE)

      # Mahalanobis距離
      D <- diag(centered %*% S_inv %*% t(centered))

      # 多変量歪度
      b1p <- mean(outer(D, D, function(x, y) x * y)) / n_mv
      # 多変量尖度
      b2p <- mean(D^2)

      expected_kurtosis <- p * (p + 2)

      list(
        mardia_skewness = round(b1p, 3),
        mardia_kurtosis = round(b2p, 3),
        expected_kurtosis = round(expected_kurtosis, 3),
        kurtosis_z = round((b2p - expected_kurtosis) / sqrt(8 * p * (p + 2) / n_mv), 3),
        interpretation = if (abs((b2p - expected_kurtosis) / sqrt(8 * p * (p + 2) / n_mv)) < 1.96) {
          "\u591a\u5909\u91cf\u6b63\u898f\u6027\u306e\u4eee\u5b9a\u3092\u68c4\u5374\u3067\u304d\u307e\u305b\u3093"
        } else {
          "\u591a\u5909\u91cf\u6b63\u898f\u6027\u304b\u3089\u306e\u9038\u8131\u304c\u793a\u5506\u3055\u308c\u307e\u3059\u3002MLR\u307e\u305f\u306fWLSMV\u306e\u4f7f\u7528\u3092\u691c\u8a0e\u3057\u3066\u304f\u3060\u3055\u3044"
        }
      )
    }
  }, error = function(e) {
    list(message = paste0("\u591a\u5909\u91cf\u691c\u5b9a\u30a8\u30e9\u30fc: ", e$message))
  })

  list(
    univariate = univariate,
    multivariate = multivariate,
    n = nrow(numeric_data)
  )
}

# =============================================================================
# R² 関数（新機能）
# =============================================================================

#' R²テーブルを作成
#' @param fit lavaanオブジェクト
#' @return データフレーム
create_rsquare_table <- function(fit) {
  if (is.null(fit)) return(NULL)

  tryCatch({
    r2 <- lavInspect(fit, "rsquare")
    if (is.null(r2) || length(r2) == 0) return(NULL)

    result <- data.frame(
      "\u5909\u6570" = names(r2),
      "R\u00b2" = round(r2, 3),
      "\u8aac\u660e\u7387(%)" = round(r2 * 100, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    # R²の解釈を追加
    result[["\u89e3\u91c8"]] <- sapply(r2, function(v) {
      if (is.na(v)) return("-")
      if (v >= 0.26) return("\u5927")
      if (v >= 0.13) return("\u4e2d")
      if (v >= 0.02) return("\u5c0f")
      return("\u5fae\u5c0f")
    })

    result
  }, error = function(e) {
    NULL
  })
}

# =============================================================================
# モデル構文テンプレート
# =============================================================================

model_templates <- list(
  cfa_1factor = list(
    name = "1\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790",
    description = "\u5358\u4e00\u306e\u6f5c\u5728\u5909\u6570\u3092\u8907\u6570\u306e\u89b3\u6e2c\u5909\u6570\u3067\u6e2c\u5b9a",
    syntax = '# 1\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790\u30e2\u30c7\u30eb
# =~ \u306f\u300c\u301c\u306b\u3088\u3063\u3066\u6e2c\u5b9a\u3055\u308c\u308b\u300d\u3092\u610f\u5473\u3057\u307e\u3059

Factor1 =~ x1 + x2 + x3 + x4
'
  ),

  cfa_2factor = list(
    name = "2\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790",
    description = "2\u3064\u306e\u76f8\u95a2\u3059\u308b\u6f5c\u5728\u5909\u6570",
    syntax = '# 2\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790\u30e2\u30c7\u30eb
# \u56e0\u5b50\u9593\u306e\u76f8\u95a2\u306f\u81ea\u52d5\u7684\u306b\u63a8\u5b9a\u3055\u308c\u307e\u3059

Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
'
  ),

  cfa_3factor = list(
    name = "3\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790",
    description = "3\u3064\u306e\u76f8\u95a2\u3059\u308b\u6f5c\u5728\u5909\u6570",
    syntax = '# 3\u56e0\u5b50\u78ba\u8a8d\u7684\u56e0\u5b50\u5206\u6790\u30e2\u30c7\u30eb

Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
Factor3 =~ x7 + x8 + x9
'
  ),

  sem_basic = list(
    name = "\u57fa\u672c\u7684\u306aSEM",
    description = "\u56e0\u5b50\u9593\u306e\u56de\u5e30\u30d1\u30b9\u3092\u542b\u3080\u30e2\u30c7\u30eb",
    syntax = '# \u57fa\u672c\u7684\u306aSEM\u30e2\u30c7\u30eb
# ~ \u306f\u56de\u5e30\u95a2\u4fc2\u3092\u8868\u3057\u307e\u3059

# \u6e2c\u5b9a\u30e2\u30c7\u30eb
Factor1 =~ x1 + x2 + x3
Factor2 =~ x4 + x5 + x6
Factor3 =~ x7 + x8 + x9

# \u69cb\u9020\u30e2\u30c7\u30eb\uff08\u56e0\u5b50\u9593\u306e\u56de\u5e30\uff09
Factor3 ~ Factor1 + Factor2
'
  ),

  sem_mediation = list(
    name = "\u5a92\u4ecb\u30e2\u30c7\u30eb",
    description = "\u9593\u63a5\u52b9\u679c\u3092\u542b\u3080\u5a92\u4ecb\u5206\u6790",
    syntax = '# \u5a92\u4ecb\u30e2\u30c7\u30eb
# \u9593\u63a5\u52b9\u679c\u306e\u691c\u5b9a\u304c\u53ef\u80fd\u3067\u3059

# \u6e2c\u5b9a\u30e2\u30c7\u30eb
X =~ x1 + x2 + x3
M =~ m1 + m2 + m3
Y =~ y1 + y2 + y3

# \u69cb\u9020\u30e2\u30c7\u30eb
M ~ a*X          # X \u2192 M \u306e\u30d1\u30b9 (a)
Y ~ b*M + c*X    # M \u2192 Y \u306e\u30d1\u30b9 (b), X \u2192 Y \u306e\u76f4\u63a5\u52b9\u679c (c)

# \u9593\u63a5\u52b9\u679c\u3068\u7dcf\u5408\u52b9\u679c\u306e\u5b9a\u7fa9
indirect := a*b      # \u9593\u63a5\u52b9\u679c
total := c + a*b     # \u7dcf\u5408\u52b9\u679c
'
  ),

  path_analysis = list(
    name = "\u30d1\u30b9\u89e3\u6790",
    description = "\u89b3\u6e2c\u5909\u6570\u306e\u307f\u3092\u4f7f\u7528\u3057\u305f\u30d1\u30b9\u89e3\u6790",
    syntax = '# \u30d1\u30b9\u89e3\u6790\u30e2\u30c7\u30eb
# \u89b3\u6e2c\u5909\u6570\u9593\u306e\u76f4\u63a5\u7684\u306a\u95a2\u4fc2\u3092\u5206\u6790

y1 ~ x1 + x2
y2 ~ x1 + x2 + y1
'
  ),

  higher_order = list(
    name = "\u9ad8\u6b21\u56e0\u5b50\u30e2\u30c7\u30eb",
    description = "\u4e0b\u4f4d\u56e0\u5b50\u3068\u4e0a\u4f4d\u56e0\u5b50\u3092\u542b\u3080\u968e\u5c64\u30e2\u30c7\u30eb",
    syntax = '# \u9ad8\u6b21\u56e0\u5b50\u30e2\u30c7\u30eb

# \u4e0b\u4f4d\u56e0\u5b50\uff08\u4e00\u6b21\u56e0\u5b50\uff09
F1 =~ x1 + x2 + x3
F2 =~ x4 + x5 + x6
F3 =~ x7 + x8 + x9

# \u4e0a\u4f4d\u56e0\u5b50\uff08\u4e8c\u6b21\u56e0\u5b50\uff09
General =~ F1 + F2 + F3
'
  ),

  bifactor = list(
    name = "\u30d0\u30a4\u30d5\u30a1\u30af\u30bf\u30fc\u30e2\u30c7\u30eb",
    description = "\u4e00\u822c\u56e0\u5b50\u3068\u7279\u6b8a\u56e0\u5b50\u3092\u540c\u6642\u306b\u63a8\u5b9a",
    syntax = '# \u30d0\u30a4\u30d5\u30a1\u30af\u30bf\u30fc\u30e2\u30c7\u30eb
# orthogonal = TRUE \u3092\u4f7f\u7528\u3057\u3066\u56e0\u5b50\u3092\u76f4\u4ea4\u3055\u305b\u308b\u5834\u5408\u304c\u591a\u3044

# \u4e00\u822c\u56e0\u5b50
G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9

# \u7279\u6b8a\u56e0\u5b50\uff08\u30b0\u30eb\u30fc\u30d7\u56e0\u5b50\uff09
S1 =~ x1 + x2 + x3
S2 =~ x4 + x5 + x6
S3 =~ x7 + x8 + x9

# \u56e0\u5b50\u9593\u306e\u76f8\u95a2\u30920\u306b\u56fa\u5b9a\uff08\u76f4\u4ea4\uff09
G ~~ 0*S1
G ~~ 0*S2
G ~~ 0*S3
S1 ~~ 0*S2
S1 ~~ 0*S3
S2 ~~ 0*S3
'
  ),

  mimic = list(
    name = "MIMIC\u30e2\u30c7\u30eb",
    description = "\u5916\u90e8\u5909\u6570\u304c\u6f5c\u5728\u5909\u6570\u306b\u5f71\u97ff\u3059\u308b\u30e2\u30c7\u30eb",
    syntax = '# MIMIC\u30e2\u30c7\u30eb (Multiple Indicators Multiple Causes)
# \u5916\u90e8\u5909\u6570\uff08\u539f\u56e0\uff09\u304c\u6f5c\u5728\u5909\u6570\u306b\u5f71\u97ff

# \u6e2c\u5b9a\u30e2\u30c7\u30eb
F1 =~ x1 + x2 + x3 + x4

# \u69cb\u9020\u30e2\u30c7\u30eb\uff08\u5916\u90e8\u5909\u6570 \u2192 \u6f5c\u5728\u5909\u6570\uff09
F1 ~ cov1 + cov2
'
  )
)

# =============================================================================
# 基本統計量関数
# =============================================================================

#' 基本統計量を計算
#' @param data データフレーム
#' @return データフレーム
calculate_descriptives <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols, drop = FALSE]

  if (ncol(data_numeric) == 0) {
    return(data.frame(message = "\u6570\u5024\u5909\u6570\u304c\u3042\u308a\u307e\u305b\u3093"))
  }

  result <- data.frame(
    "\u5909\u6570" = names(data_numeric),
    "N" = sapply(data_numeric, function(x) sum(!is.na(x))),
    "\u6b20\u640d" = sapply(data_numeric, function(x) sum(is.na(x))),
    "\u5e73\u5747" = sapply(data_numeric, mean, na.rm = TRUE),
    "\u6a19\u6e96\u504f\u5dee" = sapply(data_numeric, sd, na.rm = TRUE),
    "\u6700\u5c0f\u5024" = sapply(data_numeric, min, na.rm = TRUE),
    "\u6700\u5927\u5024" = sapply(data_numeric, max, na.rm = TRUE),
    "\u6b6a\u5ea6" = sapply(data_numeric, function(x) {
      x <- na.omit(x)
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      if (s == 0) return(NA)
      sum((x - m)^3) / (n * s^3)
    }),
    "\u5c16\u5ea6" = sapply(data_numeric, function(x) {
      x <- na.omit(x)
      n <- length(x)
      m <- mean(x)
      s <- sd(x)
      if (s == 0) return(NA)
      sum((x - m)^4) / (n * s^4) - 3
    }),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result[, -1] <- lapply(result[, -1], function(x) round(x, 3))
  result
}

# =============================================================================
# バリデーション関数
# =============================================================================

#' モデル構文のバリデーション
#' @param syntax モデル構文
#' @return リスト（valid, message）
validate_model_syntax <- function(syntax) {
  if (is.null(syntax) || trimws(syntax) == "") {
    return(list(valid = FALSE, message = "\u30e2\u30c7\u30eb\u69cb\u6587\u304c\u5165\u529b\u3055\u308c\u3066\u3044\u307e\u305b\u3093"))
  }

  tryCatch({
    parsed <- lavParseModelString(syntax)
    if (nrow(parsed) == 0) {
      return(list(valid = FALSE, message = "\u6709\u52b9\u306a\u30e2\u30c7\u30eb\u5b9a\u7fa9\u304c\u898b\u3064\u304b\u308a\u307e\u305b\u3093"))
    }

    # 構文の詳細情報を返す
    n_measurement <- sum(parsed$op == "=~")
    n_regression <- sum(parsed$op == "~")
    n_covariance <- sum(parsed$op == "~~")
    n_defined <- sum(parsed$op == ":=")

    detail <- paste0(
      "\u69cb\u6587\u306f\u6709\u52b9\u3067\u3059\uff08",
      "\u6e2c\u5b9a: ", n_measurement,
      ", \u56de\u5e30: ", n_regression,
      ", \u5171\u5206\u6563: ", n_covariance,
      if (n_defined > 0) paste0(", \u5b9a\u7fa9: ", n_defined) else "",
      "\uff09"
    )

    return(list(valid = TRUE, message = detail))

  }, error = function(e) {
    return(list(valid = FALSE, message = paste("\u69cb\u6587\u30a8\u30e9\u30fc:", e$message)))
  })
}

#' 変数名のサニタイズ
#' @param names 変数名ベクトル
#' @return サニタイズされた変数名
sanitize_variable_names <- function(names) {
  sanitized <- gsub("[^a-zA-Z0-9_.]", "_", names)
  sanitized <- ifelse(grepl("^[0-9]", sanitized), paste0("V_", sanitized), sanitized)
  sanitized <- ifelse(sanitized == "" | is.na(sanitized), paste0("var_", seq_along(sanitized)), sanitized)
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
    return(list(valid = FALSE, message = "\u30c7\u30fc\u30bf\u304cNULL\u3067\u3059", warnings = warnings))
  }

  if (!is.data.frame(data)) {
    return(list(valid = FALSE, message = "\u30c7\u30fc\u30bf\u30d5\u30ec\u30fc\u30e0\u5f62\u5f0f\u3067\u306f\u3042\u308a\u307e\u305b\u3093", warnings = warnings))
  }

  if (nrow(data) == 0) {
    return(list(valid = FALSE, message = "\u30c7\u30fc\u30bf\u304c\u7a7a\u3067\u3059", warnings = warnings))
  }

  if (ncol(data) == 0) {
    return(list(valid = FALSE, message = "\u5909\u6570\u304c\u3042\u308a\u307e\u305b\u3093", warnings = warnings))
  }

  if (nrow(data) > max_rows) {
    warnings <- c(warnings, paste0("\u884c\u6570\u304c", format(max_rows, big.mark = ","), "\u3092\u8d85\u3048\u3066\u3044\u307e\u3059"))
  }

  if (ncol(data) > max_cols) {
    warnings <- c(warnings, paste0("\u5217\u6570\u304c", max_cols, "\u3092\u8d85\u3048\u3066\u3044\u307e\u3059"))
  }

  n_numeric <- sum(sapply(data, is.numeric))
  if (n_numeric < 2) {
    return(list(valid = FALSE, message = "SEM\u5206\u6790\u306b\u306f\u6700\u4f4e2\u3064\u306e\u6570\u5024\u5909\u6570\u304c\u5fc5\u8981\u3067\u3059", warnings = warnings))
  }

  n_missing <- sum(is.na(data))
  if (n_missing > 0) {
    pct_missing <- round(n_missing / (nrow(data) * ncol(data)) * 100, 1)
    warnings <- c(warnings, paste0("\u6b20\u640d\u5024\u304c", format(n_missing, big.mark = ","), "\u500b (", pct_missing, "%) \u3042\u308a\u307e\u3059"))
  }

  list(valid = TRUE, message = "\u30c7\u30fc\u30bf\u306f\u6709\u52b9\u3067\u3059", warnings = warnings)
}

#' モデル識別性のチェック
check_model_identification <- function(n_factors, n_indicators, n_structural = 0) {
  p <- sum(n_indicators)
  n_observed <- p * (p + 1) / 2
  n_loadings <- sum(n_indicators) - n_factors
  n_factor_var <- n_factors
  n_factor_cov <- if (n_structural == 0) n_factors * (n_factors - 1) / 2 else 0
  n_residual_var <- p
  n_structural_params <- n_structural

  n_estimated <- n_loadings + n_factor_var + n_factor_cov + n_residual_var + n_structural_params
  df <- n_observed - n_estimated

  if (df < 0) {
    return(list(
      identified = FALSE, df = df,
      message = paste0("\u30e2\u30c7\u30eb\u304c\u8b58\u5225\u4e0d\u80fd\u3067\u3059\uff08\u81ea\u7531\u5ea6: ", df, "\uff09\u3002\u6307\u6a19\u5909\u6570\u3092\u8ffd\u52a0\u3059\u308b\u304b\u3001\u5236\u7d04\u3092\u8ffd\u52a0\u3057\u3066\u304f\u3060\u3055\u3044\u3002")
    ))
  } else if (df == 0) {
    return(list(
      identified = TRUE, df = df,
      message = "\u30e2\u30c7\u30eb\u306f\u3061\u3087\u3046\u3069\u8b58\u5225\u3055\u308c\u3066\u3044\u307e\u3059\uff08\u98fd\u548c\u30e2\u30c7\u30eb\uff09\u3002\u9069\u5408\u5ea6\u691c\u5b9a\u306f\u3067\u304d\u307e\u305b\u3093\u3002"
    ))
  } else {
    return(list(
      identified = TRUE, df = df,
      message = paste0("\u30e2\u30c7\u30eb\u306f\u904e\u5270\u8b58\u5225\u3055\u308c\u3066\u3044\u307e\u3059\uff08\u81ea\u7531\u5ea6: ", df, "\uff09\u3002")
    ))
  }
}

# =============================================================================
# 結果サマリー・エクスポート関数
# =============================================================================

#' 結果のサマリーテキストを生成
generate_summary_text <- function(fit) {
  if (is.null(fit)) return("")

  fm <- fitMeasures(fit)

  text <- paste0(
    "=== \u30e2\u30c7\u30eb\u9069\u5408\u5ea6\u30b5\u30de\u30ea\u30fc ===\n\n",
    sprintf("\u03c7\u00b2 = %.3f, df = %.0f, p = %.4f\n", fm["chisq"], fm["df"], fm["pvalue"]),
    sprintf("CFI = %.3f, TLI = %.3f\n", fm["cfi"], fm["tli"]),
    sprintf("RMSEA = %.3f [%.3f, %.3f]\n", fm["rmsea"], fm["rmsea.ci.lower"], fm["rmsea.ci.upper"]),
    sprintf("SRMR = %.3f\n", fm["srmr"]),
    sprintf("AIC = %.1f, BIC = %.1f\n", fm["aic"], fm["bic"])
  )

  # R²を追加
  r2 <- tryCatch(lavInspect(fit, "rsquare"), error = function(e) NULL)
  if (!is.null(r2) && length(r2) > 0) {
    text <- paste0(text, "\n=== R\u00b2 ===\n")
    for (nm in names(r2)) {
      text <- paste0(text, sprintf("%s: %.3f (%.1f%%)\n", nm, r2[nm], r2[nm] * 100))
    }
  }

  text
}

#' 結果のエクスポート用フォーマット
format_results_for_export <- function(fit, format = "txt") {
  if (is.null(fit)) return(NULL)

  if (format == "txt") {
    result <- capture.output(summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))
    paste(result, collapse = "\n")
  } else if (format == "csv") {
    parameterEstimates(fit, standardized = TRUE)
  } else {
    NULL
  }
}

# =============================================================================
# パス図ヘルパー関数（重複排除）
# =============================================================================

#' semPathsの共通パラメータを生成
#' @param input Shiny inputオブジェクト
#' @return パラメータリスト
get_semplot_params <- function(input) {
  what_val <- if (is.null(input$what)) "std" else input$what
  what_param <- switch(
    what_val,
    "std" = "std", "est" = "est", "par" = "par", "nothing" = "nothing",
    "std"
  )

  list(
    what = what_param,
    whatLabels = what_param,
    layout = if (is.null(input$layout)) "tree" else input$layout,
    style = "lisrel",
    residuals = if (is.null(input$residuals)) TRUE else input$residuals,
    intercepts = if (is.null(input$intercepts)) FALSE else input$intercepts,
    thresholds = if (is.null(input$thresholds)) FALSE else input$thresholds,
    nCharNodes = 0,
    nCharEdges = 0,
    sizeMan = if (is.null(input$node_size)) 8 else input$node_size,
    sizeLat = (if (is.null(input$node_size)) 8 else input$node_size) * 1.2,
    edge.label.cex = if (is.null(input$label_size)) 1 else input$label_size,
    edge.width = if (is.null(input$edge_size)) 1 else input$edge_size,
    curve = 2,
    curvePivot = TRUE,
    mar = c(2, 2, 2, 2),
    color = list(
      lat = if (is.null(input$lat_color)) "#3498db" else input$lat_color,
      man = if (is.null(input$man_color)) "#2ecc71" else input$man_color
    ),
    border.color = "#2c3e50",
    edge.color = "#34495e",
    label.color = "#2c3e50"
  )
}

#' semPathsを描画する共通関数
#' @param fit lavaanオブジェクト
#' @param params semPathsパラメータリスト
draw_semplot <- function(fit, params) {
  tryCatch({
    do.call(semPaths, c(list(object = fit), params))
  }, error = function(e) {
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    text(0.5, 0.5,
         paste0("\u30d1\u30b9\u56f3\u306e\u751f\u6210\u4e2d\u306b\u30a8\u30e9\u30fc\u304c\u767a\u751f\u3057\u307e\u3057\u305f:\n", e$message),
         cex = 1.2, col = "#e74c3c")
  })
}

# =============================================================================
# エラーメッセージヘルパー
# =============================================================================

#' lavaan エラーメッセージを日本語に翻訳しヘルプを追加
#' @param error_msg 英語のエラーメッセージ
#' @return リスト（message, help）
translate_lavaan_error <- function(error_msg) {
  help_msg <- ""

  patterns <- list(
    list(pattern = "covariance matrix",
         help = "\u30c7\u30fc\u30bf\u306b\u554f\u984c\u304c\u3042\u308b\u53ef\u80fd\u6027\u304c\u3042\u308a\u307e\u3059\u3002\u6b20\u640d\u5024\u3084\u5916\u308c\u5024\u3092\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
    list(pattern = "not positive definite",
         help = "\u5171\u5206\u6563\u884c\u5217\u304c\u6b63\u5b9a\u5024\u3067\u306f\u3042\u308a\u307e\u305b\u3093\u3002\u5909\u6570\u9593\u306b\u5b8c\u5168\u306a\u76f8\u95a2\u304c\u306a\u3044\u304b\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
    list(pattern = "convergence",
         help = "\u30e2\u30c7\u30eb\u304c\u53ce\u675f\u3057\u307e\u305b\u3093\u3067\u3057\u305f\u3002\u30e2\u30c7\u30eb\u3092\u7c21\u7565\u5316\u3059\u308b\u304b\u3001\u958b\u59cb\u5024\u3092\u8abf\u6574\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
    list(pattern = "singular",
         help = "\u884c\u5217\u304c\u7279\u7570\u3067\u3059\u3002\u5197\u9577\u306a\u5909\u6570\u3084\u7dda\u5f62\u5f93\u5c5e\u304c\u306a\u3044\u304b\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
    list(pattern = "degrees of freedom",
         help = "\u81ea\u7531\u5ea6\u304c\u8ca0\u3067\u3059\u3002\u30e2\u30c7\u30eb\u304c\u904e\u5270\u8b58\u5225\u3055\u308c\u3066\u3044\u306a\u3044\u53ef\u80fd\u6027\u304c\u3042\u308a\u307e\u3059\u3002"),
    list(pattern = "unknown variable",
         help = "\u30e2\u30c7\u30eb\u69cb\u6587\u306e\u5909\u6570\u540d\u304c\u30c7\u30fc\u30bf\u306b\u5b58\u5728\u3057\u307e\u305b\u3093\u3002\u5909\u6570\u540d\u3092\u78ba\u8a8d\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),
    list(pattern = "sample size",
         help = "\u30b5\u30f3\u30d7\u30eb\u30b5\u30a4\u30ba\u304c\u4e0d\u5341\u5206\u3067\u3059\u3002\u30d1\u30e9\u30e1\u30fc\u30bf\u6570\u306e5\u301c10\u500d\u306e\u30b5\u30f3\u30d7\u30eb\u304c\u63a8\u5968\u3055\u308c\u307e\u3059\u3002"),
    list(pattern = "Heywood|negative variance",
         help = "Heywood\u30b1\u30fc\u30b9\uff08\u8ca0\u306e\u5206\u6563\uff09\u304c\u691c\u51fa\u3055\u308c\u307e\u3057\u305f\u3002\u30e2\u30c7\u30eb\u306e\u518d\u691c\u8a0e\u304c\u5fc5\u8981\u3067\u3059\u3002")
  )

  for (p in patterns) {
    if (grepl(p$pattern, error_msg, ignore.case = TRUE)) {
      help_msg <- p$help
      break
    }
  }

  list(message = error_msg, help = help_msg)
}

# =============================================================================
# 外れ値検出関数（新機能）
# =============================================================================

#' Mahalanobis距離による外れ値検出
#' @param data データフレーム（数値のみ）
#' @param alpha 有意水準
#' @return リスト（distances, outliers, n_outliers）
detect_outliers_mahalanobis <- function(data, alpha = 0.001) {
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  complete_data <- numeric_data[complete.cases(numeric_data), , drop = FALSE]

  if (nrow(complete_data) < ncol(complete_data) + 1) {
    return(list(
      distances = NULL,
      outliers = NULL,
      n_outliers = 0,
      message = "\u30b5\u30f3\u30d7\u30eb\u6570\u304c\u5909\u6570\u6570\u3088\u308a\u5c11\u306a\u3044\u305f\u3081\u3001\u5916\u308c\u5024\u691c\u51fa\u304c\u3067\u304d\u307e\u305b\u3093"
    ))
  }

  tryCatch({
    center <- colMeans(complete_data)
    cov_mat <- cov(complete_data)
    distances <- mahalanobis(complete_data, center, cov_mat)

    # カイ二乗分布の臨界値
    p <- ncol(complete_data)
    critical_value <- qchisq(1 - alpha, df = p)

    outlier_indices <- which(distances > critical_value)

    result <- data.frame(
      "\u884c\u756a\u53f7" = 1:nrow(complete_data),
      "Mahalanobis\u8ddd\u96e2" = round(distances, 3),
      "\u81e8\u754c\u5024" = round(critical_value, 3),
      "\u5916\u308c\u5024" = ifelse(distances > critical_value, "\u25cf", ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    list(
      distances = result,
      outlier_indices = outlier_indices,
      n_outliers = length(outlier_indices),
      critical_value = critical_value,
      message = paste0(
        length(outlier_indices), "\u4ef6\u306e\u5916\u308c\u5024\u304c\u691c\u51fa\u3055\u308c\u307e\u3057\u305f",
        "(\u03b1 = ", alpha, ", \u81e8\u754c\u5024 = ", round(critical_value, 3), ")"
      )
    )
  }, error = function(e) {
    list(
      distances = NULL,
      outlier_indices = integer(0),
      n_outliers = 0,
      message = paste0("\u5916\u308c\u5024\u691c\u51fa\u30a8\u30e9\u30fc: ", e$message)
    )
  })
}
