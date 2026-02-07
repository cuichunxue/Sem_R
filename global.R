# =============================================================================
# グローバル設定
# Production Version 2.0
# =============================================================================

# オプション設定
options(
  shiny.maxRequestSize = 100 * 1024^2,  # 最大アップロードサイズ: 100MB
  scipen = 999,                          # 科学的記数法を避ける
  digits = 4,                            # 表示桁数
  shiny.sanitize.errors = TRUE           # エラーサニタイズ
)

# ロケール設定（日本語対応）
tryCatch(
  Sys.setlocale("LC_ALL", "ja_JP.UTF-8"),
  warning = function(w) {
    tryCatch(
      Sys.setlocale("LC_ALL", "Japanese_Japan.utf8"),
      warning = function(w2) NULL
    )
  }
)
