# =============================================================================
# グローバル設定
# =============================================================================

# オプション設定
options(
  shiny.maxRequestSize = 50 * 1024^2,  # 最大アップロードサイズ: 50MB（app.Rと統一）
  scipen = 999,                         # 科学的記数法を避ける
  digits = 4                            # 表示桁数
)

# ロケール設定（日本語対応、失敗しても続行）
tryCatch(
  Sys.setlocale("LC_ALL", "ja_JP.UTF-8"),
  warning = function(w) message("Note: Japanese locale not available, using system default"),
  error = function(e) message("Note: Japanese locale not available, using system default")
)
