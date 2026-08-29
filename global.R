# =============================================================================
# グローバル設定
# =============================================================================

# オプション設定
options(
  scipen = 999,
  digits = 4
)

# ロケール設定（日本語対応、失敗しても続行）
tryCatch(
  Sys.setlocale("LC_ALL", "ja_JP.UTF-8"),
  warning = function(w) message("Note: Japanese locale not available, using system default"),
  error = function(e) message("Note: Japanese locale not available, using system default")
)
