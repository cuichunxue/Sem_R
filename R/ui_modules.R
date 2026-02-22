# =============================================================================
# UI モジュール
# =============================================================================

# -----------------------------------------------------------------------------
# データタブ UI
# -----------------------------------------------------------------------------
data_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = tags$span(tags$i(class = "fas fa-upload me-2"), "データ読み込み"),

      # ファイルアップロード
      fileInput(
        ns("file_upload"),
        label = NULL,
        accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".sav", ".sas7bdat", ".dta", ".rds"),
        placeholder = "ファイルを選択...",
        buttonLabel = tags$span(tags$i(class = "fas fa-folder-open me-1"), "参照")
      ),

      tags$p(class = "help-text",
        "対応形式: CSV, Excel, SPSS, SAS, Stata, RDS"
      ),

      hr(),

      # サンプルデータ
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-flask"),
        "サンプルデータ"
      ),

      selectInput(
        ns("sample_data"),
        label = NULL,
        choices = c(
          "選択してください" = "",
          "HolzingerSwineford1939 (CFA)" = "hs",
          "PoliticalDemocracy (SEM)" = "pd",
          "カスタムサンプル" = "custom"
        )
      ),

      actionButton(
        ns("load_sample"),
        label = tags$span(tags$i(class = "fas fa-download me-1"), "読み込み"),
        class = "btn-outline-primary w-100 mt-2"
      ),

      hr(),

      # データ情報
      conditionalPanel(
        condition = sprintf("output['%s'] !== null", ns("data_loaded")),
        ns = ns,

        tags$div(
          class = "section-title",
          tags$i(class = "fas fa-info-circle"),
          "データ情報"
        ),

        uiOutput(ns("data_info"))
      )
    ),

    # メインコンテンツ
    navset_card_tab(
      id = ns("data_tabs"),
      title = tags$span(tags$i(class = "fas fa-table me-2"), "データビューア"),

      nav_panel(
        title = "データプレビュー",
        icon = icon("eye"),
        card_body(
          class = "p-0",
          DTOutput(ns("data_preview"))
        )
      ),

      nav_panel(
        title = "基本統計量",
        icon = icon("calculator"),
        card_body(
          DTOutput(ns("descriptives"))
        )
      ),

      nav_panel(
        title = "相関行列",
        icon = icon("th"),
        card_body(
          fluidRow(
            column(4,
              selectInput(
                ns("cor_method"),
                "相関係数",
                choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")
              )
            ),
            column(4,
              selectInput(
                ns("cor_display"),
                "表示形式",
                choices = c("ヒートマップ" = "heatmap", "テーブル" = "table")
              )
            ),
            column(4,
              checkboxInput(ns("cor_sig"), "有意性表示", value = TRUE)
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'heatmap'", ns("cor_display")),
            plotOutput(ns("cor_plot"), height = "500px")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", ns("cor_display")),
            DTOutput(ns("cor_table"))
          )
        )
      ),

      nav_panel(
        title = "欠損値",
        icon = icon("question-circle"),
        card_body(
          plotOutput(ns("missing_plot"), height = "300px"),
          hr(),
          DTOutput(ns("missing_table"))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# モデル定義タブ UI
# -----------------------------------------------------------------------------
model_ui <- function(id) {
  ns <- NS(id)

  div(
    # 入力方法の選択
    navset_card_pill(
      id = ns("model_input_method"),
      title = tags$span(tags$i(class = "fas fa-edit me-2"), "モデル定義方法"),

      # --- GUIビルダータブ ---
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-mouse-pointer me-1"), "GUIビルダー"),
        value = "gui",
        card_body(
          # クイックスタートボタン
          tags$div(
            class = "mb-3 p-2 bg-light rounded",
            fluidRow(
              column(8,
                tags$span(class = "small text-muted me-2",
                  tags$i(class = "fas fa-bolt me-1"), "クイックスタート:"
                ),
                actionButton(
                  ns("quick_2factor"),
                  "2因子CFA",
                  class = "btn-sm btn-outline-primary me-1"
                ),
                actionButton(
                  ns("quick_3factor"),
                  "3因子CFA",
                  class = "btn-sm btn-outline-primary me-1"
                ),
                actionButton(
                  ns("quick_mediation"),
                  "媒介分析",
                  class = "btn-sm btn-outline-info me-1"
                ),
                actionButton(
                  ns("clear_all_factors"),
                  tags$span(tags$i(class = "fas fa-eraser me-1"), "クリア"),
                  class = "btn-sm btn-outline-secondary"
                )
              ),
              column(4,
                uiOutput(ns("auto_setup_button"))
              )
            )
          ),
          fluidRow(
            # 左: 因子定義
            column(6,
              card(
                card_header(
                  class = "d-flex justify-content-between align-items-center py-2",
                  tags$span(tags$i(class = "fas fa-layer-group me-2"), "因子定義（測定モデル）"),
                  actionButton(
                    ns("add_factor"),
                    tags$span(tags$i(class = "fas fa-plus me-1"), "因子追加"),
                    class = "btn-sm btn-success"
                  )
                ),
                card_body(
                  style = "max-height: 550px; overflow-y: auto;",
                  # 変数フィルター
                  tags$div(
                    class = "mb-2",
                    fluidRow(
                      column(8,
                        textInput(
                          ns("var_filter"),
                          NULL,
                          placeholder = "変数をフィルター（例: x, item）",
                          width = "100%"
                        )
                      ),
                      column(4,
                        actionButton(
                          ns("clear_filter"),
                          tags$i(class = "fas fa-times"),
                          class = "btn-sm btn-outline-secondary w-100 mt-1"
                        )
                      )
                    )
                  ),
                  tags$p(class = "text-muted small mb-2",
                    tags$i(class = "fas fa-info-circle me-1"),
                    "各因子に3つ以上の指標変数を選択することを推奨"
                  ),
                  uiOutput(ns("factor_definitions")),
                  uiOutput(ns("factor_validation"))
                )
              )
            ),

            # 右: 構造モデル
            column(6,
              card(
                card_header(
                  class = "py-2",
                  tags$span(tags$i(class = "fas fa-arrows-alt me-2"), "構造モデル（因子間の関係）")
                ),
                card_body(
                  tags$div(
                    class = "section-title mb-2",
                    tags$i(class = "fas fa-arrow-right"),
                    "回帰パス（→）"
                  ),
                  uiOutput(ns("structural_paths")),

                  hr(),

                  tags$div(
                    class = "section-title mb-2",
                    tags$i(class = "fas fa-arrows-alt-h"),
                    "共分散（↔）"
                  ),
                  uiOutput(ns("covariance_paths"))
                )
              ),

              # 追加オプション
              card(
                class = "mt-3",
                card_header(
                  class = "py-2",
                  tags$span(tags$i(class = "fas fa-cog me-2"), "追加オプション")
                ),
                card_body(
                  class = "py-2",
                  checkboxInput(
                    ns("equal_loadings"),
                    tags$span(
                      "因子負荷量を等値制約",
                      tags$i(class = "fas fa-question-circle ms-1 text-muted",
                        title = "各因子内の全ての因子負荷量を同じ値に制約します（τ等価モデル）")
                    ),
                    value = FALSE
                  ),
                  checkboxInput(
                    ns("add_indirect_effect"),
                    tags$span(
                      "間接効果を計算",
                      tags$i(class = "fas fa-question-circle ms-1 text-muted",
                        title = "媒介分析の間接効果（a*b）を定義パラメータとして追加します")
                    ),
                    value = FALSE
                  )
                )
              ),

              # 生成ボタン
              actionButton(
                ns("generate_syntax"),
                tags$span(tags$i(class = "fas fa-magic me-2"), "構文を生成"),
                class = "btn-primary btn-lg w-100 mt-3"
              )
            )
          ),

          hr(),

          # モデルサマリーと生成されたプレビュー
          fluidRow(
            column(4,
              card(
                card_header(
                  class = "py-2 bg-light",
                  tags$span(tags$i(class = "fas fa-chart-pie me-2"), "モデルサマリー")
                ),
                card_body(
                  uiOutput(ns("model_summary"))
                )
              )
            ),
            column(8,
              card(
                card_header(
                  class = "py-2",
                  tags$span(tags$i(class = "fas fa-code me-2"), "生成された構文プレビュー")
                ),
                card_body(
                  verbatimTextOutput(ns("generated_preview")),
                  tags$div(
                    class = "mt-2 d-flex justify-content-end",
                    actionButton(
                      ns("copy_syntax"),
                      tags$span(tags$i(class = "fas fa-copy me-1"), "コピー"),
                      class = "btn-sm btn-outline-secondary me-2"
                    ),
                    actionButton(
                      ns("apply_generated"),
                      tags$span(tags$i(class = "fas fa-check me-1"), "この構文を使用"),
                      class = "btn-sm btn-success"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # --- テンプレートタブ ---
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-copy me-1"), "テンプレート"),
        value = "template",
        card_body(
          fluidRow(
            column(4,
              # テンプレート選択
              radioGroupButtons(
                ns("template_type"),
                label = "カテゴリ",
                choices = c(
                  "CFA" = "cfa",
                  "SEM" = "sem",
                  "パス" = "path",
                  "高度" = "advanced"
                ),
                status = "primary",
                justified = TRUE,
                size = "sm"
              ),
              hr(),
              uiOutput(ns("template_cards"))
            ),
            column(8,
              tags$div(
                class = "section-title mb-2",
                tags$i(class = "fas fa-list"),
                "利用可能な変数（クリックで追加）"
              ),
              uiOutput(ns("variable_chips")),
              hr(),
              tags$textarea(
                id = ns("template_syntax"),
                class = "form-control syntax-editor",
                rows = 12,
                placeholder = "左のテンプレートを選択するか、直接入力してください"
              ),
              actionButton(
                ns("apply_template"),
                tags$span(tags$i(class = "fas fa-check me-2"), "この構文を使用"),
                class = "btn-primary w-100 mt-3"
              )
            )
          )
        )
      ),

      # --- 直接入力タブ ---
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-keyboard me-1"), "直接入力"),
        value = "direct",
        card_body(
          fluidRow(
            column(8,
              tags$textarea(
                id = ns("model_syntax"),
                class = "form-control syntax-editor",
                rows = 18,
                placeholder = "# lavaan モデル構文を直接入力\n\n# 確認的因子分析の例:\n# Factor1 =~ x1 + x2 + x3\n# Factor2 =~ x4 + x5 + x6\n\n# 回帰の例:\n# y ~ x1 + x2\n\n# 共分散の例:\n# x1 ~~ x2"
              ),
              fluidRow(
                column(6,
                  actionButton(
                    ns("validate_syntax"),
                    tags$span(tags$i(class = "fas fa-check me-2"), "構文チェック"),
                    class = "btn-outline-primary w-100 mt-2"
                  )
                ),
                column(6,
                  actionButton(
                    ns("clear_syntax"),
                    tags$span(tags$i(class = "fas fa-eraser me-2"), "クリア"),
                    class = "btn-outline-secondary w-100 mt-2"
                  )
                )
              ),
              uiOutput(ns("syntax_validation"))
            ),
            column(4,
              card(
                card_header(class = "py-2", "構文リファレンス"),
                card_body(
                  class = "small",
                  tags$table(
                    class = "table table-sm table-bordered mb-0",
                    tags$tbody(
                      tags$tr(tags$td(tags$code("=~")), tags$td("測定")),
                      tags$tr(tags$td(tags$code("~")), tags$td("回帰")),
                      tags$tr(tags$td(tags$code("~~")), tags$td("共分散")),
                      tags$tr(tags$td(tags$code("~1")), tags$td("切片")),
                      tags$tr(tags$td(tags$code(":=")), tags$td("定義パラメータ")),
                      tags$tr(tags$td(tags$code("*")), tags$td("ラベル"))
                    )
                  )
                )
              ),
              tags$div(
                class = "section-title mt-3 mb-2",
                tags$i(class = "fas fa-list"),
                "変数リスト"
              ),
              uiOutput(ns("variable_list"))
            )
          )
        )
      )
    ),

    # 現在のモデル構文表示
    card(
      class = "mt-3",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span(tags$i(class = "fas fa-code me-2"), "現在のモデル構文"),
        uiOutput(ns("syntax_status_badge"))
      ),
      card_body(
        verbatimTextOutput(ns("current_syntax_display")),
        tags$p(class = "text-muted small mt-2",
          "この構文が「推定設定」タブで使用されます。")
      )
    )
  )
}

# -----------------------------------------------------------------------------
# 推定設定タブ UI
# -----------------------------------------------------------------------------
estimation_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(4, 8),

    # 左カラム: 設定
    card(
      card_header(
        tags$span(tags$i(class = "fas fa-sliders-h me-2"), "推定オプション")
      ),
      card_body(
        # 推定方法
        selectInput(
          ns("estimator"),
          tags$span(
            "推定方法",
            tags$i(class = "fas fa-info-circle ms-1 text-muted",
                   title = "データ特性に応じて選択してください")
          ),
          choices = c(
            "最尤法 (ML)" = "ML",
            "ロバスト最尤法 (MLR)" = "MLR",
            "最尤法・平均補正 (MLM)" = "MLM",
            "重み付き最小二乗 (WLS)" = "WLS",
            "ロバストWLS (WLSMV)" = "WLSMV",
            "一般化最小二乗 (GLS)" = "GLS",
            "不偏最小二乗 (ULS)" = "ULS"
          ),
          selected = "ML"
        ),

        hr(),

        # 欠損値処理
        selectInput(
          ns("missing"),
          "欠損値処理",
          choices = c(
            "リストワイズ削除" = "listwise",
            "ペアワイズ削除" = "pairwise",
            "完全情報最尤法 (FIML)" = "fiml"
          ),
          selected = "listwise"
        ),

        hr(),

        # 標準誤差
        selectInput(
          ns("se"),
          "標準誤差",
          choices = c(
            "標準" = "standard",
            "ロバスト (Huber-White)" = "robust.huber.white",
            "ロバスト (サンドイッチ)" = "robust.sem",
            "ブートストラップ" = "bootstrap"
          ),
          selected = "standard"
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'bootstrap'", ns("se")),
          numericInput(
            ns("bootstrap_n"),
            "ブートストラップ回数",
            value = 1000,
            min = 100,
            max = 10000,
            step = 100
          )
        ),

        hr(),

        # 追加オプション
        tags$div(
          class = "section-title",
          tags$i(class = "fas fa-cog"),
          "追加オプション"
        ),

        checkboxInput(
          ns("std_lv"),
          "潜在変数の分散を1に固定",
          value = FALSE
        ),

        checkboxInput(
          ns("fixed_x"),
          "外生変数を固定",
          value = TRUE
        ),

        checkboxInput(
          ns("meanstructure"),
          "平均構造を推定",
          value = FALSE
        ),

        checkboxInput(
          ns("orthogonal"),
          "因子を直交化",
          value = FALSE
        ),

        hr(),

        # 実行ボタン
        actionButton(
          ns("run_analysis"),
          tags$span(
            tags$i(class = "fas fa-play me-2"),
            "分析を実行"
          ),
          class = "btn-primary btn-lg w-100",
          style = "font-size: 1.1rem;"
        )
      )
    ),

    # 右カラム: ステータス
    div(
      # ステータスカード
      card(
        card_header(
          class = "card-header-success",
          tags$span(tags$i(class = "fas fa-tasks me-2"), "分析ステータス")
        ),
        card_body(
          uiOutput(ns("analysis_status"))
        )
      ),

      # プレビューカード
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-eye me-2"), "設定プレビュー")
        ),
        card_body(
          uiOutput(ns("settings_preview"))
        )
      ),

      # 警告・エラー
      uiOutput(ns("warnings_errors"))
    )
  )
}

# -----------------------------------------------------------------------------
# 結果タブ UI
# -----------------------------------------------------------------------------
results_ui <- function(id) {
  ns <- NS(id)

  div(
    # 適合度サマリーカード
    fluidRow(
      column(12,
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              tags$i(class = "fas fa-chart-line me-2"),
              "モデル適合度"
            ),
            downloadButton(
              ns("download_results"),
              "結果をダウンロード",
              class = "btn-sm btn-outline-light"
            )
          ),
          card_body(
            uiOutput(ns("fit_summary_cards")),
            hr(),
            DTOutput(ns("fit_indices_table"))
          )
        )
      )
    ),

    # パラメータ推定値
    fluidRow(
      column(12,
        navset_card_tab(
          title = tags$span(tags$i(class = "fas fa-table me-2"), "パラメータ推定値"),

          nav_panel(
            title = "測定モデル",
            icon = icon("ruler"),
            card_body(
              checkboxInput(ns("std_loadings"), "標準化係数を表示", value = TRUE),
              DTOutput(ns("loadings_table"))
            )
          ),

          nav_panel(
            title = "構造モデル",
            icon = icon("arrows-alt"),
            card_body(
              checkboxInput(ns("std_regressions"), "標準化係数を表示", value = TRUE),
              DTOutput(ns("regressions_table"))
            )
          ),

          nav_panel(
            title = "共分散/相関",
            icon = icon("link"),
            card_body(
              DTOutput(ns("covariances_table"))
            )
          ),

          nav_panel(
            title = "分散",
            icon = icon("chart-bar"),
            card_body(
              DTOutput(ns("variances_table"))
            )
          ),

          nav_panel(
            title = "定義パラメータ",
            icon = icon("tag"),
            card_body(
              DTOutput(ns("defined_table"))
            )
          ),

          nav_panel(
            title = "全パラメータ",
            icon = icon("list"),
            card_body(
              checkboxInput(ns("std_all"), "標準化係数を表示", value = TRUE),
              DTOutput(ns("all_params_table"))
            )
          )
        )
      )
    ),

    # R²（決定係数）
    fluidRow(
      column(12,
        card(
          card_header(
            tags$span(tags$i(class = "fas fa-bullseye me-2"), "R²（決定係数）")
          ),
          card_body(
            DTOutput(ns("rsquare_table"))
          )
        )
      )
    ),

    # 修正指標
    fluidRow(
      column(12,
        card(
          card_header(
            tags$span(tags$i(class = "fas fa-wrench me-2"), "修正指標")
          ),
          card_body(
            fluidRow(
              column(4,
                numericInput(
                  ns("mi_threshold"),
                  "閾値 (MI > )",
                  value = 4,
                  min = 0,
                  step = 0.5
                )
              ),
              column(4,
                selectInput(
                  ns("mi_sort"),
                  "並び替え",
                  choices = c("MI降順" = "mi_desc", "EPC降順" = "epc_desc")
                )
              )
            ),
            DTOutput(ns("modification_indices"))
          )
        )
      )
    ),

    # Raw出力
    fluidRow(
      column(12,
        card(
          card_header(
            tags$span(tags$i(class = "fas fa-terminal me-2"), "lavaan 詳細出力")
          ),
          card_body(
            verbatimTextOutput(ns("lavaan_summary"))
          )
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# パス図タブ UI
# -----------------------------------------------------------------------------
diagram_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = tags$span(tags$i(class = "fas fa-paint-brush me-2"), "表示設定"),

      # レイアウト
      selectInput(
        ns("layout"),
        "レイアウト",
        choices = c(
          "ツリー" = "tree",
          "ツリー2" = "tree2",
          "ツリー3" = "tree3",
          "円形" = "circle",
          "円形2" = "circle2",
          "スプリング" = "spring"
        ),
        selected = "tree"
      ),

      # 係数表示
      selectInput(
        ns("what"),
        "表示する値",
        choices = c(
          "標準化係数" = "std",
          "非標準化係数" = "est",
          "パラメータ名" = "par",
          "なし" = "nothing"
        ),
        selected = "std"
      ),

      hr(),

      # 色設定
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-palette"),
        "色設定"
      ),

      colourInput(
        ns("lat_color"),
        "潜在変数",
        value = "#3498db"
      ),

      colourInput(
        ns("man_color"),
        "観測変数",
        value = "#2ecc71"
      ),

      hr(),

      # サイズ設定
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-expand"),
        "サイズ設定"
      ),

      sliderInput(
        ns("node_size"),
        "ノードサイズ",
        min = 3,
        max = 15,
        value = 8,
        step = 1
      ),

      sliderInput(
        ns("edge_size"),
        "エッジサイズ",
        min = 0.5,
        max = 3,
        value = 1,
        step = 0.1
      ),

      sliderInput(
        ns("label_size"),
        "ラベルサイズ",
        min = 0.5,
        max = 2,
        value = 1,
        step = 0.1
      ),

      hr(),

      # その他オプション
      checkboxInput(
        ns("residuals"),
        "残差を表示",
        value = TRUE
      ),

      checkboxInput(
        ns("intercepts"),
        "切片を表示",
        value = FALSE
      ),

      checkboxInput(
        ns("thresholds"),
        "閾値を表示",
        value = FALSE
      ),

      hr(),

      # ダウンロード
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-download"),
        "ダウンロード"
      ),

      fluidRow(
        column(6,
          numericInput(ns("download_width"), "幅", value = 10, min = 5, max = 20)
        ),
        column(6,
          numericInput(ns("download_height"), "高さ", value = 8, min = 5, max = 20)
        )
      ),

      selectInput(
        ns("download_format"),
        "形式",
        choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg")
      ),

      downloadButton(
        ns("download_diagram"),
        "パス図をダウンロード",
        class = "btn-primary w-100"
      )
    ),

    # メインコンテンツ
    card(
      card_header(
        tags$span(tags$i(class = "fas fa-diagram-project me-2"), "パス図")
      ),
      card_body(
        class = "diagram-container",
        plotOutput(ns("sem_diagram"), height = "600px")
      )
    )
  )
}

# -----------------------------------------------------------------------------
# モデル比較タブ UI
# -----------------------------------------------------------------------------
comparison_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = tags$span(tags$i(class = "fas fa-save me-2"), "モデル管理"),

      # 現在のモデルを保存
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-plus-circle"),
        "モデルを保存"
      ),

      textInput(
        ns("model_name"),
        "モデル名",
        placeholder = "例: Model 1 (3因子)"
      ),

      textAreaInput(
        ns("model_description"),
        "説明（任意）",
        rows = 2,
        placeholder = "モデルの特徴やメモ"
      ),

      actionButton(
        ns("save_model"),
        tags$span(tags$i(class = "fas fa-save me-2"), "現在のモデルを保存"),
        class = "btn-primary w-100 mt-2"
      ),

      hr(),

      # 保存済みモデル一覧
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-list"),
        "保存済みモデル"
      ),

      uiOutput(ns("saved_models_list")),

      hr(),

      # 比較設定
      tags$div(
        class = "section-title",
        tags$i(class = "fas fa-balance-scale"),
        "比較設定"
      ),

      checkboxGroupInput(
        ns("compare_indices"),
        "表示する指標",
        choices = c(
          "χ²" = "chisq",
          "df" = "df",
          "CFI" = "cfi",
          "TLI" = "tli",
          "RMSEA" = "rmsea",
          "SRMR" = "srmr",
          "AIC" = "aic",
          "BIC" = "bic"
        ),
        selected = c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
      ),

      hr(),

      # 一括操作
      actionButton(
        ns("clear_all_models"),
        tags$span(tags$i(class = "fas fa-trash me-2"), "全モデルをクリア"),
        class = "btn-outline-danger w-100"
      )
    ),

    # メインコンテンツ
    div(
      # 比較テーブル
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(
            tags$i(class = "fas fa-table me-2"),
            "モデル適合度比較"
          ),
          downloadButton(
            ns("download_comparison"),
            "比較表をダウンロード",
            class = "btn-sm btn-outline-light"
          )
        ),
        card_body(
          uiOutput(ns("no_models_message")),
          DTOutput(ns("comparison_table"))
        )
      ),

      # カイ二乗差検定
      card(
        card_header(
          tags$span(
            tags$i(class = "fas fa-calculator me-2"),
            "ネストモデル比較（χ²差検定）"
          )
        ),
        card_body(
          fluidRow(
            column(5,
              selectInput(
                ns("model_1"),
                "モデル1（制約モデル）",
                choices = NULL
              )
            ),
            column(2,
              tags$div(
                class = "text-center pt-4",
                tags$i(class = "fas fa-arrows-alt-h fa-2x text-muted")
              )
            ),
            column(5,
              selectInput(
                ns("model_2"),
                "モデル2（自由モデル）",
                choices = NULL
              )
            )
          ),
          actionButton(
            ns("run_chisq_diff"),
            tags$span(tags$i(class = "fas fa-play me-2"), "χ²差検定を実行"),
            class = "btn-info"
          ),
          hr(),
          uiOutput(ns("chisq_diff_result"))
        )
      ),

      # 適合度指標の視覚的比較
      card(
        card_header(
          tags$span(
            tags$i(class = "fas fa-chart-bar me-2"),
            "適合度指標の視覚的比較"
          )
        ),
        card_body(
          fluidRow(
            column(6,
              selectInput(
                ns("plot_index"),
                "表示する指標",
                choices = c(
                  "CFI" = "cfi",
                  "TLI" = "tli",
                  "RMSEA" = "rmsea",
                  "SRMR" = "srmr",
                  "AIC" = "aic",
                  "BIC" = "bic"
                ),
                selected = "cfi"
              )
            ),
            column(6,
              checkboxInput(
                ns("show_threshold"),
                "基準線を表示",
                value = TRUE
              )
            )
          ),
          plotOutput(ns("comparison_plot"), height = "350px")
        )
      ),

      # モデル詳細比較
      card(
        card_header(
          tags$span(
            tags$i(class = "fas fa-search-plus me-2"),
            "パラメータ比較"
          )
        ),
        card_body(
          fluidRow(
            column(6,
              selectInput(
                ns("param_model_1"),
                "モデル1",
                choices = NULL
              )
            ),
            column(6,
              selectInput(
                ns("param_model_2"),
                "モデル2",
                choices = NULL
              )
            )
          ),
          DTOutput(ns("parameter_comparison"))
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# ヘルプタブ UI
# -----------------------------------------------------------------------------
help_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6),

    # 左カラム
    div(
      # クイックスタート
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-rocket me-2"), "クイックスタート")
        ),
        card_body(
          tags$ol(
            tags$li(
              tags$strong("データを読み込む"),
              tags$p("「データ」タブでCSV、Excel、SPSSなどのファイルをアップロードするか、サンプルデータを選択します。")
            ),
            tags$li(
              tags$strong("モデルを定義する"),
              tags$p("「モデル定義」タブでlavaanの構文を使ってモデルを記述します。テンプレートも利用できます。")
            ),
            tags$li(
              tags$strong("推定設定を行う"),
              tags$p("「推定設定」タブで推定方法や欠損値処理などのオプションを選択します。")
            ),
            tags$li(
              tags$strong("分析を実行する"),
              tags$p("「分析を実行」ボタンをクリックしてSEM分析を実行します。")
            ),
            tags$li(
              tags$strong("結果を確認する"),
              tags$p("「結果」タブで適合度指標やパラメータ推定値を確認します。")
            ),
            tags$li(
              tags$strong("パス図を確認する"),
              tags$p("「パス図」タブでモデルを視覚的に確認し、画像としてダウンロードできます。")
            )
          )
        )
      ),

      # 推定方法ガイド
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-cogs me-2"), "推定方法ガイド")
        ),
        card_body(
          tags$table(
            class = "table table-sm table-striped",
            tags$thead(
              tags$tr(
                tags$th("推定方法"),
                tags$th("用途")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$code("ML")),
                tags$td("標準的な最尤法。正規分布を仮定。")
              ),
              tags$tr(
                tags$td(tags$code("MLR")),
                tags$td("ロバスト最尤法。非正規データに推奨。")
              ),
              tags$tr(
                tags$td(tags$code("MLM")),
                tags$td("Satorra-Bentler補正。非正規データ向け。")
              ),
              tags$tr(
                tags$td(tags$code("WLSMV")),
                tags$td("カテゴリカル/順序データに最適。")
              ),
              tags$tr(
                tags$td(tags$code("GLS")),
                tags$td("一般化最小二乗法。大規模データ向け。")
              )
            )
          )
        )
      )
    ),

    # 右カラム
    div(
      # 適合度指標
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-chart-bar me-2"), "適合度指標の目安")
        ),
        card_body(
          tags$table(
            class = "table table-sm table-bordered",
            tags$thead(
              tags$tr(
                tags$th("指標"),
                tags$th("良好"),
                tags$th("許容")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("CFI"),
                tags$td(class = "fit-good", "≥ 0.95"),
                tags$td(class = "fit-acceptable", "≥ 0.90")
              ),
              tags$tr(
                tags$td("TLI"),
                tags$td(class = "fit-good", "≥ 0.95"),
                tags$td(class = "fit-acceptable", "≥ 0.90")
              ),
              tags$tr(
                tags$td("RMSEA"),
                tags$td(class = "fit-good", "≤ 0.05"),
                tags$td(class = "fit-acceptable", "≤ 0.08")
              ),
              tags$tr(
                tags$td("SRMR"),
                tags$td(class = "fit-good", "≤ 0.05"),
                tags$td(class = "fit-acceptable", "≤ 0.08")
              )
            )
          ),
          tags$p(
            class = "small text-muted mt-2",
            "※ これらは一般的な目安であり、研究分野や状況によって異なる基準が適用される場合があります。"
          )
        )
      ),

      # lavaan構文
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-code me-2"), "lavaan 構文詳細")
        ),
        card_body(
          tags$h6("モデル定義例"),
          tags$pre(
            class = "bg-dark text-light p-3 rounded",
            style = "font-size: 12px;",
'# 確認的因子分析 (CFA)
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9

# 構造モデル (回帰)
speed ~ visual + textual

# 共分散
visual ~~ textual

# 分散
x1 ~~ x1

# ラベル付きパラメータ
F1 =~ a*x1 + b*x2 + c*x3

# 等値制約
F1 =~ a*x1 + a*x2 + a*x3

# 定義されたパラメータ (間接効果など)
indirect := a*b
'
          )
        )
      ),

      # トラブルシューティング
      card(
        card_header(
          tags$span(tags$i(class = "fas fa-exclamation-triangle me-2"), "トラブルシューティング")
        ),
        card_body(
          tags$h6("よくある問題と解決策"),
          tags$dl(
            tags$dt("モデルが収束しない"),
            tags$dd("開始値を変更するか、モデルを簡略化してください。"),

            tags$dt("負の分散が推定された"),
            tags$dd("モデルの特定に問題がある可能性があります。制約を追加するか、モデルを再検討してください。"),

            tags$dt("適合度が悪い"),
            tags$dd("修正指標を参考にモデルを改善するか、理論的な見直しを行ってください。"),

            tags$dt("非正規データでの推定"),
            tags$dd("MLRまたはWLSMVの使用を検討してください。")
          )
        )
      )
    )
  )
}
