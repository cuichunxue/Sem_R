# =============================================================================
# サーバーモジュール
# =============================================================================

# -----------------------------------------------------------------------------
# データモジュール サーバー
# -----------------------------------------------------------------------------
data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 数値データのリアクティブキャッシュ ---
    numeric_data <- reactive({
      req(rv$data)
      rv$data[, sapply(rv$data, is.numeric), drop = FALSE]
    })

    # --- ウェルカムバナー（データ未読込時のみ表示）---
    output$welcome_banner <- renderUI({
      if (!is.null(rv$data)) return(NULL)

      tags$div(
        class = "welcome-card",
        tags$h2("SEM分析を始めましょう!"),
        tags$p(
          "構造方程式モデリング（SEM）の分析ツールへようこそ。",
          "初めての方は、まずデモで体験してみてください。"
        ),
        fluidRow(
          column(6,
            actionButton(
              ns("run_demo"),
              tags$span(
                tags$i(class = "fas fa-play me-2"),
                "ワンクリックデモ（データ読込 → モデル設定 → 分析実行）"
              ),
              class = "demo-btn w-100"
            )
          ),
          column(6,
            tags$div(
              class = "ps-3",
              style = "color: rgba(255,255,255,0.9);",
              tags$p(class = "mb-1", tags$i(class = "fas fa-check me-2"), "サンプルデータを自動読込"),
              tags$p(class = "mb-1", tags$i(class = "fas fa-check me-2"), "3因子CFAモデルを自動設定"),
              tags$p(class = "mb-0", tags$i(class = "fas fa-check me-2"), "分析を自動実行して結果を表示")
            )
          )
        )
      )
    })

    # --- ワンクリックデモ ---
    observeEvent(input$run_demo, {
      waiter <- Waiter$new(
        html = tagList(
          spin_fading_circles(),
          tags$h4("デモを準備中...", class = "text-white mt-3"),
          tags$p("サンプルデータ読み込み → モデル設定 → 分析実行", class = "text-white-50")
        ),
        color = "rgba(44, 62, 80, 0.9)"
      )
      waiter$show()

      tryCatch({
        # Step 1: サンプルデータ読み込み
        rv$data <- as.data.frame(HolzingerSwineford1939)
        rv$data_name <- "HolzingerSwineford1939"

        # Step 2: モデル設定
        rv$model_syntax <- paste(
          "# 3因子確認的因子分析モデル（自動設定）",
          "visual  =~ x1 + x2 + x3",
          "textual =~ x4 + x5 + x6",
          "speed   =~ x7 + x8 + x9",
          sep = "\n"
        )

        # Step 3: 分析実行
        fit <- sem(
          model = rv$model_syntax,
          data = rv$data,
          estimator = "ML"
        )

        rv$fit <- fit
        rv$fit_summary <- summary(fit, standardized = TRUE, fit.measures = TRUE)
        rv$estimation_complete <- TRUE
        rv$error_message <- NULL
        rv$error_help <- NULL

        waiter$hide()

        showNotification(
          tags$div(
            tags$strong("デモ完了!"),
            tags$br(),
            "サンプルデータの3因子CFAを実行しました。",
            tags$br(),
            "「結果」タブで分析結果を確認できます。"
          ),
          type = "message",
          duration = 8
        )

        # 結果タブに移動
        rv$navigate_to <- "results_tab"

      }, error = function(e) {
        waiter$hide()
        showNotification(
          paste("デモエラー:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # --- 次のステップへ（モデル定義タブへ移動）---
    observeEvent(input$goto_model, {
      rv$navigate_to <- "model_tab"
    })

    # --- ファイルアップロード ---
    observeEvent(input$file_upload, {
      req(input$file_upload)

      tryCatch({
        waiter <- Waiter$new(
          html = tagList(
            spin_fading_circles(),
            tags$h4("データを読み込んでいます...", class = "text-white mt-3")
          ),
          color = "rgba(44, 62, 80, 0.8)"
        )
        waiter$show()

        rv$data <- read_data_file(input$file_upload)
        rv$data_name <- input$file_upload$name

        waiter$hide()

        showNotification(
          paste0("データを読み込みました: ", nrow(rv$data), " 行 × ", ncol(rv$data), " 列"),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        waiter$hide()
        showNotification(
          paste("エラー:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    # --- サンプルデータ読み込み ---
    observeEvent(input$load_sample, {
      req(input$sample_data)

      tryCatch({
        data <- switch(
          input$sample_data,
          "hs" = HolzingerSwineford1939,
          "pd" = PoliticalDemocracy,
          "custom" = create_custom_sample(),
          NULL
        )

        if (!is.null(data)) {
          rv$data <- as.data.frame(data)
          rv$data_name <- switch(
            input$sample_data,
            "hs" = "HolzingerSwineford1939",
            "pd" = "PoliticalDemocracy",
            "custom" = "カスタムサンプルデータ"
          )

          showNotification(
            paste0("サンプルデータを読み込みました: ", nrow(rv$data), " 行 × ", ncol(rv$data), " 列"),
            type = "message",
            duration = 5
          )
        }

      }, error = function(e) {
        showNotification(
          paste("エラー:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    # --- データ読み込み状態 ---
    output$data_loaded <- reactive({
      !is.null(rv$data)
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    # --- データ情報 ---
    output$data_info <- renderUI({
      req(rv$data)

      n_numeric <- ncol(numeric_data())
      n_missing <- sum(is.na(rv$data))

      tags$div(
        tags$p(
          tags$i(class = "fas fa-file me-2"),
          tags$strong("ファイル: "), rv$data_name
        ),
        tags$p(
          tags$i(class = "fas fa-rows me-2"),
          tags$strong("行数: "), format(nrow(rv$data), big.mark = ",")
        ),
        tags$p(
          tags$i(class = "fas fa-columns me-2"),
          tags$strong("列数: "), as.character(ncol(rv$data))
        ),
        tags$p(
          tags$i(class = "fas fa-hashtag me-2"),
          tags$strong("数値変数: "), as.character(n_numeric)
        ),
        if (n_missing > 0) {
          tags$p(
            class = "text-warning",
            tags$i(class = "fas fa-exclamation-triangle me-2"),
            tags$strong("欠損値: "), format(n_missing, big.mark = ",")
          )
        }
      )
    })

    # --- データプレビュー ---
    output$data_preview <- renderDT({
      req(rv$data)

      datatable(
        rv$data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          language = list(
            search = "検索:",
            lengthMenu = "_MENU_ 件表示",
            info = "_START_ - _END_ / _TOTAL_ 件",
            paginate = list(
              first = "最初",
              last = "最後",
              previous = "前",
              `next` = "次"
            )
          )
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        class = 'display compact'
      )
    })

    # --- 基本統計量 ---
    output$descriptives <- renderDT({
      req(rv$data)

      desc <- calculate_descriptives(rv$data)

      datatable(
        desc,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = c("平均", "標準偏差", "最小値", "最大値", "歪度", "尖度"), digits = 3)
    })

    # --- 相関行列プロット ---
    output$cor_plot <- renderPlot({
      req(rv$data)

      num_data <- numeric_data()
      req(ncol(num_data) >= 2)

      cor_matrix <- cor(num_data, use = "pairwise.complete.obs", method = input$cor_method)

      # corrplot 共通パラメータ
      cor_params <- list(
        corr = cor_matrix,
        method = "color",
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45,
        addCoef.col = "black",
        number.cex = 0.7,
        col = colorRampPalette(c("#e74c3c", "white", "#3498db"))(200)
      )

      # 有意性検定（ベクトル化で高速化）
      if (input$cor_sig) {
        nc <- ncol(num_data)
        p_matrix <- matrix(NA, nrow = nc, ncol = nc)
        for (i in 1:(nc - 1)) {
          for (j in (i + 1):nc) {
            test <- cor.test(num_data[[i]], num_data[[j]], method = input$cor_method)
            p_matrix[i, j] <- test$p.value
            p_matrix[j, i] <- test$p.value
          }
        }
        cor_params$p.mat <- p_matrix
        cor_params$sig.level <- 0.05
        cor_params$insig <- "label_sig"
        cor_params$pch.cex <- 1.5
      }

      do.call(corrplot, cor_params)
    })

    # --- 相関行列テーブル ---
    output$cor_table <- renderDT({
      req(rv$data)

      num_data <- numeric_data()
      req(ncol(num_data) >= 2)

      cor_matrix <- cor(num_data, use = "pairwise.complete.obs", method = input$cor_method)
      cor_df <- as.data.frame(round(cor_matrix, 3))
      cor_df <- cbind(変数 = rownames(cor_df), cor_df)

      datatable(
        cor_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    })

    # --- 欠損値プロット ---
    output$missing_plot <- renderPlot({
      req(rv$data)

      missing_counts <- colSums(is.na(rv$data))
      missing_df <- data.frame(
        variable = names(missing_counts),
        missing = missing_counts,
        stringsAsFactors = FALSE
      )
      missing_df <- missing_df[order(-missing_df$missing), ]
      missing_df <- missing_df[missing_df$missing > 0, ]

      if (nrow(missing_df) == 0) {
        plot.new()
        text(0.5, 0.5, "欠損値はありません", cex = 1.5, col = "#18bc9c")
        return()
      }

      missing_df$variable <- factor(missing_df$variable, levels = missing_df$variable)

      ggplot(missing_df, aes(x = variable, y = missing)) +
        geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
        geom_text(aes(label = missing), vjust = -0.5, size = 3.5) +
        labs(x = "変数", y = "欠損数", title = "変数別欠損値数") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold")
        )
    })

    # --- 欠損値テーブル ---
    output$missing_table <- renderDT({
      req(rv$data)

      missing_counts <- colSums(is.na(rv$data))
      total <- nrow(rv$data)

      missing_df <- data.frame(
        変数 = names(missing_counts),
        欠損数 = missing_counts,
        欠損率 = round(missing_counts / total * 100, 2),
        有効数 = total - missing_counts,
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      datatable(
        missing_df,
        options = list(
          pageLength = 15,
          order = list(list(1, 'desc'))
        ),
        rownames = FALSE
      )
    })

    return(rv)
  })
}

# --- カスタムサンプルデータ作成 ---
create_custom_sample <- function() {
  set.seed(123)
  n <- 300

  # 潜在変数の生成
  F1 <- rnorm(n)
  F2 <- 0.5 * F1 + rnorm(n, sd = 0.866)
  F3 <- 0.3 * F1 + 0.4 * F2 + rnorm(n, sd = 0.8)

  # 観測変数の生成
  data.frame(
    x1 = 0.8 * F1 + rnorm(n, sd = 0.6),
    x2 = 0.7 * F1 + rnorm(n, sd = 0.714),
    x3 = 0.75 * F1 + rnorm(n, sd = 0.661),
    x4 = 0.85 * F2 + rnorm(n, sd = 0.527),
    x5 = 0.7 * F2 + rnorm(n, sd = 0.714),
    x6 = 0.8 * F2 + rnorm(n, sd = 0.6),
    x7 = 0.75 * F3 + rnorm(n, sd = 0.661),
    x8 = 0.8 * F3 + rnorm(n, sd = 0.6),
    x9 = 0.7 * F3 + rnorm(n, sd = 0.714)
  )
}

# -----------------------------------------------------------------------------
# モデルモジュール サーバー
# -----------------------------------------------------------------------------
model_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- ステップインジケーター ---
    output$model_step_indicator <- renderUI({
      data_done <- !is.null(rv$data)
      model_done <- !is.null(rv$model_syntax) && trimws(rv$model_syntax) != ""
      analysis_done <- isTRUE(rv$estimation_complete)
      step_indicator_ui(active_step = 2, data_done = data_done, model_done = model_done, analysis_done = analysis_done)
    })

    # --- 次のステップボタン ---
    output$goto_estimation_btn <- renderUI({
      if (!is.null(rv$model_syntax) && trimws(rv$model_syntax) != "") {
        tags$div(
          class = "next-step-banner",
          tags$div(
            tags$div(class = "next-step-text", "モデル設定完了!"),
            tags$div(class = "next-step-hint", "次は分析を実行しましょう")
          ),
          actionButton(
            ns("goto_estimation"),
            tags$span(tags$i(class = "fas fa-arrow-right me-1"), "推定設定へ"),
            class = "btn-primary btn-sm"
          )
        )
      }
    })

    # --- 推定設定タブへ移動 ---
    observeEvent(input$goto_estimation, {
      rv$navigate_to <- "estimation_tab"
    })

    # --- ローカルリアクティブ値 ---
    local_rv <- reactiveValues(
      factors = list(),
      factor_counter = 0,
      generated_syntax = ""
    )

    # =========================================================================
    # GUIビルダー機能
    # =========================================================================

    # --- 因子追加 ---
    observeEvent(input$add_factor, {
      local_rv$factor_counter <- local_rv$factor_counter + 1
      factor_id <- paste0("F", local_rv$factor_counter)

      local_rv$factors[[factor_id]] <- list(
        name = factor_id,
        indicators = character(0)
      )
    })

    # --- クイックスタート: 2因子CFA ---
    observeEvent(input$quick_2factor, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 2

      local_rv$factors[["F1"]] <- list(name = "Factor1", indicators = character(0))
      local_rv$factors[["F2"]] <- list(name = "Factor2", indicators = character(0))

      showNotification("2因子CFAモデルを作成しました。各因子に指標変数を選択してください。", type = "message")
    })

    # --- クイックスタート: 3因子CFA ---
    observeEvent(input$quick_3factor, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 3

      local_rv$factors[["F1"]] <- list(name = "Factor1", indicators = character(0))
      local_rv$factors[["F2"]] <- list(name = "Factor2", indicators = character(0))
      local_rv$factors[["F3"]] <- list(name = "Factor3", indicators = character(0))

      showNotification("3因子CFAモデルを作成しました。各因子に指標変数を選択してください。", type = "message")
    })

    # --- クイックスタート: 媒介分析 ---
    observeEvent(input$quick_mediation, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 3

      local_rv$factors[["F1"]] <- list(name = "X", indicators = character(0))
      local_rv$factors[["F2"]] <- list(name = "M", indicators = character(0))
      local_rv$factors[["F3"]] <- list(name = "Y", indicators = character(0))

      showNotification(
        "媒介分析モデル（X→M→Y）を作成しました。各因子に指標変数を選択し、構造パスでM←XとY←M、Y←Xを設定してください。",
        type = "message",
        duration = 8
      )
    })

    # --- 全因子クリア ---
    observeEvent(input$clear_all_factors, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 0
      local_rv$generated_syntax <- ""

      showNotification("全ての因子をクリアしました", type = "message")
    })

    # --- サンプルデータ自動設定ボタン表示 ---
    output$auto_setup_button <- renderUI({
      req(rv$data_name)

      # サンプルデータの場合のみボタンを表示
      if (rv$data_name == "HolzingerSwineford1939") {
        actionButton(
          ns("auto_setup_hs"),
          tags$span(tags$i(class = "fas fa-magic me-1"), "HS1939を自動設定"),
          class = "btn-sm btn-success w-100"
        )
      } else if (rv$data_name == "PoliticalDemocracy") {
        actionButton(
          ns("auto_setup_pd"),
          tags$span(tags$i(class = "fas fa-magic me-1"), "PDを自動設定"),
          class = "btn-sm btn-success w-100"
        )
      } else {
        NULL
      }
    })

    # --- HolzingerSwineford1939 自動設定 ---
    observeEvent(input$auto_setup_hs, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 3

      local_rv$factors[["F1"]] <- list(name = "visual", indicators = c("x1", "x2", "x3"))
      local_rv$factors[["F2"]] <- list(name = "textual", indicators = c("x4", "x5", "x6"))
      local_rv$factors[["F3"]] <- list(name = "speed", indicators = c("x7", "x8", "x9"))

      showNotification(
        "HolzingerSwineford1939の典型的なCFAモデルを設定しました（visual, textual, speed）",
        type = "message",
        duration = 5
      )
    })

    # --- PoliticalDemocracy 自動設定 ---
    observeEvent(input$auto_setup_pd, {
      local_rv$factors <- list()
      local_rv$factor_counter <- 3

      local_rv$factors[["F1"]] <- list(name = "ind60", indicators = c("x1", "x2", "x3"))
      local_rv$factors[["F2"]] <- list(name = "dem60", indicators = c("y1", "y2", "y3", "y4"))
      local_rv$factors[["F3"]] <- list(name = "dem65", indicators = c("y5", "y6", "y7", "y8"))

      showNotification(
        "PoliticalDemocracyの典型的なSEMモデルを設定しました（ind60→dem60→dem65）。構造パスも設定してください。",
        type = "message",
        duration = 8
      )
    })

    # --- 変数フィルタークリア ---
    observeEvent(input$clear_filter, {
      updateTextInput(session, "var_filter", value = "")
    })

    # --- 因子定義UI ---
    output$factor_definitions <- renderUI({
      if (is.null(rv$data)) {
        return(tags$div(
          class = "alert alert-info",
          tags$i(class = "fas fa-info-circle me-2"),
          "まずデータを読み込んでください"
        ))
      }

      factors <- local_rv$factors
      all_vars <- names(rv$data)[sapply(rv$data, is.numeric)]

      # フィルター適用
      filter_text <- input$var_filter
      if (!is.null(filter_text) && trimws(filter_text) != "") {
        vars <- all_vars[grepl(filter_text, all_vars, ignore.case = TRUE)]
      } else {
        vars <- all_vars
      }

      if (length(factors) == 0) {
        return(tags$div(
          class = "text-center text-muted py-4",
          tags$i(class = "fas fa-plus-circle fa-2x mb-2"),
          tags$p("「因子追加」ボタンで因子を追加してください")
        ))
      }

      tagList(
        lapply(names(factors), function(fid) {
          f <- factors[[fid]]
          n_indicators <- length(f$indicators)

          # 指標数に応じた色を設定
          border_color <- if (n_indicators == 0) "#e74c3c" else if (n_indicators < 3) "#f39c12" else "#18bc9c"
          badge_class <- if (n_indicators == 0) "bg-danger" else if (n_indicators < 3) "bg-warning" else "bg-success"

          tags$div(
            class = "card mb-3 factor-card",
            style = paste0("border-left: 4px solid ", border_color, ";"),
            tags$div(
              class = "card-body py-2 px-3",
              # 因子名入力とバッジ
              fluidRow(
                column(6,
                  textInput(
                    ns(paste0("factor_name_", fid)),
                    NULL,
                    value = f$name,
                    placeholder = "因子名を入力"
                  )
                ),
                column(3,
                  tags$div(
                    class = "pt-2 text-center",
                    tags$span(class = paste("badge", badge_class),
                      paste0(n_indicators, "個選択")
                    )
                  )
                ),
                column(3,
                  actionButton(
                    ns(paste0("delete_factor_", fid)),
                    tags$i(class = "fas fa-trash"),
                    class = "btn-sm btn-outline-danger w-100 mt-1",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                      ns("delete_factor"), fid
                    )
                  )
                )
              ),
              # 指標変数選択
              tags$label(class = "small text-muted", "指標変数を選択（クリックで追加/削除）:"),
              checkboxGroupInput(
                ns(paste0("indicators_", fid)),
                label = NULL,
                choices = vars,
                selected = f$indicators,
                inline = TRUE
              )
            )
          )
        })
      )
    })

    # --- 因子削除 ---
    observeEvent(input$delete_factor, {
      fid <- input$delete_factor
      if (fid %in% names(local_rv$factors)) {
        local_rv$factors[[fid]] <- NULL
      }
    })

    # --- 因子名の同期 ---
    observe({
      factors <- isolate(local_rv$factors)
      for (fid in names(factors)) {
        # 因子名の同期
        name_input <- input[[paste0("factor_name_", fid)]]
        if (!is.null(name_input) && name_input != "" && name_input != factors[[fid]]$name) {
          local_rv$factors[[fid]]$name <- name_input
        }

        # 指標変数の同期（NULLの場合は既存値を保持）
        indicators_input <- input[[paste0("indicators_", fid)]]
        if (!is.null(indicators_input)) {
          current_indicators <- factors[[fid]]$indicators
          if (is.null(current_indicators)) current_indicators <- character(0)

          # フィルターで非表示の変数も保持するため、既存の選択とマージ
          all_vars <- if (!is.null(rv$data)) names(rv$data)[sapply(rv$data, is.numeric)] else character(0)
          filter_text <- input$var_filter
          if (!is.null(filter_text) && trimws(filter_text) != "") {
            hidden_vars <- all_vars[!grepl(filter_text, all_vars, ignore.case = TRUE)]
            hidden_selected <- intersect(current_indicators, hidden_vars)
            indicators_input <- unique(c(indicators_input, hidden_selected))
          }

          # 値が変わった場合のみ更新（不要な再描画を防ぐ）
          if (!setequal(indicators_input, current_indicators)) {
            local_rv$factors[[fid]]$indicators <- indicators_input
          }
        }
      }
    })

    # --- 構造モデル（回帰パス）UI ---
    output$structural_paths <- renderUI({
      factors <- local_rv$factors

      if (length(factors) < 2) {
        return(tags$p(class = "text-muted small", "2つ以上の因子を定義すると、回帰パスを設定できます"))
      }

      factor_names <- sapply(factors, function(f) f$name)

      # 全ての組み合わせを生成
      pairs <- expand.grid(from = factor_names, to = factor_names, stringsAsFactors = FALSE)
      pairs <- pairs[pairs$from != pairs$to, ]

      # チェックボックスの選択肢を作成
      choices <- setNames(
        paste0(pairs$to, " ~ ", pairs$from),
        paste0(pairs$to, " ← ", pairs$from)
      )

      checkboxGroupInput(
        ns("structural_paths_selected"),
        label = NULL,
        choices = choices,
        selected = character(0)
      )
    })

    # --- 共分散パスUI ---
    output$covariance_paths <- renderUI({
      factors <- local_rv$factors

      if (length(factors) < 2) {
        return(tags$p(class = "text-muted small", "2つ以上の因子を定義すると、共分散を設定できます"))
      }

      factor_names <- sapply(factors, function(f) f$name)

      # 共分散は対称なので、半分の組み合わせ
      pairs <- combn(factor_names, 2, simplify = FALSE)

      # チェックボックスの選択肢を作成
      choices <- setNames(
        sapply(pairs, function(p) paste0(p[1], " ~~ 0*", p[2])),
        sapply(pairs, function(p) paste0(p[1], " ↔ ", p[2], " を0に固定"))
      )

      tags$div(
        tags$p(class = "small text-muted", "※ 因子間の共分散はデフォルトで推定されます。チェックすると0に固定します。"),
        checkboxGroupInput(
          ns("covariance_paths_selected"),
          label = NULL,
          choices = choices,
          selected = character(0)
        )
      )
    })

    # --- 構文生成 ---
    observeEvent(input$generate_syntax, {
      factors <- local_rv$factors

      if (length(factors) == 0) {
        showNotification("因子を1つ以上定義してください", type = "error")
        return()
      }

      # 測定モデル生成
      measurement_lines <- character(0)
      equal_loadings <- isTRUE(input$equal_loadings)

      for (f in factors) {
        if (length(f$indicators) > 0) {
          if (equal_loadings && length(f$indicators) > 1) {
            # 等値制約: 全ての負荷量に同じラベルをつける
            label <- paste0("l_", gsub("[^a-zA-Z0-9]", "", f$name))
            labeled_indicators <- paste0(label, "*", f$indicators)
            line <- paste0(f$name, " =~ ", paste(labeled_indicators, collapse = " + "))
          } else {
            line <- paste0(f$name, " =~ ", paste(f$indicators, collapse = " + "))
          }
          measurement_lines <- c(measurement_lines, line)
        }
      }

      if (length(measurement_lines) == 0) {
        showNotification("各因子に少なくとも1つの指標変数を選択してください", type = "error")
        return()
      }

      # 構造モデル（回帰）生成 - checkboxGroupInputから取得
      structural_lines <- input$structural_paths_selected
      if (is.null(structural_lines)) structural_lines <- character(0)

      # 間接効果計算用にラベル付きの構造パスを生成
      labeled_structural_lines <- character(0)
      indirect_effect_lines <- character(0)

      if (isTRUE(input$add_indirect_effect) && length(structural_lines) >= 2) {
        # パスにラベルを付与
        path_labels <- list()
        for (i in seq_along(structural_lines)) {
          path <- structural_lines[i]
          label <- letters[i]
          labeled_structural_lines <- c(labeled_structural_lines, gsub("~", paste0("~ ", label, "*"), path))
          path_labels[[path]] <- label
        }

        # 3因子媒介モデル（X→M→Y）の場合、間接効果を計算
        if (length(factors) == 3 && length(structural_lines) >= 2) {
          # a*b形式の間接効果を追加
          if (length(path_labels) >= 2) {
            labels <- unlist(path_labels)
            indirect_effect_lines <- c(indirect_effect_lines,
              paste0("indirect := ", labels[1], "*", labels[2]))
            if (length(labels) >= 3) {
              indirect_effect_lines <- c(indirect_effect_lines,
                paste0("total := ", labels[3], " + ", labels[1], "*", labels[2]))
            }
          }
        }

        structural_lines <- labeled_structural_lines
      }

      # 共分散制約（0に固定）生成 - checkboxGroupInputから取得
      covariance_lines <- input$covariance_paths_selected
      if (is.null(covariance_lines)) covariance_lines <- character(0)

      # 構文を組み立て
      syntax_parts <- character(0)

      syntax_parts <- c(syntax_parts, "# 測定モデル")
      syntax_parts <- c(syntax_parts, measurement_lines)

      if (length(structural_lines) > 0) {
        syntax_parts <- c(syntax_parts, "", "# 構造モデル")
        syntax_parts <- c(syntax_parts, structural_lines)
      }

      if (length(covariance_lines) > 0) {
        syntax_parts <- c(syntax_parts, "", "# 共分散制約")
        syntax_parts <- c(syntax_parts, covariance_lines)
      }

      if (length(indirect_effect_lines) > 0) {
        syntax_parts <- c(syntax_parts, "", "# 間接効果・総合効果")
        syntax_parts <- c(syntax_parts, indirect_effect_lines)
      }

      generated <- paste(syntax_parts, collapse = "\n")
      local_rv$generated_syntax <- generated
      rv$model_syntax <- generated

      showNotification("構文を生成しました！", type = "message")
    })

    # --- モデルサマリー表示 ---
    output$model_summary <- renderUI({
      factors <- local_rv$factors
      structural <- input$structural_paths_selected
      covariance <- input$covariance_paths_selected

      n_factors <- length(factors)
      n_indicators <- sum(sapply(factors, function(f) length(f$indicators)))
      n_structural <- if (is.null(structural)) 0 else length(structural)
      n_covariance <- if (is.null(covariance)) 0 else length(covariance)

      if (n_factors == 0) {
        return(tags$div(
          class = "text-center text-muted py-3",
          tags$i(class = "fas fa-info-circle fa-2x mb-2"),
          tags$p("因子を追加してモデルを構築してください")
        ))
      }

      # モデルタイプの判定
      model_type <- if (n_structural > 0) {
        if (n_factors == 3 && n_structural >= 2) "媒介分析（SEM）"
        else "構造方程式モデル（SEM）"
      } else {
        if (n_factors == 1) "1因子確認的因子分析"
        else paste0(n_factors, "因子確認的因子分析")
      }

      # パラメータ数の概算
      n_loadings <- n_indicators
      n_factor_var <- n_factors
      n_residual_var <- n_indicators
      n_factor_cov <- if (n_factors > 1) choose(n_factors, 2) - n_covariance else 0
      total_params <- n_loadings + n_structural + n_factor_var + n_residual_var + n_factor_cov

      tags$div(
        tags$table(
          class = "table table-sm table-borderless mb-2",
          tags$tbody(
            tags$tr(
              tags$td(tags$i(class = "fas fa-tag me-2 text-primary")),
              tags$td(tags$strong("モデルタイプ")),
              tags$td(model_type)
            ),
            tags$tr(
              tags$td(tags$i(class = "fas fa-layer-group me-2 text-info")),
              tags$td("因子数"),
              tags$td(as.character(n_factors))
            ),
            tags$tr(
              tags$td(tags$i(class = "fas fa-th-list me-2 text-success")),
              tags$td("指標変数"),
              tags$td(as.character(n_indicators))
            ),
            tags$tr(
              tags$td(tags$i(class = "fas fa-arrow-right me-2 text-warning")),
              tags$td("回帰パス"),
              tags$td(as.character(n_structural))
            ),
            tags$tr(
              tags$td(tags$i(class = "fas fa-calculator me-2 text-secondary")),
              tags$td("推定パラメータ"),
              tags$td(paste0("約 ", total_params, " 個"))
            )
          )
        ),
        if (n_indicators < n_factors * 3) {
          tags$div(
            class = "small text-warning",
            tags$i(class = "fas fa-exclamation-triangle me-1"),
            "識別のため各因子に3+指標を推奨"
          )
        }
      )
    })

    # --- 因子バリデーション表示 ---
    output$factor_validation <- renderUI({
      factors <- local_rv$factors

      if (length(factors) == 0) return(NULL)

      warnings <- character(0)

      for (f in factors) {
        n_indicators <- length(f$indicators)
        if (n_indicators == 0) {
          warnings <- c(warnings, paste0("「", f$name, "」に指標変数が選択されていません"))
        } else if (n_indicators < 3) {
          warnings <- c(warnings, paste0("「", f$name, "」の指標変数が", n_indicators, "個です（推奨: 3個以上）"))
        }
      }

      if (length(warnings) > 0) {
        tags$div(
          class = "alert alert-warning mt-3 py-2",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          tags$strong("確認事項:"),
          tags$ul(
            class = "mb-0 mt-1",
            lapply(warnings, function(w) tags$li(w))
          )
        )
      }
    })

    # --- 生成プレビュー ---
    output$generated_preview <- renderText({
      if (local_rv$generated_syntax == "") {
        return("（ここに生成された構文が表示されます）")
      }
      local_rv$generated_syntax
    })

    # --- 構文をコピー（クリップボードにコピーするためのJSを発火） ---
    observeEvent(input$copy_syntax, {
      if (local_rv$generated_syntax != "") {
        # JavaScript経由でクリップボードにコピー
        session$sendCustomMessage("copyToClipboard", local_rv$generated_syntax)
        showNotification("構文をクリップボードにコピーしました", type = "message")
      } else {
        showNotification("コピーする構文がありません。まず「構文を生成」をクリックしてください。", type = "warning")
      }
    })

    # --- 生成した構文を適用 ---
    observeEvent(input$apply_generated, {
      if (local_rv$generated_syntax != "") {
        rv$model_syntax <- local_rv$generated_syntax
        showNotification("構文を適用しました。「推定設定」タブで分析を実行できます。", type = "message")
      } else {
        showNotification("適用する構文がありません。まず「構文を生成」をクリックしてください。", type = "warning")
      }
    })

    # =========================================================================
    # テンプレート機能
    # =========================================================================

    # --- テンプレートカード ---
    output$template_cards <- renderUI({
      template_type <- if (is.null(input$template_type)) "cfa" else input$template_type
      templates <- switch(
        template_type,
        "cfa" = model_templates[c("cfa_1factor", "cfa_2factor", "cfa_3factor")],
        "sem" = model_templates[c("sem_basic", "sem_mediation")],
        "path" = model_templates[c("path_analysis")],
        "advanced" = model_templates[c("higher_order", "bifactor")],
        model_templates[1:3]
      )

      tagList(
        lapply(names(templates), function(key) {
          t <- templates[[key]]
          tags$div(
            class = "card template-card mb-2",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("select_template"), key
            ),
            tags$div(
              class = "card-body py-2 px-3",
              tags$h6(class = "card-title mb-1", t$name),
              tags$small(class = "text-muted", t$description)
            )
          )
        })
      )
    })

    # --- テンプレート選択 ---
    observeEvent(input$select_template, {
      template <- model_templates[[input$select_template]]
      if (!is.null(template)) {
        updateTextAreaInput(session, "template_syntax", value = template$syntax)
      }
    })

    # --- テンプレート適用 ---
    observeEvent(input$apply_template, {
      syntax <- input$template_syntax
      if (!is.null(syntax) && trimws(syntax) != "") {
        rv$model_syntax <- syntax
        showNotification("モデル構文を設定しました", type = "message")
      }
    })

    # --- 変数チップス（クリックで追加）---
    output$variable_chips <- renderUI({
      if (is.null(rv$data)) {
        return(tags$p(class = "text-muted", "データを読み込んでください"))
      }

      vars <- names(rv$data)[sapply(rv$data, is.numeric)]

      tags$div(
        class = "d-flex flex-wrap gap-1",
        lapply(vars, function(v) {
          tags$span(
            class = "badge bg-secondary",
            style = "cursor: pointer; font-size: 0.85rem;",
            onclick = sprintf(
              "var ta = document.getElementById('%s'); var pos = ta.selectionStart; var val = ta.value; ta.value = val.substring(0, pos) + '%s' + val.substring(pos); ta.focus();",
              ns("template_syntax"), v
            ),
            v
          )
        })
      )
    })

    # =========================================================================
    # 直接入力機能
    # =========================================================================

    # --- 変数リスト ---
    output$variable_list <- renderUI({
      if (is.null(rv$data)) {
        return(tags$p(class = "text-muted small", "データを読み込んでください"))
      }

      vars <- names(rv$data)[sapply(rv$data, is.numeric)]

      tags$div(
        style = "max-height: 200px; overflow-y: auto;",
        lapply(vars, function(v) {
          tags$div(
            class = "badge bg-light text-dark me-1 mb-1",
            style = "cursor: pointer;",
            onclick = sprintf(
              "var ta = document.getElementById('%s'); ta.value += ' + %s'; ta.focus();",
              ns("model_syntax"), v
            ),
            v
          )
        })
      )
    })

    # --- 構文バリデーション ---
    observeEvent(input$validate_syntax, {
      syntax <- input$model_syntax
      result <- validate_model_syntax(syntax)

      output$syntax_validation <- renderUI({
        if (result$valid) {
          tags$div(
            class = "alert alert-success mt-3 py-2",
            tags$i(class = "fas fa-check-circle me-2"),
            result$message
          )
        } else {
          tags$div(
            class = "alert alert-danger mt-3 py-2",
            tags$i(class = "fas fa-times-circle me-2"),
            result$message
          )
        }
      })
    })

    # --- 構文クリア ---
    observeEvent(input$clear_syntax, {
      updateTextAreaInput(session, "model_syntax", value = "")
      rv$model_syntax <- NULL
      output$syntax_validation <- renderUI(NULL)
    })

    # --- 直接入力の同期 ---
    observe({
      if (!is.null(input$model_syntax) && trimws(input$model_syntax) != "") {
        rv$model_syntax <- input$model_syntax
      }
    })

    # =========================================================================
    # 共通: 現在の構文表示
    # =========================================================================

    output$current_syntax_display <- renderText({
      if (is.null(rv$model_syntax) || trimws(rv$model_syntax) == "") {
        return("（モデル構文が設定されていません）")
      }
      rv$model_syntax
    })

    output$syntax_status_badge <- renderUI({
      if (is.null(rv$model_syntax) || trimws(rv$model_syntax) == "") {
        tags$span(class = "badge bg-warning", "未設定")
      } else {
        result <- validate_model_syntax(rv$model_syntax)
        if (result$valid) {
          tags$span(class = "badge bg-success", "有効")
        } else {
          tags$span(class = "badge bg-danger", "エラー")
        }
      }
    })

    return(rv)
  })
}

# -----------------------------------------------------------------------------
# 推定モジュール サーバー
# -----------------------------------------------------------------------------
estimation_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- 推定方法の推奨 ---
    output$estimator_recommendation <- renderUI({
      if (is.null(rv$data)) return(NULL)

      num_data <- rv$data[, sapply(rv$data, is.numeric), drop = FALSE]
      n_missing <- sum(is.na(num_data))
      n_obs <- nrow(rv$data)

      recommendations <- character(0)

      if (n_missing > 0) {
        pct <- round(n_missing / (nrow(num_data) * ncol(num_data)) * 100, 1)
        recommendations <- c(recommendations,
          paste0("欠損値が", n_missing, "個(", pct, "%)あります。「欠損値処理」をFIMLにすることを推奨します。"))
      }

      if (n_obs < 200) {
        recommendations <- c(recommendations,
          paste0("サンプルサイズが", n_obs, "と小さめです。結果の解釈に注意してください。"))
      }

      # 正規性の簡易チェック（各変数の歪度・尖度）
      if (ncol(num_data) > 0) {
        skew_issues <- sapply(num_data, function(x) {
          x <- na.omit(x)
          if (length(x) < 3) return(FALSE)
          n <- length(x)
          m <- mean(x)
          s <- sd(x)
          if (s == 0) return(FALSE)
          abs(sum((x - m)^3) / (n * s^3)) > 2
        })
        if (any(skew_issues)) {
          recommendations <- c(recommendations,
            "一部の変数に強い歪みがあります。MLR（ロバスト最尤法）の使用を検討してください。")
        }
      }

      if (length(recommendations) > 0) {
        tags$div(
          class = "alert alert-info py-2 small",
          tags$i(class = "fas fa-info-circle me-1"),
          tags$strong("データに基づく推奨: "),
          tags$ul(
            class = "mb-0 mt-1",
            lapply(recommendations, function(r) tags$li(r))
          )
        )
      }
    })

    # --- 分析実行 ---
    observeEvent(input$run_analysis, {
      # バリデーション
      if (is.null(rv$data)) {
        showNotification("データを読み込んでください", type = "error")
        return()
      }

      if (is.null(rv$model_syntax) || trimws(rv$model_syntax) == "") {
        showNotification("モデル構文を入力してください", type = "error")
        return()
      }

      # 分析実行
      waiter <- Waiter$new(
        html = tagList(
          spin_fading_circles(),
          tags$h4("SEM分析を実行中...", class = "text-white mt-3"),
          tags$p("しばらくお待ちください", class = "text-white-50")
        ),
        color = "rgba(44, 62, 80, 0.9)"
      )
      waiter$show()

      tryCatch({
        # 推定オプションの設定
        bootstrap_n <- if (input$se == "bootstrap") input$bootstrap_n else NULL

        # lavaan実行（withCallingHandlers で警告を捕捉しつつ実行を継続）
        fit <- withCallingHandlers(
          sem(
            model = rv$model_syntax,
            data = rv$data,
            estimator = input$estimator,
            missing = input$missing,
            se = input$se,
            bootstrap = bootstrap_n,
            std.lv = input$std_lv,
            fixed.x = input$fixed_x,
            meanstructure = input$meanstructure,
            orthogonal = input$orthogonal
          ),
          warning = function(w) {
            warn_msg <- w$message
            warn_help <- ""

            if (grepl("negative variance", warn_msg, ignore.case = TRUE)) {
              warn_help <- "（Heywoodケース: モデルの再検討を推奨）"
            } else if (grepl("not converged", warn_msg, ignore.case = TRUE)) {
              warn_help <- "（収束していない可能性あり）"
            }

            showNotification(
              paste0("警告: ", warn_msg, " ", warn_help),
              type = "warning",
              duration = 10
            )
            invokeRestart("muffleWarning")
          }
        )

        rv$fit <- fit
        rv$fit_summary <- summary(fit, standardized = TRUE, fit.measures = TRUE)
        rv$estimation_complete <- TRUE
        rv$error_message <- NULL
        rv$error_help <- NULL

        waiter$hide()

        showNotification(
          "分析が完了しました",
          type = "message",
          duration = 5
        )

        # 結果タブに移動（rv経由でapp.Rの親observerに通知）
        rv$navigate_to <- "results_tab"

      }, error = function(e) {
        waiter$hide()

        # エラーメッセージを解析して日本語のヘルプを追加
        error_msg <- e$message
        help_msg <- ""

        if (grepl("covariance matrix", error_msg, ignore.case = TRUE)) {
          help_msg <- "データに問題がある可能性があります。欠損値や外れ値を確認してください。"
        } else if (grepl("not positive definite", error_msg, ignore.case = TRUE)) {
          help_msg <- "共分散行列が正定値ではありません。変数間に完全な相関がないか確認してください。"
        } else if (grepl("convergence", error_msg, ignore.case = TRUE)) {
          help_msg <- "モデルが収束しませんでした。モデルを簡略化するか、開始値を調整してください。"
        } else if (grepl("singular", error_msg, ignore.case = TRUE)) {
          help_msg <- "行列が特異です。冗長な変数や線形従属がないか確認してください。"
        } else if (grepl("degrees of freedom", error_msg, ignore.case = TRUE)) {
          help_msg <- "自由度が負です。モデルが過剰識別されていない可能性があります。"
        } else if (grepl("unknown variable", error_msg, ignore.case = TRUE)) {
          help_msg <- "モデル構文の変数名がデータに存在しません。変数名を確認してください。"
        }

        rv$error_message <- error_msg
        rv$error_help <- help_msg
        rv$estimation_complete <- FALSE

        showNotification(
          tags$div(
            tags$strong("分析エラー"),
            tags$br(),
            tags$span(error_msg),
            if (help_msg != "") tags$br(),
            if (help_msg != "") tags$small(class = "text-info", help_msg)
          ),
          type = "error",
          duration = 15
        )
      })
    })

    # --- エラークリア ---
    observeEvent(input$clear_error, {
      rv$error_message <- NULL
      rv$error_help <- NULL
    })

    # --- 分析ステータス ---
    output$analysis_status <- renderUI({
      if (is.null(rv$data)) {
        return(
          tags$div(
            class = "text-center py-4",
            tags$i(class = "fas fa-database fa-3x text-muted mb-3"),
            tags$p(class = "text-muted", "データを読み込んでください")
          )
        )
      }

      if (is.null(rv$model_syntax) || trimws(rv$model_syntax) == "") {
        return(
          tags$div(
            class = "text-center py-4",
            tags$i(class = "fas fa-code fa-3x text-muted mb-3"),
            tags$p(class = "text-muted", "モデルを定義してください")
          )
        )
      }

      if (!rv$estimation_complete) {
        # エラーがあれば表示
        if (!is.null(rv$error_message) && rv$error_message != "") {
          return(
            tags$div(
              class = "text-center py-3",
              tags$i(class = "fas fa-exclamation-circle fa-3x text-danger mb-3"),
              tags$p(class = "text-danger", tags$strong("分析エラー")),
              tags$p(class = "small text-muted", rv$error_message),
              if (!is.null(rv$error_help) && rv$error_help != "") {
                tags$p(class = "small text-info", rv$error_help)
              },
              actionButton(
                ns("clear_error"),
                "エラーをクリア",
                class = "btn-sm btn-outline-secondary mt-2"
              )
            )
          )
        }

        return(
          tags$div(
            class = "text-center py-4",
            tags$i(class = "fas fa-play-circle fa-3x text-primary mb-3"),
            tags$p(class = "text-primary", "「分析を実行」をクリックしてください")
          )
        )
      }

      # 分析完了時
      fm <- fitMeasures(rv$fit)
      cfi <- fm["cfi"]
      rmsea <- fm["rmsea"]

      cfi_eval <- evaluate_fit_index("cfi", cfi)
      rmsea_eval <- evaluate_fit_index("rmsea", rmsea)

      tags$div(
        tags$div(
          class = "status-badge status-success mb-3",
          tags$i(class = "fas fa-check-circle me-2"),
          "分析完了"
        ),
        fluidRow(
          column(6,
            tags$div(
              class = "text-center",
              tags$span(class = "value-highlight", sprintf("%.3f", cfi)),
              tags$br(),
              tags$span(class = "value-label", "CFI"),
              tags$br(),
              tags$span(class = paste("badge", gsub("fit-", "bg-", cfi_eval$class)), cfi_eval$judgment)
            )
          ),
          column(6,
            tags$div(
              class = "text-center",
              tags$span(class = "value-highlight", sprintf("%.3f", rmsea)),
              tags$br(),
              tags$span(class = "value-label", "RMSEA"),
              tags$br(),
              tags$span(class = paste("badge", gsub("fit-", "bg-", rmsea_eval$class)), rmsea_eval$judgment)
            )
          )
        )
      )
    })

    # --- 設定プレビュー ---
    output$settings_preview <- renderUI({
      # タブ未表示時にinputがNULLになるため安全にデフォルト値を使用
      estimator_val <- if (is.null(input$estimator)) "ML" else input$estimator
      missing_val <- if (is.null(input$missing)) "listwise" else input$missing
      se_val <- if (is.null(input$se)) "standard" else input$se
      std_lv_val <- isTRUE(input$std_lv)
      meanstructure_val <- isTRUE(input$meanstructure)

      tags$table(
        class = "table table-sm mb-0",
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("推定方法")),
            tags$td(estimator_val)
          ),
          tags$tr(
            tags$td(tags$strong("欠損値処理")),
            tags$td(missing_val)
          ),
          tags$tr(
            tags$td(tags$strong("標準誤差")),
            tags$td(se_val)
          ),
          if (se_val == "bootstrap") {
            tags$tr(
              tags$td(tags$strong("ブートストラップ回数")),
              tags$td(as.character(if (is.null(input$bootstrap_n)) 1000 else input$bootstrap_n))
            )
          },
          tags$tr(
            tags$td(tags$strong("潜在変数分散=1")),
            tags$td(if (std_lv_val) "はい" else "いいえ")
          ),
          tags$tr(
            tags$td(tags$strong("平均構造")),
            tags$td(if (meanstructure_val) "推定" else "なし")
          )
        )
      )
    })

    # --- 警告・エラー表示 ---
    output$warnings_errors <- renderUI({
      if (!is.null(rv$fit)) {
        # 収束チェック
        if (!lavInspect(rv$fit, "converged")) {
          return(
            tags$div(
              class = "alert alert-danger",
              tags$i(class = "fas fa-exclamation-triangle me-2"),
              tags$strong("警告: "), "モデルが収束していません。"
            )
          )
        }

        # 負の分散チェック
        variances <- lavInspect(rv$fit, "est")$theta
        if (any(diag(variances) < 0)) {
          return(
            tags$div(
              class = "alert alert-warning",
              tags$i(class = "fas fa-exclamation-circle me-2"),
              tags$strong("警告: "), "負の分散が推定されました（Heywood case）。"
            )
          )
        }
      }
      NULL
    })

    return(rv)
  })
}

# -----------------------------------------------------------------------------
# 結果モジュール サーバー
# -----------------------------------------------------------------------------
results_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- リアクティブキャッシュ（重複計算を排除）---
    cached_fit_measures <- reactive({
      req(rv$fit)
      fitMeasures(rv$fit)
    })

    cached_params <- reactive({
      req(rv$fit)
      parameterEstimates(rv$fit, standardized = TRUE)
    })

    # --- 総合解釈（初心者向け）---
    output$overall_interpretation <- renderUI({
      req(rv$fit)

      interp <- interpret_fit(rv$fit)
      if (is.null(interp)) return(NULL)

      # アイコンと色の設定
      icon_class <- switch(interp$overall_class,
        "good" = "fas fa-check-circle fa-2x text-success",
        "acceptable" = "fas fa-exclamation-circle fa-2x text-warning",
        "poor" = "fas fa-times-circle fa-2x text-danger",
        "fas fa-question-circle fa-2x text-muted"
      )

      tags$div(
        class = paste("interpretation-card", interp$overall_class),
        fluidRow(
          column(1,
            tags$div(class = "pt-1", tags$i(class = icon_class))
          ),
          column(11,
            tags$div(class = "interpretation-title",
              paste("総合判定:", interp$overall_judgment)
            ),
            tags$div(class = "interpretation-text", interp$summary_text),
            if (length(interp$details) > 0) {
              tags$div(
                class = "mt-2",
                lapply(interp$details, function(d) {
                  tags$div(
                    class = "small",
                    tags$span(
                      class = paste("badge me-1", gsub("fit-", "bg-", d$eval$class)),
                      d$eval$judgment
                    ),
                    d$text
                  )
                })
              )
            }
          )
        )
      )
    })

    # --- 適合度サマリーカード ---
    output$fit_summary_cards <- renderUI({
      req(rv$fit)

      fm <- cached_fit_measures()

      create_fit_card <- function(name, value, good_threshold = NULL, direction = "lower") {
        formatted <- if (name %in% c("df")) {
          sprintf("%.0f", value)
        } else if (name %in% c("AIC", "BIC")) {
          sprintf("%.1f", value)
        } else {
          sprintf("%.3f", value)
        }

        eval_result <- evaluate_fit_index(name, value)

        tags$div(
          class = "col-md-2 col-sm-4 mb-3",
          tags$div(
            class = "text-center p-3 rounded",
            style = "background: #f8f9fa;",
            tags$span(class = "value-highlight", formatted),
            tags$br(),
            tags$span(class = "value-label", name),
            if (eval_result$judgment != "-") {
              tagList(
                tags$br(),
                tags$span(
                  class = paste("badge mt-1", gsub("fit-", "bg-", eval_result$class)),
                  eval_result$judgment
                )
              )
            }
          )
        )
      }

      fluidRow(
        create_fit_card("CFI", fm["cfi"]),
        create_fit_card("TLI", fm["tli"]),
        create_fit_card("RMSEA", fm["rmsea"]),
        create_fit_card("SRMR", fm["srmr"]),
        create_fit_card("AIC", fm["aic"]),
        create_fit_card("BIC", fm["bic"])
      )
    })

    # --- 適合度指標テーブル ---
    output$fit_indices_table <- renderDT({
      req(rv$fit)

      fit_table <- create_fit_table(rv$fit)

      datatable(
        fit_table,
        options = list(
          dom = 't',
          pageLength = 20,
          ordering = FALSE
        ),
        rownames = FALSE,
        class = 'table-striped table-bordered'
      )
    })

    # --- 因子負荷量テーブル ---
    output$loadings_table <- renderDT({
      req(rv$fit)

      params <- cached_params()
      loadings <- params[params$op == "=~", ]

      if (nrow(loadings) == 0) {
        return(datatable(data.frame(message = "測定モデルがありません")))
      }

      if (input$std_loadings) {
        result <- loadings %>%
          select(
            因子 = lhs,
            指標 = rhs,
            推定値 = est,
            標準化 = std.all,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue,
            `95%CI下限` = ci.lower,
            `95%CI上限` = ci.upper
          )
      } else {
        result <- loadings %>%
          select(
            因子 = lhs,
            指標 = rhs,
            推定値 = est,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue,
            `95%CI下限` = ci.lower,
            `95%CI上限` = ci.upper
          )
      }

      datatable(
        result,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3) %>%
        formatStyle(
          'p値',
          backgroundColor = styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#d4edda', '#fff3cd', 'white'))
        )
    })

    # --- 回帰係数テーブル ---
    output$regressions_table <- renderDT({
      req(rv$fit)

      params <- cached_params()
      regressions <- params[params$op == "~", ]

      if (nrow(regressions) == 0) {
        return(datatable(data.frame(message = "構造モデル（回帰）がありません")))
      }

      if (input$std_regressions) {
        result <- regressions %>%
          select(
            従属変数 = lhs,
            独立変数 = rhs,
            推定値 = est,
            標準化 = std.all,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue,
            `95%CI下限` = ci.lower,
            `95%CI上限` = ci.upper
          )
      } else {
        result <- regressions %>%
          select(
            従属変数 = lhs,
            独立変数 = rhs,
            推定値 = est,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue,
            `95%CI下限` = ci.lower,
            `95%CI上限` = ci.upper
          )
      }

      datatable(
        result,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3) %>%
        formatStyle(
          'p値',
          backgroundColor = styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#d4edda', '#fff3cd', 'white'))
        )
    })

    # --- 共分散テーブル ---
    output$covariances_table <- renderDT({
      req(rv$fit)

      params <- cached_params()
      covariances <- params[params$op == "~~" & params$lhs != params$rhs, ]

      if (nrow(covariances) == 0) {
        return(datatable(data.frame(message = "共分散パラメータがありません")))
      }

      result <- covariances %>%
        select(
          変数1 = lhs,
          変数2 = rhs,
          共分散 = est,
          相関 = std.all,
          標準誤差 = se,
          z値 = z,
          p値 = pvalue
        )

      datatable(
        result,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3)
    })

    # --- 分散テーブル ---
    output$variances_table <- renderDT({
      req(rv$fit)

      params <- cached_params()
      variances <- params[params$op == "~~" & params$lhs == params$rhs, ]

      if (nrow(variances) == 0) {
        return(datatable(data.frame(message = "分散パラメータがありません")))
      }

      result <- variances %>%
        select(
          変数 = lhs,
          分散 = est,
          標準化 = std.all,
          標準誤差 = se,
          z値 = z,
          p値 = pvalue
        )

      datatable(
        result,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3)
    })

    # --- 定義パラメータテーブル ---
    output$defined_table <- renderDT({
      req(rv$fit)

      params <- cached_params()
      defined <- params[params$op == ":=", ]

      if (nrow(defined) == 0) {
        return(datatable(data.frame(message = "定義されたパラメータがありません")))
      }

      result <- defined %>%
        select(
          名前 = lhs,
          推定値 = est,
          標準誤差 = se,
          z値 = z,
          p値 = pvalue,
          `95%CI下限` = ci.lower,
          `95%CI上限` = ci.upper
        )

      datatable(
        result,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3) %>%
        formatStyle(
          'p値',
          backgroundColor = styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#d4edda', '#fff3cd', 'white'))
        )
    })

    # --- 全パラメータテーブル ---
    output$all_params_table <- renderDT({
      req(rv$fit)

      params <- cached_params()

      if (input$std_all) {
        result <- params %>%
          mutate(パス = paste(lhs, op, rhs)) %>%
          select(
            パス,
            推定値 = est,
            標準化 = std.all,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue
          )
      } else {
        result <- params %>%
          mutate(パス = paste(lhs, op, rhs)) %>%
          select(
            パス,
            推定値 = est,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue
          )
      }

      datatable(
        result,
        options = list(
          pageLength = 25,
          scrollY = "400px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = which(sapply(result, is.numeric)), digits = 3) %>%
        formatStyle(
          'p値',
          backgroundColor = styleInterval(c(0.001, 0.01, 0.05), c('#d4edda', '#d4edda', '#fff3cd', 'white'))
        )
    })

    # --- 修正指標 ---
    output$modification_indices <- renderDT({
      req(rv$fit)

      tryCatch({
        mi <- modificationIndices(rv$fit, sort. = TRUE)

        if (is.null(mi) || nrow(mi) == 0) {
          return(datatable(data.frame(message = "修正指標を計算できません")))
        }

        # 閾値でフィルタ
        mi <- mi[mi$mi >= input$mi_threshold, ]

        if (nrow(mi) == 0) {
          return(datatable(data.frame(message = paste("MI >=", input$mi_threshold, "の修正指標はありません"))))
        }

        # 並び替え
        if (input$mi_sort == "epc_desc") {
          mi <- mi[order(-abs(mi$epc)), ]
        }

        result <- mi %>%
          mutate(パス = paste(lhs, op, rhs)) %>%
          select(
            パス,
            MI = mi,
            EPC = epc,
            `標準化EPC` = sepc.all
          ) %>%
          head(50)

        datatable(
          result,
          options = list(
            pageLength = 15,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv')
          ),
          extensions = 'Buttons',
          rownames = FALSE
        ) %>%
          formatRound(columns = c("MI", "EPC", "標準化EPC"), digits = 3)

      }, error = function(e) {
        datatable(data.frame(message = paste("エラー:", e$message)))
      })
    })

    # --- R2（決定係数）テーブル ---
    output$rsquare_table <- renderDT({
      req(rv$fit)

      tryCatch({
        r2 <- lavInspect(rv$fit, "rsquare")
        if (length(r2) == 0) {
          return(datatable(data.frame(message = "R2 を計算できません（内生変数がありません）")))
        }

        result <- data.frame(
          変数 = names(r2),
          R2 = round(r2, 3),
          `説明率(%)` = round(r2 * 100, 1),
          row.names = NULL,
          check.names = FALSE
        )

        datatable(
          result,
          options = list(
            dom = 't',
            pageLength = 50,
            ordering = TRUE
          ),
          rownames = FALSE,
          class = 'table-striped table-bordered'
        ) %>%
          formatStyle(
            'R2',
            background = styleColorBar(c(0, 1), '#3498db'),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }, error = function(e) {
        datatable(data.frame(message = paste("R2 の計算中にエラー:", e$message)))
      })
    })

    # --- lavaan詳細出力 ---
    output$lavaan_summary <- renderPrint({
      req(rv$fit)
      summary(rv$fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
    })

    # --- 結果ダウンロード ---
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("sem_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        # HTMLレポート生成
        html_content <- generate_html_report(rv$fit, rv$model_syntax, rv$data_name)
        writeLines(html_content, file)
      }
    )
  })
}

# --- 安全なフォーマットヘルパー ---
safe_sprintf <- function(fmt, value) {
  if (is.null(value) || is.na(value)) return("NA")
  sprintf(fmt, value)
}

# --- HTML エスケープヘルパー ---
html_escape <- function(text) {
  if (is.null(text) || length(text) == 0) return("")
  text <- as.character(text)
  if (is.na(text) || length(text) != 1) return("")
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  text
}

# --- HTMLレポート生成 ---
generate_html_report <- function(fit, model_syntax, data_name) {
  fm <- fitMeasures(fit)
  params <- parameterEstimates(fit, standardized = TRUE)

  # R2 の取得
  r2 <- tryCatch(lavInspect(fit, "rsquare"), error = function(e) NULL)

  html <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>SEM Analysis Report</title>
  <style>
    body { font-family: "Helvetica Neue", Arial, sans-serif; margin: 40px; line-height: 1.6; }
    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; }
    table { border-collapse: collapse; width: 100%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }
    th { background-color: #2c3e50; color: white; }
    tr:nth-child(even) { background-color: #f9f9f9; }
    .fit-good { background-color: #d4edda; }
    .fit-acceptable { background-color: #fff3cd; }
    .fit-poor { background-color: #f8d7da; }
    pre { background: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; }
    .meta { color: #666; font-size: 0.9em; }
  </style>
</head>
<body>
  <h1>SEM Analysis Report</h1>
  <p class="meta">Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
  <p class="meta">Data: ', html_escape(data_name), '</p>

  <h2>Model Syntax</h2>
  <pre>', html_escape(model_syntax), '</pre>

  <h2>Fit Indices</h2>
  <table>
    <tr><th>Index</th><th>Value</th></tr>
    <tr><td>Chi-square</td><td>', sprintf("%.3f", fm["chisq"]), '</td></tr>
    <tr><td>df</td><td>', sprintf("%.0f", fm["df"]), '</td></tr>
    <tr><td>p-value</td><td>', sprintf("%.4f", fm["pvalue"]), '</td></tr>
    <tr><td>CFI</td><td>', sprintf("%.3f", fm["cfi"]), '</td></tr>
    <tr><td>TLI</td><td>', sprintf("%.3f", fm["tli"]), '</td></tr>
    <tr><td>RMSEA</td><td>', sprintf("%.3f", fm["rmsea"]), '</td></tr>
    <tr><td>SRMR</td><td>', sprintf("%.3f", fm["srmr"]), '</td></tr>
    <tr><td>AIC</td><td>', sprintf("%.1f", fm["aic"]), '</td></tr>
    <tr><td>BIC</td><td>', sprintf("%.1f", fm["bic"]), '</td></tr>
  </table>

  <h2>Parameter Estimates</h2>
  <table>
    <tr><th>Path</th><th>Estimate</th><th>Std.All</th><th>SE</th><th>z</th><th>p</th><th>95% CI</th></tr>')

  for (i in 1:nrow(params)) {
    ci_text <- sprintf("[%.3f, %.3f]", params$ci.lower[i], params$ci.upper[i])
    html <- paste0(html, '
    <tr>
      <td>', html_escape(paste(params$lhs[i], params$op[i], params$rhs[i])), '</td>
      <td>', sprintf("%.3f", params$est[i]), '</td>
      <td>', sprintf("%.3f", params$std.all[i]), '</td>
      <td>', sprintf("%.3f", params$se[i]), '</td>
      <td>', sprintf("%.3f", params$z[i]), '</td>
      <td>', sprintf("%.4f", params$pvalue[i]), '</td>
      <td>', ci_text, '</td>
    </tr>')
  }

  html <- paste0(html, '
  </table>')

  # R2 セクション
  if (!is.null(r2) && length(r2) > 0) {
    html <- paste0(html, '
  <h2>R-squared</h2>
  <table>
    <tr><th>Variable</th><th>R&sup2;</th><th>Explained (%)</th></tr>')
    for (i in seq_along(r2)) {
      html <- paste0(html, '
    <tr>
      <td>', html_escape(names(r2)[i]), '</td>
      <td>', sprintf("%.3f", r2[i]), '</td>
      <td>', sprintf("%.1f%%", r2[i] * 100), '</td>
    </tr>')
    }
    html <- paste0(html, '
  </table>')
  }

  html <- paste0(html, '
</body>
</html>')

  html
}

# -----------------------------------------------------------------------------
# パス図モジュール サーバー
# -----------------------------------------------------------------------------
diagram_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- パス図描画パラメータ（重複排除）---
    diagram_params <- reactive({
      safe <- function(x, default) if (is.null(x)) default else x
      what_val <- safe(input$what, "std")
      what_param <- switch(what_val, "std" = "std", "est" = "est", "par" = "par", "nothing" = "nothing", "std")
      node_size <- safe(input$node_size, 8)

      list(
        what = what_param,
        whatLabels = what_param,
        layout = safe(input$layout, "tree"),
        style = "lisrel",
        residuals = safe(input$residuals, TRUE),
        intercepts = safe(input$intercepts, FALSE),
        thresholds = safe(input$thresholds, FALSE),
        nCharNodes = 0,
        nCharEdges = 0,
        sizeMan = node_size,
        sizeLat = node_size * 1.2,
        edge.label.cex = safe(input$label_size, 1),
        edge.width = safe(input$edge_size, 1),
        curve = 2,
        curvePivot = TRUE,
        mar = c(2, 2, 2, 2),
        color = list(
          lat = safe(input$lat_color, "#3498db"),
          man = safe(input$man_color, "#2ecc71")
        ),
        border.color = "#2c3e50",
        edge.color = "#34495e",
        label.color = "#2c3e50"
      )
    })

    # --- semPaths 描画ヘルパー ---
    draw_sem_diagram <- function(fit, params) {
      args <- c(list(object = fit), params)
      do.call(semPaths, args)
    }

    # --- パス図 ---
    output$sem_diagram <- renderPlot({
      req(rv$fit)
      tryCatch({
        draw_sem_diagram(rv$fit, diagram_params())
      }, error = function(e) {
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0, 1))
        text(0.5, 0.5,
             paste0("パス図の生成中にエラーが発生しました:\n", e$message),
             cex = 1.2, col = "#e74c3c")
      })
    })

    # --- パス図ダウンロード ---
    output$download_diagram <- downloadHandler(
      filename = function() {
        format_val <- if (is.null(input$download_format)) "png" else input$download_format
        paste0("sem_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", format_val)
      },
      content = function(file) {
        req(rv$fit)

        format_val <- if (is.null(input$download_format)) "png" else input$download_format
        width_val <- if (is.null(input$download_width)) 10 else input$download_width
        height_val <- if (is.null(input$download_height)) 8 else input$download_height

        if (format_val == "png") {
          png(file, width = width_val, height = height_val, units = "in", res = 300)
        } else if (format_val == "pdf") {
          pdf(file, width = width_val, height = height_val)
        } else {
          svg(file, width = width_val, height = height_val)
        }

        tryCatch({
          draw_sem_diagram(rv$fit, diagram_params())
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste0("エラー: ", e$message), cex = 1)
        })

        dev.off()
      }
    )
  })
}

# -----------------------------------------------------------------------------
# モデル比較モジュール サーバー
# -----------------------------------------------------------------------------
comparison_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- ローカルリアクティブ値 ---
    local_rv <- reactiveValues(
      saved_models = list()
    )

    # --- モデル保存 ---
    observeEvent(input$save_model, {
      # バリデーション
      if (is.null(rv$fit)) {
        showNotification("保存するモデルがありません。先に分析を実行してください。", type = "error")
        return()
      }

      model_name <- trimws(input$model_name)
      if (model_name == "") {
        model_name <- paste0("Model ", length(local_rv$saved_models) + 1)
      }

      # 同名チェック
      if (model_name %in% names(local_rv$saved_models)) {
        showNotification("同じ名前のモデルが既に存在します。別の名前を指定してください。", type = "warning")
        return()
      }

      # モデルを保存
      local_rv$saved_models[[model_name]] <- list(
        fit = rv$fit,
        syntax = rv$model_syntax,
        description = input$model_description,
        timestamp = Sys.time(),
        fit_measures = fitMeasures(rv$fit)
      )

      # 入力をクリア
      updateTextInput(session, "model_name", value = "")
      updateTextAreaInput(session, "model_description", value = "")

      # 選択肢を更新
      update_model_choices()

      showNotification(paste0("モデル「", model_name, "」を保存しました"), type = "message")
    })

    # --- 選択肢更新関数 ---
    update_model_choices <- function() {
      choices <- names(local_rv$saved_models)
      if (length(choices) == 0) choices <- NULL

      updateSelectInput(session, "model_1", choices = choices)
      updateSelectInput(session, "model_2", choices = choices)
      updateSelectInput(session, "param_model_1", choices = choices)
      updateSelectInput(session, "param_model_2", choices = choices)
    }

    # --- 保存済みモデル一覧 ---
    output$saved_models_list <- renderUI({
      models <- local_rv$saved_models

      if (length(models) == 0) {
        return(tags$p(class = "text-muted text-center", "保存されたモデルはありません"))
      }

      tagList(
        lapply(names(models), function(name) {
          m <- models[[name]]
          fm <- m$fit_measures

          tags$div(
            class = "card mb-2",
            tags$div(
              class = "card-body py-2 px-3",
              tags$div(
                class = "d-flex justify-content-between align-items-start",
                tags$div(
                  tags$strong(name),
                  tags$br(),
                  tags$small(
                    class = "text-muted",
                    sprintf("CFI=%.3f, RMSEA=%.3f", fm["cfi"], fm["rmsea"])
                  ),
                  if (!is.null(m$description) && nzchar(m$description)) {
                    tagList(
                      tags$br(),
                      tags$small(class = "text-info", m$description)
                    )
                  }
                ),
                tags$button(
                  class = "btn btn-sm btn-outline-danger",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("delete_model"), name),
                  tags$i(class = "fas fa-times")
                )
              )
            )
          )
        })
      )
    })

    # --- モデル削除 ---
    observeEvent(input$delete_model, {
      model_name <- input$delete_model
      if (model_name %in% names(local_rv$saved_models)) {
        local_rv$saved_models[[model_name]] <- NULL
        update_model_choices()
        showNotification(paste0("モデル「", model_name, "」を削除しました"), type = "message")
      }
    })

    # --- 全モデルクリア ---
    observeEvent(input$clear_all_models, {
      local_rv$saved_models <- list()
      update_model_choices()
      showNotification("全てのモデルをクリアしました", type = "message")
    })

    # --- メッセージ表示 ---
    output$no_models_message <- renderUI({
      if (length(local_rv$saved_models) < 2) {
        tags$div(
          class = "alert alert-info",
          tags$i(class = "fas fa-info-circle me-2"),
          "比較するには2つ以上のモデルを保存してください。",
          tags$br(),
          tags$small("「推定設定」タブで分析を実行し、左のパネルからモデルを保存できます。")
        )
      }
    })

    # --- 比較テーブル ---
    output$comparison_table <- renderDT({
      models <- local_rv$saved_models

      if (length(models) < 1) {
        return(NULL)
      }

      # 適合度指標を取得
      indices <- input$compare_indices
      if (is.null(indices) || length(indices) == 0) {
        indices <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic")
      }

      # テーブル作成
      comparison_df <- data.frame(
        モデル = names(models),
        stringsAsFactors = FALSE
      )

      for (idx in indices) {
        values <- sapply(models, function(m) {
          val <- m$fit_measures[idx]
          if (is.na(val)) return(NA)
          if (idx %in% c("chisq", "aic", "bic")) {
            sprintf("%.2f", val)
          } else if (idx == "df") {
            sprintf("%.0f", val)
          } else {
            sprintf("%.3f", val)
          }
        })
        comparison_df[[toupper(idx)]] <- values
      }

      # 最良モデルをハイライト
      datatable(
        comparison_df,
        options = list(
          dom = 't',
          pageLength = 20,
          ordering = TRUE
        ),
        rownames = FALSE,
        class = 'table-striped table-bordered'
      )
    })

    # --- χ²差検定 ---
    observeEvent(input$run_chisq_diff, {
      model_1_name <- input$model_1
      model_2_name <- input$model_2

      if (is.null(model_1_name) || is.null(model_2_name)) {
        showNotification("2つのモデルを選択してください", type = "error")
        return()
      }

      if (model_1_name == model_2_name) {
        showNotification("異なるモデルを選択してください", type = "error")
        return()
      }

      model_1 <- local_rv$saved_models[[model_1_name]]
      model_2 <- local_rv$saved_models[[model_2_name]]

      if (is.null(model_1) || is.null(model_2)) {
        showNotification("モデルが見つかりません", type = "error")
        return()
      }

      # 適合度指標取得
      fm1 <- model_1$fit_measures
      fm2 <- model_2$fit_measures

      # χ²差検定
      chisq_diff <- abs(fm1["chisq"] - fm2["chisq"])
      df_diff <- abs(fm1["df"] - fm2["df"])

      if (df_diff == 0) {
        output$chisq_diff_result <- renderUI({
          tags$div(
            class = "alert alert-warning",
            tags$i(class = "fas fa-exclamation-triangle me-2"),
            "自由度の差が0です。これらのモデルはネストされていない可能性があります。"
          )
        })
        return()
      }

      p_value <- pchisq(chisq_diff, df_diff, lower.tail = FALSE)

      output$chisq_diff_result <- renderUI({
        significant <- p_value < 0.05

        # どちらが良いか判定
        if (fm1["aic"] < fm2["aic"]) {
          better_model <- model_1_name
          better_reason <- "AICが低い"
        } else {
          better_model <- model_2_name
          better_reason <- "AICが低い"
        }

        tags$div(
          tags$div(
            class = "result-panel",
            tags$h5("χ²差検定結果"),
            tags$table(
              class = "table table-sm",
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong("Δχ²")),
                  tags$td(sprintf("%.3f", chisq_diff))
                ),
                tags$tr(
                  tags$td(tags$strong("Δdf")),
                  tags$td(sprintf("%.0f", df_diff))
                ),
                tags$tr(
                  tags$td(tags$strong("p値")),
                  tags$td(
                    sprintf("%.4f", p_value),
                    if (significant) tags$span(class = "badge bg-success ms-2", "有意") else tags$span(class = "badge bg-secondary ms-2", "非有意")
                  )
                )
              )
            )
          ),
          tags$div(
            class = if(significant) "alert alert-success" else "alert alert-info",
            if (significant) {
              tagList(
                tags$i(class = "fas fa-check-circle me-2"),
                tags$strong("結論: "),
                "2つのモデル間に統計的に有意な差があります（p < .05）。",
                tags$br(),
                sprintf("情報量基準に基づくと、「%s」が推奨されます（%s）。", better_model, better_reason)
              )
            } else {
              tagList(
                tags$i(class = "fas fa-info-circle me-2"),
                tags$strong("結論: "),
                "2つのモデル間に統計的に有意な差はありません（p ≥ .05）。",
                tags$br(),
                "より節約的なモデル（自由度が大きい方）を選択することが推奨されます。"
              )
            }
          )
        )
      })
    })

    # --- 視覚的比較プロット ---
    output$comparison_plot <- renderPlot({
      models <- local_rv$saved_models

      if (length(models) < 1) {
        plot.new()
        text(0.5, 0.5, "比較するモデルを保存してください", cex = 1.2, col = "#95a5a6")
        return()
      }

      index <- input$plot_index

      # データ準備
      plot_data <- data.frame(
        model = names(models),
        value = sapply(models, function(m) m$fit_measures[index]),
        stringsAsFactors = FALSE
      )
      plot_data$model <- factor(plot_data$model, levels = plot_data$model)

      # 基準値
      thresholds <- list(
        cfi = list(good = 0.95, acceptable = 0.90, direction = "higher"),
        tli = list(good = 0.95, acceptable = 0.90, direction = "higher"),
        rmsea = list(good = 0.05, acceptable = 0.08, direction = "lower"),
        srmr = list(good = 0.05, acceptable = 0.08, direction = "lower"),
        aic = list(good = NA, acceptable = NA, direction = "lower"),
        bic = list(good = NA, acceptable = NA, direction = "lower")
      )

      threshold <- thresholds[[index]]

      # プロット
      p <- ggplot(plot_data, aes(x = model, y = value, fill = model)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = sprintf("%.3f", value)), vjust = -0.5, size = 4) +
        labs(
          x = "モデル",
          y = toupper(index),
          title = paste0(toupper(index), " の比較")
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_brewer(palette = "Set2")

      # 基準線
      if (input$show_threshold && !is.na(threshold$good)) {
        p <- p +
          geom_hline(yintercept = threshold$good, linetype = "dashed", color = "#18bc9c", size = 1) +
          geom_hline(yintercept = threshold$acceptable, linetype = "dashed", color = "#f39c12", size = 1) +
          annotate("text", x = Inf, y = threshold$good, label = "良好", hjust = 1.1, color = "#18bc9c") +
          annotate("text", x = Inf, y = threshold$acceptable, label = "許容", hjust = 1.1, color = "#f39c12")
      }

      p
    })

    # --- パラメータ比較 ---
    output$parameter_comparison <- renderDT({
      model_1_name <- input$param_model_1
      model_2_name <- input$param_model_2

      if (is.null(model_1_name) || is.null(model_2_name)) {
        return(NULL)
      }

      model_1 <- local_rv$saved_models[[model_1_name]]
      model_2 <- local_rv$saved_models[[model_2_name]]

      if (is.null(model_1) || is.null(model_2)) {
        return(NULL)
      }

      # パラメータ取得
      params_1 <- parameterEstimates(model_1$fit, standardized = TRUE)
      params_2 <- parameterEstimates(model_2$fit, standardized = TRUE)

      # パス名を作成
      params_1$path <- paste(params_1$lhs, params_1$op, params_1$rhs)
      params_2$path <- paste(params_2$lhs, params_2$op, params_2$rhs)

      # マージ
      comparison <- merge(
        params_1[, c("path", "est", "std.all", "pvalue")],
        params_2[, c("path", "est", "std.all", "pvalue")],
        by = "path",
        all = TRUE,
        suffixes = c("_1", "_2")
      )

      # 差分計算
      comparison$diff_est <- comparison$est_2 - comparison$est_1
      comparison$diff_std <- comparison$std.all_2 - comparison$std.all_1

      # 列名を整理
      result <- comparison %>%
        select(
          パス = path,
          `推定値(M1)` = est_1,
          `推定値(M2)` = est_2,
          差分 = diff_est,
          `標準化(M1)` = std.all_1,
          `標準化(M2)` = std.all_2
        )

      datatable(
        result,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatRound(columns = c("推定値(M1)", "推定値(M2)", "差分", "標準化(M1)", "標準化(M2)"), digits = 3)
    })

    # --- 比較表ダウンロード ---
    output$download_comparison <- downloadHandler(
      filename = function() {
        paste0("model_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        models <- local_rv$saved_models

        if (length(models) == 0) {
          write.csv(data.frame(message = "保存されたモデルがありません"), file, row.names = FALSE)
          return()
        }

        indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic")

        comparison_df <- data.frame(
          Model = names(models),
          stringsAsFactors = FALSE
        )

        for (idx in indices) {
          values <- sapply(models, function(m) m$fit_measures[idx])
          comparison_df[[toupper(idx)]] <- values
        }

        write.csv(comparison_df, file, row.names = FALSE)
      }
    )

    # rv にモデルリストを公開
    observe({
      rv$saved_models <- local_rv$saved_models
    })
  })
}

# -----------------------------------------------------------------------------
# ヘルプモジュール サーバー
# -----------------------------------------------------------------------------
help_server <- function(id, rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # --- ヘルプタブからのデモ起動 ---
    observeEvent(input$start_demo, {
      if (is.null(rv)) return()

      waiter <- Waiter$new(
        html = tagList(
          spin_fading_circles(),
          tags$h4("デモを準備中...", class = "text-white mt-3")
        ),
        color = "rgba(44, 62, 80, 0.9)"
      )
      waiter$show()

      tryCatch({
        rv$data <- as.data.frame(HolzingerSwineford1939)
        rv$data_name <- "HolzingerSwineford1939"

        rv$model_syntax <- paste(
          "# 3因子確認的因子分析モデル（デモ）",
          "visual  =~ x1 + x2 + x3",
          "textual =~ x4 + x5 + x6",
          "speed   =~ x7 + x8 + x9",
          sep = "\n"
        )

        fit <- sem(
          model = rv$model_syntax,
          data = rv$data,
          estimator = "ML"
        )

        rv$fit <- fit
        rv$fit_summary <- summary(fit, standardized = TRUE, fit.measures = TRUE)
        rv$estimation_complete <- TRUE
        rv$error_message <- NULL
        rv$error_help <- NULL

        waiter$hide()

        showNotification(
          "デモ完了! 「結果」タブで分析結果を確認できます。",
          type = "message",
          duration = 8
        )

        rv$navigate_to <- "results_tab"

      }, error = function(e) {
        waiter$hide()
        showNotification(paste("デモエラー:", e$message), type = "error")
      })
    })
  })
}
