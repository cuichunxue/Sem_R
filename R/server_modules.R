# =============================================================================
# サーバーモジュール
# =============================================================================

# -----------------------------------------------------------------------------
# データモジュール サーバー
# -----------------------------------------------------------------------------
data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      n_numeric <- sum(sapply(rv$data, is.numeric))
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
          tags$strong("列数: "), ncol(rv$data)
        ),
        tags$p(
          tags$i(class = "fas fa-hashtag me-2"),
          tags$strong("数値変数: "), n_numeric
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

      numeric_data <- rv$data[, sapply(rv$data, is.numeric), drop = FALSE]
      req(ncol(numeric_data) >= 2)

      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = input$cor_method)

      # 有意性検定
      if (input$cor_sig) {
        n <- nrow(numeric_data)
        p_matrix <- matrix(NA, nrow = ncol(numeric_data), ncol = ncol(numeric_data))
        for (i in 1:ncol(numeric_data)) {
          for (j in 1:ncol(numeric_data)) {
            if (i != j) {
              test <- cor.test(numeric_data[[i]], numeric_data[[j]], method = input$cor_method)
              p_matrix[i, j] <- test$p.value
            }
          }
        }

        corrplot(
          cor_matrix,
          method = "color",
          type = "upper",
          order = "hclust",
          tl.col = "black",
          tl.srt = 45,
          addCoef.col = "black",
          number.cex = 0.7,
          p.mat = p_matrix,
          sig.level = 0.05,
          insig = "label_sig",
          pch.cex = 1.5,
          col = colorRampPalette(c("#e74c3c", "white", "#3498db"))(200)
        )
      } else {
        corrplot(
          cor_matrix,
          method = "color",
          type = "upper",
          order = "hclust",
          tl.col = "black",
          tl.srt = 45,
          addCoef.col = "black",
          number.cex = 0.7,
          col = colorRampPalette(c("#e74c3c", "white", "#3498db"))(200)
        )
      }
    })

    # --- 相関行列テーブル ---
    output$cor_table <- renderDT({
      req(rv$data)

      numeric_data <- rv$data[, sapply(rv$data, is.numeric), drop = FALSE]
      req(ncol(numeric_data) >= 2)

      cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = input$cor_method)
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

    # --- テンプレートカード ---
    output$template_cards <- renderUI({
      templates <- switch(
        input$template_type,
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
        updateTextAreaInput(
          session,
          "model_syntax",
          value = template$syntax
        )
        rv$model_syntax <- template$syntax
      }
    })

    # --- 変数リスト ---
    output$variable_list <- renderUI({
      if (is.null(rv$data)) {
        return(tags$p(class = "text-muted", "データを読み込んでください"))
      }

      vars <- names(rv$data)
      numeric_vars <- vars[sapply(rv$data, is.numeric)]

      tags$div(
        style = "max-height: 200px; overflow-y: auto;",
        tags$ul(
          class = "list-unstyled mb-0",
          lapply(numeric_vars, function(v) {
            tags$li(
              class = "py-1 px-2 rounded",
              style = "cursor: pointer; transition: background 0.2s;",
              onmouseover = "this.style.background='#e8f4fc'",
              onmouseout = "this.style.background='transparent'",
              onclick = sprintf(
                "var ta = document.getElementById('%s'); ta.value += ' + %s'; ta.focus();",
                ns("model_syntax"), v
              ),
              tags$code(v)
            )
          })
        )
      )
    })

    # --- 構文バリデーション ---
    observeEvent(input$validate_syntax, {
      syntax <- input$model_syntax
      result <- validate_model_syntax(syntax)

      output$syntax_validation <- renderUI({
        if (result$valid) {
          tags$div(
            class = "alert alert-success mt-3",
            tags$i(class = "fas fa-check-circle me-2"),
            result$message
          )
        } else {
          tags$div(
            class = "alert alert-danger mt-3",
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

    # --- 構文の同期 ---
    observe({
      rv$model_syntax <- input$model_syntax
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

        # lavaan実行
        fit <- sem(
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
        )

        rv$fit <- fit
        rv$fit_summary <- summary(fit, standardized = TRUE, fit.measures = TRUE)
        rv$estimation_complete <- TRUE

        waiter$hide()

        showNotification(
          "分析が完了しました",
          type = "message",
          duration = 5
        )

        # 結果タブに移動
        updateNavbarPage(session, "main_nav", selected = "results_tab")

      }, error = function(e) {
        waiter$hide()
        rv$error_message <- paste("分析エラー:", e$message)
        rv$estimation_complete <- FALSE
      }, warning = function(w) {
        showNotification(
          paste("警告:", w$message),
          type = "warning",
          duration = 10
        )
      })
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
      tags$table(
        class = "table table-sm mb-0",
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("推定方法")),
            tags$td(input$estimator)
          ),
          tags$tr(
            tags$td(tags$strong("欠損値処理")),
            tags$td(input$missing)
          ),
          tags$tr(
            tags$td(tags$strong("標準誤差")),
            tags$td(input$se)
          ),
          if (input$se == "bootstrap") {
            tags$tr(
              tags$td(tags$strong("ブートストラップ回数")),
              tags$td(input$bootstrap_n)
            )
          },
          tags$tr(
            tags$td(tags$strong("潜在変数分散=1")),
            tags$td(if (input$std_lv) "はい" else "いいえ")
          ),
          tags$tr(
            tags$td(tags$strong("平均構造")),
            tags$td(if (input$meanstructure) "推定" else "なし")
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

    # --- 適合度サマリーカード ---
    output$fit_summary_cards <- renderUI({
      req(rv$fit)

      fm <- fitMeasures(rv$fit)

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
              tags$br()
              tags$span(
                class = paste("badge mt-1", gsub("fit-", "bg-", eval_result$class)),
                eval_result$judgment
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)
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
            p値 = pvalue
          )
      } else {
        result <- loadings %>%
          select(
            因子 = lhs,
            指標 = rhs,
            推定値 = est,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)
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
            p値 = pvalue
          )
      } else {
        result <- regressions %>%
          select(
            従属変数 = lhs,
            独立変数 = rhs,
            推定値 = est,
            標準誤差 = se,
            z値 = z,
            p値 = pvalue
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)
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

      params <- parameterEstimates(rv$fit, standardized = TRUE)

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

# --- HTMLレポート生成 ---
generate_html_report <- function(fit, model_syntax, data_name) {
  fm <- fitMeasures(fit)
  params <- parameterEstimates(fit, standardized = TRUE)

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
  <p class="meta">Data: ', data_name, '</p>

  <h2>Model Syntax</h2>
  <pre>', model_syntax, '</pre>

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
    <tr><th>Path</th><th>Estimate</th><th>Std.All</th><th>SE</th><th>z</th><th>p</th></tr>')

  for (i in 1:nrow(params)) {
    html <- paste0(html, '
    <tr>
      <td>', params$lhs[i], ' ', params$op[i], ' ', params$rhs[i], '</td>
      <td>', sprintf("%.3f", params$est[i]), '</td>
      <td>', sprintf("%.3f", params$std.all[i]), '</td>
      <td>', sprintf("%.3f", params$se[i]), '</td>
      <td>', sprintf("%.3f", params$z[i]), '</td>
      <td>', sprintf("%.4f", params$pvalue[i]), '</td>
    </tr>')
  }

  html <- paste0(html, '
  </table>
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

    # --- パス図 ---
    output$sem_diagram <- renderPlot({
      req(rv$fit)

      what_param <- switch(
        input$what,
        "std" = "std",
        "est" = "est",
        "par" = "par",
        "nothing" = "nothing"
      )

      semPaths(
        rv$fit,
        what = what_param,
        whatLabels = what_param,
        layout = input$layout,
        style = "lisrel",
        residuals = input$residuals,
        intercepts = input$intercepts,
        thresholds = input$thresholds,
        nCharNodes = 0,
        nCharEdges = 0,
        sizeMan = input$node_size,
        sizeLat = input$node_size * 1.2,
        edge.label.cex = input$label_size,
        edge.width = input$edge_size,
        curve = 2,
        curvePivot = TRUE,
        mar = c(2, 2, 2, 2),
        color = list(
          lat = input$lat_color,
          man = input$man_color
        ),
        border.color = "#2c3e50",
        edge.color = "#34495e",
        label.color = "#2c3e50"
      )
    })

    # --- パス図ダウンロード ---
    output$download_diagram <- downloadHandler(
      filename = function() {
        paste0("sem_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$download_format)
      },
      content = function(file) {
        what_param <- switch(
          input$what,
          "std" = "std",
          "est" = "est",
          "par" = "par",
          "nothing" = "nothing"
        )

        if (input$download_format == "png") {
          png(file, width = input$download_width, height = input$download_height, units = "in", res = 300)
        } else if (input$download_format == "pdf") {
          pdf(file, width = input$download_width, height = input$download_height)
        } else {
          svg(file, width = input$download_width, height = input$download_height)
        }

        semPaths(
          rv$fit,
          what = what_param,
          whatLabels = what_param,
          layout = input$layout,
          style = "lisrel",
          residuals = input$residuals,
          intercepts = input$intercepts,
          thresholds = input$thresholds,
          nCharNodes = 0,
          nCharEdges = 0,
          sizeMan = input$node_size,
          sizeLat = input$node_size * 1.2,
          edge.label.cex = input$label_size,
          edge.width = input$edge_size,
          curve = 2,
          curvePivot = TRUE,
          mar = c(2, 2, 2, 2),
          color = list(
            lat = input$lat_color,
            man = input$man_color
          ),
          border.color = "#2c3e50",
          edge.color = "#34495e",
          label.color = "#2c3e50"
        )

        dev.off()
      }
    )
  })
}

# -----------------------------------------------------------------------------
# ヘルプモジュール サーバー
# -----------------------------------------------------------------------------
help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ヘルプタブは静的コンテンツのみ
  })
}
