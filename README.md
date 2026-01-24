# SEM Analysis Web Application

lavaan パッケージを使用した構造方程式モデリング（SEM）解析のための実務レベル Web アプリケーション。

## 特徴

- **直感的なUI**: モダンでレスポンシブなインターフェース
- **多様なデータ形式**: CSV, Excel, SPSS, SAS, Stata, RDS に対応
- **豊富なモデルテンプレート**: CFA, SEM, 媒介分析, パス解析など
- **柔軟な推定オプション**: ML, MLR, WLSMV など複数の推定方法
- **詳細な結果表示**: 適合度指標、パラメータ推定値、修正指標
- **パス図の可視化**: カスタマイズ可能なパス図の生成とダウンロード
- **結果のエクスポート**: HTML レポートとして結果を保存

## 必要条件

- R 4.0 以上
- 必要な R パッケージ（下記参照）

## インストール

### 方法1: インストールスクリプトを使用

```r
source("install_packages.R")
```

### 方法2: 手動インストール

```r
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "shinyjs", "waiter",
  "DT", "dplyr", "tidyr", "readr", "readxl", "haven",
  "lavaan", "semPlot", "ggplot2", "corrplot", "colourpicker",
  "knitr", "kableExtra", "htmltools"
))
```

## 起動方法

### 方法1: RStudio から

```r
shiny::runApp()
```

### 方法2: コマンドラインから

```bash
Rscript run_app.R
```

### 方法3: Docker を使用

```bash
docker build -t sem-app .
docker run -p 3838:3838 sem-app
```

ブラウザで http://localhost:3838 にアクセス

## 使い方

### 1. データの読み込み

「データ」タブでファイルをアップロードするか、サンプルデータを選択します。

対応形式:
- CSV (.csv)
- Excel (.xlsx, .xls)
- SPSS (.sav)
- SAS (.sas7bdat)
- Stata (.dta)
- R data (.rds)

### 2. モデルの定義

「モデル定義」タブで lavaan 構文を使ってモデルを記述します。

#### lavaan 構文の例

```
# 確認的因子分析 (CFA)
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9

# 構造モデル（回帰）
speed ~ visual + textual
```

#### 構文リファレンス

| 演算子 | 意味 | 例 |
|--------|------|-----|
| `=~` | 測定（因子負荷） | `F1 =~ x1 + x2 + x3` |
| `~` | 回帰 | `y ~ x1 + x2` |
| `~~` | 共分散/分散 | `x1 ~~ x2` |
| `~1` | 切片 | `x1 ~ 1` |
| `:=` | 定義パラメータ | `ind := a*b` |
| `*` | ラベル/制約 | `F1 =~ a*x1` |

### 3. 推定設定

「推定設定」タブで以下のオプションを選択:

- **推定方法**: ML, MLR, MLM, WLS, WLSMV, GLS, ULS
- **欠損値処理**: リストワイズ、ペアワイズ、FIML
- **標準誤差**: 標準、ロバスト、ブートストラップ

### 4. 分析実行

「分析を実行」ボタンをクリック

### 5. 結果の確認

「結果」タブで以下を確認:

- **適合度指標**: χ², CFI, TLI, RMSEA, SRMR, AIC, BIC
- **パラメータ推定値**: 因子負荷、回帰係数、共分散、分散
- **修正指標**: モデル改善のための提案

### 6. パス図

「パス図」タブでモデルを視覚化し、PNG/PDF/SVG としてダウンロード

## 適合度指標の基準

| 指標 | 良好 | 許容 |
|------|------|------|
| CFI | ≥ 0.95 | ≥ 0.90 |
| TLI | ≥ 0.95 | ≥ 0.90 |
| RMSEA | ≤ 0.05 | ≤ 0.08 |
| SRMR | ≤ 0.05 | ≤ 0.08 |

## ディレクトリ構造

```
Sem_R/
├── app.R                 # メインアプリケーション
├── global.R              # グローバル設定
├── run_app.R             # 起動スクリプト
├── install_packages.R    # パッケージインストーラー
├── Dockerfile            # Docker設定
├── R/
│   ├── utils.R           # ユーティリティ関数
│   ├── ui_modules.R      # UIモジュール
│   └── server_modules.R  # サーバーモジュール
├── data/
│   └── sample_sem_data.csv
└── www/
    └── custom.css        # カスタムスタイル
```

## トラブルシューティング

### モデルが収束しない

- 開始値を変更する
- モデルを簡略化する
- サンプルサイズを確認する

### 負の分散が推定された

- モデルの特定に問題がある可能性
- 制約を追加する
- モデルを再検討する

### 適合度が悪い

- 修正指標を参考にモデルを改善
- 理論的な見直しを行う

## ライセンス

MIT License

## 参考文献

- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
- Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models. Structural Equation Modeling, 22(3), 474-483.
