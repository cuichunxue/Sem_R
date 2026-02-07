# SEM Analysis Web Application v2.0

lavaan パッケージを使用した構造方程式モデリング（SEM）解析のための商用レベル Web アプリケーション。

## 主要機能

### コア機能
- **直感的なUI**: モダンでレスポンシブなインターフェース（Bootstrap 5 + Flatly テーマ）
- **多様なデータ形式**: CSV, Excel, SPSS, SAS, Stata, RDS に対応
- **豊富なモデルテンプレート**: CFA, SEM, 媒介分析, パス解析, MIMIC, 高次因子, バイファクター
- **柔軟な推定オプション**: ML, MLR, WLSMV など複数の推定方法
- **詳細な結果表示**: 適合度指標、パラメータ推定値、修正指標、R²
- **パス図の可視化**: カスタマイズ可能なパス図の生成とダウンロード
- **モデル比較**: 複数モデルの適合度比較とカイ二乗差検定

### v2.0 新機能
- **正規性検定**: Shapiro-Wilk 単変量検定 + Mardia の多変量正規性検定
- **外れ値検出**: Mahalanobis 距離による多変量外れ値の検出
- **信頼性分析**: Cronbach's Alpha（項目分析付き）+ McDonald's Omega（CFA ベース）
- **R² 表示**: 各変数の説明率と効果量の解釈（Cohen の基準）
- **セキュリティ強化**: HTML エスケープによる XSS 防止、入力バリデーション強化
- **改善された HTML レポート**: 適合度サマリーカード、R² セクション、95% CI
- **MIMIC モデルテンプレート**: Multiple Indicators Multiple Causes モデル
- **lavaan エラーの日本語翻訳**: 分かりやすいエラーメッセージとヘルプ

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
  "knitr", "kableExtra", "htmltools", "jsonlite"
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
- CSV (.csv), TSV (.tsv), テキスト (.txt)
- Excel (.xlsx, .xls)
- SPSS (.sav)
- SAS (.sas7bdat)
- Stata (.dta)
- R data (.rds)

### 2. データの診断（推奨）

アップロード後、以下の診断を確認:

- **基本統計量**: 平均、標準偏差、歪度、尖度
- **相関行列**: Pearson/Spearman/Kendall 相関
- **正規性検定**: Shapiro-Wilk 検定 + Mardia の多変量検定
- **外れ値検出**: Mahalanobis 距離による検出
- **信頼性分析**: Cronbach's Alpha（項目分析付き）
- **欠損値**: 変数ごとの欠損パターン

### 3. モデルの定義

「モデル定義」タブで lavaan 構文を使ってモデルを記述します。

#### 入力方法
1. **GUIビルダー**: 視覚的にモデルを構築
2. **テンプレート**: 定型モデルから選択・修正
3. **直接入力**: lavaan 構文を直接記述

#### lavaan 構文リファレンス

| 演算子 | 意味 | 例 |
|--------|------|-----|
| `=~` | 測定（因子負荷） | `F1 =~ x1 + x2 + x3` |
| `~` | 回帰 | `y ~ x1 + x2` |
| `~~` | 共分散/分散 | `x1 ~~ x2` |
| `~1` | 切片 | `x1 ~ 1` |
| `:=` | 定義パラメータ | `ind := a*b` |
| `*` | ラベル/制約 | `F1 =~ a*x1` |

### 4. 推定設定

「推定設定」タブで以下のオプションを選択:

- **推定方法**: ML, MLR, MLM, WLS, WLSMV, GLS, ULS
- **欠損値処理**: リストワイズ、ペアワイズ、FIML
- **標準誤差**: 標準、ロバスト、ブートストラップ

| 推定方法 | 用途 |
|----------|------|
| ML | 標準的な最尤法（正規分布仮定） |
| MLR | ロバスト最尤法（非正規データに推奨） |
| MLM | Satorra-Bentler補正（非正規データ向け） |
| WLSMV | カテゴリカル/順序データに最適 |

### 5. 結果の確認

「結果」タブで以下を確認:

- **適合度指標**: χ², CFI, TLI, RMSEA (90% CI), SRMR, AIC, BIC
- **パラメータ推定値**: 因子負荷、回帰係数、共分散、分散（標準化/非標準化）
- **R²（説明率）**: 各変数の分散説明率と効果量
- **信頼性（ω）**: McDonald's Omega（CFA ベース合成信頼性）
- **修正指標**: モデル改善のための提案

### 6. パス図

「パス図」タブでモデルを視覚化し、PNG/PDF/SVG としてダウンロード

### 7. モデル比較

「モデル比較」タブで複数モデルの適合度を比較:
- 適合度指標の一覧比較
- カイ二乗差検定（ネストモデル）
- AIC/BIC による情報量基準比較

## 適合度指標の基準

| 指標 | 良好 | 許容 | 注意事項 |
|------|------|------|----------|
| CFI | >= 0.95 | >= 0.90 | 比較適合度指標 |
| TLI | >= 0.95 | >= 0.90 | 非標準化適合度指標 |
| RMSEA | <= 0.05 | <= 0.08 | 近似の二乗平均平方根誤差 |
| SRMR | <= 0.05 | <= 0.08 | 標準化残差平方根平均 |

## ディレクトリ構造

```
Sem_R/
├── app.R                 # メインアプリケーション
├── global.R              # グローバル設定
├── run_app.R             # 起動スクリプト
├── install_packages.R    # パッケージインストーラー
├── Dockerfile            # Docker設定
├── R/
│   ├── utils.R           # ユーティリティ関数（v2.0: 信頼性/正規性/外れ値/R²）
│   ├── ui_modules.R      # UIモジュール（v2.0: 新タブ追加）
│   └── server_modules.R  # サーバーモジュール（v2.0: セキュリティ修正）
├── data/
│   └── sample_sem_data.csv
├── tests/
│   ├── run_tests.R
│   └── testthat/
│       ├── test-sem-analysis.R
│       ├── test-utils.R
│       └── test-model-comparison.R
└── www/
    └── custom.css        # カスタムスタイル（アクセシビリティ対応）
```

## テスト

```bash
Rscript tests/run_tests.R
```

## トラブルシューティング

### モデルが収束しない
- 開始値を変更する
- モデルを簡略化する
- サンプルサイズを確認する（パラメータ数の5〜10倍を推奨）

### 負の分散が推定された（Heywood ケース）
- モデルの特定に問題がある可能性
- 制約を追加する
- モデルを再検討する

### 適合度が悪い
- 修正指標を参考にモデルを改善
- 理論的な見直しを行う
- 交差負荷量の追加を検討

### 非正規データでの推定
- 「正規性検定」タブで確認
- MLR または WLSMV の使用を検討

## ライセンス

MIT License

## 参考文献

- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.
- Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models. Structural Equation Modeling, 22(3), 474-483.
- Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum Associates.
- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. Structural Equation Modeling, 6(1), 1-55.
