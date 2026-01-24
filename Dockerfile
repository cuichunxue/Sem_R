# =============================================================================
# SEM Analysis Web App - Production Dockerfile
# Version: 1.0.0
# =============================================================================

FROM rocker/shiny:4.3.0

LABEL maintainer="SEM Analysis Tool"
LABEL version="1.0.0"
LABEL description="Production-ready SEM analysis web application using lavaan"

# 環境変数
ENV SHINY_LOG_STDERR=1
ENV SHINY_APP_DIR=/srv/shiny-server/sem-app
ENV R_LIBS_USER=/usr/local/lib/R/site-library

# システム依存関係のインストール
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Rパッケージのインストール（キャッシュ効率化のため分割）
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/')); \
    install.packages(c('shiny', 'bslib', 'shinyWidgets', 'shinyjs', 'waiter', 'DT'))"

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/')); \
    install.packages(c('dplyr', 'tidyr', 'readr', 'readxl', 'haven'))"

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/')); \
    install.packages(c('lavaan', 'semPlot', 'ggplot2'))"

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/')); \
    install.packages(c('corrplot', 'colourpicker', 'htmltools', 'jsonlite'))"

# セキュリティ: 非rootユーザーで実行
RUN useradd -m -s /bin/bash appuser || true

# アプリケーションディレクトリ作成
RUN mkdir -p ${SHINY_APP_DIR} \
    && mkdir -p /var/log/shiny-server \
    && mkdir -p /tmp/shiny-uploads

# アプリケーションファイルをコピー
COPY --chown=shiny:shiny . ${SHINY_APP_DIR}/

# 権限設定
RUN chmod -R 755 ${SHINY_APP_DIR} \
    && chmod -R 777 /tmp/shiny-uploads \
    && chown -R shiny:shiny /var/log/shiny-server

# 一時ファイル用ディレクトリ
VOLUME ["/tmp/shiny-uploads", "/var/log/shiny-server"]

# ポート公開
EXPOSE 3838

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# 作業ディレクトリ
WORKDIR ${SHINY_APP_DIR}

# shinyユーザーで実行
USER shiny

# アプリ起動
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
