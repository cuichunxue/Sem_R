# =============================================================================
# SEM Analysis Web App - Dockerfile
# =============================================================================

FROM rocker/shiny:4.3.0

# システム依存関係のインストール
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Rパッケージのインストール
RUN R -e "install.packages(c( \
    'shiny', \
    'bslib', \
    'shinyWidgets', \
    'shinyjs', \
    'waiter', \
    'DT', \
    'dplyr', \
    'tidyr', \
    'readr', \
    'readxl', \
    'haven', \
    'lavaan', \
    'semPlot', \
    'ggplot2', \
    'corrplot', \
    'colourpicker', \
    'knitr', \
    'kableExtra', \
    'htmltools' \
  ), repos='https://cloud.r-project.org/')"

# アプリケーションファイルをコピー
COPY . /srv/shiny-server/sem-app/

# 権限設定
RUN chown -R shiny:shiny /srv/shiny-server/sem-app

# ポート公開
EXPOSE 3838

# 作業ディレクトリ
WORKDIR /srv/shiny-server/sem-app

# アプリ起動
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/sem-app', host='0.0.0.0', port=3838)"]
