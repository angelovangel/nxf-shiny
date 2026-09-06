# Dockerfile for nxf-shiny — R Shiny app that launches Nextflow pipelines via tmux
FROM --platform=linux/amd64 rocker/shiny:latest

# Install system dependencies: Nextflow (Java), tmux, git, curl, R dev libs
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libuv1-dev \
    git \
    tmux \
    default-jre-headless \
    singularity-container \
    squashfuse \
    fuse \
    && rm -rf /var/lib/apt/lists/*

# Install and pin Nextflow
ENV NXF_VER=25.04.7
RUN curl -fsSL https://github.com/nextflow-io/nextflow/releases/download/v${NXF_VER}/nextflow \
    -o /usr/local/bin/nextflow && \
    chmod +x /usr/local/bin/nextflow

# Use Posit PPM binary mirror for faster R package installs
ENV R_REPOS="https://packagemanager.posit.co/cran/__linux__/jammy/latest"

WORKDIR /app

# Copy renv files first for Docker layer caching
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/

# Restore R package environment
RUN R -e "options(repos = c(CRAN = Sys.getenv('R_REPOS'))); install.packages('renv'); renv::restore()"

# Copy remaining app source files
COPY . .

# Create /mnt mount point (bind-mounted at runtime) and fix /tmp for tmux sockets
RUN mkdir -p /mnt && chmod 1777 /tmp

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
