# ==============================================================================
# File: _targets.R
# Mục đích: Định nghĩa các bước xử lý (pipeline) và quản lý cache dữ liệu
# ==============================================================================

library(targets)
library(tarchetypes)

# Thiết lập cấu hình chung cho targets
tar_option_set(
  packages = c("tidyverse", "glue", "jsonlite", "dotenv"),
  format = "rds"
)

# Nạp các file script chứa hàm nghiệp vụ
source("R/config.R")
source("R/get_all_tables.R")

# Định nghĩa danh sách các target trong pipeline
list(
  # 1. Target xác định tên database từ file .env
  tar_target(
    db_name,
    Sys.getenv("D1_DATABASE")
  ),
  
  # 2. Target tải toàn bộ dữ liệu từ D1 về dưới dạng list dataframe
  tar_target(
    all_tables,
    get_all_tables_list(db_name)
  ),
  
  # 3. Target tự động render dashboard khi dữ liệu (all_tables) thay đổi
  tar_quarto(
    dashboard_report,
    "report.qmd",
    quiet = FALSE
  )
)
