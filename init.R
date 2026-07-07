# ==============================================================================
# File: init.R
# Mục đích: Thiết lập môi trường phát triển (Development Setup)
#           và tải toàn bộ dữ liệu từ D1/cache targets vào môi trường Global.
#           Dành cho việc viết mã và debug các biểu đồ trong thư mục `charts`.
# ==============================================================================

# 1. Tự động kiểm tra và cài đặt các thư viện nếu thiếu
required_packages <- c("targets", "tarchetypes", "tidyverse", "glue", 
                       "jsonlite", "dotenv", "here", "plotly", "highcharter")

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  message("Đang cài đặt các thư viện còn thiếu: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(targets)
library(dotenv)

# 2. Đọc file cấu hình dự án & biến môi trường (.env)
message("\n[+] Đang nạp cấu hình dự án từ R/config.R...")
if (file.exists("R/config.R")) {
  source("R/config.R")
} else {
  stop("Không tìm thấy file R/config.R! Vui lòng kiểm tra lại thư mục làm việc.")
}

# 3. Kiểm tra xem targets pipeline đã được chạy lần nào chưa
# Nếu chưa có thư mục cache '_targets', ta tiến hành chạy tar_make() để lấy dữ liệu từ D1
if (!dir.exists("_targets")) {
  message("\n[!] Không tìm thấy dữ liệu cache '_targets'. Đang bắt đầu chạy pipeline lần đầu để kéo dữ liệu từ Cloudflare D1...")
  tar_make()
} else {
  message("\n[+] Đã tìm thấy thư mục cache '_targets'.")
}

# 4. Tải dữ liệu từ cache targets
message("\n[+] Đang tải dữ liệu 'all_tables' từ targets cache...")
all_tables <- tryCatch({
  tar_read(all_tables)
}, error = function(e) {
  message("[!] Lỗi khi đọc cache: ", e$message)
  message("[!] Đang thử chạy tar_make() để xây dựng lại cache...")
  tar_make()
  tar_read(all_tables)
})

# 5. Unpack toàn bộ dataframe trong list ra môi trường Global (.GlobalEnv)
message(paste0("[+] Đang giải nén ", length(all_tables), " bảng dữ liệu vào môi trường làm việc (Global)..."))
invisible(list2env(all_tables, envir = .GlobalEnv))

# Liệt kê các bảng dữ liệu đã nạp thành công
table_names <- names(all_tables)
message("\n=== CÁC BẢNG DỮ LIỆU ĐÃ NẠP THÀNH CÔNG ===")
for (name in table_names) {
  if (exists(name, envir = .GlobalEnv)) {
    df_obj <- get(name, envir = .GlobalEnv)
    message(sprintf("  - %-30s: %d dòng, %d cột", name, nrow(df_obj), ncol(df_obj)))
  }
}

message("\n=======================================================================")
message(" SẴN SÀNG! Bạn có thể bắt đầu code và chạy thử các file trong charts/")
message(" Ví dụ để vẽ biểu đồ Tổng tài sản (charts/tts.R):")
message("   source('charts/tts.R')")
message("   chart_tts")
message("=======================================================================")
