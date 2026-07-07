# ==============================================================================
# File: R/get_all_tables.R
# Tác giả: Antigravity
# Mục đích: Định nghĩa hàm tải toàn bộ các bảng dữ liệu từ Cloudflare D1
# ==============================================================================

library(tidyverse)
library(glue)

#' Tải toàn bộ các bảng dữ liệu từ Cloudflare D1 và trả về một danh sách các Dataframe
#' @param db_name Tên của D1 Database
#' @return Danh sách các dataframe có tên dạng df_{table_name}
get_all_tables_list <- function(db_name) {
  # 1. Truy vấn danh sách các bảng trong database (loại trừ các bảng hệ thống)
  query_get_tables <- "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' AND name NOT LIKE '_cf_%' AND name NOT LIKE 'd1_%'"
  
  message("--- Bắt đầu lấy danh sách bảng từ Cloudflare D1 ---")
  tables_df <- pull_from_d1(db_name = db_name, query = query_get_tables)
  
  if (is.null(tables_df) || nrow(tables_df) == 0) {
    stop("Không tìm thấy bảng dữ liệu nào trong database D1.")
  }
  
  # Lấy danh sách tên bảng dạng vector
  table_names <- tables_df %>% 
    pull(name)
  
  message(glue("Tìm thấy {length(table_names)} bảng: {paste(table_names, collapse = ', ')}"))
  
  # 2. Tải từng bảng về và gán vào danh sách
  dfs <- map(table_names, function(tbl) {
    message(glue("\n[+] Đang tải bảng: '{tbl}'"))
    pull_from_d1(db_name = db_name, table_name = tbl)
  })
  
  # Đặt tên cho từng phần tử trong danh sách dạng df_{table_name}
  names(dfs) <- paste0("df_", table_names)
  
  message("\n--- Hoàn tất tải dữ liệu từ D1! ---")
  return(dfs)
}
