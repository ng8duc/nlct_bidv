# File: pull_from_d1.R
# Tác giả: Antigravity
# Mục đích: Kéo dữ liệu từ Cloudflare D1 về R Dataframe thông qua Wrangler CLI

library(jsonlite)
library(glue)

#' Kéo dữ liệu từ Cloudflare D1 về R Dataframe (có hỗ trợ chunk/pagination)
#' @param db_name Tên database (ví dụ: "my-db")
#' @param table_name Tên table cần lấy dữ liệu
#' @param query Câu lệnh SQL tùy chỉnh
#' @param chunk_size Số dòng lấy mỗi lần (mặc định 5000)
#' @return Một Dataframe chứa toàn bộ dữ liệu
pull_from_d1 <- function(db_name, table_name = NULL, query = NULL, chunk_size = 5000) {
  
  if (is.null(query) && is.null(table_name)) {
    stop("Bạn phải cung cấp ít nhất table_name hoặc query.")
  }
  
  # Xây dựng query cơ sở
  base_query <- if (!is.null(query)) query else glue("SELECT * FROM {table_name}")
  
  all_results <- list()
  offset <- 0
  has_more <- TRUE
  
  message(glue("Bắt đầu kéo dữ liệu từ database '{db_name}'..."))
  
  while (has_more) {
    # Bọc query gốc để áp dụng LIMIT và OFFSET một cách an toàn
    paged_query <- glue("SELECT * FROM ({base_query}) LIMIT {chunk_size} OFFSET {offset}")
    
    message(glue("  [+] Đang lấy dòng {offset + 1} đến {offset + chunk_size}..."))
    
    res <- system2("wrangler", 
                   args = c("d1", "execute", db_name, "--command", shQuote(paged_query), "--json", "--remote"),
                   stdout = TRUE, 
                   stderr = TRUE)
    
    if (length(res) == 0) {
      warning("Không nhận được phản hồi từ wrangler ở chunk này. Dừng lại tại đây.")
      break
    }
    
    json_output <- paste(res, collapse = "\n")
    parsed_data <- tryCatch({
      fromJSON(json_output, simplifyDataFrame = TRUE)
    }, error = function(e) {
      stop(glue("Lỗi parse JSON: {e$message}\nOutput: {json_output}"))
    })
    
    # Lấy results
    chunk_df <- if (is.list(parsed_data) && "results" %in% names(parsed_data[[1]])) {
      parsed_data[[1]]$results
    } else if (is.list(parsed_data) && "results" %in% names(parsed_data)) {
      parsed_data$results
    } else {
      NULL
    }
    
    if (is.null(chunk_df) || (is.data.frame(chunk_df) && nrow(chunk_df) == 0) || length(chunk_df) == 0) {
      has_more <- FALSE
    } else {
      chunk_df <- as.data.frame(chunk_df)
      all_results[[length(all_results) + 1]] <- chunk_df
      
      # Nếu số dòng trả về ít hơn chunk_size, nghĩa là đã hết dữ liệu
      if (nrow(chunk_df) < chunk_size) {
        has_more <- FALSE
      } else {
        offset <- offset + chunk_size
      }
    }
  }
  
  if (length(all_results) == 0) {
    message("Kết quả trả về rỗng.")
    return(data.frame())
  }
  
  # Gộp tất cả các chunk thành một dataframe duy nhất
  final_df <- do.call(rbind, all_results)
  
  message(glue("Hoàn tất! Tổng cộng đã tải về {nrow(final_df)} dòng."))
  return(final_df)
}

# --- Hướng dẫn nhanh ---
# 1. Cài đặt package jsonlite nếu chưa có: install.packages("jsonlite")
# 2. Load file này: source("pull_from_d1.R")
# 3. Chạy lệnh: 
#    df <- pull_from_d1("tên-db-của-bạn", "test_table")
#    Hoặc dùng query tùy chỉnh:
#    df <- pull_from_d1("tên-db-của-bạn", query = "SELECT * FROM users WHERE active = 1")
