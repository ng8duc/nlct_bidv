df <- list(
  "Thu lãi ròng" = df_ty_trong_thu_lai_rong,
  "Thu ngoài lãi ròng" = df_ty_trong_thu_phi_lai_rong
) %>% 
  bind_rows(.id = "category") %>% 
  mutate(yq = as.Date(yq))

df <- df %>% 
  filter(nchar(name) <= 4)

df <- df %>% 
  filter(month(yq) == month(max(yq))) %>% 
  arrange(name)

df1 <- df %>% 
  filter(yq == CUR_DATE) %>% 
  pivot_wider(names_from = "category", values_from = "value")

df2 <- df %>% 
  filter(yq == REF_DATE) %>% 
  pivot_wider(names_from = "category", values_from = "value")

plot_co_cau_thu_nhap <- function(df) {
  highchart() %>% 
    hc_yAxis(
      title = list(text = "%"),
      gridLineColor = "#e6e6e6"
    ) %>%
    hc_xAxis(
      categories = df$name,
      gridLineWidth = 1,
      gridLineColor = "#e6e6e6"
    ) %>%
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu lãi ròng`*100),
      type = "column",
      name = "Thu lãi ròng",
      color = "#006b68",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.2f}%", # Hiển thị nhãn giá trị với 2 chữ số thập phân
        style = list(fontSize = "10px")
      ),
      tooltip = list(
        valueSuffix = "%"
      )
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu ngoài lãi ròng`*100),
      type = "column",
      name = "Thu ngoài lãi ròng",
      color = "#fdb71a",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.2f}%", # Hiển thị nhãn giá trị với 2 chữ số thập phân
        style = list(fontSize = "10px")
      ),
      tooltip = list(
        valueSuffix = "%"
      )
    ) %>% 
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = 2
    ) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_legend(
      align = "center",
      verticalAlign = "top",
      layout = "horizontal",
      symbolRadius = 0
    ) %>% 
    hc_plotOptions(
      column = list(stacking = "percent")
    ) %>% 
    hc_title(
      align = "center",
      text = str_glue("Cơ cấu thu nhập của 11 NHTM quy mô lớn"),
      style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
    ) %>% 
    hc_subtitle(
      align = "center",
      text = ifelse(
        month(max(df$yq)) == 10,
        str_glue("Năm {year(max(df$yq))}"),
        str_glue("Lũy kế {month(max(df$yq)) + 2} tháng đầu năm {year(max(df$yq))}")
      ),
      style = list(fontStyle = "italic", color = "#666666")
    )
}

plot_co_cau_thu_nhap(df = df1)
plot_co_cau_thu_nhap(df = df2)

