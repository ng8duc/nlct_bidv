df <- list(
  "Thu dịch vụ thuần" = df_ty_trong_thu_dvr,
  "Thu thuần từ chứng khoán kinh doanh" = df_ty_trong_thu_rong_tu_ck_kinh_doanh,
  "Thu thuần từ chứng khoán đầu tư" = df_ty_trong_thu_rong_tu_ck_dau_tu,
  "Thu thuần từ kinh doanh ngoại hối" = df_ty_trong_thu_rong_tu_kd_ngoai_hoi,
  "Thu thuần từ góp vốn, mua cổ phần" = df_ty_trong_thu_rong_tu_von_gop_co_phan,
  "Thu thuần từ hoạt động khác" = df_ty_trong_thu_rong_tu_hd_khac
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

plot_co_cau_thu_ngoai_lai <- function(df) {
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
      mapping = hcaes(x = name, y = `Thu dịch vụ thuần`*100),
      type = "column",
      name = "Thu dịch vụ thuần",
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu thuần từ chứng khoán kinh doanh`*100),
      type = "column",
      name = "Thu thuần từ chứng khoán kinh doanh",
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu thuần từ chứng khoán đầu tư`*100),
      type = "column",
      name = "Thu thuần từ chứng khoán đầu tư",
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu thuần từ kinh doanh ngoại hối`*100),
      type = "column",
      name = "Thu thuần từ kinh doanh ngoại hối",
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu thuần từ góp vốn, mua cổ phần`*100),
      type = "column",
      name = "Thu thuần từ góp vốn, mua cổ phần",
    ) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = `Thu thuần từ hoạt động khác`*100),
      type = "column",
      name = "Thu thuần từ hoạt động khác",
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
      text = str_glue("Cơ cấu thu ngoài lãi của 11 NHTM quy mô lớn"),
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

plot_co_cau_thu_ngoai_lai(df = df1)
plot_co_cau_thu_ngoai_lai(df = df2)

