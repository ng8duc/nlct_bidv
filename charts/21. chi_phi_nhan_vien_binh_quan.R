df_slld <- df_so_luong_lao_dong %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(-date, names_to = "name", values_to = "value")

df_slld2 <- df_slld %>% 
  filter(month(date) == 12) %>% 
  mutate(target_year = year(date) + 1) %>% 
  select(-date) %>% 
  rename_with(
    .fn = ~ str_c("q4_prev_", .x),
    .cols = -c(name, target_year)
  )

df_slld <- df_slld %>% 
  mutate(current_year = year(date)) %>% 
  left_join(df_slld2, by = c("name" = "name", "current_year" = "target_year")) %>% 
  mutate(ldbq = (value + q4_prev_value)/2) %>% 
  mutate(yq = floor_date(date, unit = "quarter")) %>% 
  select(yq, name, ldbq)


df <- df_chi_phi_nhan_vien %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(yq %in% c(CUR_DATE, REF_DATE))

df <- df %>% 
  filter(nchar(name) <= 4)

df <- df %>% 
  left_join(df_slld, by = c("yq", "name"))

df <- df %>% 
  mutate(value = value/ldbq/12*1000)

df <- df %>%
  group_by(name) %>%
  arrange(yq) %>%
  mutate(
    growth_rate = ((value/lag(value))^(1/YEAR_DIFF) - 1)*100,
  ) %>%
  ungroup()

df1 <- df %>% 
  filter(yq == CUR_DATE) %>% 
  arrange(desc(value))

order_names <- df1$name

df2 <- df %>% 
  filter(yq == REF_DATE) %>% 
  mutate(name = factor(name, levels = order_names)) %>% 
  arrange(name)

df1 <- df1 %>% 
  mutate(name = factor(name, levels = order_names))


chart_chi_phi_nhan_vien_binh_quan <- highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "Chi phí nhân viên (triệu đồng)"),
      gridLineColor = "#e6e6e6"
    ),
    list(
      title = list(text = "Tăng trưởng bình quân/năm (%)"),
      opposite = TRUE,
      gridLineWidth = 0, # Ẩn vạch kẻ ngang của trục thứ hai để tránh rối mắt
      labels = list(format = "{value:,.1f}%")
    )
  ) %>%
  hc_xAxis(
    categories = df1$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_add_series(
    data = df2,
    mapping = hcaes(x = name, y = value),
    type = "column",
    name = ifelse(month(REF_DATE) == 10, year(REF_DATE), str_glue("{month(REF_DATE)+2}T/year(CUR_DATE)")),
    color = "#006b68",
    yAxis = 0,
    tooltip = list(
      valueSuffix = " triệu đồng"
    )
  ) %>%
  hc_add_series(
    data = df1,
    mapping = hcaes(x = name, y = value),
    type = "column",
    name = ifelse(month(CUR_DATE) == 10, year(CUR_DATE), str_glue("{month(CUR_DATE)+2}T/year(CUR_DATE)")),
    color = "#fdb71a",
    yAxis = 0,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}", # Hiển thị nhãn giá trị với 2 chữ số thập phân
      style = list(fontSize = "10px")
    ),
    tooltip = list(
      valueSuffix = " nghìn tỷ đồng"
    )
  ) %>% 
  hc_add_series(
    data = df1,
    mapping = hcaes(x = name, y = growth_rate),
    type = "line",
    lineWidth = 0,
    marker = list(enabled = TRUE, radius = 5),
    name = "Tăng trưởng bình quân/năm",
    yAxis = 1,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.1f}%",
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
  hc_chart(zoomType = "x") %>% 
  hc_title(
    align = "center",
    text = "Chi phí nhân viên bình quân/người/tháng của 11 NHTM quy mô lớn",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
