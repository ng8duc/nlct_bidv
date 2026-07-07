df <- df_von_dieu_le %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(yq %in% c(CUR_DATE, REF_DATE))

df <- df %>% 
  filter(nchar(name) <= 4)

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


highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "Vốn điều lệ (nghìn tỷ đồng)"),
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
    mapping = hcaes(x = name, y = value / 1000),
    type = "column",
    name = str_glue("{strftime(max(df2$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    color = "#006b68",
    yAxis = 0,
    tooltip = list(
      valueSuffix = " nghìn tỷ đồng"
    )
  ) %>%
  hc_add_series(
    data = df1,
    mapping = hcaes(x = name, y = value / 1000),
    type = "column",
    name = str_glue("{strftime(max(df1$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
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
      format = "{point.y:,.2f}%",
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
    text = "Vốn điều lệ của 11 NHTM quy mô lớn",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
