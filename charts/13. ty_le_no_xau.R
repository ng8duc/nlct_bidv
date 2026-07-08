df <- df_ty_le_no_xau %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(yq %in% c(CUR_DATE, REF_DATE))

df <- df %>% 
  filter(nchar(name) <= 4)

df1 <- df %>% 
  filter(yq == CUR_DATE) %>% 
  arrange(value)

order_names <- df1$name

df2 <- df %>% 
  filter(yq == REF_DATE) %>% 
  mutate(name = factor(name, levels = order_names)) %>% 
  arrange(name)

df1 <- df1 %>% 
  mutate(name = factor(name, levels = order_names))

chart_ty_le_no_xau <- highchart() %>% 
  hc_yAxis(
    title = list(text = "%"),
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_xAxis(
    categories = df1$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_add_series(
    data = df2,
    mapping = hcaes(x = name, y = value * 100),
    type = "column",
    name = str_glue("{strftime(max(df2$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    color = "#006b68",
    yAxis = 0,
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
    data = df1,
    mapping = hcaes(x = name, y = value * 100),
    type = "column",
    name = str_glue("{strftime(max(df1$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    color = "#fdb71a",
    yAxis = 0,
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
  hc_chart(zoomType = "x") %>% 
  hc_title(
    align = "center",
    text = "Tỷ lệ nợ xấu của 11 NHTM quy mô lớn",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
