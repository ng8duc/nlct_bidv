df <- df_car

df <- df %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  fill(everything(), .direction = "down")

df <- df %>% 
  pivot_longer(-c(date), names_to = "name", values_to = "value")

df <- df %>% 
  mutate(yq = floor_date(date, unit = "quarter")) %>% 
  select(-date)

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
    mapping = hcaes(x = name, y = value),
    type = "column",
    name = str_glue("{strftime(max(df2$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    color = "#006b68",
    yAxis = 0,
    tooltip = list(
      valueSuffix = "%"
    )
  ) %>% 
  hc_add_series(
    data = df1,
    mapping = hcaes(x = name, y = value),
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
    text = "Tỷ lệ an toàn vốn (CAR) của 11 NHTM quy mô lớn",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
