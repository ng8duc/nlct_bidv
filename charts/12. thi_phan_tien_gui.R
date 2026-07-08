library(tidyverse)
library(highcharter)
library(timetk)

df_tien_gui <- df_tien_gui %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(nchar(name) <= 4)

df_tien_gui_total <- df_tien_gui_total %>% 
  mutate(ym = as.Date(ym)) %>% 
  filter(month(ym) %in% c(3, 6, 9, 12)) %>% 
  summarise_by_time(.date_var = ym,
                    .by = "quarter",
                    tien_gui = max(tien_gui)) %>% 
  rename(yq := ym)

df0 <- df_tien_gui %>% 
  mutate(value = -value) %>% 
  select(-c(name)) %>% 
  bind_rows(rename(df_tien_gui_total, value := tien_gui)) %>% 
  group_by(yq) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(name = "Khác") %>% 
  filter(value > 0)

df <- bind_rows(df0, df_tien_gui %>% filter(yq <= max(df0$yq)))

df <- df %>% 
  group_by(yq) %>% 
  mutate(share = value/sum(value)) %>% 
  ungroup()

df1 <- df %>% 
  filter(yq == CUR_DATE)

df1 <- bind_rows(filter(df1, name != "Khác") %>% arrange(desc(share)), filter(df1, name == "Khác"))

df2 <- df %>% 
  filter(yq == REF_DATE)

df2 <- bind_rows(filter(df2, name != "Khác") %>% arrange(desc(share)), filter(df2, name == "Khác"))

plot_thi_phan_tien_gui <- function(df){
  highchart() %>% 
    hc_colors(colors = c(
      "#006b68", "#fdb71a", "#0088cc", "#e65c00", 
      "#22b14c", "#d9383a", "#8e44ad", "#bdc3c7", 
      "#16a085", "#e84393", "#f39c90", "#2c3e50"
    )) %>% 
    hc_add_series(
      data = df,
      mapping = hcaes(x = name, y = share*100),
      type = "pie",
      name = "Thị phần tiền gửi"
    ) %>% 
    hc_tooltip(
      valueDecimals = 2,
      valueSuffix = "%"
    ) %>% 
    hc_plotOptions(
      pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(
          enabled = TRUE,
          format = "<b>{point.name}</b>: {point.percentage:.1f}%"
        )
      )
    ) %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(
      align = "center",
      text = "Thị phần huy động tiền gửi của các NHTM",
      style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
    ) %>% 
    hc_subtitle(
      align = "center",
      text = str_glue("Ngày số liệu: {strftime(max(df$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
      style = list(fontStyle = "italic", color = "#666666")
    )
}

chart_thi_phan_tien_gui1 <- plot_thi_phan_tien_gui(df1)
chart_thi_phan_tien_gui2 <- plot_thi_phan_tien_gui(df2)
