library(KlimaKonformC3)

csv.files <- list.files(
  path = "D:/AHomoudi/KlimaKonform/5ter_Lauf_2022-10-28",
  full.names = T,
  recursive = T,
  pattern = ".csv$"
)

# select random 3 files
csv.files <- csv.files[c(13, 113, 313)]

# read files
co2_df <- csv.files %>%
  lapply(readr::read_csv, skip = 2)

# 167 rows & 133 columns
names(co2_df) <- unlist(lapply(csv.files, function(x) {
  x <- unlist(stringr::str_split(string = x, "/"))

  x.row <- x[c(length(x) - 2)]

  x.col <- unlist(stringr::str_split(
    string = x[length(x)],
    "[.]"
  ))[1] %>%
    readr::parse_number()

  return(paste0(x.row, "_", x.col))
}))


co2_df <- dplyr::bind_rows(co2_df, .id = "id")

colnames(co2_df) <- unlist(lapply(colnames(co2_df), function(x) {
  x <- unlist(stringr::str_split(string = x, ":"))


  return(x[length(x)])
}))


co2_df$Date <- as.Date(co2_df$Date)


# clean
co2_df <- co2_df %>%
  dplyr::select(id, Date, AtmCO2)

bla <- data.frame(
  id = c("260201_-67", "450501_0", "851801_0"),
  ID = c("RCP2.6", "RCP4.5", "RCP8.5")
)

co2_df <- co2_df %>%
  dplyr::left_join(bla, by = "id")

library(ggplot2)


(ggplot2::ggplot() +
  ggplot2::geom_path(co2_df,
    mapping = aes(
      x = Date,
      y = AtmCO2,
      group = ID,
      color = ID
    ),
    linewidth = 0.35
  ) +
  ggplot2::theme_bw(base_size = 6) +
  ggplot2::scale_y_continuous(
    limits = c(250, 1300),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_date(
    date_breaks = "10 years",
    minor_breaks = "5 years",
    expand = c(0, 0),
    date_labels = "%Y"
  ) +
  ggplot2::scale_colour_manual(values = c("green", "blue", "red")) + # ,
  # label = c("RCP2.6","RCP4.5","RCP8.5")) +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::ggtitle(label = "AtmCO2") +
  ggplot2::labs(color = "Szenario")
) %>%
  ggplot2::ggsave(
    filename = "AtmCO2.png",
    units = "mm",
    width = 150,
    height = 80,
    dpi = 300,
    device = "png"
  )

fig <- ggplot2::ggplot() +
  ggplot2::geom_path(co2_df,
    mapping = aes(
      x = Date,
      y = AtmCO2,
      group = ID,
      color = ID
    ),
    linewidth = 0.35
  ) +
  ggplot2::theme_bw(base_size = 6) +
  ggplot2::scale_y_continuous(
    limits = c(250, 1300),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_date(
    date_breaks = "10 years",
    minor_breaks = "5 years",
    expand = c(0, 0),
    date_labels = "%Y"
  ) +
  ggplot2::scale_colour_manual(values = c("green", "blue", "red")) + # ,
  # label = c("RCP2.6","RCP4.5","RCP8.5")) +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::ggtitle(label = "AtmCO2") +
  ggplot2::labs(color = "Szenario")

p <- plotly::ggplotly(fig)

htmlwidgets::saveWidget(plotly::as_widget(p), "AtmCO2.html")
