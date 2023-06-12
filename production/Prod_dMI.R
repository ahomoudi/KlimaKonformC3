library(stringr)
library(KlimaKonformC3)

input_dir <- "/media/HDD/Daten/WHK2/Data/ReKIS"

csv.files <- list.files(
  path = input_dir,
  pattern = ".csv$",
  recursive = T,
  full.names = T
)[c(-1, -2)]



# dMI ---------------------------------------------------------------------


output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/dMI/"

# loop over files
lapply(csv.files, function(ifile) {
  clima_dMI(
    csv.file = ifile,
    resolution = c(
      "Jahr (Jan-Dez)",
      "Vegetationsperiode I (April-Juni)",
      "Vegetationsperiode II (Juli-Sept)"
    ),
    plot.type = c("ts", "bp"),
    axis.scales = "free_y",
    output_path
  )
})

clima_dMI_multiple(csv.files,
  resolution = c(
    "Jahr (Jan-Dez)",
    "Vegetationsperiode I (April-Juni)",
    "Vegetationsperiode II (Juli-Sept)"
  ),
  plot.type = c("ts", "bp"),
  axis.scales = "free_y",
  output_path
)
# dMI1 ---------------------------------------------------------------------


output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/dMI2/"

# loop over files
lapply(csv.files, function(ifile) {
  clima_dMI(
    csv.file = ifile,
    resolution = c(
      "Jahr (Jan-Dez)",
      "Vegetationsperiode I (April-Juni)",
      "Vegetationsperiode II (Juli-Sept)"
    ),
    plot.type = c("ts", "bp"),
    axis.scales = "fixed",
    output_path
  )
})

clima_dMI_multiple(csv.files,
  resolution = c(
    "Jahr (Jan-Dez)",
    "Vegetationsperiode I (April-Juni)",
    "Vegetationsperiode II (Juli-Sept)"
  ),
  plot.type = c("ts", "bp"),
  axis.scales = "fixed",
  output_path
)

# dMI3 ---------------------------------------------------------------------


output_path <- "~/NextCloud/DATA/Shared/KlimaKonform-Results/dMI3/"

# loop over files
lapply(csv.files, function(ifile) {
  clima_dMI(
    csv.file = ifile,
    resolution = c(
      "Jahr (Jan-Dez)",
      "Vegetationsperiode I (April-Juni)",
      "Vegetationsperiode II (Juli-Sept)",
      "Okt-Dez",
      "Jan-Mrz"
    ),
    plot.type = c("ts", "bp"),
    axis.scales = "fixed",
    output_path
  )
})

clima_dMI_multiple(csv.files,
  resolution = c(
    "Jahr (Jan-Dez)",
    "Vegetationsperiode I (April-Juni)",
    "Vegetationsperiode II (Juli-Sept)",
    "Okt-Dez",
    "Jan-Mrz"
  ),
  plot.type = c("ts", "bp"),
  axis.scales = "fixed",
  output_path
)
