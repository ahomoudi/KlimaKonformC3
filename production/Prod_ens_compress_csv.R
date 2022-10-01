
library(stringr)
# 2ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/2ter_lauf_ensemble"

csv.files <- list.files(
  path = input_dir,
  pattern = ".csv$",
  recursive = T,
  full.names = T
)

for(ifile in 1:length(csv.files)){

  compress_csv(csv_file = csv.files[ifile])

  file.remove(csv.files[ifile])
}

# 3ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/3ter_lauf_ensemble/"

csv.files <- list.files(
  path = input_dir,
  pattern = ".csv$",
  recursive = T,
  full.names = T
)

for(ifile in 1:length(csv.files)){

  compress_csv(csv_file = csv.files[ifile])

  file.remove(csv.files[ifile])
}

# 4ter --------------------------------------------------------------------

input_dir <- "/media/ahmed/Volume/WHK2-tmp/Plotting_data/4ter_lauf_ensemble"

csv.files <- list.files(
  path = input_dir,
  pattern = ".csv$",
  recursive = T,
  full.names = T
)

for(ifile in 1:length(csv.files)){

  compress_csv(csv_file = csv.files[ifile])

  file.remove(csv.files[ifile])
}

