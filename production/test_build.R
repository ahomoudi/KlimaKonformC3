# to do list --------------------------------------------------------------
#fix english version of clima_diagramm_abs() & clima_diagramm_change()
#y axis issue
# round the change to the nearest number
# reduce space between the middle plots
#https://rmarkdown.rstudio.com/authoring_basics.html
devtools::document()
print("                   ***                   ")
devtools::load_all()
print("                   ***                   ")
devtools::check(args = "--no-examples",env_vars = c(NOT_CRAN = "false"))
print("                   ***                   ")
styler::style_pkg()
# building ----------------------------------------------------------------
Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = "0")

devtools::build_manual()
devtools::build()
devtools::build(binary = T)
#devtools::build_site()

#usethis::use_pkgdown()
#usethis::use_github_pages(branch = usethis::git_default_branch(), path = "/docs")
# checking ----------------------------------------------------------------
devtools::check_built("/media/ahmed/Daten/WHK2/R.2.0/KlimaKonformC3_2.0.0.tar.gz")
devtools::check_man()

# release ------------------------------------------------------------------

usethis::use_github_release()

# installing --------------------------------------------------------------
devtools::install(upgrade = F)


# conert no ASCII to text
library(stringi)
stringi::stri_escape_unicode("Ä")


# use mask from terra to remove unwanted classes

# add a logo
usethis::use_logo("~/Desktop/hex-KlimaKonformC3.png")
# add badges
![GitHub commits since tagged version](https://img.shields.io/github/commits-since/ahmathlete/KlimaKonformC3/v2.0.0?style=plastic)
usethis::use_badge(
  badge_name = "License: GPL v3.0",
  src = "https://img.shields.io/badge/License-GPL%20v3-blue.svg",
  href = "http://www.gnu.org/licenses/gpl-3.0"
)
# let github make the action check
usethis::use_github_action_check_standard()

#https://shields.io/
ahmathlete KlimaKonformC3
#[![repo status - closed](https://img.shields.io/badge/repo_status-closed-ff0000)](https://)
# R  ----------------------------------------------------------------------

print("                   ***                   ")
usethis::use_r("clima_diagramm_change")
print("                   ***                   ")
usethis::use_data_raw("standard_output_en")
print("                   ***                   ")
usethis::use_package("zoo")
print("                   ***                   ")

# inst --------------------------------------------------------------------

usethis::use_gpl3_license()
usethis::use_travis() #

use_github_actions()

# climate chart  ----------------------------------------------------------

# calculate monthly values

data_mon<-data%>%
  dplyr::mutate(yearmonth = zoo::as.yearmon(Date))%>%
  dplyr::group_by(yearmonth, corners)%>%
  dplyr::select(-Date)%>%
  dplyr::summarise(Precip = sum(Precip, na.rm = T),
                   Tmin = mean(Tmin, na.rm = T),
                   Tavg= mean(Tavg, na.rm = T),
                   Tmax = mean(Tmax, na.rm = T))


# get periods
r.time<-zoo::as.Date(data_mon$yearmonth)
time_indices<-  periods <- list(
  period1 = which(r.time > as.Date("1970-12-31") &
                    r.time < as.Date("2000-02-01")),
  period2 = which(r.time > as.Date("1990-12-31") &
                    r.time < as.Date("2020-02-01")),
  period3 = which(r.time > as.Date("2020-12-31") &
                    r.time < as.Date("2050-02-01")),
  period4 = which(r.time > as.Date("2069-12-31") &
                    r.time < as.Date("2099-02-01"))
)

#assign periods
data_mon$Period<-NA
data_mon$Period[time_indices$period1]<-"1971-2000"
data_mon$Period[time_indices$period2]<-"1991-2020"
data_mon$Period[time_indices$period3]<-"2021-2050"
data_mon$Period[time_indices$period4]<-"2070-2099"


# calculate periods mean
data_seasonal<-data_mon%>%
  dplyr::mutate(Date = zoo::as.Date(yearmonth))%>%
  dplyr::mutate(MONTH = lubridate::month(Date))%>%
  dplyr::group_by(corners, Period, MONTH)%>%
  dplyr::select(-yearmonth, -Date)%>%
  dplyr::summarise(Precip = mean(Precip, na.rm = T),
                   Tmin = mean(Tmin, na.rm = T),
                   Tavg= mean(Tavg, na.rm = T),
                   Tmax = mean(Tmax, na.rm = T))

# clean the data
data_seasonal<-na.omit(data_seasonal)


ggplot2::ggplot(data = data_seasonal,
                mapping = ggplot2::aes(x = MONTH))+

  ggplot2::geom_col(mapping = ggplot2::aes(
    y = Precip))+

  ggplot2::geom_line(mapping = ggplot2::aes(
    y = Tmin))+

  ggplot2::facet_grid(Period~corners)+

  ggplot2::scale_y_continuous(breaks = seq(-10,50,5),
                              sec.axis = ggplot2::sec_axis(~.*100))
# others ------------------------------------------------------------------
library(readr)
X2ter_lauf <- read_csv("~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/2ter_lauf.csv")

X2ter_lauf$minimum<-X2ter_lauf$maximum<-NULL

colnames(X2ter_lauf)<-c("sim_id","variable","GCM","RCM", "rcp",
                          "ensemble", "version", "start", "end","minimum","maximum")
data.table::fwrite(X2ter_lauf %>%
                     dplyr::mutate_if(is.numeric, round, digits = 3),
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/2ter_lauf.csv")

rm(X2ter_lauf)

X3ter_lauf <- read_csv("~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/3ter_lauf.csv")

data.table::fwrite(X3ter_lauf %>%
                     dplyr::mutate_if(is.numeric, round, digits = 3),
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/3ter_lauf.csv")


X4ter_lauf <- read_csv("~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/4ter_lauf.csv")

colnames(X4ter_lauf)<-c("sim_id","variable","GCM","RCM", "rcp",
                        "ensemble", "version", "start", "end","minimum","maximum")

data.table::fwrite(X4ter_lauf %>%
                     dplyr::mutate_if(is.numeric, round, digits = 3),
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/4ter_lauf.csv")


# find . -iname '*SpTr*'  -exec cp --parents {}  ../test \;

#lubridate
usethis::use_r("sim_linear_trend")
usethis::use_package("ggridges")
usethis::use_data_raw("standard_output_en")
usethis::edit_r_environ()

usethis::use_description()
usethis::use_spell_check(vignettes = TRUE, lang = "en-GB", error = FALSE)
load("R/sysdata.rda")
# only one
usethis::use_gpl3_license()
usethis::use_readme_md()
usethis::use_lic("stable")

devtools::install_github("GuangchuangYu/badger")

badger::badge_code_size()
KlimaKonformC3::standard_output_de


#styler::cache_clear()
devtools::document()
devtools::load_all()

#ensemble
devtools::install(upgrade = F)

data.table::fwrite(X3ter_lauf%>%
                     dplyr::mutate_if(is.numeric, round, digits = 3),
                   "~/CloudStore/Nextcloud/Shared/KlimaKonform-Results/info_simulations/3ter_lauf.csv")

devtools::build_site()


knitr::stitch('R/virtuoso_function.R')

tinytex::tlmgr_install("makeindex")
usethis::use_package("sf")
usethis:: importFrom("utils", "globalVariables")
importFrom("stats", "na.omit")

KlimaKonformC3::standard_output_de

# testing -----------------------------------------------------------------
args_in<-readLines("production/input_text_boxplots")

netCDF.files <-args_in[1:3]
variable <- args_in[4]
region <- args_in[5]
landcover <- args_in[6]
language <- args_in[7]
run_id <- args_in[8]
output_path <- args_in[9]
output_csv <- args_in[10]


netCDF.files <-args_in[1:3]
variable <- args_in[4]
region <- args_in[5]
landcover <- args_in[6]
language <- args_in[7]
stat_var <- args_in[8]
run_id <- args_in[9]
output_path <- args_in[10]
output_csv <- args_in[11]


args_in<-readLines("production/input_text_public_output")

netCDF.file <-args_in[1]
variable <- args_in[2]
region <- args_in[3]
landcover <- args_in[4]
language <- args_in[5]
stat_var <- args_in[6]
run_id <- args_in[7]
output_path <- args_in[8]
output_csv <- args_in[9]

library(terra)
set.seed(0)
r <- rast(nrows=10, ncols=10, nlyrs=100)
values(r) <- runif(ncell(r) * nlyr(r))

x <-
# note how this returns one layer
x <- sum(c(r, r[[2]]), 5)
