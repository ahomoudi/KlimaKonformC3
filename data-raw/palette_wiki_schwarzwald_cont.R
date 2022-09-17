## code to prepare `palette_wiki_schwarzwald_cont` dataset goes here

# Color palette -------------------------

pal_csv <- read.csv("data-raw/palette_wiki-schwarzwald-cont.csv")

# convert to hex code, e.g.,  #FFFFFF
palette_wiki_schwarzwald_cont <- unlist(lapply(
  seq_len(nrow(pal_csv)),
  function(i) rgb(pal_csv[i, ], maxColorValue = 255)
))

# save to csv file
# write.csv(palette_wiki_schwarzwald_cont, "data-raw/palette_wiki-schwarzwald-cont.csv", row.names = F)

palette_wiki_schwarzwald_cont <- rev(palette_wiki_schwarzwald_cont)

# use it in r package
usethis::use_data(palette_wiki_schwarzwald_cont, overwrite = TRUE)
usethis::use_data(palette_wiki_schwarzwald_cont, overwrite = TRUE, internal = TRUE)

# plot(NULL, xlim=c(0,length(palette_wiki_schwarzwald_cont)), ylim=c(0,1),
#      xlab="", ylab="", xaxt="n", yaxt="n")
# rect(0:(length(palette_wiki_schwarzwald_cont)-1), 0, 1:length(palette_wiki_schwarzwald_cont), 1,
#      col=palette_wiki_schwarzwald_cont)
# plot(seq_len(length(palette_wiki_schwarzwald_cont)), rep_len(1, length(palette_wiki_schwarzwald_cont)),
#      col = palette_wiki_schwarzwald_cont, pch = 16, cex = 3, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
