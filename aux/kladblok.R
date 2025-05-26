
pid <- sf_kwg$pandid[270]
x <- sf_kwg$straatnaam
y <- gsub("</", "\\u003c/", x, fixed = TRUE)
id <- 527
sf_kwg$straatnaam[id]
sf_kwg$pandid[id]
z <- sf_kwg$straatnaam[id]
h <- charToRaw(sf_kwg$straatnaam[id])
charToRaw("ndi")

y <- gsub("\xeb", "e", z)
y <- gsub("\xf6", "ö", z)


