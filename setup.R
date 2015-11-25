## file structure
if (! file.exists("data")) dir.create("data")

if (! file.exists("img")) dir.create("img")

if (!file.exists("data/cytokines_3groups.csv")) {
    download.file("http://files.figshare.com/2443264/cytokines_3groups.csv",
                  "data/cytokines_3groups.csv")
}


## knitr options
library(knitr)
knitr::opts_chunk$set(results='hide', fig.path='img/R-wehi_3hr',
                      comment = "#>")
