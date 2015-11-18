# Multivariate Analysis of Cytokines in Two Patient Groups ----------------

# Example Dataset saved as "cytokines.csv"
#setwd("~/Documents/r_scripts/R-wehi_3hr")

# Import Data
cytokines <- read.csv(file="data/cytokines_3groups.csv", header = TRUE)

# generate final data.frame transforming dataset e.g. log2
cytokines_L <- data.frame(cytokines[,c(1:2)], log2(cytokines[,-c(1,2)]))

str(cytokines_L)

# ANOVA
fit = aov(cytokines_L[,3] ~ cytokines_L$group)
fit
summary.aov(fit)

# Graphics 
boxplot(cytokines_L$IL17F~cytokines_L$group
        , ylab = "log.conc"
        , xlab = "Groups"
        , main = "IL17F serum levels in patients")

IL17F_levels = boxplot(cytokines_L$IL17F~cytokines_L$group
        , ylab = "log.conc"
        , xlab = "Groups"
        , main = "IL17F serum levels in patients")

IL17F_levels + points(cytokines_L$IL17F~cytokines_L$group,
                      col=as.numeric(cytokines_L$group))

