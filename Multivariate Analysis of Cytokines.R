# Multivariate Analysis of Cytokines in Two Patient Groups ----------------

# Example Dataset saved as "cytokines.csv"
#setwd("~/Documents/r_scripts/R-wehi_3hr")

# Import Data
cytokines <- read.csv(file="data/cytokines_3groups.csv", header = TRUE)

# generate final data.frame transforming dataset e.g. log2
cytokines_L <- data.frame(cytokines[,c(1:2)], log2(cytokines[,-c(1,2)]))

str(cytokines_L)

# Identify groups (control,infected)
groups <- cytokines_L$group
groups[grep("control", groups)] <- "control"
groups[grep("infection_A", groups)] <- "infection_A"
groups[grep("infection_B", groups)] <- "infection_B"
groups <- factor(groups)

# Identify cytokines as one factor with 8 levels
cyto8 <- colnames(transformed_cytokines[,-c(1,2)])
cyto8 <- factor(cyto8)

# ANOVA
fit <- lapply(1:dim(log2cytokines)[2], function(i){
  aov(log2cytokines[,i] ~ groups)
})
names(fit) <- colnames(cytokines[, -c(1:2)])
fit

# Plot Data
pairs(cytokines.df[,-c(1,2)])
# example(pairs)
pairs(cytokines.df[,-c(1,2)], pch = 21, bg = c("white","black","grey")[unclass(cytokines$group)])
pairs(cytokines.df[,c("IL17F","IL2","CXCL10","ENA78")], pch = 21, bg = c("white","black","grey")[unclass(cytokines$group)])

boxplot(cytokines[,-c(1,2)],xlab = "Cytokines", ylab ="log2(conc)", main = "Cytokine profiles during infection")

boxplot(subset(cytokines[,-c(1,2)], groups == "control"), col = "white")
boxplot(subset(cytokines[,-c(1,2)], groups == "infection_A"), col = "black", add = TRUE)
boxplot(subset(cytokines[,-c(1,2)], groups == "infection_B"), col = "grey", add = TRUE)

# Graphics with ggplot2
library(ggplot2)

# qplot()
qplot(factor(groups), cytokines.df$IL17F, geom = "boxplot", ylab = "log.conc", xlab = "Groups", main = "IL17F serum levels in patients")
# overlay point on boxplot
qplot(factor(groups), cytokines.df$IL17F, geom = c("boxplot","point"), ylab = "log.conc", xlab = "Patient Groups", main = "IL17F serum levels in patients")

# multiple cytokines in one display
###### still to do!
#### qplot(data = cytokines.df, aes(x = cyto8, y = cytokines.df, color = groups))
#ggplot(aes(y = boxthis, x = f2, fill = f1), data = df)
#(position = position_dodge(width = .9))

## Optional Analysis
# transpose dataset for principal component analysis
tcytokines.df <- t(cytokines.df[,-c(1,2)])
colnames(tcytokines.df) <- cytokines$group
cytokines.pca <- prcomp(tcytokines.df)
pairs(cytokines.pca$rotation)
plot(cytokines.pca$rotation[,1], cytokines.pca$rotation[,2])














boxplot(subset(Cytokines[,-c(1,2)], groups == "Control"), col = "dark green", xlab = "Cytokines", ylab ="log2(conc)", main = "Cytokine profiles during X infection")
boxplot(subset(Cytokines[,-c(1,2)], groups == "Infected"), col = "orange", add = TRUE)

#help(boxplot)
#example(boxplot)






# PART 2: Hands-On Session ------------------------------------------------





# Functions ---------------------------------------------------------------
# call a funtion by using its name(), e.g.
sum(2, 3, 5)
# store results by assigning a function to a variable, e.g.
x <- sum(2,3,5)
x

# Getting Help ------------------------------------------------------------
help(sum)
example(sum)

# Vectors -----------------------------------------------------------------
# vector values must be the same type e.g. all numbers
# variable x with numbers between 2 and 4 (again using similar format to excel)
x <- 2:4
# variable y with numbers 3, 6, 8 (using 'c' to 'concatenate'/'combine')
y <- c(3,6,8)
# perform operation on a vector, e.g.
y
2 * y
# add two vectors together
x + y
# compare vectors
x < y
x == c(2, 5, 10)

# extract individual values from vectors using [position], e.g.
celltypes <- c('B cells', 'T cells', 'Neutrophils', 'Monocytes')
celltypes[3]

# new values can be assigned to an existing vector, e.g.
celltypes[2] <- "CD4 T cells"

# add more values to the end of a vector
celltypes[5] <- "CD8 T cells"

# retrieve values from vector
celltypes[3:4]

# assign names to vector values, e.g
y
names(y) <- c("DCs", "monocytes", "macrophages")
y
# retrieve values using names()
y["monocytes"]




# Barplots ---------------------------------------------------------
infiltrating.cells <- c(30, 50, 120)
names(infiltrating.cells) <- c('T cells', 'B cells', 'Neutrophils')
barplot(infiltrating.cells)
# can full cutomize graph output (e.g. adjust size of text, add titles, change colors of bars, etc) - see help(barplots)
barplot(infiltrating.cells, main = "Infiltrating Cells", col = rainbow(20), cex.main = 1.8, cex.names = 1.5, cex.axis = 1.4, font = 1)


# Scatter Plots -----------------------------------------------------------
x <- c(1, 3, 4, 6, 7, 10, 15, 16, 20, 30, 42, 55, 56, 64)
y <- sqrt(x)
plot(x,y)
help("plot")
# again, you can fully customize your plot e.g. change from points to stairs
plot (x,y, type = "s")
# or change colors of points
plot(x,y, type = "s", col = 'blue')
help("plot")
# see also par() for more info on graphical parameters
par()


# Matrices ----------------------------------------------------------------
# Add the number 1:15 to matrix with 5 rows x 3 columns
matrix(data=1:15, nrow = 5, ncol = 3)
# also can be done,
x <- 1:15
matrix(x,5,3)
# you can also re-shape a vector into a new matrix
dim(x) <- c(3,5)
x

# Accessing values from Matrix
x
x[2,3]   # row 2, column 3

# can assign new values within a matrix
x[2,3] <- 20
x

#retrieve entire row
x[2,]
# or column
x[,3]


# Factors -----------------------------------------------------------------
# Use the 'factor' function to group data into categories
# e.g. to organise cell.type data collected from tumors
tumors <- c('Neut', 'Macs', 'B cells', 'T cells')
cell.types <- factor(tumors)

# factors helps you to see the number of unique 'Levels'
tumors
cell.types
levels(cell.types)


# Dataframes --------------------------------------------------------------
cell.types
cell.numbers <- c(100, 200, 20, 30)
identification.marker <- c('Ly6G','F4/80', 'B220', 'CD3')
tumor.infiltrating.immune.cells <- data.frame(cell.types, cell.numbers, identification.marker)
tumor.infiltrating.immune.cells

# retrieve columns
tumor.infiltrating.immune.cells[[2]]
tumor.infiltrating.immune.cells[["cell.numbers"]]
# quickest way is to use '$'
tumor.infiltrating.immune.cells$cell.numbers


