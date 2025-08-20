library(haven)

setwd("/Users/zhangyidi/Desktop/LCA10FAQ/data")

# Read the SPSS .sav file
data <- read_sav("SEHS Combined All 4 10_12.sav")

# Export to CSV
write.csv(data, "lca10faq.csv", row.names = FALSE)

write.csv(data, "/Users/zhangyidi/Desktop/LCA10FAQ/data/lca10faq.csv", row.names = FALSE)

data <- read_sav("PYDI subset for Bengu.sav")


data <- read.table("PYDI.dat", header = TRUE)  # adjust header = FALSE if needed
