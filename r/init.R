# initial settings
root <- "C:/Users/nao/Documents/GitHub/RecSys2013YelpBusinessRatingPrediction"

folder <- list()
folder_name <- c(
    "data",
    "docs",
    "log",
    "py",
    "r",
    "raw",
    "rdata",
    "submit")

for(name in folder_name) {
    folder[[name]] <- file.path(root, name)
    dir.create(folder[[name]], showWarnings = FALSE)
}

# metric
RMSE <- function(predicted, actual) sqrt(mean((predicted - actual)^2))
