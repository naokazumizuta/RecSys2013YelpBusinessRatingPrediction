# init
source("init.R")
# root <- "C:/Users/nao/Documents/GitHub/RecSys2013YelpBusinessRatingPrediction"
# source(file.path(root, "/r/init.R"))

require(rjson)

##### read data
IdLookupTable <- read.csv(file.path(folder$raw, "IdLookupTable.csv"))
### read business

readRaw <- function(filename) {
    temp_file <- file(file.path(folder$raw, filename), open = "r")
    temp_lines <- readLines(temp_file)
    close(temp_file)
    temp_list <- lapply(temp_lines, fromJSON)

    if (grepl("business", filename)) {
        temp_list <- lapply(
            temp_list,
            function(x) {
                x[["categories"]] <- paste(x[["categories"]], collapse = ",")
                return (x)})
    }

    temp_list <- lapply(temp_list, unlist)

    if (grepl("checkin", filename)) {
        checkin_time <- paste(
            0:23, 
            matrix(0:6, nrow = 24, ncol = 7, byrow = TRUE),
            sep = '-')
        data_names <- c(
            paste("checkin_info", checkin_time, sep = '.'),
            "type",
            "business_id")
        getFlat <- function(x) {
            x <- x[data_names]
            names(x) <- data_names
            return (x)
        }
        temp_list <- lapply(temp_list, getFlat)
    }
    temp_data <- do.call(rbind, temp_list)
    temp_data <- data.frame(temp_data)

    return (temp_data)
}

train_business <- readRaw("yelp_training_set_business.json")
test_business <- readRaw("yelp_test_set_business.json")
final_test_business <- readRaw("final_test_set_business.json")

train_checkin <- readRaw("yelp_training_set_checkin.json")
test_checkin <- readRaw("yelp_test_set_checkin.json")
final_test_checkin <- readRaw("final_test_set_checkin.json")

train_review <- readRaw("yelp_training_set_review.json")
test_review <- readRaw("yelp_test_set_review.json")
final_test_review <- readRaw("final_test_set_review.json")

train_user <- readRaw("yelp_training_set_user.json")
test_user <- readRaw("yelp_test_set_user.json")
final_test_user <- readRaw("final_test_set_user.json")

save(
    IdLookupTable,
    train_business,
    test_business,
    final_test_business,
    train_checkin,
    test_checkin,
    final_test_checkin,
    train_review,
    test_review,
    final_test_review,
    train_user,
    test_user,
    final_test_user,
    file = file.path(folder$rdata,"rawdata.RData"))
