# init
source("init.R")
# root <- "C:/Users/nao/Documents/GitHub/RecSys2013YelpBusinessRatingPrediction"
# source(file.path(root, "/r/init.R"))

load(file.path(folder$rdata, "cleandata.RData"))
require(tm)
require(rJava)
require(RWeka)
require(Matrix)

business_keep <- c(
    "business_id",
    "name",
    "city",
    "categories")

business <- rbind(
    train_business[, business_keep],
    final_test_business[, business_keep])

review_keep <- c(
    "user_id",
    "business_id")

review <- rbind(
    train_review[, review_keep],
    final_test_review[, review_keep])

row.names(business) <- business$business_id
business_data <- business[review$business_id, ]

DTMtoMatrix <- function(dtm) {
    return(sparseMatrix(
        i = dtm$i,
        j = dtm$j,
        x = dtm$v,
        dims = c(dtm$nrow, dtm$ncol),
        dimnames = dtm$dimnames))
}

categories <- gsub(" ", "", business_data$categories)
categories <- gsub(",", " ", categories)
dtm <- DocumentTermMatrix(Corpus(VectorSource(categories)))
dtm_matrix <- DTMtoMatrix(dtm)

name <- tolower(business_data$name)
name <- gsub("and", " ", name)
name <- gsub("'n", " ", name)
name <- gsub("'s", "s", name)
name <- gsub("s'", "s", name)
name <- gsub("[-&!/:@,.]", " ", name)
name <- gsub("\\s+$", "", name)
name <- gsub("\\s+", " ", name)

name <- factor(name)
name_matrix <- sparse.model.matrix(~ name - 1)

dtm <- DocumentTermMatrix(
    Corpus(VectorSource(as.character(name))),
    control = list(
        tokenize = function(x)
            NGramTokenizer(x, Weka_control(min = 1, max = 2))))

name_dtm_matrix <- DTMtoMatrix(dtm)

city <- factor(business_data$city)
city_matrix <- sparse.model.matrix(~ city - 1)

user <- factor(review$user_id)
user_matrix <- sparse.model.matrix(~ user - 1)

business <- factor(review$business_id)
business_matrix <- sparse.model.matrix(~ business - 1)

static_features <- cBind(
    dtm_matrix,
    name_matrix,
    name_dtm_matrix,
    city_matrix,
    user_matrix)

user_business_features <- cBind(
    user_matrix,
    business_matrix)

save(static_features, file = file.path(folder$rdata, "static_features.RData"))
save(user_business_features, file = file.path(folder$rdata, "user_business_features.RData"))

# external source
readExternal <- function(filename) {
    temp <- read.table(filename, header = FALSE)[, 1]
    temp <- gsub("\\s+", " ", temp)
    temp <- unlist(strsplit(temp, split = ' '))
    data <- matrix(temp, ncol = 4, byrow = TRUE)
    return (data)
}

dist_all <- read.table(file.path(folder$data, "dist.all.last"))
dist_male <- read.table(file.path(folder$data, "dist.male.first"))
dist_female <- read.table(file.path(folder$data, "dist.female.first"))

username <- c(train_user$name, final_test_user$name)
user_name <- list()
user_name$in_dist_all <- (toupper(username) %in% dist_all[, 1]) * 1
user_name$in_dist_male <- (toupper(username) %in% dist_male[, 1]) * 1
user_name$in_dist_female <- (toupper(username) %in% dist_female[, 1]) * 1
user_name$is_upper <- (toupper(username) == username) * 1
user_name$is_lower <- (tolower(username) == username) * 1
user_name$nchar1 <- (nchar(username) == 1) * 1
user_name$nchar2 <- (nchar(username) == 2) * 1
user_name <- data.frame(user_name)
row.names(user_name) <- c(train_user$user_id, final_test_user$user_id)

user_name_data <- user_name[review$user_id, ]
user_name_data$unknown <- is.na(user_name_data$in_dist_all) * 1
user_name_data <- as.matrix(user_name_data)
user_name_data[is.na(user_name_data)] <- 0

user_name_matrix <- Matrix(user_name_data)
save(user_name_matrix, file = file.path(folder$rdata, "user_name_matrix.RData"))
