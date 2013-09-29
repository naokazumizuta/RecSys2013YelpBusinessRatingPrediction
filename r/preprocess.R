# init
source("init.R")
# root <- "C:/Users/nao/Documents/GitHub/RecSys2013YelpBusinessRatingPrediction"
# source(file.path(root, "/r/init.R"))

load(file.path(folder$rdata, "rawdata.RData"))

train_review <- within(train_review, {
    votes.funny <- as.numeric(votes.funny)
    votes.useful <- as.numeric(votes.useful)
    votes.cool <- as.numeric(votes.cool)
    stars <- as.numeric(stars)
    date <- as.Date(date)})

train_user <- within(train_user, {
    votes.funny <- as.numeric(votes.funny)
    votes.useful <- as.numeric(votes.useful)
    votes.cool <- as.numeric(votes.cool)
    user_average <- as.numeric(average_stars)
    review_count <- as.numeric(review_count)
    rm(average_stars)})

train_business <- within(train_business, {
    open <- as.logical(open)
    business_average <- as.numeric(stars)
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
    review_count <- as.numeric(review_count)
    rm(stars)})

test_user <- within(test_user, {
    review_count <- as.numeric(review_count)})

test_business <- within(test_business, {
    open <- as.logical(open)
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
    review_count <- as.numeric(review_count)})

final_test_user <- within(final_test_user, {
    review_count <- as.numeric(review_count)})

final_test_business <- within(final_test_business, {
    open <- as.logical(open)
    longitude <- as.numeric(longitude)
    latitude <- as.numeric(latitude)
    review_count <- as.numeric(review_count)})

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
    file = file.path(folder$rdata,"cleandata.RData"))
