# init
source("init.R")
# root <- "C:/Users/nao/Documents/GitHub/RecSys2013YelpBusinessRatingPrediction"
# source(file.path(root, "/r/init.R"))

load(file.path(folder$rdata, "cleandata.RData"))
load(file.path(folder$rdata, "static_features.RData"))
load(file.path(folder$rdata, "user_name_matrix.RData"))
load(file.path(folder$rdata, "user_business_features.RData"))

require(Matrix)
require(glmnet)

is_training <- c(
    rep(TRUE, nrow(train_review)),
    rep(FALSE, nrow(final_test_review)))

static_features <- cBind(
    user_name_matrix,
    static_features)

date <- train_review$date
train_start <- as.Date("2008-07-01")
is_train <- date >= train_start
target <- train_review$stars

dim(static_features)

set.seed(1)
cv_glmnet_fit <- cv.glmnet(
    x = static_features[is_training, ][is_train, ],
    y = target[is_training][is_train],
    family = "gaussian",
    type.measure = "mse",
    standardize = FALSE,
    alpha = .15,
    intercept = TRUE,
    nfolds = 100)
pred_static <- predict(cv_glmnet_fit, static_features[!is_training, ])

require(glmnet)
cv_glmnet_fit <- cv.glmnet(
    x = user_business_features[is_training, ][is_train, ],
    y = target[is_training][is_train],
    family = "gaussian",
    type.measure = "mse",
    standardize = FALSE,
    alpha = .02,
    intercept = TRUE,
    nfolds = 100)
pred_user_business <- predict(cv_glmnet_fit, user_business_features[!is_training, ])

pred <- pred_static * .8 + pred_user_business * .2

### post processing
train_review$count <- 1
user_offset <- aggregate(
    cbind(count, stars) ~ user_id,
    data = train_review,
    subset = date >= train_start,
    FUN = sum)
user_offset <- merge(user_offset, train_user)
user_offset <- within(user_offset, {
    user_offset_count <- ifelse(is.na(review_count), -1, review_count - count)
    stars_all <- round(review_count * user_average)
    user_offset <- (stars_all - stars) / user_offset_count})

user_offset <- subset(
    user_offset,
    select = c(user_id, user_offset_count, user_offset))

test_data <- subset(
    final_test_review,
    select = c(user_id, business_id))
test_data$uid <- 1:nrow(test_data)

test_data <- merge(test_data, user_offset, all.x = TRUE)
test_data <- test_data[order(test_data$uid), ]

pred_data <- data.frame(
    pred,
    user_offset = test_data$user_offset)

threshold <- 5
has_user_offset <- with(
    test_data,
    user_offset_count >= threshold & !is.na(user_offset))

pred_data$user_offset[!has_user_offset] <- NA

weight_mean <- function(x) weighted.mean(x, c(.7, .3), na.rm = TRUE)

pred_post <- apply(pred_data, 1, weight_mean)
lowest <- 1
highest <- 5
pred_post[pred_post < lowest] <- lowest
pred_post[pred_post > highest] <- highest

# submission
submit_form <- data.frame(
    review_id = final_test_review$review_id,
    stars = as.numeric(pred_post))

write.csv(
    submit_form,
    file = file.path(folder$submit, "submission.csv"),
    row.names = FALSE)
