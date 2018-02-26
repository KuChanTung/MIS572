####################
#### Unit 7 - In-Database Analytics using R, MADlib, and MPP Databases
#### R codes
####################

library(PivotalR); library(DBI); library(RPostgreSQL); library(ggplot2)

diamonds = as.data.frame(diamonds)

#### Note that you should replace the connection credentials with your own
## Using RPostgreSQL/DBI
# Create a PostgreSQL DB connection
pgConn = dbConnect("PostgreSQL", dbname = "yihuang", user="yihuang", password="yihuang")

dbListTables(pgConn) # list all tables in the database

# Create table "diamonds". Delete if it exists.
ifelse(dbExistsTable(pgConn, "diamonds"), dbRemoveTable(pgConn, "diamonds"),
       dbWriteTable(pgConn, "diamonds", as.data.frame(diamonds)))

# Read existing table
dbReadTable(pgConn, "diamonds")
# Get Column names
dbListFields(pgConn,"diamonds")
# Run SQL statements by creating a resultset object
rs = dbSendQuery(pgConn, statement = "SELECT count(*) FROM diamonds"); fetch(rs)

dbColumnInfo(rs) # column info.

# Clear resultset
dbClearResult(rs); rm(rs)
# Or we can use dbGetQuery() without creating a resultset insetad
dbGetQuery(pgConn, "SELECT count(*) FROM diamonds")

# Table summary statistics
dbSendQuery(pgConn, "SELECT * FROM madlib.summary('diamonds', 'diamonds_summmary')")
dia_sum = dbReadTable(pgConn, "diamonds_summmary"); dia_sum
# Remove table
dbRemoveTable(pgConn,"diamonds_summmary")

# Correlation matrix
dbSendQuery(pgConn, "SELECT * from madlib.correlation( 'diamonds', 'diamonds_pearson', 'price, depth, carat')")
# 2 tables are generated
dbReadTable(pgConn, "diamonds_pearson")
dbReadTable(pgConn, "diamonds_pearson_summary")

# Remove tables if we don't need them
Map(function(tb) dbRemoveTable(pgConn, tb), 
    c("diamonds_pearson", "diamonds_pearson_summary") )

# Linear Regression. Equivalent to lm(price ~ carat + depth, diamonds) in R 
dbSendQuery(pgConn, "select madlib.linregr_train('diamonds', 'diamonds_lm','price', 'ARRAY[1, carat, depth]')")
dbReadTable(pgConn, "diamonds_lm")
dbReadTable(pgConn, "diamonds_lm_summary")

# Remove tables
Map(function(tb) dbRemoveTable(pgConn, tb), 
    c("diamonds_lm", "diamonds_lm_summary") )

## Using PivotalR
# Create a PivotalR DB connection
dbc = db.connect(dbname = "yihuang", user="yihuang", 
                 password="yihuang", default.schema = "public, madlib", verbose = T)

# List all PivotalR DB connections
db.list() # you can fit model & predict from tables across different databases!

# List all objects/tables of a connection in default schema "public"
db.objects("public.", conn.id = dbc)

# Link to the table "diamonds" in the database
db_diamonds = db.data.frame("diamonds", dbc, verbose = T)
object.size(db_diamonds) # "db_diamonds" is just an object pointing to "diamonds"! 
dim(db_diamonds) # dimension
stats = summary(db_diamonds) # summary statistics
stats$distinct_values

names(db_diamonds) # column names

# Split-Apply-Combine 
# Unfortunately, only works on SQL aggregation functions 
mean_price_by_cut = by(db_diamonds$price, db_diamonds$cut, mean)

# mean_price_by_cut is just an db.Rquery object, no data is loaded in memory
content(mean_price_by_cut$cut) # check the actual content of "mean_price_by_cut"
# Actually execute SQL query and see the result
lk(mean_price_by_cut)

# Table operations
# Let's say we only need a few variables
db_diamonds_few_vars = db_diamonds[,c("carat", "cut","clarity", "depth", "price")]
# db_diamonds_few_vars is a db.Rquery before we call as.db.data.frame()
db_diamonds_few_vars = as.db.data.frame(db_diamonds_few_vars, "db_diamonds_few_vars", verbose = T)
# Check first 10 observations
lk(db_diamonds_few_vars, 10)
# Let's try creating a new table
db_diamonds_price_clarity = db_diamonds[, c("price", "carat")]
# A new column "log of price"
db_diamonds_price_clarity$logprice = log(db_diamonds_price_clarity$price)
# The actual SQL code
content(db_diamonds_price_clarity)
# Save as a real table in the database
db_diamonds_price_clarity = as.db.data.frame(db_diamonds_price_clarity, "db_diamonds_price_clarity")
# Take a look
lk(db_diamonds_price_clarity, nrows = 10)
# Remove the table if you'd like
delete(db_diamonds_price_clarity)
# Clean all temporary MADlib tables
clean.madlib.temp()


## Matrix Operations. 
pgConn = dbConnect("PostgreSQL", dbname = "yihuang", user="yihuang", password="yihuang")
# 1000 normal random numbers as example
# Add serial ID/names numbers for easy operations on db.data.frame
db_nrand_1000 = as.db.data.frame(data.frame(matrix(rnorm(1000,0,1), ncol = 5)),
                                 "db_nrand_1000", add.row.names = T)
# For Greenplum: 
# db_nrand_10k = as.db.data.frame( data.frame(matrix(rnorm(10^8,0,1), nrow=1000)),
#                                table.name = "db_nrand_10k", add.row.names = T, key = "row_names", distributed.by="row_names")

lk(db_nrand_1000, 10)

# In MADlib, matrices are often implemented/represented as a collection of 1-D arrays. 
db_nrand_1000_ary = as.db.data.frame( cbind(db_nrand_1000[,1], db.array(db_nrand_1000[,2:6]) ),
                                      table.name = "db_nrand_1000_ary" )
# Take a look at the first 5 row of the data in array form
lk(db_nrand_1000_ary, array = F, nrows = 5)

# Use db.q() to send SQL query to the database. Compute simple statistics
stat_ary = db.q("select row_names, madlib.array_sum(agg_opr), 
                madlib.array_mean(agg_opr), madlib.array_stddev(agg_opr) from db_nrand_1000_ary", nrows = 5)

# Get Euclidean norm. Then save as a new table in database
db.q("select row_names, sqrt(madlib.array_dot(agg_opr, agg_opr)) as norm into db_nrand_1000_ary_norm
     from db_nrand_1000_ary") 
db_nrand_1000_ary_norm = db.data.frame("db_nrand_1000_ary_norm")
lk(db_nrand_1000_ary_norm, nrows = 20) 
# Matrix Transposition: "200 x 5" to "5 x 200"
dim(db_nrand_1000_ary) # it is actually 200 x 2, 2nd column is an array data type
# Specify key and array column, output to table "db_nrand_1000_ary_t"
db.q("select madlib.matrix_trans('db_nrand_1000_ary', 'row=row_names, val=agg_opr', 'db_nrand_1000_ary_t')")
db_nrand_1000_ary_t = db.data.frame("db_nrand_1000_ary_t")
dim(db_nrand_1000_ary_t) # it's now 5 x 2
tmp = lk(db_nrand_1000_ary_t,  array = T) # Get the transposed matrix
# Singular Value Decomposition 
# svd( source_table,output_table_prefix, row_id, num of singular vectors, n_iterations,result_summary_table);
db.q("select madlib.svd('db_nrand_1000_ary', 'db_nrand_100_svd', 'row_names', 3)")
# List all the SVD output table names
svd_tables = db.objects("db_nrand_100_svd");svd_tables
Map(function(x) db.q(paste("select * from ", x, " order by row_id")) , svd_tables )

# General Linear models
dbc = db.connect(dbname = "yihuang", user="yihuang", 
                 password = "yihuang", default.schema = "public, madlib", verbose=T) 
db_diamonds = db.data.frame("diamonds")
# linear model
price_db_lm = madlib.lm(log(price) ~ carat + clarity +  x + y + z, data = db_diamonds)
price_db_lm
price_db_lm$model@.name # model information is saved in a temporary table
db.q(paste("select * from ", price_db_lm$model@.name ))
# using "|" to fit models conditioned on the values of one or more variables
price_db_by_cut_glm = madlib.glm(log(price) ~ carat +  x + y + z | cut, data = db_diamonds,
                                 family = gaussian)
price_db_by_cut_glm

# Syntax: generic.cv(train_fun, predict_fun, metric_fun, data, k = 10)
# 10-fold CV RMSE
set.seed(1)
# For the table without row names
dbRemoveTable(pgConn, "diamonds")
db_diamonds = as.db.data.frame(diamonds, table.name = "diamonds", add.row.names = T)
# 70% as the training set
db_diamonds_train = sample(db_diamonds, 0.7 * nrow(db_diamonds))
# Create testing dataset 
db.q(paste("select * into diamonds_test from diamonds where row_names not in (select row_names from", 
           noquote(paste(db_diamonds_train@.name, collapse = ".")), ")" ))
db_diamonds_test = db.data.frame("diamonds_test")

generic.cv( function(d) madlib.lm(log(price) ~ carat + x + y + z, data = d), 
            predict, function(pred, d) lookat(sqrt(mean((log(d$price) - pred)^2))),
            data = db_diamonds_train, k = 10)

# How about different models?
Map(function(f) generic.cv( function(d) madlib.lm(f, data = d), 
                            predict, function(pred, d) lookat(sqrt(mean((log(d$price) - pred)^2))), data = db_diamonds_train, k = 10), 
    list(log(price) ~ carat, log(price) ~ carat + cut, log(price) ~ carat + cut + x + y +z )
)

# Logistic regression
library(ISLR); data("Default")
set.seed(1)
train_idx = sample(1:nrow(Default), 0.7 * nrow(Default));
test_idx = setdiff(1:nrow(Default), train_idx)
db_defaults_train = as.db.data.frame(Default[train_idx,], "db_defaults_train", add.row.names = T, key = "row_names" )
db_defaults_test = as.db.data.frame(Default[test_idx,], "db_defaults_test", add.row.names = T, key = "row_names" )
# Learn logit model from trainning data
Default_logit = madlib.glm(default ~ student + balance, data = db_defaults_train, family = binomial)
Default_logit
# Odd ratios
Default_logit$odds_ratios 
# Compute predicted probabilities
db_Defaults_test_pred = as.db.data.frame(predict(Default_logit, db_defaults_test, type="prob"), "db_Defaults_test_pred")
lk(db_Defaults_test_pred)

# CART
Default_CT = madlib.rpart(default ~ student + balance, id = "row_names",
                          data = db_defaults_train, parms = list(split='gini'), control = list(cp=0))
Default_CT
# Plot the tree
plot(Default_CT); text(Default_CT)
db_defaults_test_pred = as.db.data.frame(predict(Default_CT, db_defaults_test), "db_defaults_test_pred")
# Confusion Matrix
db.q("select p.estimated_default, a.default as act_default, count(*)
     from db_defaults_test_pred p, db_defaults_test a 
     where p.row_names = a.row_names group by estimated_default, act_default ")


set.seed(1)
train_idx = sample(1:nrow(diamonds), 0.7 * nrow(diamonds));
test_idx = setdiff(1:nrow(diamonds), train_idx)
db_diamonds_train = as.db.data.frame(diamonds[train_idx, ], "db_diamonds_train", add.row.names=T, key="row_names" )
db_diamonds_test = as.db.data.frame(diamonds[test_idx, ], "db_diamonds_test", add.row.names=T, key="row_names" )

# Bagging linear model
bagLM = generic.bagging(function(d){
  madlib.lm(log(price) ~ carat + clarity +  x + y + z, data = d)
}, data = db_diamonds_train, nbags = 1, fraction = 0.5)

# Bagging regression tree 
bagRT = generic.bagging(function(d){
  madlib.rpart(log(price) ~ carat + clarity +  x + y + z, data = d)
}, data = db_diamonds_train, nbags = 1, fraction = 0.5)


# RMSEs for bagging LM, RT, and one LM
bigLMPred = predict(bagLM, db_diamonds_test)
lookat(sqrt(mean((log(db_diamonds_test$price) - bigLMPred)^2 ) ))
bigRTPred = predict(bagRT, db_diamonds_test)
sqrt(mean(( log(lk(db_diamonds_test$price, "all")) - lk(bigRTPred[,"estimated_ln(price)_opr"], "all" ) )^2 ) )

# One single linear model
simpleLM = madlib.lm(log(price)~carat + clarity + x + y + z, data = db_diamonds_train)
simpleLMPred = predict(simpleLM, db_diamonds_test)
lookat(sqrt(mean( (log(db_diamonds_test$price) - simpleLMPred)^2 ) ))


# Building a random forest      
diamonds_RF = madlib.randomForest(log(price) ~ carat + clarity +  x + y + z, 
                                  data = db_diamonds_train, id = "row_names",
                                  ntree = 20, importance = T)
# Variable Importance
diamonds_RF$importance
# Predicted results
diamonds_RF_pred = as.db.data.frame(predict(diamonds_RF, db_diamonds_test), "diamonds_RF_pred")
# Unfortunately, the predicted result IDs doesn't match the onces in the database
# Sort by the row_names first 
act_RF = lk(db_diamonds_test[,c("row_names", "price")], "all")
act_RF = act_RF[order(act_RF[,1]),]
pred_RF = lk(diamonds_RF_pred, "all")
pred_RF = pred_RF[order(pred_RF[,1]),]
# RMSE
sqrt(mean(( log(act_RF$price) - pred_RF[,2])^2 ) )



