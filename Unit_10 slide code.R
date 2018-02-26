####################
#### Unit 10 - Fast Scalable Data Analytics using R and H2O
#### R codes
####################

# Load H2O package
library(h2o) 

# Create a connection to an existing H2O cluster, 
remoteH2O = h2o.init(ip="localhost", port = 54321, startH2O = F, nthreads = -1)

# Or you can create a local 1-node H2O cluster with limited memory
# NOTE: DO NOT run the following code on our CM server
## localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = T, max_mem_size = "10G", nthreads = -1)
# Shutdown H2O cluster
## h2o.shutdown()

# Checking the cluster information
h2o.clusterInfo()

# Checking the healthy status of the cluster
h2o.clusterStatus()

# Check out overall network connection speed. 
# NOTE: It's for testing purpose. DO NOT run the following code on the CM server
## h2o.networkTest()

# Import remote IRIS data into H2O as a H2OFrame
iris.hex = h2o.importFile("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
                          destination_frame = "iris.hex",sep = ",")
iris.hex 
# iris.hex is actually a pointer to the H2OFrame in remote H2O cluster
class(iris.hex); object.size(iris.hex); str(iris.hex)

# Get more info. about "iris.hex" H2OFrame via H2O REST APIs
library(jsonlite)
# Basic frame info.
iris.hex.info = fromJSON("http://localhost:54321/3/Frames")
# What is the URL for the frame?
iris.hex.info$frames$frame_id
# More detail about "iris.hex"
iris.hex.detail = fromJSON("http://localhost:54321/4/Frames/iris.hex")
# Summary of "iris.hex" distribution
iris.hex.detail$frames$distribution_summary

# Get info. of a H2OFrame, similar to functions in base R 
str(iris.hex); nrow(iris.hex); summary(iris.hex, exact_quantiles = T); 
# Or, just write a function that provides readable distribution info.
getH2OFrameDistInfo = function(h2oFrameName, host = "127.0.0.1", port = 54321){
  library(jsonlite)
  frameInfoJSON = fromJSON(paste("http://", host,":", port, "/4/Frames/", h2oFrameName, sep = ""))
  hexDistInfo = as.matrix(t(frameInfoJSON$frames$distribution_summary$data[[1]]))
  colnames(hexDistInfo) = frameInfoJSON$frames$distribution_summary$columns[[1]]$description
  return(hexDistInfo)
}
getH2OFrameDistInfo(h2oFrameName = "iris.hex")

# Airlines Delays dataset, 31 columns
allyears2k.hex = h2o.importFile("https://s3.amazonaws.com/h2o-airlines-unpacked/allyears2k.csv",
                                header = T, destination_frame = "allyears2k.hex", sep = ",", na.strings = "NA")
getH2OFrameDistInfo("allyears2k.hex")

colnames(allyears2k.hex) # column names
# List all existing H2O objects (keys)
h2o.ls()
# Only 1-way frequency table is supported by h2o.table()
h2o.table(allyears2k.hex$IsDepDelayed)
# It creates crosstab in flat way
flatCrosstab = h2o.tabulate(allyears2k.hex, "IsDepDelayed",  "IsArrDelayed" )
# Get our familiar "crosstab". Note that "as.data.frame()" save H2OFrame as local R Data Frame
xtabs(counts ~ isdepdelayed + isarrdelayed, as.data.frame(flatCrosstab$count_table))

# Split-Apply-Combine
# ERROR: H2O doesn't recognize function "return()" and "class()" in the function as the input
h2o.ddply(allyears2k.hex, .variables = "Year", FUN = function(df){ class(df) } )

# Get average distance by "Year" x "IsDepDelayed" 
# Note that we can only access columns of the intermediate H2OFrames by column numbers. 
meanDelayedDistance = h2o.ddply(allyears2k.hex, .variables = c("Year", "IsDepDelayed"), 
                                FUN = function(df) {mean(df[, 19])} )
# Print all rows
head(meanDelayedDistance, n = nrow(meanDelayedDistance))


h2o.ls() # Checking all keys again in H2O 
# A column vector of a H2OFrame is still a (1-D) H2OFrame 
class(allyears2k.hex$IsDepDelayed)
# There is a key for this column, but it doesn't actually exist! 
h2o.getId(allyears2k.hex$IsDepDelayed)
# Instantiate (create a copy of) the column and assign a new key for it
h2o.assign(allyears2k.hex$IsDepDelayed, "IsDepDelayed.hex")
h2o.ls(); getH2OFrameDistInfo("IsDepDelayed.hex")
# You may remove everything in H2O by running:
# h2o.removeAll()

# H2OFrame R environment is just a pointer, so it can be enclosed in an R list
listOfH2OFrame = list(allyears2k.hex$Year, allyears2k.hex$IsDepDelayed)
# Frequency tables of two vectors/columns. 
# Note that the output of H2O functions are still H2OFrames (pointers in R) 
freqTbl = Map(function(v) h2o.table(v), listOfH2OFrame)
# What are the keys/IDs for both H2OFrames (frequency tables)?
freqTblKeys = Map(h2o.getId, freqTbl)
# Let's convert both tables into "local" R data frames
freqTblLocal = Map(function(h2oKey) as.data.frame(h2o.getFrame(h2oKey)), freqTblKeys)
# Another example to remove those columns with 50%+ NAs.
Map(function(vec){ 
  if(sum(is.na(vec)) > (nrow(allyears2k.hex) / 2))
    allyears2k.hex[, colnames(vec) ] <<- NULL 
}, allyears2k.hex)

# Fitting GLM with diamonds data.
library(ggplot2); diamonds_local = as.data.frame(diamonds)
diamonds_local$price = log(diamonds_local$price); colnames(diamonds_local)[7] = "logPrice"
# Unfortunately, H2O doesn't recognize "ordered factors"
diamonds_local = data.frame(Map(function(x) 
  if(is.factor(x)) as.character(x) else x , diamonds_local ), stringsAsFactors = F)
diamonds.hex = as.h2o(diamonds_local, "diamonds.hex")
getH2OFrameDistInfo("diamonds.hex") # Get H2OFrame info.
# Convert string into h2o factors.
Map(function(vecName) diamonds.hex[,vecName] <<- h2o::as.factor(diamonds.hex[,vecName]), 
    list("cut", "clarity", "color") )
# Difference between R glm() and H2O h2o.glm()
dm_glm_local <- glm(logPrice ~ ., family = "gaussian", data = diamonds_local)
dm_glm_h2o <- h2o.glm(x = (1:10)[-7], y = "logPrice", family = "gaussian",
                      training_frame = diamonds.hex, model_id = "dm_glm_h2o")

# Get all my H2OModels using REST APIs, where
# HTTP GET "/4/Models" returns all Models
# HTTP GET "/4/Models/myModelID" returns model "myModelID" info. in JSON.
AllH2OModelInfo = fromJSON("http://localhost:54321/4/Models")
AllH2OModelInfo$models$model_id
# Previous model with ID "dm_glm_h2o"
dm_glm_h2o_info = fromJSON("http://localhost:54321/4/Models/dm_glm_h2o")
dm_glm_h2o_info$models

# We can always get our H2OModel back as long as we have the ID 
# and the model is still in the cluster.
h2o.ls();  dm_glm_h2o_copy = h2o.getModel("dm_glm_h2o")
# Also, H2OModels can be saved as binary objects in local storage 
# and load it back to H2O cluster later as needed. 
h2o.saveModel(dm_glm_h2o_copy, "./")
# Remove the H2O object by the key "dm_glm_h2o"
h2o.rm(dm_glm_h2o)
# Load the model back to the cluster
dm_glm_h2o = h2o.loadModel("./dm_glm_h2o")

rm(list = ls()); gc()
library(sparklyr); library(DBI); library(dplyr); library(ggplot2)
# Check available Spark version
spark_available_versions()
# Install a local Spark for development/testing purpose
spark_install(version = "2.0.2", hadoop_version = "2.7")
# Check what version(s) has been installed
spark_installed_versions()
# Remove the installed spark if you'd like:
# spark_uninstall(version = "2.0.2",  hadoop_version = "2.7")

# Spark connection configuration
conf = spark_config()
conf$spark.ui.port = 4040 # default port
conf$spark.executor.memory = "4G" # memory per working node
conf$spark.yarn.executor.memoryOverhead = "1024" 
# Create a connection to local Spark
# You may check out http://localhost:4040/ for Spark UI 
sc = spark_connect(master = "local", version = "2.0.2", app_name = "local_app", config = conf)
# Check Spark running version
spark_version(sc)

# You can surely load your file from HDFS or local filesystem
# Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
# Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.2.jar")
# library(rhdfs)
# hdfs.init(); hdfs.ls("/")
# mySDF = spark_read_csv(sc, name = "myfile", path = "hdfs:/my_file_path.csv", header = T)

# Move R dataframe to Spark. Movies dataset as the example. 
movies_sdf = copy_to(sc, df = ggplot2movies::movies, name = "movies", overwrite = T)
head(movies_sdf) # first 10 observations
# List all available Spark data frames in Spark memory
dbListTables(sc)

# Check Spark log file for monitoring purposes
spark_log(sc, n = 20)

# "Explain" complex query plan before you actually hit "run"
movies_sdf %>% select(title, rating) %>% explain()

# Or, if we just need SQL translations
movies_sdf %>% select(title, rating) %>% sql_render()

# The good, the bad, and the ugly(very long movie title?)
movies_sdf %>% select(title, length, rating ) %>%
  filter(rating > 9 | rating < 2 | nchar(title) > 80) %>% arrange(desc(nchar(title)))

# Give me "Star Trek"!
movies_sdf %>% select(title, rating, budget) %>%  filter(title %like% "%Star Trek%") 

# Movie title with r1-r5, the select helpers are also supported.
# type ?select_helpers for more information
movies_sdf %>% select(title, num_range(prefix="r", range = 1:5))

# title with any variable names that contains numbers
movies_sdf %>% select(title, matches("[[:digit:]]"))

# How many long/short movies?
movies_sdf_longShort = 
  movies_sdf %>% select(title, rating, length) %>%  mutate(longShort = ifelse(length > 130, "long", "short")) %>% 
  group_by(longShort) %>% summarise(count = n())

# Create the temporary table "movies_sdf_longShort"
compute(movies_sdf_longShort, "movies_longShort_ct")
dbListTables(sc) # It's now in Spark as a temporary RDD

# You can actually create (cache) a Spark DataFrame (RDD) in cluster memory.
# It may improve performance. Remember to check out your Spark UI/Tab.
tbl_cache(sc, "movies_longShort_ct")

# Create an SDF pointer to existing RDD
movies_longShort_ct_sdf = tbl(sc, from = "movies_longShort_ct")

# For small SDFs, we may save the result as local R data frames
movies_longShort_ct_df = collect(movies_longShort_ct_sdf)

# We can surely save or remove RDDs
# spark_write_csv(movies_longShort_ct_sdf, path = "file:///home/myfolder/movies_longShort_ct.csv")
# spark_write_csv(movies_longShort_ct_sdf, path = "hdfs:///home/myfolder/movies_longShort_ct.csv")
rm(movies_longShort_ct_sdf)
tbl_uncache(sc, "movies_longShort_ct")

# Running SQL via DBI is not intended to create new SDFs/RDDs. 
# It should only be used in query purpose

# Save query result as R data frame
movies_10obs = dbGetQuery(sc, "select * from movies limit 10")

# Or a query result in Spark 
rs = dbSendQuery(sc, "select title, rating from movies limit 100")
dbFetch(rs, n = 5)
# Remove objects
dbClearResult(rs); rm(rs)

# Use dbWriteTable() to create a new RDD, diamonds with "log of Price"
dbWriteTable(sc, "diamonds", mutate(diamonds, logPrice = log(price) ))
diamonds_sdf = tbl(sc, "diamonds")
tbl_cache(sc, "diamonds"); diamonds_sdf

# Sampling
# 70% as the training set
diamonds_train_sdf =  sdf_sample(diamonds_sdf, 0.7, seed = 1 )
nrow(diamonds_train_sdf)
# 30% as the testing set 
diamonds_test_sdf = setdiff(diamonds_sdf, diamonds_train_sdf)
nrow(diamonds_test_sdf)
# Or we can just use sdf_partition() to make your life easier
diamonds_train_test = sdf_partition(diamonds_sdf, training = 0.7, test = 0.3, seed =1)

# Actually create RDDs to faciliate model fitting
compute(diamonds_train_sdf, "diamonds_train", temporary = F)
tbl_cache(sc, "diamonds_train", force = T)
compute(diamonds_test_sdf, "diamonds_test", temporary = F)
tbl_cache(sc, "diamonds_test", force = T)

# General linear model
diamonds_lm = ml_linear_regression( logPrice ~ carat + cut + clarity + x + y + z, data = diamonds_train_sdf  )
summary(diamonds_lm)

# We can also save model to local filesystem. 
# It is actually a folder instead of file
ml_save(diamonds_lm, file = "./diamonds_spark_lm.RData")
rm(diamonds_lm) # delete the model
# Then load it back later. 
# Unfortunately, until sparklyr 0.55, only model coefficients can be serialized. 
# The reloaded model can only be used for prediction
diamonds_lm = ml_load(sc, "./diamonds_spark_lm.RData")

# Predicted response/outcome column is called "prediction"
diamonds_lm_testpred = sdf_predict(diamonds_lm, newdata = diamonds_test_sdf)
diamonds_lm_testpred %>% select(actualLogPrice = logPrice, predictedLogPrice = prediction) %>% head()
# RMSE
diamonds_lm_testpred %>% transmute(MSE  = mean((logPrice - prediction)^2) ) %>% 
  head(1) %>% transmute(RMSE = round(sqrt(MSE), 4) ) %>% collect()

# RMSEs for different models
Map(function(f){
  m = ml_linear_regression(f, data = diamonds_train_sdf);
  
  RMSE = sdf_predict(m, diamonds_test_sdf) %>% transmute(MSE  = mean((logPrice - prediction)^2) ) %>% 
    head(1) %>% transmute(RMSE = round(sqrt(MSE), 4) ) %>% collect();
  
  return(paste("RMSE for", format(f), "=" , RMSE$RMSE)) ;
}, list(logPrice ~ carat, logPrice ~ carat + x + y + z,
        logPrice ~ carat + cut + clarity + x + y + z ) )

# Disconnect Spark and remove everything in R environment
spark_disconnect(sc)
rm(list = ls()); gc()

