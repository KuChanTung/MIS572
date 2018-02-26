####################
#### Unit 4 R codes
####################

# Setting Hadoop environment variables

Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.8.1.jar")

# Load RHadoop packages
library("rhdfs")
library("rmr2")
library("plyrmr")

# Initialize connection
hdfs.init()

# *****************************************************************************************
# ***** Note that you must replace folder name "/home/yihuang" with your own folder, e.g. /home/B1234567890 ***** 
# *****************************************************************************************
# E.g. You have a plain text file, text.txt, and would like
# to upload it to /home/myfolder

# Create a text file
write.table("plain texts", file = "test.txt")

library("rhdfs"); hdfs.init() # Create aconnection to the HDFS
hdfs.put("test.txt","/home/myfolder/test.txt")
hdfs.ls("/home/yihuang")
hdfs.cat("/home/myfolder/test.txt")

# Write R object to HDFS
to.dfs(mtcars, "/home/myfolder/mtcars.RData")
# Read R object from HDFS
from.dfs("/home/myfolder/mtcars.RData",format = "native") # native R object

######### word count example
line = "I am a student.
You are a student.
He is a student
We are all students"

# Remove existing files in HDFS if needed
#hdfs.del("/home/myfolder/small_doc.txt")
#hdfs.del("/home/myfolder/small_doc_wc.RData")

to.dfs(line, output='/home/myfolder/small_doc.txt', format="text")

wordcount = function(input, output = NULL, 
                     pattern = '[[:punct:][:space:][:digit:]]+'){
  mapreduce(input = input, output = output, input.format = "text",
            map = function(k, lines) 
              keyval(unlist( strsplit(lines,split = pattern)), 1),
            reduce = function(word, counts) 
              keyval(word, sum(counts)))
}

wordcount("/home/myfolder/small_doc.txt", output="/home/myfolder/small_doc_wc.RData")
from.dfs("/home/myfolder/small_doc_wc.RData", format = "native")
######### End of word count example

# Find longest words
line = "I am a student. 
You are a student.
He is a student. We are all students!"
to.dfs(line, output='/home/myfolder/small_doc.txt', format="text")

findLongestWord = function(input, output = NULL, 
                           pattern = '[[:punct:][:space:][:digit:]]+'){
  mapreduce(input = input, output = output, input.format = "text",
            map = function(k, lines) {
              words = unlist( strsplit(lines,split = pattern));
              maxLenWord = words[nchar(words) == max(nchar(words))];
              # the longest word(s) in one line of the text
              return(keyval(1, maxLenWord )  );
            }, 
            reduce = function(k, v)
              keyval(k, Reduce(function(w1,w2){
                ifelse( (nchar(w1) > nchar(w2)),w1 ,w2 )}, v ) )
  )
}
findLongestWord(input  = "/home/myfolder/small_doc.txt",
                output = "/home/myfolder/small_doc_lw.RData")
from.dfs("/home/myfolder/small_doc_lw.RData") # check the result


# Save mtcars as native R object to the HDFS
to.dfs(mtcars,"/home/myfolder/mtcars.RData", format = "native")
# the "format" could be "text", "json", "csv", "native","sequence.typedbytes", "hbase", "pig.hive" or a function. 

# We can also check the content of the R data frame
from.dfs("/home/myfolder/mtcars.RData", format = "native")

# Similar to SQL "where" statements, here we filter out some 
# rows/observations. Keep only those with "am == 1"
mapreduce(input = "/home/myfolder/mtcars.RData",
          output = "/home/myfolder/mtcars_am_1.RData",
          input.format = "native",
          map = function(k, v) keyval(key = NULL, 
                                      val = v[v$am == 1,]) )
from.dfs("/home/myfolder/mtcars_am_1.RData") # Check the result.

# Similar to SQL "select", here we select some columns we need.
mapreduce(input = "/home/myfolder/mtcars.RData",
          output = "/home/myfolder/mtcars_wt_mpg.RData",
          input.format = "native",
          map = function(k, v) keyval(key = NULL, val = v[,c("wt","mpg")]) )
from.dfs("/home/myfolder/mtcars_wt_mpg.RData") # Check the result.

# Group the following data by first two characters of "x1".
# How many rows with "x1" beginning with "aa", "bb", or "cc"?
twochr = data.frame(x1=c('aa11','aa35','bb23','bb34','cc23','bb33'), 
                    x2=c(1,1,2,3,4,4))
to.dfs(twochr, "/home/myfolder/twochr.RData")

# Using substr() to get first 2 characters of the "x1" as the key
from.dfs( 
  mapreduce(input = "/home/myfolder/twochr.RData",
            map = function(k, v) 
              keyval(key = substr(v$x1, start = 1, stop = 2), val = v ) ,
            reduce = function(k, V) 
              keyval(k, val = nrow(V))
  )
)

# Find distinct gear values from mtcars
mapreduce(input = "/home/myfolder/mtcars.RData",
          output = "/home/myfolder/mtcars_distinct_gear.RData",
          map = function(k, v) keyval(key = v$gear, 1))
from.dfs("/home/myfolder/mtcars_distinct_gear.RData")

# Delete existing file
hdfs.del("/home/myfolder/mtcars_distinct_gear.RData")

mapreduce(input = "/home/myfolder/mtcars.RData",
          output = "/home/myfolder/mtcars_distinct_gear.RData",
          map = function(k, v) keyval(key = v$gear, 1),
          reduce = function(k, v) keyval(key = k, 1))

from.dfs("/home/myfolder/mtcars_distinct_gear.RData")

# Or we can do it this way. 
from.dfs(
  mapreduce(
    input = "/home/myfolder/mtcars.RData",
    map = function(k, v){
      return(keyval(v$gear, v$gear));
    },
    reduce = function(k, v){
      return(keyval(k, length(v) ) ); # Also get the frequencies of "gears"
    }
  )
)

# Sort by gear and mpg
from.dfs(mapreduce(input = "/home/myfolder/mtcars.RData", input.format = "native",
                   map = function(k, v) keyval( v$gear, val = v),
                   reduce = function(k, v) keyval(k, v[order(v$mpg),])))

# Using composite key also works, too
from.dfs(mapreduce(input = "/home/myfolder/mtcars.RData", input.format = "native",
                   map = function(k, v) keyval( v[, c("gear", "mpg")], val = v),
                   reduce = function(k, v) keyval(k, v)))

# Create & upload a data frame with duplicates 
dup = data.frame(x1=c('a','a','b','b','c','c'), x2=c(1,1,2,3,4,4))

to.dfs(dup, "/home/myfolder/dup.RData")
# Remove duplicates
from.dfs( mapreduce( input = "/home/myfolder/dup.RData",
                     map = function(k, v) keyval(key = v, val = 1 ), 
                     reduce = function(k, V) keyval(key = k, val = sum(V))
))

# create 2 temporary data frames, A & B
A = data.frame(id=c(1,3,5), val=c('a','x','c'));
B = data.frame(id=c(3,5,6), val=c('x','y','z'));
to.dfs(kv = A, output = "/home/myfolder/A.RData");
to.dfs(kv = B, output = "/home/myfolder/B.RData");
# Inner join
from.dfs( equijoin(left.input = "/home/myfolder/A.RData",
                   map.left = function(k,v) keyval(v$id, v),
                   right.input = "/home/myfolder/B.RData",
                   map.right = function(k,v) keyval(v$id, v),
                   outer = NULL)) 
# Specify outer = "left", "right", or "full" for other outer joins
# left outer join
from.dfs( equijoin(left.input = "/home/myfolder/A.RData",
                   map.left = function(k,v) keyval(v$id, v),
                   right.input = "/home/myfolder/B.RData",
                   map.right = function(k,v) keyval(v$id, v),
                   outer = "left" ))

# We here simply specify multiple inputs
from.dfs(mapreduce(
  input = c("/home/myfolder/A.RData","/home/myfolder/B.RData"),
  map = function(k, v) keyval(NULL, v)))

# How about some aggregation tasks? Say how many val = 'x'?
from.dfs(mapreduce(
  input = c("/home/myfolder/A.RData","/home/myfolder/B.RData"),
  map = function(k, v) keyval(v$id, v),
  reduce = function(k, v) keyval(k, sum(v$val == 'x'))))

# Let's say we have many CSV file with the same column names, 
# "a","b","c", "d", "e" in /home/myfolder/many_csv
# Here we create 3 CSV file from a data frame "df"
df = data.frame(matrix(round(runif(15,0,5)), ncol = 5)); colnames(df) = letters[1:5]

# Split "df", write to local drive, then put them to HDFS "/home/myfolder/many_csv"
hdfs.mkdir("/home/myfolder/many_csv")
set.seed(1)
lapply(split(df,rownames(df)), FUN = function(df_part){ 
  outFile = paste("file_", rownames(df_part), ".csv", sep="");
  write.csv(df_part, outFile, row.names = F, quote = F);
  hdfs.put( src = outFile, dest = "/home/myfolder/many_csv");
}
)
# Concatenate CSV files then output as a HAR    
mapreduce( input = hdfs.ls("/home/myfolder/many_csv")$file,
           input.format = make.input.format("csv", mode = "text",  
                                            sep = ",", col.names = letters[1:5],
                                            skip = 1, stringsAsFactors = F),
           output = "/home/myfolder/output.har",
           map = function(k, v) keyval(NULL, v)
)
# Read HAR file
from.dfs("/home/myfolder/output.har")
# HAR file is actually a "folder" in HDFS
hdfs.ls("/home/myfolder/output.har")

# Create an sample matrix CSV file
write.table(matrix(1:6, ncol = 3, byrow = T), quote = F,sep = ",", col.names = F, row.names = F, file = "matrix.csv")
hdfs.put("matrix.csv", "/home/myfolder/matrix.csv")

# Let's say we have the matrix in plain text CSV
from.dfs("/home/myfolder/matrix.csv", format="csv")

from.dfs(mapreduce(
  input = "/home/myfolder/matrix.csv",
  input.format = make.input.format(format = "csv",
                                   mode = "text",  sep = ","),
  # Emit index of each cell (of each row) as the key
  map = function(k, v) keyval( 1:length(v), unclass(v)),
  reduce = function(k, V) keyval(NULL, rbind(unlist(V)))
))

# "mtcars" again for example
to.dfs(mtcars, "/home/myfolder/mtcars.RData", format="native")
# Function to get crosstab given two categorical variables, x & y
crosstab_MR = function(dfs_data, x, y, ylevels){
  mapreduce( input = dfs_data,
             map = function(k, v){
               # Output values of "x" as the keys
               return(keyval(key = v[,x], val = v[, y] ));
             },
             reduce = function(k, v){
               tab = rbind(table(factor(v,levels=ylevels) ));
               rownames(tab) = k;  return(keyval(key=k, val=tab));
             }
  )}
from.dfs(crosstab_MR("/home/myfolder/mtcars.RData",
                     x = 'am', y = 'gear', ylevels = c(3,4,5)));

# Sort "mtcars" by given two variables
chainMRSort = function(input, output, byVar1, byVar2){
  mapreduce( input = mapreduce( input = input, 
                                map = function(k, v) keyval(v[, byVar1], v)),
             # Output composite key (col1, col2). Then sort by the key.
             # Note that the composite key is therefore a row of a "data frame".
             map = function(k, v) 
               keyval( data.frame(col1=k, col2=v[, byVar2]), v),
             reduce = function(k,v) 
               keyval(k,v), output = output)
}
chainMRSort("/home/myfolder/mtcars.RData", "/home/myfolder/chainSort1.RData", "am", "mpg")
from.dfs("/home/myfolder/chainSort1.RData")

# Wordcount() that uses reduce() as the combiner.
wordcount_w_combiner = function(input, output = NULL, 
                                pattern = '[[:punct:][:space:][:digit:]]+'){
  mapreduce(input = input, output = output, input.format = "text",
            map = function(k, lines) 
              keyval(unlist( strsplit(lines,split = pattern)),1),
            reduce = function(word, counts) 
              keyval(word, sum(counts)), combine = T)  
}

# A vector of functions (it's actually an "R list")
someCalculationList = c(function(x) x^2, function(x) x^3, mean, max)
lapply(someCalculationList, FUN = function(f) f(1:10))

two_numbers = to.dfs(kv = keyval(key = 1:2,val = c("num1", "num2")))
two_numbers # your output should be different from below.

two_numbers() 

# workers: a list of functions to save the return values of to.dfs() 
workers = list(); numOfWorkers = 20; x_bars_per_worker = 50
# to.dfs() returns a function that contains the temporary location of file in 
# HDFS. Here we consider putting 20 temporary small files on HDFS as 20
# workers. We'd like each worker to compute 50 x_bars.
# These small files are just key-value pairs: 1-> 50, 2-> 50, ..., 20 -> 50
for(i in 1:numOfWorkers) 
  workers[[i]] = to.dfs(keyval(i, x_bars_per_worker))

x_bar_kv = from.dfs( mapreduce(input=workers, map = function(k, v){
  set.seed(k)# different seed to avoid generating identical random numbers
  # An x_bar is the mean of 100 uniformly-generated numbers (between 0 and 1)
  matrix_worker_by_xbars = matrix(runif( v[1] * 100,0,1), ncol = v[1] )
  x_bars = colMeans(matrix_worker_by_xbars)
  return(keyval(1, list(x_bars) ))
},reduce = function(k, v)(keyval(k,  v))
))
ggplot2::qplot(x = unlist(x_bar_kv$val),geom = "histogram" )

# To distribute the task of building a random forest model that predict "price" in diamond data
library(ggplot2); library(randomForest); diamonds_df = as.data.frame(diamonds)
# Let's say we have a big  R dataset. We here convert the dataset into compressed key-value pair data
pryr::object_size(diamonds_df) # ~3.46 MB
compressed_diamonds_df = list(memCompress(serialize(diamonds_df, NULL),type = "gzip" ))
pryr::object_size(compressed_diamonds_df) # ~ 509 KB
# Remove unnecessary objects. 
rm(diamonds_df); gc()

# Number of logical "workers"
workers = character(10) # 10 workers
for(i in 1:10){
  # Locations of 10 identical datasets. The only difference is the "key" (from 1 to 10)
  workers[i] =  paste("/home/myfolder/compress_diamonds_", i, ".RData", sep = "")
  kv = keyval(i, compressed_diamonds_df ) # The key is from 1 to 10. Used as the random seed value
  to.dfs(kv, workers[i])
}

# 10 workers by 10 trees per worker = 100 regression trees in the forest
system.time({
  rf = from.dfs(mapreduce(input = workers, 
                          map = function(k, v){
                            # Decompress the data 
                            dat = unserialize(memDecompress(v[[1]], type = "gzip"))
                            set.seed(k) # set seed to make random forest "random".
                            # Compress the randomforest model object
                            return( keyval(k, list(memCompress(serialize(
                              randomForest::randomForest(price ~ ., data = dat, ntree = 10, na.action = na.omit), NULL),type = "gzip" ))) )
                          }, reduce = function(k, v) keyval(k,v)
  ))
})
# "rf" is now a key (1-10) to value(10 random forests. 10 trees for each)
rfs = list(); rf_val = values(rf)
for(i in 1:10) {rfs[i] = list(unserialize(memDecompress(rf_val[[i]], type = "gzip")))}
# Combine 10 forests. The final random forest model has 100 trees
mergedRF = Reduce(f = function(rf1, rf2) randomForest::combine(rf1, rf2), rfs)
pryr::object_size(mergedRF) # The size of the forest



# Package plyrmr
# Again, let's say we have the "mtcars" in HDFS
dim(input("/home/myfolder/mtcars.RData", format="native"))

input("/home/myfolder/mtcars.RData", format="native") %|% dim
# Also remember to check out plyrmr::nrow() and plyrmr::ncol()

input("/home/myfolder/mtcars.RData", format="native") %|%
  where(am == 1) %|%
  output(path = "/home/myfolder/mtcars_am1.RData")
hdfs.ls("/home/myfolder/mtcars_am1.RData")

# Let's check out the output data frame.
from.dfs("/home/myfolder/mtcars_am1.RData")

# Built-in iris data
to.dfs(iris, "/home/myfolder/iris.RData")
# Equivalent to "select Sepal.Length, Sepal.Width, Species
# from iris where Species == "setosa")
where(select(input("/home/myfolder/iris.RData"), Sepal.Length, Sepal.Width, Species ), Species == "setosa")

# Equivalent to "select am, count(am) from mtcars group by am"
# Note that the nrow() is NOT base::nrow()
input("/home/myfolder/mtcars.RData") %|%
  group(.columns=c("am")) %|% nrow %|%
  output(path="/home/myfolder/mtcars_nrow_by_am.RData")

# Adding a new column, kpl, for "km per liter".
# Only keep those with kpl > 10
bind.cols(input("/home/myfolder/mtcars.RData"), kpl = mpg * 0.425143707) %|%
  where(.cond = kpl > 10) %|% output(path="/home/myfolder/mtcars_kpl_gt10.RData")
from.dfs("/home/myfolder/mtcars_kpl_gt10.RData")

# Put two new car datasets to the HDFS
to.dfs(data.frame(carName = rownames(mtcars), mpg = mtcars[,c("mpg")]), "/home/myfolder/car_mpg1.RData")
to.dfs(data.frame(carName = "VW Golf", mpg = 22 ),  "/home/myfolder/car_mpg2.RData")
# Concatenate two datasets
plyrmr::rbind(input("/home/myfolder/car_mpg2.RData"),
              input("/home/myfolder/car_mpg1.RData")) %|%
  output(path="/home/myfolder/car_mpg.RData")

# Five Numbers
transmute(input("/home/myfolder/mtcars.RData"), fivenum(mpg), fivenum(hp) )

# sqldf("select * from mtcars where mpg >= (select avg(mpg) from mtcars)")
# Get average mpg as a numeric value
avg_mpg = as.data.frame(transmute(input("/home/myfolder/mtcars.RData"), mean(mpg)))[[1]]
input("/home/myfolder/mtcars.RData") %|% where(.cond = mpg >= avg_mpg)

# Frequency
result = as.data.frame(transmute(input("/home/myfolder/iris.RData"), list(table(Species))))
result[[1]]

# Normality tests
result = as.data.frame(transmute(input("/home/myfolder/mtcars.RData"),
                                 shapiro = list(shapiro.test(mpg)),
                                 lillie = list(nortest::lillie.test(mpg) ) ))
result[[1]]
# Linear model
result = as.data.frame(transmute(input("/home/myfolder/mtcars.RData"),
                                 list( lm(formula = mpg ~ hp + am + hp:am))))
result[[1]]

A = data.frame(id=c(1,3,5), val=c('a','x','c'), stringsAsFactors = F);
B = data.frame(id=c(3,5,6), val=c('x','y','z'), stringsAsFactors = F);
to.dfs(A, "/home/myfolder/A.RData")
to.dfs(B, "/home/myfolder/B.RData")

# Left outer join A and B by "id"
merge(input("/home/myfolder/A.RData"), input("/home/myfolder/B.RData"), by = "id", all.x = T) %|%  
  output(path="/home/myfolder/AB_inner.RData")

# Inner join A and B by "id"
plyrmr::union(input("/home/myfolder/A.RData"),
              input("/home/myfolder/B.RData")) %|% output("/home/myfolder/AB_union.RData")


# Union
plyrmr::union(input("/home/myfolder/A.RData"),
              input("/home/myfolder/B.RData")) %|% output("/home/myfolder/AB_union.RData")

# Reshape
CO2_df = data.frame(lapply(CO2, FUN = function(v){ if(is.factor(v)){v = as.character(v)} else {v = v} }),
                    stringsAsFactors = F)
to.dfs(CO2_df, "/home/myfolder/CO2.RData")

# Average CO2 uptake for different Treatments and the origin of the plants
plyrmr::dcast(input("/home/myfolder/CO2.RData"),
              formula = Type ~ Treatment,
              value.var = "uptake", fun.aggregate = mean )

# Five numbers for different "am"
group(input("/home/myfolder/mtcars.RData"), am) %|%
  gapply(.f = function(x) list(fivenum(x$mpg)) ) %|%
  output("/home/myfolder/mtcars_am_fivenum.RData")

# Linear models for different "am"
gapply(group(input("/home/myfolder/mtcars.RData"), am), function(x) list(lm(mpg ~ wt, data = x)))


