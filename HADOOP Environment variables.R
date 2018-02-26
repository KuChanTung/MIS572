#### RHadoop 
# HADOOP Environment variables
Sys.setenv(HADOOP_CMD="/home/hadoop/hadoop/bin/hadoop")
Sys.setenv(JAVA_HOME="/usr/java/latest")
Sys.setenv(HADOOP_STREAMING="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.1.jar")
Sys.setenv(HADOOP_HOME="/home/hadoop/hadoop")

library(rhdfs)
library(rmr2)
library(plyrmr)

hdfs.init()


#### Spark
# Stop previous SparkR session if necessary 
sparkR.stop() 

# Spark Environment variables
Sys.setenv(SPARK_HOME="/home/hadoop/apache-spark")
Sys.setenv(YARN_CONF_DIR="/home/hadoop/hadoop/etc/hadoop")
library("SparkR")

#Initializes a new SparkContext.
sc = sparkR.init(master="spark://192.168.1.100:7077")
sqlContext = sparkRSQL.init(sc)


