#' A function to get crosstab of x by y in MapReduce manner.
#' @description Given a data frame with x and y variables, this MapReduce
#' function creates a crosstab of x by y.
#' The level names of y is required to label all rows and columns.
#' @author Yihuang Kang
#' @param dfs_data A data frame in HDFS
#' @param x Column name X in the data frame
#' @param y Column name Y in the data frame
#' @param ylevels A char vector with all level names of variable/column Y
#' @examples from.dfs(crosstab_MR("/home/yihuang/mtcars.RData",x = 'am', y = 'gear', ylevels = c(3,4,5)));
crosstab_MR = function(dfs_data, x, y, ylevels){
  mapreduce(
    input = dfs_data,
    map = function(k, v){
      # Split by "x" values as the keys
      return(keyval(key = v[,x], val = v[, y] ));
    },
    reduce = function(k, v){
      tab = rbind(table(factor(v,levels=ylevels) ));
      rownames(tab) = k;
      return(keyval(key=k, val=tab));
    }
  )
}
