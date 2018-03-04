library(RCurl)
library(XML)

url <- "https://www.bloomberg.com/markets"
html <- htmlParse(getURL(url, .encoding = "utf-8"), encoding = "utf-8")
xpathSApply(html, "//h1[@class='top-news-v3-story-headline']", xmlValue)