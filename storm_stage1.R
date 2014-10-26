install.packages("markdown")
library(markdown)
rpubsUpload(title, htmlFile, id = NULL, properties = list(), method = getOption("rpubs.upload.method", "internal"))

