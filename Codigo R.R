#Pregunta 1.1
library(httr)
library(XML)
html <-  GET("https://www.mediawiki.org/wiki/MediaWiki")
content <-  content(html, as= "text")
parsedHtml <-  htmlParse (content, asText = TRUE)



#Pregunta 1.2

title <- xpathSApply(parsedHtml,  "//title", xmlValue)
texts <- xpathSApply(parsedHtml, "//p", xmlValue)

images_url <- xpathSApply(parsedHtml, "//img", xmlGetAttr, 'src')

#Pregunta 1.3

links_text <- xpathSApply(parsedHtml, "//a", xmlValue)
links_url <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')

#Comprobando valores nulos

valores_nulos <-  sapply(links_text, is.null)

#Asignar valor por defecto NA a valores nulos

valores_nulos <-  NA 

#Pregunta 1.4

links <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')
linkTexts <- xpathSApply(parsedHtml, "//a", function(x) trimws(xpathSApply(x, "string(.)")))
linkData <- data.frame(Link = links, Text = linkTexts, stringsAsFactors = FALSE)
linkCounts <- as.data.frame(table(linkData$Link))
colnames(linkCounts) <- c("Link", "Count")
linkTable <- merge(linkData, linkCounts, by = "Link")
linkTable <- linkTable[order(linkTable$Count, decreasing = TRUE),]

#Pregunta 1.5

enlaces <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')

linkTable$isabolsute <- length(grep("http", linkTable$Link)) >0|length(grep("https", linkTable$Link)) > 0




