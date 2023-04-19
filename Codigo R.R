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

valores

