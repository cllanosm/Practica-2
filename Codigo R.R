##Pregunta 1.1 Descargar la página web de la URL indicada, y almacenarlo en un formato de R apto para ser tratado.
library(httr)
library(XML)
library(stringr)
library(dplyr) 
library(purrr)
library(ggplot2)
library(gridExtra)
html <-  GET("https://www.mediawiki.org/wiki/MediaWiki")
content <-  content(html, as= "text")




##Pregunta 1.2 Analizar el contenido de la web, buscando el título de la página (que en HTML se etiqueta como “title”).
parsedHtml <-  htmlParse (content) 
title <- xpathSApply(parsedHtml,  "//title", xmlValue)

##Pregunta 1.3 Analizar el contenido de la web, buscando todos los enlaces (que en HTML se etiquetan como “a”), buscando el texto del enlace, así como la URL.

links_text <- xpathSApply(parsedHtml, "//a", xmlValue)
links_url <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')

nulls <- sapply(links_text, is.null)
nulls_links_url <- sapply(links_url, is.null)

links_text[nulls] <- NA
links_url[nulls_links_url] <- NA

links_text <-  unlist (links_text)
links_url <- unlist (links_url)


#Pregunta 1.4 Generar una tabla con cada enlace encontrado, indicando el texto que acompaña el enlace, y el número de veces que aparece un enlace con ese mismo objetivo.

links <- xpathSApply(parsedHtml, "//a", xmlGetAttr, 'href')
linkTexts <- xpathSApply(parsedHtml, "//a", function(x) trimws(xpathSApply(x, "string(.)")))
linkData <- data.frame(Link = links, Text = linkTexts, stringsAsFactors = FALSE)
linkCounts <- as.data.frame(table(linkData$Link))
colnames(linkCounts) <- c("Link", "Count")
linkTable <- merge(linkData, linkCounts, by = "Link")
linkTable <- linkTable[order(linkTable$Count, decreasing = TRUE),]

#Pregunta 1.5 Para cada enlace, seguirlo e indicar si está activo (podemos usar el código de status HTTP al hacer una petición a esa URL).


linkData$LinkAbsoluto <-  case_when(
  
  grepl("^/wiki/",linkData$Link) ~ paste0("https://www.mediawiki.org", linkData$Link),
  grepl("^/w/",linkData$Link) ~ paste0("https://www.mediawiki.org", linkData$Link),
  grepl("^//",linkData$Link) ~ paste0("https://www.mediawiki.org", linkData$Link),
  grepl("^#",linkData$Link) ~ paste0("https://www.mediawiki.org","/wiki/MediaWiki", linkData$Link),
  grepl("^https",linkData$Link) ~ paste0("",linkData$Link)
)

# Lectura de datos para incorporar HEAD status_code
status_code <- map(linkData$LinkAbsoluto, HEAD)
# Agregando columna Status_Code con valor respectivo
linkData$Status_Code <- sapply(status_code, status_code)


#Pregunta 2.1 Un histograma con la frecuencia de aparición de los enlaces, pero separado por URLs absolutas (con “http…”) y URLs relativas.

library(ggplot2)

linkData$Tipo_url <- ifelse(grepl("^http",linkData$Link),"Absoluto","Relativa" )

#Creando Histograma para Url's Absolutas

ggplot(linkData, aes(x=Freq, fill=link_type)) + 
  geom_bar(binwidth = 1, position = "dodge") +
  scale_fill_manual(values=c("#FF5733", "#6B33FF")) +
  labs(x = "Frecuencia de apariciones", y = "N° de links_link", title ="links_link MediaWiki") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  theme_light()


#Creando Histograma para Url's Relativas


#Unificando Histogramas


grafico_barras <- ggplot(linkData, aes(x=Freq)) + 
  geometrico(aes(fill=link_type), 
             binwidth = 1, 
             position = "dodge") +
  scale_fill_manual(values=c("#FF5733", "#6B33FF")) +
  labs(x = "Frecuencia de apariciones", y = "N° de links_link", title ="links_link MediaWiki") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10), 
                     expand = c(0, 0)) +
  theme_light()


rel_linkData <- linkData[linkData$Tipo_url == "relativo", ]
hist(rel_linkData$Link, main = "Histograma de Frecuencia de Registros Relativos en Link",
     xlab = "Link", ylab = "Frecuencia")
