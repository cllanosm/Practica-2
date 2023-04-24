library(httr)
library(XML)
library(stringr)
library(dplyr) 
library(purrr)
library(ggplot2)
library(gridExtra)

##Pregunta 1.1 Descargar la página web de la URL indicada, y almacenarlo en un formato de R apto para ser tratado.

html <-  GET("https://www.mediawiki.org/wiki/MediaWiki")
status_code(html)
content <-  content(html, as= "text")
print (content)

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

Tabla_estatus <- linkData$Status_Code

#Pregunta 2.1 Un histograma con la frecuencia de aparición de los enlaces, pero separado por URLs absolutas (con “http…”) y URLs relativas.

library(ggplot2)
library(ggpubr)

linkData$Tipo_url <- ifelse(grepl("^http",linkData$Link),"Absoluto","Relativa" )


#Creando Histograma para Url's Absolutas

url_freq <- table(linkData$Link)

url_df <- data.frame(URL =names(url_freq), Frecuencia = as.numeric(url_freq))

ggplot(url_df, aes(x = Frecuencia)) + 
  geom_histogram() +
  facet_wrap(~ ifelse(grepl("^https?://", URL), "URLs absolutas", "URLs relativas"), ncol = 2) +
  ggtitle("Histograma de Frecuencia de Aparicion de URLs")
  geom_text(stat = "count", aes(label = ..count..), vjust = 1)
  
  
#Pregunta 2.2 Un gráfico de barras indicando la suma de enlaces que apuntan a otros dominios o 
#             servicios (distinto a https://www.mediawiki.org en el caso de ejemplo) vs. la suma 
#             de los otros enlaces.
  
  
  
#Identificando tipo de enlace
  linkData$Tipo_Enlace <- ifelse(grepl("^https://www.mediawiki.org",linkData$LinkAbsoluto),"Interno","Externo")
  
  otros_links <- linkData[!grepl("^https://www.mediawiki.org", linkData$link) & !is.na(linkData$link), ]
  suma_otros_links <- sum(otros_links$count, na.rm = TRUE)
  media_links <- linkData[grepl("^https://www.mediawiki.org", linkData$link) & !is.na(linkData$link), ]
  suma_media_links <- sum(media_links$count, na.rm = TRUE)
  suma_links <- data.frame(Tipo = c("MediaWiki", "Otros"),
                           Cantidad = c(suma_media_links, suma_otros_links))
  
  ggplot(suma_links, aes(x = Tipo, y = Cantidad, fill = Tipo)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Suma de enlaces internos vs externos", x = "Tipo de enlace", y = "Cantidad") +
    theme_light() +
    scale_fill_manual(values = c("#00FF00", "#0000FF")) +
    scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 20))
  
  
  
  
#Pregunta  2.3 Un gráfico de tarta (pie chart) indicando los porcentajes de Status de nuestro análisis.

# Calculando la frecuencia de cada Status_Code
status_freq <- table(linkData$Status_Code) 

# Crear un data.frame con los valores y porcentajes
status_df <- data.frame(Status_Code = names(status_freq), 
                        Count = as.numeric(status_freq),
                        Percentage = paste0(round(100*as.numeric(status_freq)/sum(status_freq), 2), "%"))

# Generando el gráfico de tarta
ggplot(status_df, aes(x="", y=Count, fill=Status_Code)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.1)) +
  labs(title = "Porcentaje de registros por Status_Code")
  
