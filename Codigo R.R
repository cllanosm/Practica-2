html <-  GET("https://www.mediawiki.org/wiki/MediaWiki")
content <-  content(html, as= "text")
parsedHtml <-  htmlParse (content, asText = TRUE)