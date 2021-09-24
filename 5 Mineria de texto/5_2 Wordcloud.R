
#--------------------------------------------------------------------------------
# Tema:       Miner?a de texto
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Texto
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Crear corpus
#       4. Limpiar texto
#           4.1 Definimos funcion
#           4.2 Limpieza
#       5. Palabras vacias
#       6. Matriz
#       7. Nube de palabras
#       8. Frecuencias de palabras
#       9. Asociaciones de palabras
#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(tm)   # analizar texto
library(SnowballC)    # Nube palabras
library(wordcloud)    # Nube palabras
library(RColorBrewer) # colores
library(foreign)  
library(dplyr)
library(ggplot2)
library(igraph)       # asociaciones



#2.  Directorio de trabajo

setwd("..../5_Datos")
#3. Crear corpus

# readLines = cargar texto. Argumento encoding= señalar que es español
# VectorSource=Guardar en una matrix
# Corpus= prepara los datos para analizarlos

docs <- Corpus(VectorSource(readLines("TEXTO.txt", encoding = "UTF-8")))

#4. Limpiar texto

    #4.1 Definimos funcion
    # Esta función me reemplaza los símbolos por espacio en blanco
    reemplazar <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    
    #4.2 Limpieza
    docs <- tm_map(docs, reemplazar, "/")
    docs <- tm_map(docs, reemplazar, "@")
    docs <- tm_map(docs, reemplazar, ";")
    #docs <- tm_map(docs, reemplazar, "?")
    docs <- tm_map(docs, reemplazar, ":")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("spanish"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    
    inspect(docs)


#5. Palabras vacias

docs <- tm_map(docs, removeWords, c("pues","tenia")) 


#6. Matriz

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#7. Nube de palabras

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=400, random.order=FALSE, rot.per=0.7, 
          colors=brewer.pal(8, "Dark2"))


#8. Frecuencias de palabras

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Palabras frecuentes",
        ylab = "Frecuencias")


#9. Asociaciones de palabras
findAssocs(dtm, terms = c("m�xico"), corlimit = 0.50)	






