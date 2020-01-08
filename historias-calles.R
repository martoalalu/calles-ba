library(httr)
library(tidyverse)
library(splitstackshape)
library(WikidataR)

url="https://raw.githubusercontent.com/martoalalu/calles-ba/master/data/calles-actuales.txt"
request <- GET(url = url)

request$status_code
response <- content(request, as = "text", encoding = "UTF-8")

#Splitteamos
response_split <- strsplit(response, "\n\n")

#Probamos
response_split[[1]][2007]

#Pasamos a DF
df <- data.frame(matrix(unlist(response_split), ncol =length(response_split), byrow=T))

#Reonmbramos columna
df<-rename(df,calle=matrix.unlist.response_split...ncol...length.response_split...)

#Spliteamos para quedarnos con una columna que tenga el nombre de calle

glosario<- cSplit(df, "calle", sep="\n")
calles <- read.csv("https://raw.githubusercontent.com/martoalalu/calles-ba/master/data/callejero.csv", encoding = 'UTF-8')

glosario$calle_01<-chartr('ÁÉÍÓÚÜ','AEIOUU',glosario$calle_01)
calles$nomoficial <- toupper(calles$nomoficial)

calles_glosario <- left_join(calles, glosario, by = c("nomoficial"="calle_01"))

#Extraemos el año de la ordenanza / ley a través del cual se dio nombre a esa calle
calles_glosario <- calles_glosario %>% 
  mutate(anio_calle=str_extract(calle_02, pattern = '\\d{4}'))




# Traemos el libro con la descripción de las calles
url="https://raw.githubusercontent.com/martoalalu/calles-ba/master/data/calles-antiguas.txt"
request <- GET(url = url)

request$status_code
response <- content(request, as = "text", encoding = "UTF-8")
response
#Splitteamos
response_split <- strsplit(response, "\n\n")

#Probamos
response_split[[1]][2007]

#Pasamos a DF
df_bio <- data.frame(matrix(unlist(response_split), ncol =length(response_split), byrow=T))

#Reonmbramos columna
df_bio<-rename(df_bio,calle=matrix.unlist.response_split...ncol...length.response_split...)

#Spliteamos para quedarnos con una columna que tenga el nombre de calle

bio<- cSplit(df_bio, "calle", sep="\n")
bio$calle_01<-chartr('ÁÉÍÓÚÜ','AEIOUU',bio$calle_01)

View(bio[1:20])





str_locate(bio$calle_01, "-")