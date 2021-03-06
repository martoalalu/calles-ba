library(httr)
library(tidyverse)
library(splitstackshape)
library(WikidataR)
library(sf)

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
  mutate(anio_calle=str_extract(calle_02, pattern = '\\d{4}')) %>%
  select(codigo, nomoficial, calle_02, anio_calle)

calles_glosario$codigo <- as.numeric(calles_glosario$codigo)

#Le sacamos los ".AV"
calles_glosario[] <- lapply(calles_glosario, gsub, pattern="AV.", replacement='')

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
bio$calle_2 <- toupper(bio$calle_2)
bio$calle_1<-chartr('ÁÉÍÓÚÜ','AEIOUU',bio$calle_1)
bio$calle_2<-chartr('ÁÉÍÓÚÜÑ','AEIOUUN',bio$calle_2)

bio <- bio %>% 
  cSplit("calle_1", sep="-") %>% 
  select(calle_1_1,calle_2) %>% 
  rename(calle=calle_1_1,
         descripcion=calle_2)

bio <- cSplit(bio,"descripcion", sep=":")
bio <- select(bio,calle,descripcion_01, descripcion_02)
bio$clave <- gsub("([A-Za-z]+).*", "\\1", bio$descripcion_02)

calles_glosario <- left_join(calles_glosario,bio,by=c("nomoficial"="calle"))
calles_glosario$codigo <- as.numeric(calles_glosario$codigo)

#Traemos el archvivo geográfico con las calles
callejero <- read_sf("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/calles/callejero-ba.geojson")
ggplot()+
  geom_sf(data=callejero)

callejero <- left_join(callejero,calles_glosario,by=c("codigo"="codigo"))
callejero$anio_calle <- as.numeric(callejero$anio_calle)

sum(is.na(callejero$anio_calle))

write_sf(callejero,"C:/Users/20332842324/Documents/GitHub/calles-ba/data/callejero.shp")

# NLP

library(tm)


corpus <- callejero$descripcion
myCorpus = VCorpus(VectorSource(corpus))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords(kind = "es"))
myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)

x <-as.data.frame(m) 
y <- colSums(x)
z <- data.frame(as.list(y))
categorias <- as.data.frame(t(as.matrix(z)))
