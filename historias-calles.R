library(httr)
library(tidyverse)
library(splitstackshape)

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

glosario$calle_01<-chartr('ÁÉÍÓÚ','AEIOU',glosario$calle_01)
calles$nomoficial <- toupper(calles$nomoficial)

calles_glosario <- left_join(calles, glosario, by = c("nomoficial"="calle_01"))

#####
library(tidyverse)
library(readtext)
library(tm)

#Importamos el libro de las calles y el listado de calles
glosario <- readtext("https://raw.githubusercontent.com/martoalalu/calles-ba/master/data/calles-actuales.txt", encoding = 'UTF-8')
calles <- read.csv("https://raw.githubusercontent.com/martoalalu/calles-ba/master/data/callejero.csv", encoding = 'UTF-8')

#Pasamos a mayusculas los textos
glosario$text <- toupper(glosario$text)
calles$nomoficial <- toupper(calles$nomoficial)


texto <- calles$nomoficial[1]


#Todos los titulos de las calles empiezan con \n\n y luego el nombre de la calle
enc <- "\n\n"
fin <- ".\n"

calles <- calles %>% 
  mutate(regex=paste0(enc,nomoficial))

#Probamos detectando la calle 11 de Septiembre y la ubicación exacta
texto2 <- paste0(enc,"11 DE SEPTIEMBRE")

str_detect(glosario$text, texto2)
str_locate(glosario$text, texto2)