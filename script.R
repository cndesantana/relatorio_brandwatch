library(tidyverse)
library(plotrix)
library(stringr)
library(quanteda)
library(readtext)
library(tm)
library(dplyr)
library(scales)
library(qdapRegex)
library(grDevices)
library(treemap)
library(stylo)
library(tidytext)
library(tokenizers)
library(rtweet)
library(readxl)
library(lubridate)
library(treemapify)

### Variables and Functions

corpositivo <- "#20B2AA";
cornegativo <- "#c00000";
corneutro <- "#FFA500";
cornovo <- "orange3"
badwords <- c("compartilhado","boa","scontent.xx.fbcdn.net","https","oh","oe","pra","v","y9zuzurte",
              "como","para","de","do","da","das","dos","isso","esse",
              "nisso","nesse","aquele","nesses","aqueles","aquela",
              "aquelas","que","q","é","sr","senhor","comentário","perfil",
              "mais","com","está","por","uma","tem","vai","pelo","meu",
              "sobre","não","já","nos","sem","quando","xed","xbd","ser",
              "xbe","xa0","x8f","xb9","xb2","xb0","xb1","xb8","x8c","xa3",
              "xbc","xaa","www.youtube.com","scontent.xx.fbcdn.net","https",
              "oh","oe","pra","v","como","para","de","do","da","das","dos",
              "isso","esse","nisso","nesse","aquele","nesses","aqueles","aquela",
              "aquelas","que","q","é","sr","senhor","comentário","perfil","r","que",
              "nao","sim","comentário","feito","comentario","imagem","comentario feito no perfil de secretaria",
              "secretaria","foi","photos","http","bit.ly","sou","mais","bahia","vídeo","timeline","video","er",
              "enem","vçpt","vç","x","vc", "aqui", "você", "tá", "dia", "amanhã", "ba","aqui","governador",
              "com","que","nao","meu","mais","por","uma",
              "pra","para","um","mais","mas","clap","para","tone","skin","type","heart","facebook","iticas","munici","3","4",
              "unamused","esses","essas","até","são","ate","sao","todas","todos","toda","todo","essa", "esse","2")
palette <- c("#ff9ff3","#feca57","#ff6b6b","#48dbfb","#1dd1a1")

getPositionY <- function(test){
  labelpos <- array(NA,length(test$Sentiment))
  labelpos[ which(test$Sentiment == "positive") ] <- "0.02"
  labelpos[ which(test$Sentiment == "negative") ] <- "0.98"
  datasb <- test$Data[which(test$Sentiment == "neutral")];
  posvarb <- which(test$Sentiment == "neutral");
  for(i in 1:length(datasb)){
    datasb_ <- datasb[i];
    positionobsb <- which(test$Data == datasb_ & test$Sentiment == "positive")
    obsb <- ifelse(length(positionobsb) > 0, test$freq[positionobsb], 0);
    labelpos[posvarb[i]] <- obsb + 0.02
  }
  return(as.numeric(labelpos))
}

getUnigram <- function(text){
  text <- removeWords(text,c(stopwords("portuguese"),badwords))
  text <- rm_nchar_words(text, n= "1")#remove words with only one character
  text <- rm_nchar_words(text, n="2")#remove words with two characters
  text <- rm_nchar_words(text, n="3")#remove words with two characters
  text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
  text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
  text <- stringi::stri_trans_general(text, "latin-ascii")
  unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 1L, n_min = 1L, simplify = TRUE)))
  return(unigram)
}

fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
getMatrizDeOcorrencias <- function(text){
  text <- stringi::stri_trans_tolower(text)
  temp <- fa(text)
  temp <- rm_nchar_words(temp, "1")#remove words with only one character
  temp <- rm_nchar_words(temp, "2")#remove words with two characters
  temp <- rm_nchar_words(temp, "3")#remove words with two characters
  # Lowercase
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp=str_replace_all(temp,"[^[:graph:]]", " ") 
  # Split it
  any(grepl("I_WAS_NOT_ASCII", iconv(temp, "latin1", "ASCII", sub="I_WAS_NOT_ASCII")))
  temp <- stringi::stri_trans_general(temp, "latin-ascii")
  temp <- removePunctuation(temp)
  temp <- unlist(stringr::str_split(temp, " "))
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  }
  docs <- Corpus(VectorSource(temp))
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("portuguese"))
  # Remove your own stop word
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2","que","ser","pelo","tem","o","lhe","por","pra","de","da","do","essa","esse","isso","aquele","aquilo","desse","disso","daquilo","uma","um","NA")) 
  #  Remove punctuations
  docs <- tm_map(docs, stripWhitespace)
  #
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  return(d)
}

getIndiceDeFavorabilidade <- function(polarization){
  sentimentos <- toupper(polarization)
  allsentimentos <- c("NEUTRO","NEGATIVO","POSITIVO");
  mp <- length(which(sentimentos==allsentimentos[3]));#POSITIVA
  mt <- length(polarization);#Total
  
  indicefavorabilidade <- ifelse(mt == 0, 0, as.numeric(mp/mt))#changing sentiment index to a "positive feedback index"
  indicefavorabilidade <- round((indicefavorabilidade),digits=2) 
  
  return(indicefavorabilidade)
}

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

plotIndiceFavorabilidade = function() {
  filepath <- input$file$datapath
  file <- read_xlsx(filepath)
  
  allpolarization <- toupper(file$polarization)
  isent <- getIndiceDeFavorabilidade(allpolarization);
  
  colfunc <- colorRampPalette(c(corpositivo,corneutro))
  legend_image <- as.raster(matrix(colfunc(20), ncol=1))
  plot(c(1,20),c(0,10),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  text(x=3, y = 10*signif(isent,2), labels = paste(intToUtf8(9664),paste0(signif(isent,2))),pos=4)
  text(x = 0.45, y=10,  labels = 1,pos = 4)
  text(x = 0.4, y=0, labels = 0,pos = 4)
  rasterImage(legend_image, 1,0.1,3,9.9)
}

plotDetratoresApoiadores = function() {
  filepath <- input$file$datapath
  file <- read_xlsx(filepath)
  file %>% 
    dplyr::select(user_id, polarization) %>%
    group_by(user_id) %>% 
    count(user_id, polarization) %>%
    arrange(n, user_id) %>%
    tail(20) %>% 
    ggplot() + 
    geom_bar(stat = "identity", 
             aes(x = reorder(user_id,as.numeric(n)), y = as.numeric(n), fill = polarization)) + 
    ylab("Número de comentários") +
    xlab("") +
    scale_fill_manual("Polaridade", values = c("Positivo" = corpositivo, "Negativo" = cornegativo, "Neutro" = corneutro)) +
    geom_text( aes (x = reorder(user_id,as.numeric(n)), y = as.numeric(n), label = as.numeric(n) ) , vjust = 0, hjust = 0, size = 2 ) + 
    coord_flip() +
    theme_bw()
}

plotPalavrasDetratores = function(){
  filepath <- input$file$datapath
  file <- read_xlsx(filepath)
  
  text <- file %>%
    filter(toupper(polarization) == "NEGATIVO") %>%
    dplyr::select(text) %>%
    toupper()
  
  mydfm <- getDFMatrix(text);
  words_td <- topfeatures(mydfm,30)
  ggplot() + 
    geom_bar(stat = "identity", 
             aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
             fill = cornegativo) + 
    ylab("Número de ocorrências") +
    xlab("") +
    labs(title = "Palavras mais citadas por detratores")+
    geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
    coord_flip() +
    theme_bw()      
  
}

plotPalavrasApoiadores = function(){
  filepath <- input$file$datapath
  file <- read_xlsx(filepath)
  
  text <- file %>%
    filter(toupper(polarization) == "POSITIVO") %>%
    dplyr::select(text) %>%
    toupper()
  
  mydfm <- getDFMatrix(text);
  words_td <- topfeatures(mydfm, 30)
  ggplot() + 
    geom_bar(stat = "identity", 
             aes(x = reorder(names(words_td),as.numeric(words_td)), y = as.numeric(words_td)),
             fill = corpositivo) + 
    ylab("Número de ocorrências") +
    xlab("") + ggtitle("Palavras mais citadas por apoiadores") +
    geom_text( aes (x = reorder(names(words_td),as.numeric(words_td)), y = words_td, label = words_td ) , vjust = 0, hjust = 0, size = 2 ) + 
    coord_flip() +
    theme_bw()
}

plotSerieTemporal = function(amostra) {
  media <- stringr::str_remove_all(amostra$Domain,".com")
  df_datas <- amostra %>%
    mutate( Data = ymd_hms(Date) %>%
              as.Date() %>%
              format("%d/%m/%Y"),
            Sentiment = as.factor(toupper(Sentiment))
    ) %>%
    group_by(Data, Sentiment) %>%
    summarise(count = n()) %>%
    group_by(Data) %>%
    mutate(freq = count / sum(count))
  
  primeirodia <- min(dmy(df_datas$Data));
  ultimodia <- max(dmy(df_datas$Data))
  ggplot(df_datas, aes(x=dmy(Data), y=freq, fill=Sentiment)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_date(date_breaks = "1 day",
                 labels = date_format("%d/%m/%Y")) +
    theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
    scale_y_continuous(labels=scales::percent) +
    labs (title = paste("Sentimento dos posts - ",media), subtitle = paste("Semana",paste(range(df_datas$Data),collapse = " a ")), x = "", y = "Porcentagem de Posts") +
    theme(text = element_text(size=6), axis.text.x = element_text(angle=45, hjust=1)) +
    coord_cartesian(xlim = c(primeirodia, ultimodia)) +
    scale_fill_manual("Sentimento", values = c("POSITIVE" = corpositivo, "NEGATIVE" = cornegativo, "NEUTRAL" = corneutro)) +
    geom_text(size = 2, col = "white", aes(x = dmy(Data), y = getPositionY(df_datas), label = paste(as.character(100*round(df_datas$freq,2)),"%",sep=""))) + theme_bw();
  
}

## Treemap
plotTreemap = function(text){
  
  unigram <- getUnigram(text)
  unigram <- unigram %>% 
    filter(!(words %in% badwords))%>% 
    filter(!is.na(words)) %>% 
    select(words) %>% group_by(words) %>% 
    summarise(palavras = n()) %>% 
    arrange(palavras) %>% tail(50)
  numerodereferencia <- max(unigram$palavras) %/% 5
  unigram <- unigram %>% 
    mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5", 
                              palavras < 2*numerodereferencia ~ "de 5 a 10", 
                              palavras < 3*numerodereferencia ~ "de 10 a 50", 
                              palavras < 4*numerodereferencia ~ "de 50 a 100", 
                              palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
    mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
  ggplot(unigram, aes(area = palavras, 
                      fill = palette[as.numeric(unigram$classe)], 
                      label = words, 
                      subgroup=palavras)) +
    geom_treemap(fill = "black") +
    geom_treemap(aes(alpha=palavras)) +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = F, reflow=TRUE) + 
    geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1, 
                               col="white", cex=10) +
    ggtitle("Palavras mais comentadas")+
    scale_fill_identity() +
    scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
}

plotTreemapNegativo = function(file){
  media <- stringr::str_remove_all(file$Domain,".com")
  
  text <- toupper(file$text[which(toupper(file$polarization) == "NEGATIVE")])
  unigram <- getUnigram(text)
  unigram <- unigram %>% 
    filter(!(words %in% badwords))%>% 
    filter(!is.na(words)) %>% 
    select(words) %>% group_by(words) %>% 
    summarise(palavras = n()) %>% 
    arrange(palavras) %>% tail(50)
  numerodereferencia <- max(unigram$palavras) %/% 5
  unigram <- unigram %>% 
    mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5", 
                              palavras < 2*numerodereferencia ~ "de 5 a 10", 
                              palavras < 3*numerodereferencia ~ "de 10 a 50", 
                              palavras < 4*numerodereferencia ~ "de 50 a 100", 
                              palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
    mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
  colfunc <- colorRampPalette(c(cornegativo))
  ggplot(unigram, aes(area = palavras, 
                      fill = colfunc(5)[as.numeric(unigram$classe)], 
                      label = words, 
                      subgroup=palavras)) +
    geom_treemap(fill = "black") +
    geom_treemap(aes(alpha=palavras)) +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = F, reflow=TRUE) + 
    geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1, 
                               col="white", cex=10) +
    ggtitle(paste("Palavras mais comentadas em comentários Negativos - ",media))+
    scale_fill_identity() +
    scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
}

plotTreemapPositivo = function(file){
  media <- stringr::str_remove_all(file$Domain,".com")
  
  text <- toupper(file$text[which(toupper(file$polarization) == "POSITIVE")])
  unigram <- getUnigram(text)
  unigram <- unigram %>% 
    filter(!(words %in% badwords))%>% 
    filter(!is.na(words)) %>% 
    select(words) %>% group_by(words) %>% 
    summarise(palavras = n()) %>% 
    arrange(palavras) %>% tail(50)
  numerodereferencia <- max(unigram$palavras) %/% 5
  unigram <- unigram %>% 
    mutate(classe = case_when(palavras < numerodereferencia ~ "de 1 a 5", 
                              palavras < 2*numerodereferencia ~ "de 5 a 10", 
                              palavras < 3*numerodereferencia ~ "de 10 a 50", 
                              palavras < 4*numerodereferencia ~ "de 50 a 100", 
                              palavras >= 4*numerodereferencia ~ "mais que 100")) %>%
    mutate(classe = factor(classe, levels = c("de 1 a 5", "de 5 a 10", "de 10 a 50", "de 50 a 100", "mais que 100")))
  colfunc <- colorRampPalette(c(corpositivo))
  ggplot(unigram, aes(area = palavras, 
                      fill = colfunc(5)[as.numeric(unigram$classe)], 
                      label = words, 
                      subgroup=palavras)) +
    geom_treemap(fill = "black") +
    geom_treemap(aes(alpha=palavras)) +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = F, reflow=TRUE) + 
    geom_treemap_subgroup_text(place = "bottomright", grow = F, alpha = 1, 
                               col="white", cex=10) +
    ggtitle(paste("Palavras mais comentadas em comentários Positivos - ",media)+
    scale_fill_identity() +
    scale_alpha_continuous(range = c(0.4, 1),guide = 'none')
}

plotIndiceFavorabilidade = function(file) {
  media <- stringr::str_remove_all(file$Domain,".com")
  
  file %>% mutate(Date = ymd_hms(Date), 
                  Date = ceiling_date(Date, unit="day"), 
                  Sentiment = toupper(Sentiment)) %>% 
    group_by(Date) %>% 
    mutate(mt = n(), 
           ispositive = if_else(Sentiment=="POSITIVE",1,0)) %>% 
    group_by(Date, mt) %>% summarise(mp = sum(ispositive)) %>%
    ungroup()%>%
    mutate(isent=ifelse(mt == 0, 0, as.numeric(mp/mt)),
           isent=round((isent),digits=2)) %>%
    ggplot(aes(x = Date, y = isent)) +
    geom_line(cex = 1.5, col = corpositivo) +
    geom_point(cex=5, col = corpositivo)+
    theme_minimal()+
    labs(title = paste("Índice de Favorabilidade - ",media),
         subtitle = paste("Semana",paste(range(ymd_hms(file$Date) %>% format("%d-%m-%Y")),collapse = " a "))) +
    ylim(0,1)
}

### read data
fora_kalil <- search_tweets("#forakalil", n=5000)
data_ig <- read_xlsx("RP-IG.xlsx", skip = 8)
data_fb <- read_xlsx("RP-FB.xlsx", skip = 8)
data_tw <- read_xlsx("RP - Twitter .xlsx", skip = 7)
data_kalil <- read_xlsx("Kalil - Twitter.xlsx", skip=7)

### 
data_users_fora_kalil <- users_data(fora_kalil)

###

### Plots
### Palavras mais citadas no Fora Kalil
p1 <- getUnigram(data_users_fora_kalil$description) %>% 
  filter(!is.na(words)) %>% 
  count(words) %>% arrange(n) %>% tail(30) %>% 
  ggplot(aes(x = reorder(words,n), y = n)) +
  geom_bar(stat="identity", fill = cornovo) + coord_flip() +
  theme_minimal() +
  labs(title = "Palavras mais citadas na descrição do perfil", subtitle = "#ForaKalil",
       y = "Número de menções", x = "Palavra")
png("palavras_descricao_forakalil.png",width=3200,height=1800,res=300)
print(p1)
dev.off()
  
### Location mais comum no Fora Kalil
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte - MG")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte MG")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte, Brazil")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte MG Brasil")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="BRASIL")] <- "Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte, MG - BRASIL")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Mineira")] <- "Minas Gerais, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Minas, Brasil y Latinoamérica")] <- "Minas Gerais, Brasil"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Estados Unidos ")] <- "Estados Unidos"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="EUA")] <- "Estados Unidos"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="United States of America")] <- "Estados Unidos"
data_users_fora_kalil$location[which(data_users_fora_kalil$location=="Belo Horizonte MG Brasil")] <- "Belo Horizonte, Brasil"
data_users_fora_kalil$location[str_detect(data_users_fora_kalil$location, "BH")] <- "Belo Horizonte, Brasil"
p1 <- data_users_fora_kalil %>% 
  filter(!is.na(location), location !="") %>% 
  count(location) %>% arrange(n) %>% tail(10) %>% 
  ggplot(aes(x = reorder(location,n), y = n)) +
  geom_bar(stat="identity", fill = cornovo) + coord_flip() +
  theme_minimal() +
  labs(title = "Cidades mais citadas na descrição do perfil", subtitle = "#ForaKalil",
       y = "Número de menções", x = "Palavra")
png("palavras_location_forakalil.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

### treemap no Fora Kalil
png("treemap_forakalil_forakalil.png",width=3200,height=1800,res=300)
plotTreemap(fora_kalil$text)
dev.off()


#### Serie temporal
p1 <- fora_kalil %>% 
  mutate(data = ymd_hms(created_at), data = ceiling_date(data, unit = "hour")) %>% 
  count(data) %>% ggplot(aes(x = data, y = n)) + 
  geom_bar(stat="identity", fill = cornovo) + theme_minimal()  + 
  labs(title = "Número de tweets no tempo", subtitle = "#ForaKalil", 
       x = "Tempo (por hora)",  y = "Número de tweets")
png("serie_temporal_forakalil_forakalil.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

###### serie temporal de sentimentos
png("serie_temporal_facebook.png",width=3200,height=1800,res=300)
plotSerieTemporal(data_fb)
dev.off()

png("serie_temporal_instagram.png",width=3200,height=1800,res=300)
plotSerieTemporal(data_ig)
dev.off()

png("serie_temporal_twitter.png",width=3200,height=1800,res=300)
plotSerieTemporal(data_tw)
dev.off()


###### plotar treemap positivo

png("treemap_positivo_facebook.png",width=3200,height=1800,res=300)
plotTreemapPositivo(data.frame(text = data_fb$`Full Text`, polarization = data_fb$Sentiment, Domain = data_fb$Domain))
dev.off()

png("treemap_positivo_instagram.png",width=3200,height=1800,res=300)
plotTreemapPositivo(data.frame(text = data_ig$Snippet, polarization = data_ig$Sentiment, Domain = data_ig$Domain))
dev.off()

png("treemap_positivo_twitter.png",width=3200,height=1800,res=300)
plotTreemapPositivo(data.frame(text = data_tw$Snippet, polarization = data_tw$Sentiment, Domain = data_tw$Domain))
dev.off()

###### plotar treemap negativo

png("treemap_negativo_facebook.png",width=3200,height=1800,res=300)
plotTreemapNegativo(data.frame(text = data_fb$`Full Text`, polarization = data_fb$Sentiment, Domain = data_fb$Domain))
dev.off()

png("treemap_negativo_instagram.png",width=3200,height=1800,res=300)
plotTreemapNegativo(data.frame(text = data_ig$Snippet, polarization = data_ig$Sentiment, Domain = data_ig$Domain))
dev.off()

png("treemap_negativo_twitter.png",width=3200,height=1800,res=300)
plotTreemapNegativo(data.frame(text = data_tw$Snippet, polarization = data_tw$Sentiment, Domain = data_tw$Domain))
dev.off()

###### Indice de Favorabilidade

png("favorabilidade_facebook.png",width=3200,height=1800,res=300)
plotIndiceFavorabilidade(data_fb)
dev.off()

png("favorabilidade_instagram.png",width=3200,height=1800,res=300)
plotIndiceFavorabilidade(data_ig)
dev.off()

png("favorabilidade_twitter.png",width=3200,height=1800,res=300)
plotIndiceFavorabilidade(data_tw)
dev.off()


