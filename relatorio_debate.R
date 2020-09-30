twcandidatos <- c("aureacarolinax","BrunoEnglerDM","joaovitorxavier", "nilmariomiranda",  "barreto_luisa", "PaivaNOVO")
nmcandidatos <- c("Aurea Carolina", "Bruno Engler","João Vitor Xavier", "Nilmário Miranda", "Luisa Barreto", "Rodrigo Paiva")
ptcandidatos <- c("PSOL","PRTB","Cidadania","PT","PSDB","NOVO")

library(rtweet)

df <- data.frame()
for(c in twcandidatos){
  tw <- rtweet::get_friends(c, n = 5000)
  tw$candidato <- rep(c,nrow(tw))
  df <- rbind(df, tw)
}

library(tidyverse)
famous_tweeters <- lookup_users(twcandidatos)
retweets <- famous_tweeters %>% ggplot(aes( x = reorder(screen_name, retweet_count), y = retweet_count )) +
  geom_bar(stat="identity", fill = "blue3") + coord_flip()+
  theme_minimal() +
  labs(title = "Número de retweets mais recente",
       subtitle = "Candidatos mais importantes",
       x = "Candidatos",
       y = "# de retweets")

favoritos <- famous_tweeters %>% ggplot(aes( x = reorder(screen_name, favorite_count), y = favorite_count )) +
  geom_bar(stat="identity", fill = "blue3") + coord_flip()+
  theme_minimal() +
  labs(title = "Número de favoritos mais recente",
       subtitle = "Candidatos mais importantes",
       x = "Candidatos",
       y = "# de favoritos")
  
png("favoritos.png", width=3200,height=1800,res=300)
print(favoritos)
dev.off()

png("retweets.png", width=3200,height=1800,res=300)
print(retweets)
dev.off()



###

for(c in twcandidatos){
df_favorite<- rtweet::get_favorites(c, n=3000)


users_favorite<-
  df_favorite %>% 
  group_by(screen_name, user_id) %>%
  summarise(
    quant_fav= n()
  )

p1 <- users_favorite %>%
  ungroup() %>%
  top_n(10,quant_fav) %>%
  mutate(screen_name = reorder(screen_name,quant_fav)) %>%
  ggplot() + 
  geom_col(aes(x= screen_name, y= quant_fav)) +
  theme_light() +
  coord_flip() +
  labs(
    title = "Gráfico de usuários mais favoritados no twitter",
    subtitle=paste("Por",c),
    y = "Número de posts favoritados",
    x= "Screen name da galera"  )

png(paste("favoritados_por_",c,".png"),width=3200,height=1800,res=300)
print(p1)
dev.off()
}
