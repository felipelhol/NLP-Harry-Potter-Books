install.packages("wordcloud")
install.packages("devtools")
install.packages("ggraph")
library(wordcloud)
library(devtools)
library(tidyverse)      
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)

# baixar os pacotes dos livros 1 a 7 do Harry Potter da JK Rowling
# os textos por capitulos são facilmente separados em tibble dataframe pelo tidyverse instalado
# os textos serão tokenizados como palavras únicas inicialmente e por colunas com o nome do livro e capítulo
# os titulos foram escritos exatamente iguais e criado uma lista no qual 
# cada livro é um array  e cada valor no array é um capítulo
devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")
books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)
# cada livro é um array  e cada valor no array é um capítulo

# em unnest_tokens foi tokenizado cada capítulo em palavras
# Em seguida foi criado uma factor para ordernar os livros por publicação
series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}

series$book <- factor(series$book, levels = rev(titles))
series

# então unimos book e chapter com o "_"
series_ <- series %>%
  unite(book, c("book", "chapter"), sep = "_", remove = TRUE)
series_

# usamos o regex padrão e eliminamos os NAs
series_ %>% 
  mutate(word = str_extract(word, regex("[a-z']+"))) %>%
  filter(!is.na(word)) -> series_ #tokenizado por palavras

# Contamos o total de palavras por capítulo em cada livro
series_ %>%
  count(book) %>% rename(total = n) -> series2 #totais de palavra por livro
series2

series_ %>% count(book, word) -> series3 # n de cada palavra em cada livro

# realizamos um inner join de series3 com series2  --> series0
# series 0 irá apresentar livro por capítulo, palavras,
# quantidade n de palavras e o total por book
inner_join(series3, series2, by=c("book"="book")) -> series0
series0

# Utilizamos a biblioteca world cloud para representar as palvras mais usadas visualmente
# eliminamos as stop words e representamos graficamente
#CONTAR e mostrar as words pelo world cloud
series0 %>% count(word, sort = TRUE)

series0$book <- factor(series0$book, levels = rev(titles))
series0 %>% 
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# utilizando o bing para análise de sentimentos com as 100 mais frequentes e
# nas cores baseados pelo sentimento de positivo e negativo
# dessa vez os nomes próprios com Harry ou Hermione nao apareceram pois
# não classificados no bing como positivo ou negativo

series0 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkblue"),
                   max.words = 50)


#grafico da lei de Zipf
library(ggplot2)
series0 %>% mutate(tf = n/total) %>%
  group_by(book) %>% arrange(book, desc(tf)) %>%
  mutate(rank = row_number()) %>%
  ungroup() -> seriesfinal 
seriesfinal %>%
  ggplot(aes(x=rank, y=tf, color=book)) +
  geom_line(alpha=0.8, size=1.1, show.legend=FALSE) +
  scale_x_log10() +  scale_y_log10()
#melhorando o grafico
seriesfinal %>%
  lm(formula = log10(tf) ~ log10(rank)) -> modelo1
modelo1

seriesfinal %>% filter(rank>=10, rank<=1000) %>%
  lm(formula = log10(tf) ~ log10(rank)) -> modelo2

modelo2  

summary(modelo2)

seriesfinal %>%
  ggplot(aes(x=rank, y=tf, color=book)) +
  geom_line(alpha=0.8, size=1.1, show.legend=FALSE) +
  scale_x_log10() +  scale_y_log10() +
  geom_abline(intercept = -0.566,
              slope = -1.139, lty=2, color='gray50')

#Analise de sentimentos
install.packages("textdata")
library(textdata)

#dicionario sentiment lexicon --> diversos sentimentos como medo, surpresa, etc 
series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# pelo bing -> nomes nao aparecem aqui
series %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

series %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

# plotamos pelo maximo e minimo os valores de sentimento de cada livro 
# e percebemos a tendencia dos livros terem sentimentos negativos
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

# analisamos agora por bigramas de palavras
# pode ser que existam palavras que representem um significado contrario 
# Em seguida foi criado uma factor para ordernar os livros por publicação
series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}
series$book <- factor(series$book, levels = rev(titles))
series

series %>%
  count(bigram, sort = TRUE)

# remover stop words do bigrama
bigrams_separated <- series %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# nova contagem
# ao eliminar os stop words percebemos palvras como professor mcgonagall como as mais frequentes
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>% 
  count(bigram, sort = TRUE)

# realizar o tf-idf
# percebemos por exemplo professor umbridge como mais frequente no livro ordem da fenix em
# relação aos outros livros
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# a seguir é plotado de cardo com o tf idf
plot_potter<- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))
plot_potter %>% 
  top_n(20) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


#Criar a DTM onde as colunas são os documentos e a linhas sao os termos
# apareceu harry em todos os books

word_counts <- series0 %>% 
  anti_join(stop_words) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

top_n(word_counts, 10)

chapters_dtm <- word_counts %>%
  cast_dtm(book, word, n)

chapters_dtm


#LDA
# cada topico está associado ao numero de palavras com o beta
# indicando a quantidade de contribuição em cada tópico
install.packages("topicmodels")
library(topicmodels)

chapters_lda <- LDA(chapters_dtm, k = 7, control = list(seed = 123))

chapter_topics <- tidy(chapters_lda, matrix = "beta")

top_n(chapter_topics, 10)


top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#gamma
# como cada documento está associado com cada tópico
chapters_lda

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

top_n(chapters_gamma, 10)


chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

#classificacao não supervisionada
# o o titulo de consenso é determinado pelo mais frequente 
#em cada topico e pelos capitulos de cada titulo
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

# percebemos 155 capitulos foram diferente do consenso
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus) 
