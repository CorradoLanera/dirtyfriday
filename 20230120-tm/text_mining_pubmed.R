library(DT)
library(tidyverse)
library(SnowballC)
library(tm)
library(wordcloud)

pubmed_abstract_raw <- read.csv(
  'NCBI-EarAche-PubMed.csv',
  header = FALSE,
  col.names = c("abstract", "source")
)[["abstract"]]

pubmed_abstract <- pubmed_abstract_raw[pubmed_abstract_raw != ""]

corpus <- Corpus(VectorSource(pubmed_abstract))

corpus_preprocessed <- corpus |>
  tm_map(removePunctuation) |>
  tm_map(removeNumbers) |>
  tm_map(tolower) |>
  tm_map(removeWords, stopwords("english")) |>
  tm_map(stripWhitespace) |>
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(corpus_preprocessed)
freq <- colSums(as.matrix(dtm))
freq_df <- as_tibble(freq, rownames = "term") |>
  rename(frequency = value) |>
  arrange(desc(frequency))

datatable(freq_df, filter = list(position = 'top'))

# per SS controllare la funzione
findAssocs(dtm, "patient", corlimit = 0.5)
findAssocs(dtm, "ear", corlimit = 0.5)
findAssocs(dtm, "pain", corlimit = 0.5)

freq_df |>
  filter(frequency > 4) |>
  ggplot(aes(term, frequency)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

wordcloud(
  freq_df[["term"]],
  freq_df[["frequency"]],
  min.freq = 4,
  colors = brewer.pal(3, 'Dark2')
)
