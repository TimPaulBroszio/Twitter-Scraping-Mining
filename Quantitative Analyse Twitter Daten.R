# Autor: Tim Broszio 
# Ruhr-Universitaet Bochum
# Seminar: Social Movements .....
# Wintersemester 2020/2021
# Dozent*innen: Prof. Dr. S. Zajak, Herr C. Sorg

#Let's get started!----

getwd()
# das erste und wichtigste:
Sys.setenv(language="en")

# Alten Workspace bereinigen:
rm(list = ls())

# Notwendige Pakete laden----
pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("quanteda", "readtext", "tidyverse", "dplyr", "readr", "stringr", "ggraph","quanteda.textmodels",
        "rtweet", "ggthemes", "tm", "readr", "tibble", "wordcloud2", "twitteR", "widyr", "topicmodels",
        "ldatuning", "stm", "urltools","arules", "reshape2", "quanteda.textplots", "graphics",
                 "wordcloud", "wordcloud2", "tidytext", "RColorBrewer", "lubridate", "igraph")

####                                          ###
####                                          ###
####                                          ###

source("./Funktionen/funktionen.r", encoding="UTF-8")
######                          #BlackLivesMatter                                         ----
# Daten reinladen

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\BLMTweets.Rda")
BLM1 <- tweets.df.BLM

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\BLMTweets1.Rda")
BLM2 <- tweets.df.BLM1


load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\BLMTweets2.Rda")
BLM3 <- tweets.df.BLM2

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\BLMTweets3.Rda")
BLM4 <- tweets.df.BLM3

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2020-12-23 BLM.Rda")
BLM5 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2020-12-24 BLM.Rda")
BLM6 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2020-12-27 BLM.Rda")
BLM7 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2020-12-28 BLM.Rda")
BLM8 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2020-12-31 BLM.Rda")
BLM9 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-03 BLM.Rda")
BLM10 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-05 BLM.Rda")
BLM11 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-08 BLM.Rda")
BLM12 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-09 BLM.Rda")
BLM13 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-11 BLM.Rda")
BLM14 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-13 BLM.Rda")
BLM15 <- tweets.df.BLM4

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\Dataframes, raw data\\2021-01-15 BLM.Rda")
BLM16 <- tweets.df.BLM4

##
## merging der Dataframes

BLM.ALL <- rbind(BLM1, BLM2, BLM3, BLM4, BLM5,
                 BLM6, BLM7, BLM8, BLM9, BLM10,
                 BLM11, BLM12, BLM13, BLM14, BLM15, 
                 BLM16)

##
## Entfernung von Duplikaten

BLM.ALL <- BLM.ALL[!duplicated(BLM.ALL), ]

#

#

load("BLM.ALL.Rda")


BLM.ALL <- BLM.ALL %>% 
  arrange(desc(created))

## Remove Retweets from Collection

BLM.organic <- BLM.ALL %>%
  filter(isRetweet == FALSE)

save(BLM.organic, file = "BLM.organic.Rda")


BLM.retweet <- BLM.ALL %>%
  filter(isRetweet == TRUE)

save(BLM.retweet, file = "BLM.retweet.Rda")

#
##


save(BLM.ALL, file = "BLM.ALL.Rda")

####                            ANALYSE TEXTLICHER Daten----
####                                           Korpus Analyse BLM----


load("BLM.organic.Rda")

write.csv2(BLM.organic, "BLM.organic.csv")



BLM.organic <- read.csv2("BLM.organic.csv", encoding = "UTF-8")

BLM.organic$created <- as.Date(BLM.organic$created)


##

## Visualisierung Frequenz Tweets

###


color1 <- "#2596be"
color2 <- "#165a72"

FreqsPlot <- ts_plot(BLM.organic, by = "1 day") +
  ggplot2::geom_line(colour = color1, size = 0.75) +
  ggplot2::geom_point(colour = color2, size = 1.5) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets with #BLM",
    subtitle = "Twitter status (#BLM) counts aggregated using daily intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet,
         Retweets excluded"
  )

###

FreqsPlot

###



###

# Data Cleaning

###

BLM.organic$text <- BLM.organic$text %>%
  tolower()

##
# Entfernen von Sonderzeichen, emojis, etc. 
##

##

emojis <- emojis
as.data.frame(emojis)

BLM.organic$text <- gsub(emojis$code, emojis$description, BLM.organic$text) #Unicode-Block „Verschiedene piktografische Symbole“


##


BLM.organic$text <- gsub("http.*", "", BLM.organic$text)
BLM.organic$text <- gsub("https.*", "", BLM.organic$text)
BLM.organic$text <- gsub("&amp;", "&", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+1F200-\\U+1F2FF]", "", BLM.organic$text) #Unicode-Block „Umschlossene CJK-Zeichen, Zusatz“
BLM.organic$text <- gsub("[\\U+1F300-\\U+1F5FF]", "", BLM.organic$text) #Unicode-Block „Verschiedene piktografische Symbole“
BLM.organic$text <- gsub("[\\U+1F600-\\U+1F64F]", "", BLM.organic$text) #Smileys
BLM.organic$text <- gsub("[\\U+1F650-\\U+1F67F]", "", BLM.organic$text) #Ziersymbole
BLM.organic$text <- gsub("[\\U+1F900-\\U+1F9FF]", "", BLM.organic$text) #Symbole und Piktogramme, Zusatz
BLM.organic$text <- gsub("[\\U+1FA70-\\U+1FAFF]", "", BLM.organic$text) #Symbols and Pictographs Extended -A
BLM.organic$text <- gsub("[\\U+20000-\\U+2A6DF]", "", BLM.organic$text) #vereinheitlichte CJK Ideogramme, Erweiterung B
BLM.organic$text <- gsub("[\\U+2A700-\\U+2B73F]", "", BLM.organic$text) #vereinheitlichte CJK Ideogramme, Erweiterung C
BLM.organic$text <- gsub("[\\U+2B740-\\U+2B81F]", "", BLM.organic$text) #vereinheitlichte CJK Ideogramme, Erweiterung D
BLM.organic$text <- gsub("[\\U+2B820-\\U+2CEAF]", "", BLM.organic$text) #vereinheitlichte CJK Ideogramme, Erweiterung E
BLM.organic$text <- gsub("[\\U+2CEB0-\\U+2EBEF]", "", BLM.organic$text) #vereinheitlichte CJK Ideogramme, Erweiterung F
BLM.organic$text <- gsub("[\\U+2F800-\\U+2FA1F]", "", BLM.organic$text) #Unicode-Block „CJK-Kompatibilitätsideogramme, Ergänzung“
BLM.organic$text <- gsub("[\\U+30000-\\U+3134F]", "", BLM.organic$text) #Unicode-Block „CJK Unified Ideographs Extension G“
BLM.organic$text <- gsub("[\\U+4E00-\\U+9FFF]", "", BLM.organic$text) #Unicode-Block „CJK Unified Ideographs Extension G“
BLM.organic$text <- gsub("[\\U+3040-\\U+309F]", "", BLM.organic$text) #Unicode-Block „CJK Unified Ideographs Extension G“
BLM.organic$text <- gsub("[\\U+1F680-\\U+1F6FF]", "", BLM.organic$text) #Unicode-Block „CJK Unified Ideographs Extension G“
BLM.organic$text <- gsub("[\\U+1F300-\\U+1F5FF]", "", BLM.organic$text) #	Miscellaneous Symbols and Pictographs“
BLM.organic$text <- gsub("[\\U+3400-\\U+4DBF]", "", BLM.organic$text) #	Miscellaneous Symbols and Pictographs“


##

BLM.organic$text <- gsub("[\\U+2E80-\\U+2EFF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0F00-\\U+0FDA]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+1000-\\U+109F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0980-\\U+09FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0A00-\\U+0A7F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0F00-\\U+0FFF]", "", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+FE00-\\U+FE0F]", "", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+3040-\\U+309F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+30A0-\\U+30FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3130-\\U+318F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31A0-\\U+31BF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31C0-\\U+31EF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31F0-\\U+31FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3300-\\U+33FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3400-\\U+4DBF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+4E00-\\U+9FFF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+A000-\\U+A48F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+AC00-\\U+D7AF]", "", BLM.organic$text)


####                                          ###

#korpus organische tweets

####                                          ###

blmkorpus_organic <- corpus(BLM.organic, docid_field = "screenName", text_field = "text", unique_docnames = FALSE)
docvars(blmkorpus_organic, "Tweetnummer") <- sprintf("%02d", 1:ndoc(blmkorpus_organic)) # Variable Textnummer generieren

korpus.stats.org <- summary(blmkorpus_organic, n = 3000000) #Das Funktionsargument n = 1000000 wird hier nur deshalb verwendet, weil die Funktion summary ansonsten nur maximal 100 Texte zusammenfasst. In diesem Fall reicht das zwar aus, aber bei gr??eren Datens?tzen ist das eher unpraktisch
korpus.stats.org$Text <- reorder(korpus.stats.org$Text, 1:ndoc(blmkorpus_organic), order = T)


korpus.saetze.org <- corpus_reshape(blmkorpus_organic, to = "sentences")

##


### erste Visualisierung des Korpus, um Tokenisierung und Vorverarbeitungsschritte vorzubereiten!

##




##
####                                           TOKENISIERUNG DES KORPUS----

#


blmtokens <- tokens(blmkorpus_organic, remove_punct = T, 
                        remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_compound(pattern = phrase(c("black lives matter", "george floyd", "breonna taylor",
                                     "donald trump", "washington d.c.", "electoral college", "bosnian war", 
                                     "presidential election", "joe biden", "white house",
                                     "racial injustice", "#blacklivesmatter",
                                     "united states of america", "united states"))) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(stopwords("de")) %>%
  tokens_remove(pattern = c("just", "like", "now", "get", "let", "today", "eua", "#e",
                            "uf", "ha", "uffauff", "hey", "th", "gt", "är", 
                            "här", "av", "liv","city", "några", "tagits", "på", "ok",
                            "will", "can", "also", "got", "say", "got", "using",
                            "go", "de", "man", "new", "still", "year", 
                            "see", "continue", "last", "pe", "even", "say", "said", "says",
                            "really", "much", "en", "yet", "christmas", "et", "always", 
                            "dr", "lot", "put", "la", "yes", "ever", "oh", 
                            "read", "head", "som", "y'all", "since", "que", "dec", "sure", 
                            "uae", "uuuuuuuuuu", "ans", "bio", "un", "confli", "het", "two", 
                            "ou", "le", "je", "van", "mai-ka", "far", "dat", "ask", "come",
                            "rt", "gi", "im", "wow", "een", "les", "st", "lol", "det", "",
                            "sat", "week", "mot", "förde", "männisK", "else", "al", "es", 
                            "uu", "se", "sun", "ya", "ik", "jul", "etc", "ur", "da",
                            "udu", "als", "op", "te", "der", "lt", "el", "ma", "hi", 
                            "wh", "smh", "list", "plus", "das", "di", "er", "uuu", "bit", "rip"),
                valuetype = "regex") %>%
  tokens_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                            "u", "v", "w", "x", "y", "z", "é"),
                valuetype = "fixed")


####                                          ###

#
##
####        Erstellung von N-Grams (Bi- und Trigramme)                    -----


blmtokens.bi <- tokens_ngrams(blmtokens, n = 2)
blmtokens.tri <- tokens_ngrams(blmtokens, n = 3)


#           Erstellung einer Dokument-Feature-Matrix (DFM)                -----


meine.dfm.Blm <- dfm(blmtokens)
meine.dfm.Blm.bi <- dfm(blmtokens.bi)
meine.dfm.Blm.tri <- dfm(blmtokens.tri)

####                                          ###

###   
## Um die Darstellung durch Wordclouds zu verbessern, 
## können die DFMs bearbeitet werden:
## Darstellung von Wörtern/ Features, die in mehr als 10.000 Tweets vorkommen

meine.dfm.BLM1 <- meine.dfm.Blm %>% 
  dfm_trim(min_docfreq = 10000, verbose = FALSE)



####                   ERSTE VISUALISIERUNG DER WORTFREQUENZEN - Wordcloud ----

worthaeufigkeiten.BLM1 <- textstat_frequency(meine.dfm.Blm)

worthaeufigkeiten.BLM1 <- worthaeufigkeiten.BLM1%>%
  filter(frequency >= 400)

#

worthaeufigkeiten.BLM2 <- textstat_frequency(meine.dfm.Blm.bi)

worthaeufigkeiten.BLM2 <- worthaeufigkeiten.BLM2 %>%
  filter(frequency >= 100)

#

worthaeufigkeiten.BLM3 <- textstat_frequency(meine.dfm.Blm.tri)

worthaeufigkeiten.BLM3 <- worthaeufigkeiten.BLM3 %>%
  filter(frequency >= 50)

###                             Wordcloud Quanteda

set.seed(100)

Quanteda_Wordcloud <- textplot_wordcloud(meine.dfm.Blm, 
                                         min_size = 2,
                                         max_size = 4, 
                                         min_count = 100000, 
                                         max_words = 100, 
                                         rotation = 0.15,
                                         random_order = FALSE,
                                         ordered_color = TRUE)

Quanteda_Wordcloud
###                             Wordcloud2 Packet 


Wordcloud1 <- wordcloud2(worthaeufigkeiten.BLM1,
           color = "random-light",
           backgroundColor = "#414141",
           size = 4, minSize = 2,
           shuffle = FALSE, rotateRatio = 0.1,
           shape = "cloud",
           ellipticity = 0.375) 

Wordcloud1

#


Wordcloud2 <- wordcloud2(worthaeufigkeiten.BLM2,
                         color = "random-light",
                         backgroundColor = "#414141",
                         size = 1, minSize = 1,
                         shuffle = FALSE, rotateRatio = 0.1,
                         shape = "cloud",
                         ellipticity = 0.375) 

Wordcloud2

####                            Barplot
# Infos zu Farben: ColorBrewer::display.brewer.all()
# Infos zu Themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
#                  https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/

barplot.blm <- ggplot(worthaeufigkeiten.BLM.org1[1:20,], aes(x = frequency,
                                                              y = feature)) +
  geom_bar(stat = "identity", 
           fill = 'grey') +
  theme_minimal() +
  geom_text(aes(label=frequency),
            colour = "black", hjust = 1.25, size = 5.0)

barplot.blm

###                               Netzwerkdarstellung Hashtags
# spezifiziert auf die Top Hashtags und die häufigsten Wörter
# http://quanteda.io/articles/pkgdown/examples/twitter.html
  
hashtag.blm <- dfm_select(meine.dfm.Blm, pattern = ("#*"))

tophastag.blm <- names(topfeatures(hashtag.blm, 20))

meine.dfm.keywordsblm <- dfm_select(meine.dfm.Blm, pattern = tophastag.blm,
                                    selection = "keep")


## Berechnung einer FCM (Feature Cooccureance Matrix) 

meine.fcmblm <- fcm(meine.dfm.keywordsblm)

textplot_network(meine.fcmblm, edge_size = 2, min_freq = 1,  omit_isolated = TRUE,
                 edge_color = "#157eb5",
                 edge_alpha = 0.33,
                 vertex_color = "#eb9852",
                 vertex_size = 2.5,
                 vertex_labelcolor = "#293e5b",
                 vertex_labelfont = NULL,
                 vertex_labelsize = 2.5,
                 offset = NULL)


###                               Netzwerkdarstellung Nutzer/ Perspnen



user.blm <- dfm_select(meine.dfm.Blm, pattern = ("@*"))

topuser.blm <- names(topfeatures(user.blm, 20))

#

meine.dfm.keywordsblm <- dfm_select(meine.dfm.Blm, pattern = topuser.blm,
                                    selection = "keep")


## Berechnung einer FCM (Feature Cooccureance Matrix) 

meine.fcmblm <- fcm(meine.dfm.keywordsblm)


textplot_network(meine.fcmblm, edge_size = 2, min_freq = 2,  omit_isolated = TRUE,
                 edge_color = "#157eb5",
                 edge_alpha = 0.33,
                 vertex_color = "#eb9852",
                 vertex_size = 2.5,
                 vertex_labelcolor = "#293e5b",
                 vertex_labelfont = NULL,
                 vertex_labelsize = 2.5,
                 offset = NULL)




####                            ###

# BLM Kollokationen----


kollokationen.blm1 <- textstat_collocations(blmtokens.org, min_count = 100, size = 2)
arrange(kollokationen.blm1, desc(count))
arrange(kollokationen.blm1, desc(lambda))
view(kollokationen.blm1)


kollokationen.blm2 <- textstat_collocations(blmtokens.org, min_count = 50, size = 3)
arrange(kollokationen.blm2, desc(count))
arrange(kollokationen.blm2, desc(lambda))
view(kollokationen.blm2)


write_delim(kollokationen.blm1, file = "KollokateBLM,1.csv", delim = ";") # Datei ist Excel-kompatibel
write_delim(kollokationen.blm2, file = "KollokateBLM,2.csv", delim = ";") # Datei ist Excel-kompatibel





####                                          ###


# Konkordanzen der Topfeatures // Keywords in Context

Kwic.Hashtags <- kwic(blmkorpus_organic, 
                      pattern = c("#blm", "#tigray", "#antifa", 
                                  "#fbrs", "#mexico", "#covid", 
                                  "#trump", "#racism", "#acab", 
                                  "#lgbtq", "#capitol", "#capitoriots",
                                  "#georgefloyd", "#racism", "#tigray"), 
                    valuetype = "regex", window = 30 ,case_insensitive = FALSE)

#
                                        
write_delim(Kwic.Hashtags, file = "Kwic.Hashtags.csv", delim = ";") # Datei ist Excel-kompatibel

# BLM Word Similarities----

dfm.blm.seatze <- dfm(korpus.saetze.org, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("english"))

dfm.blm.seatze.tfidf <-dfm.blm.seatze %>%
  dfm_remove(stopwords("german")) %>%
  dfm_remove(pattern = c("just", "like", "now", "get", "let", "today", "eua", "#e", "tare", 
                               "uf", "ha", "uffauff", "hey", "th", "gt", "är", "jizarif", "range", "scream",
                               "här", "av", "liv","city", "några", "tagits", "på", "ok", "ir", 
                               "will", "can", "also", "got", "say", "got", "using", "rglu", "azzez",
                               "watch", "go", "de", "man", "new", "still", "year", "signsu", "rae",
                               "see", "continue", "last", "pe", "even", "say", "said", "says",
                               "really", "much", "en", "yet", "christmas", "et", "always", "jzarif",
                               "dr", "lot", "put", "la", "yes", "twitter", "ever", "oh", "in", "to",
                               "read", "head", "som", "y'all", "since", "que", "dec", "sure", "coribush",
                               "scream", "sign", "range", "glo", "pfp", "supply", "fanno", "billion", 
                               "uae", "uuuuuuuuuu", "ans", "bio", "un", "confli", "het", "two", 
                               "ou", "le", "je", "van", "mai-ka", "far", "dat", "ask", "come", "btw",
                               "rt", "gi", "im", "wow", "een", "les", "st", "lol", "det", "is",
                               "sat", "week", "mot", "förde", "männisK", "else", "al", "es", "for",
                               "uu", "se", "sun", "ya", "ik", "jul", "etc", "ur", "da", "pepper",
                               "udu", "als", "op", "te", "der", "lt", "el", "ma", "hi", "gfx", "and", 
                               "wh", "smh", "list", "plus", "das", "di", "er", "uuu", "bit", "rip"),
                   valuetype = "regex") %>%
  dfm_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                            "u", "v", "w", "x", "y", "z", "é"),
                valuetype = "fixed") %>%
  dfm_trim(min_termfreq = 0.5, termfreq_type = "quantile", min_docfreq = 0.5, docfreq_type = "quantile") %>%
  dfm_tfidf()





aehnlichkeit.blm <- textstat_simil(dfm.blm.seatze.tfidf, dfm.blm.seatze.tfidf[, c("war")], method = "cosine", margin = "features")
head(aehnlichkeit.blm[order(aehnlichkeit.blm[,1], decreasing = T),], 10)

aehnlichkeit.blm.df <- data.frame(Begriff = aehnlichkeit.blm@Dimnames[[1]], Kosinus = aehnlichkeit.blm@x, stringsAsFactors = F) %>% 
  filter(!Begriff %in% c("blm")) %>% 
  arrange(desc(Kosinus)) %>% 
  mutate(Rang = row_number()) %>% 
  filter(Rang <= 20)

ggplot(aehnlichkeit.blm.df, aes(reorder(Begriff, Rang), Kosinus)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("") + 
  ggtitle("Begriffe mit hoher Kosinusnähe zum Zielterm \'blm\' im BLM Korpus")


# shortcut conversion to tm package's DocumentTermMatrix format




####                                          ###

# BLM Sentiment Analysis/ spezialisierte Lexika----

# Moral Foundations Lexikon https://moralfoundations.org/
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WJXCT8
mft.lexikon <- dictionary(file = "lexika/moral_foundations_dictionary.dic", format = "LIWC")


meine.dfm.Blm.trim.weight <- meine.dfm.Blm.org %>%
  dfm_remove(stopwords("english")) %>%
  dfm_remove(stopwords("german")) %>%
  dfm_remove(pattern = c("just", "like", "now", "get", "let", "today", "eua", "#e", "tare", 
                         "uf", "ha", "uffauff", "hey", "th", "gt", "är", "jizarif", "range", "scream",
                         "här", "av", "liv","city", "några", "tagits", "på", "ok", "ir", 
                         "will", "can", "also", "got", "say", "got", "using", "rglu", "azzez",
                         "watch", "go", "de", "man", "new", "still", "year", "signsu", "rae",
                         "see", "continue", "last", "pe", "even", "say", "said", "says",
                         "really", "much", "en", "yet", "christmas", "et", "always", "jzarif",
                         "dr", "lot", "put", "la", "yes", "twitter", "ever", "oh", "in", "to",
                         "read", "head", "som", "y'all", "since", "que", "dec", "sure", "coribush",
                         "scream", "sign", "range", "glo", "pfp", "supply", "fanno", "billion", 
                         "uae", "uuuuuuuuuu", "ans", "bio", "un", "confli", "het", "two", 
                         "ou", "le", "je", "van", "mai-ka", "far", "dat", "ask", "come", "btw",
                         "rt", "gi", "im", "wow", "een", "les", "st", "lol", "det", "is",
                         "sat", "week", "mot", "förde", "männisK", "else", "al", "es", "for",
                         "uu", "se", "sun", "ya", "ik", "jul", "etc", "ur", "da", "pepper",
                         "udu", "als", "op", "te", "der", "lt", "el", "ma", "hi", "gfx", "and", 
                         "wh", "smh", "list", "plus", "das", "di", "er", "uuu", "bit", "rip"),
             valuetype = "regex") %>%
  dfm_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                         "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                         "u", "v", "w", "x", "y", "z", "é"),
             valuetype = "fixed") %>%
  dfm_trim(min_termfreq = 0.75, termfreq_type = "quantile", min_docfreq = 0.5, docfreq_type = "quantile") %>%
  dfm_tfidf()

meine.dfm.trim.weight <- meine.dfm.Blm.trim.weight %>%
  dfm_lookup(dictionary = mft.lexikon)


DF.BLMweight.mtf <- convert(meine.dfm.trim.weight, "data.frame")

DF.BLMweight.mtf <- melt(DF.BLMweight.mtf, id.vars=c("doc_id"))

DF.BLMweight.mtf <- DF.BLMweight.mtf %>%
  filter(value >= 1)


plot.BLM.mtf.weight <- DF.BLMweight.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #BlackLivesMatter Korpus",
       subtitle = "Moral Foundations Theory - Gewichtungsschema: TF IDF - Term/ Docfreq: 0.75/ 0.5",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.BLM.mtf.weight  






meine.dfm.BLM.mft <- dfm((blmtokens.org),
                           dictionary = mft.lexikon)


meine.dfm.BLM.mft <- meine.dfm.BLM.mft %>%
  filter(value >= 1)


DF.BLM.mtf <- convert(meine.dfm.BLM.mft, "data.frame")

DF.BLM.mtf <- melt(DF.BLM.mtf, id.vars=c("doc_id"))

DF.BLM.mtf <- DF.BLM.mtf %>%
  filter(value >= 1)

plot.BLM.mtf <- DF.BLM.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #BlackLivesMatter Korpus",
       subtitle = "Moral Foundations Theory ",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.BLM.mtf  
####                                          ###
####                                          ###
####                                          ###

# Simulating Pluralism
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AFTHMK
 
# “Language of Democracy in Hegemonic Authoritarianism”-Lexikon von Seraphine F. Maerz]
meine.dfm.un.maerz <- dfm(corpus_subset(korpus.un, year >= 1982), groups = "year", dictionary = maerz.lexikon)




####                                          ###
####                                          ###
####                                          ###

# Vergleich unterschiedlicher Lexika
# http://inhaltsanalyse-mit-r.de/sentiment.html
# Bing Liu Lexikon, , AFINN Dictionary, NRC Emotions Lexikon

# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
# http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html



sentiment.lexikon.bingliu <- dictionary(list(positive = scan("lexika/bingliu-positive-words.txt", what = "char", sep = "\n", skip = 35, quiet = T), negative = scan("lexika/bingliu-negative-words.txt", what = "char", sep = "\n", skip = 35, quiet = T)))
sentiment.lexikon.nrc <- dictionary(list(positive = scan("lexika/nrc.pos.txt", what = "char", sep = "\n", quiet = T), negative = scan("lexika/nrc.neg.txt", what = "char", sep = "\n", quiet = T)))
afinn <- read.csv("lexika/AFINN-111.txt", header = F, sep = "\t", stringsAsFactors = F)
sentiment.lexikon.afinn <- dictionary(list(positive = afinn$V1[afinn$V2>0], negative = afinn$V1[afinn$V2<0]))


meine.dfm.blm.bingliu <- dfm_weight(dfm(blmtokens.org,
                                          dictionary = sentiment.lexikon.bingliu), 
                                      scheme = "prop")

meine.dfm.blm.nrc <- dfm_weight(dfm(blmtokens.org, 
                                      dictionary = sentiment.lexikon.nrc),
                                  scheme = "prop")

meine.dfm.blm.afinn <- dfm_weight(dfm(blmtokens.org,
                                        dictionary = sentiment.lexikon.afinn), 
                                    scheme = "prop")


sentiment.blm.bingliu <- convert(meine.dfm.blm.bingliu, "data.frame") %>% mutate(Lexikon = "Bing Liu")



sentiment.blm.nrc <- convert(meine.dfm.blm.nrc, "data.frame") %>% mutate(Lexikon = "NRC")
sentiment.blm.afinn <- convert(meine.dfm.blm.afinn, "data.frame") %>% mutate(Lexikon = "AFINN")



sentiment.blm.kombi <- bind_rows(sentiment.blm.bingliu, sentiment.blm.nrc, sentiment.blm.afinn) %>% 
  gather(positive, negative, key = "Polarität", value = "Sentiment") %>% 
  filter(Polarität == "positive") 

#sentiment.blm.kombi <-spread(sentiment.blm.kombi, Lexikon, Sentiment)



plot.BLM.sentilexi <- sentiment.blm.kombi %>%
  ggplot(aes(x = doc_id, y = Sentiment, color = Lexikon, group = Lexikon)) +
  geom_point() +
  geom_line(size = 1.25, alpha = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Sentimentscores für den #BlackLivesMatter Korpus",
       subtitle = "AFINN, Bing Liu, NRC",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())

plot.BLM.sentilexi
# BLM Topicmodelling BLM----

#groben LDA-Themenmodells


anzahl.themen <- 2
dfm2topicmodelsblm <- convert(meine.dfm.Blm.org, to = "topicmodels")
lda.modellblm <- LDA(dfm2topicmodelsblm, anzahl.themen)
lda.modellblm


as.data.frame(terms(lda.modellblm, 2))

data.frame(Thema = topics(lda.modellblm))



ldatuning.metrikenblm <- FindTopicsNumber(dfm2topicmodelsblm, topics = seq(from = 2, to = 15, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
)

FindTopicsNumber_plot(ldatuning.metrikenblm)

lda.themen.aehnlichkeit.blm <- as.data.frame(lda.modellblm@beta) %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
par(mar = c(0, 4, 4, 2))
plot(lda.themen.aehnlichkeit.blm, main = "LDA-Themenähnlichkeit nach Features", xlab = "", sub = "")


# stm Topic Modelling----
# https://juliasilge.com/blog/sherlock-holmes-stm/

topic_model_blm <- stm(meine.dfm.Blm.org, K = 2, 
                        verbose = FALSE, init.type = "Spectral")

summary(topic_model_blm)
write_delim(topic_model_blm, file = "topicmodellblm.csv")
td_betablm <- tidy(topic_model_blm)

td_betablm %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")


td_gamma_blm <- tidy(topic_model_blm, matrix = "gamma",                    
                      document_names = rownames(meine.dfm.Blm.org))
scipen(999)
ggplot(td_gamma_blm, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))






# MAGA-----
#Datenframes vorverarbeiten

load("MAGATweets1.Rda")
Maga1 <- tweets.df.MAGA1

load("2020-12-23 MAGA.Rda")
Maga2 <- tweets.df.MAGA

load("2020-12-24 MAGA.Rda")
Maga3 <- tweets.df.MAGA

load("2020-12-27 MAGA.Rda")
Maga4 <- tweets.df.MAGA

load("2020-12-28 MAGA.Rda")
Maga5 <- tweets.df.MAGA

load("2020-12-31 MAGA.Rda")
Maga6 <- tweets.df.MAGA

load("2021-01-03 MAGA.Rda")
Maga7 <- tweets.df.MAGA

load("2021-01-05 MAGA.Rda")
Maga8 <- tweets.df.MAGA

load("2021-01-06 MAGA.Rda")
Maga9 <- tweets.df.MAGA

load("2021-01-09 MAGA.Rda")
Maga10 <- tweets.df.MAGA

load("2021-01-11 MAGA.Rda")
Maga11 <- tweets.df.MAGA

load("2021-01-13 MAGA.Rda")
Maga12 <- tweets.df.MAGA

load("2021-01-14 MAGA.Rda")
Maga13 <- tweets.df.MAGA

load("2021-01-15 MAGA.Rda")
Maga14 <- tweets.df.MAGA



# Dataframe aufbereiten (Spalten ggf entfernen, neu hinzufügen)----

Maga1 <- Maga1 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga2 <- Maga2 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga3 <- Maga3 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga4 <- Maga4 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga5 <- Maga5 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga6 <- Maga6 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga7 <- Maga7 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga8 <- Maga8 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga9 <- Maga9 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga10 <- Maga10 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga11 <- Maga11 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga12 <- Maga12 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga13 <- Maga13 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Maga14 <- Maga14 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 
 

# Ueberschneidungen rausfiltern!

Maga2 <- Maga2 %>%
   filter(id >= 1341041043202981891)

Maga3 <- Maga3 %>%
  filter(id >= 1341755252115197953)

Maga4 <- Maga4 %>%
  filter(id >= 1342101808307695616)

Maga5 <-  Maga5 %>%
  filter(id >= 1343235273195307008)

Maga6 <- Maga6 %>%
  filter(id >= 1343561927822372866)

Maga7 <- Maga7 %>%
  filter(id >= 1344726735535755264)

Maga8 <- Maga8 %>%
  filter(id >= 1345647917428662279)

Maga10 <- Maga10 %>%
  filter(id >= 1347072282355666944)

Maga11 <- Maga11 %>%
  filter(id >= 1347940973725306880)

Maga12 <- Maga12 %>%
  filter(id >= 1348710120386138112)

Maga13 <- Maga13 %>%
  filter(id >= 1349331325405290500)


# Die Dataframes haben nun die identische Anzahl an Spalten, 
# Daher sollen die DFs chronolgisch gestacked werden

MAGA.Sammlung <- rbind(Maga14, Maga13, Maga12, Maga11, Maga10, Maga9, Maga8, Maga7,Maga6, Maga5, Maga4, Maga3, Maga2, Maga1)

save(MAGA.Sammlung, file = "Maga Sammlung.Rda")

load("Maga Sammlung.Rda")


#ohne retweets

MAGA.organic <- MAGA.Sammlung %>%
  filter(isRetweet == FALSE)

MAGA.retweet <- MAGA.Sammlung %>%
  filter(isRetweet == TRUE)

save(MAGA.organic, file = "MagaOrganic.Rda")
write.csv2(MAGA.organic, "tweets.org.csv")
write.csv2(MAGA.retweet, "tweets.re.csv")


# https://unicode-table.com/en/
# https://www.compart.com/de/unicode/block/U+30A0

#Korpus Analyse Maga----

file.choose()
MAGA.organic <-  read.csv2("C://RUB//Master GTG//WS 20 21//IT 1 - Social movements, digital network and transnational solidarity//Twitter Mining/tweets.org.csv",
                        header = T, check.names = F)
MAGA.organic$text <- MAGA.organic$text %>%
  tolower()

MAGA.organic$text <- gsub("http.*", "", MAGA.organic$text)
MAGA.organic$text <- gsub("https.*", "", MAGA.organic$text)
MAGA.organic$text <- gsub("&amp;", "&", MAGA.organic$text)
MAGA.organic$text <- gsub("[\\U+2E80-\\U+2EFF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+0F00-\\U+0FDA]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+1000-\\U+109F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+0980-\\U+09FF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+0A00-\\U+0A7F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+0F00-\\U+0FFF]", "", MAGA.organic$text)
MAGA.organic$text <- gsub("[\\U+FE00-\\U+FE0F]", "", MAGA.organic$text)
MAGA.organic$text <- gsub("[\\U+1000-\\U+109F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+3040-\\U+309F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+30A0-\\U+30FF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+3130-\\U+318F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+31A0-\\U+31BF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+31C0-\\U+31EF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+31F0-\\U+31FF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+3300-\\U+33FF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+3400-\\U+4DBF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+4E00-\\U+9FFF]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+A000-\\U+A48F]", "", MAGA.organic$text) 
MAGA.organic$text <- gsub("[\\U+AC00-\\U+D7AF]", "", MAGA.organic$text)


MAGA.retweet <-  read.csv2("C://RUB//Master GTG//WS 20 21//IT 1 - Social movements, digital network and transnational solidarity//Twitter Mining//tweets.re.csv",
                           header = T, check.names = F)
MAGA.retweet$text <- MAGA.retweet$text %>%
  tolower()

MAGA.retweet$text <- gsub("http.*", "", MAGA.retweet$text)
MAGA.retweet$text <- gsub("https.*", "", MAGA.retweet$text)
MAGA.retweet$text <- gsub("&amp;", "&", MAGA.retweet$text)
MAGA.retweet$text <- gsub("[\\U+2E80-\\U+2EFF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+0F00-\\U+0FDA]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+1000-\\U+109F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+0980-\\U+09FF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+0A00-\\U+0A7F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+0F00-\\U+0FFF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+1000-\\U+109F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+3040-\\U+309F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+30A0-\\U+30FF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+3130-\\U+318F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+31A0-\\U+31BF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+31C0-\\U+31EF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+31F0-\\U+31FF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+3300-\\U+33FF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+3400-\\U+4DBF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+4E00-\\U+9FFF]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+A000-\\U+A48F]", "-", MAGA.retweet$text) 
MAGA.retweet$text <- gsub("[\\U+AC00-\\U+D7AF]", "-", MAGA.retweet$text)


# korpus organic tweets
korpus_MAGA_org <- corpus(MAGA.organic, docid_field = "screenName", text_field = "text", unique_docnames = FALSE)
docvars(korpus_MAGA_org, "Tweetnummer") <- sprintf("%02d", 1:ndoc(korpus_MAGA_org)) # Variable Textnummer generieren

korpus.stats.org <- summary(korpus_MAGA_org, n = 3000000) #Das Funktionsargument n = 1000000 wird hier nur deshalb verwendet, weil die Funktion summary ansonsten nur maximal 100 Texte zusammenfasst. In diesem Fall reicht das zwar aus, aber bei gr??eren Datens?tzen ist das eher unpraktisch
korpus.stats.org$Text <- reorder(korpus.stats.org$Text, 1:ndoc(korpus_MAGA_org), order = T)


korpus.saetze.org <- corpus_reshape(korpus_MAGA_org, to = "sentences")



# korpus retweets

korpus_MAGA_re <- corpus(MAGA.retweet, docid_field = "screenName", text_field = "text", unique_docnames = FALSE)
docvars(korpus_MAGA_re, "Tweetnummer") <- sprintf("%02d", 1:ndoc(korpus_MAGA_re)) # Variable Textnummer generieren

korpus.stats.re <- summary(korpus_MAGA_re, n = 3000000) #Das Funktionsargument n = 1000000 wird hier nur deshalb verwendet, weil die Funktion summary ansonsten nur maximal 100 Texte zusammenfasst. In diesem Fall reicht das zwar aus, aber bei gr??eren Datens?tzen ist das eher unpraktisch
korpus.stats.re$Text <- reorder(korpus.stats.re$Text, 1:ndoc(korpus_MAGA_re), order = T)


korpus.saetze.re <- corpus_reshape(korpus_MAGA_re, to = "sentences")


# Suche nach Unicodes 
# https://unicode-table.com/en/#109F
# https://www.compart.com/de/unicode/block

# MAGA Tokens----

tokens.org <- tokens(korpus_MAGA_org, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = c("uu", "uf", "uuf", "will", "lo", "que", "las", "los", 
                            "el", "la", "le", "just", "now", "like", "get", "can",
                            "#eua", "hey", "u", "ha", "hey", "#eua",  "yes", "go",
                            "going", "think", "see", "still", "years", "say", 
                            "said", "saying", "find", "person", "told", "y'all", 
                            "sir", "yeah", "ok", "rt", "red", "line", "soon", 
                            "talking", "hold", "old", "st", "ask", "yo", "ago",
                            "morning", "instead", "gt", "second", "se", "others", 
                            "ya", "re", "set", "al", "co", "almost", "es", "via", 
                            "anymore", "bs", "fo", "hi", "etc", "sea", "nd", "im",
                            "asked", "former", "ap", "yep", "fb", "hop", "ts", "por",
                            "#a", "em", "tr", "da", "simply", "ex", "ma", "lin", "et",
                            "dandac", "add", "mo", "sh", "ag", "ali", "till", "ar", 
                            "rd", "pr", "dr", "#r", "si", "djt", "pic", "thi", 
                            "ron", "di", "pm", "mi", "ne", "twitter",
                            "man", "another", "yet", "also", "says", "since", 
                            "de", "mr", "far", "ever", "one", "many", "even", 
                            "really", "day", "oh", "may", "already", "check",
                            "hear", "making", "en", "need", "name", "imagine", "giving"),
                valuetype = "regex") %>%
  tokens_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                            "u", "v", "w", "x", "y", "z", "é"),
                valuetype = "fixed")


tokens.org.bi <- tokens_ngrams(tokens.org, n = 2)
tokens.org.tri <- tokens_ngrams(tokens.org, n = 3)

tokens.re <- tokens(korpus_MAGA_re, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern =  c("uu", "uf", "uuf", "will"),
                valuetype = "regex")# %>%
  tokens_wordstem()

tokens.re.bi <- tokens_ngrams(tokens.re, n = 2)
tokens.re.tri <- tokens_ngrams(tokens.re, n = 3)

# MAGA DFM via tokens----

meine.dfm.Maga.org <- dfm(tokens.org)
meine.dfm.Maga.org.bi <- dfm(tokens.org.bi)
meine.dfm.Maga.org.tri <- dfm(tokens.org.tri)

meine.dfm.Maga.re <- dfm(tokens.re)
meine.dfm.Maga.re.bi <- dfm(tokens.re.bi)
meine.dfm.Maga.re.tri <- dfm(tokens.re.tri)


# MAGA Visualisierung Tokens - Wordcloud----

worthaeufigkeiten.Maga.org1 <- textstat_frequency(meine.dfm.Maga.org)
worthaeufigkeiten.Maga.org2 <- textstat_frequency(meine.dfm.Maga.org.bi)
worthaeufigkeiten.Maga.org3 <- textstat_frequency(meine.dfm.Maga.org.tri)

worthaeufigkeiten.Maga.re1 <- textstat_frequency(meine.dfm.Maga.re)
worthaeufigkeiten.Maga.re2 <- textstat_frequency(meine.dfm.Maga.re.bi)
worthaeufigkeiten.Maga.re3 <- textstat_frequency(meine.dfm.Maga.re.tri)

worthaeufigkeiten.Maga.org1 <- worthaeufigkeiten.Maga.org1 %>%
  filter(frequency >= 400)

worthaeufigkeiten.Maga.org2 <- worthaeufigkeiten.Maga.org2 %>%
  filter(frequency >= 100)

worthaeufigkeiten.Maga.org3 <- worthaeufigkeiten.Maga.org3 %>%
  filter(frequency >= 50)

wordcloud2(worthaeufigkeiten.Maga.org3,
           color = "random-light",
           backgroundColor = "#414141",
           size = 1, minSize = 0.01,
           shuffle = FALSE, rotateRatio = 0.1,
           shape = "cloud",
           ellipticity = 0.375) 

# Visualisierung via quanteda package, max words 200, min count 100, 
textplot_wordcloud(meine.dfm.Maga.org.bi, min_size = 1, max_size = 5, 
                   min_count = 200, max_words = 200, color = "steelblue1", 
                   rotation = 0.15, random_order = FALSE, ordered_color = TRUE)
# MAGA Kollokationen----

kollokationen.maga1 <- textstat_collocations(tokens.org, min_count = 100, size = 2)
arrange(kollokationen.maga1, desc(count))
arrange(kollokationen.maga1, desc(lambda))
view(kollokationen.maga1)


kollokationen.maga2 <- textstat_collocations(tokens.org.bi, min_count = 20, size = 2)
arrange(kollokationen.maga2, desc(count))
arrange(kollokationen.maga2, desc(lambda))
view(kollokationen.maga2)


kollokationen.maga3 <- textstat_collocations(tokens.org.tri, min_count = 10, size = 2)
arrange(kollokationen.maga3, desc(count))
arrange(kollokationen.maga3, desc(lambda))
view(kollokationen.maga3)

write_delim(kollokationen.maga1, file = "KollokateMAGA,1.csv", delim = ";") # Datei ist Excel-kompatibel
write_delim(kollokationen.maga2, file = "KollokateMAGA,2.csv", delim = ";") # Datei ist Excel-kompatibel
write_delim(kollokationen.maga3, file = "KollokateMAGA,3.csv", delim = ";") # Datei ist Excel-kompatibel





# MAGA Netzwerkdarstellung der Topfeatures/ Keywords-----

# http://quanteda.io/articles/pkgdown/examples/twitter.html

hashtag.maga <- dfm_select(meine.dfm.Maga.org, pattern = ("#*"))
tophastag.maga <- names(topfeatures(hashtag.maga, 50))

meine.dfm.keywordsmaga <- dfm_select(meine.dfm.Maga.org, pattern = tophastag.maga, selection = "keep")


## Berechnung einer FCM (Feature Cooccureance Matrix) 

meine.fcmmaga <- fcm(meine.dfm.keywordsmaga)

textplot_network(meine.fcmmaga, edge_size = 1, min_freq = 1,  omit_isolated = TRUE,
                 edge_color = "#b9b3d6",
                 edge_alpha = 0.5,
                 vertex_color = "#dec486",
                 vertex_size = 3,
                 vertex_labelcolor = "#293e5b",
                 vertex_labelfont = NULL,
                 vertex_labelsize = 2.5,
                 offset = NULL)


# MAGA Sentiment Analysis/ spezialisierte Lexika----

# Moral Foundations Lexikon https://moralfoundations.org/
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WJXCT8


mft.lexikon <- dictionary(file = "lexika/moral_foundations_dictionary.dic", format = "LIWC")


meine.dfm.Maga.trim.weight <- meine.dfm.Maga.org %>%
  dfm_remove(stopwords("english")) %>%
  dfm_remove(stopwords("german")) %>%
  dfm_remove(pattern = c("just", "like", "now", "get", "let", "today", "eua", "#e", "tare", 
                         "uf", "ha", "uffauff", "hey", "th", "gt", "är", "jizarif", "range", "scream",
                         "här", "av", "liv","city", "några", "tagits", "på", "ok", "ir", 
                         "will", "can", "also", "got", "say", "got", "using", "rglu", "azzez",
                         "watch", "go", "de", "man", "new", "still", "year", "signsu", "rae",
                         "see", "continue", "last", "pe", "even", "say", "said", "says",
                         "really", "much", "en", "yet", "christmas", "et", "always", "jzarif",
                         "dr", "lot", "put", "la", "yes", "twitter", "ever", "oh", "in", "to",
                         "read", "head", "som", "y'all", "since", "que", "dec", "sure", "coribush",
                         "scream", "sign", "range", "glo", "pfp", "supply", "fanno", "billion", 
                         "uae", "uuuuuuuuuu", "ans", "bio", "un", "confli", "het", "two", 
                         "ou", "le", "je", "van", "mai-ka", "far", "dat", "ask", "come", "btw",
                         "rt", "gi", "im", "wow", "een", "les", "st", "lol", "det", "is",
                         "sat", "week", "mot", "förde", "männisK", "else", "al", "es", "for",
                         "uu", "se", "sun", "ya", "ik", "jul", "etc", "ur", "da", "pepper",
                         "udu", "als", "op", "te", "der", "lt", "el", "ma", "hi", "gfx", "and", 
                         "wh", "smh", "list", "plus", "das", "di", "er", "uuu", "bit", "rip"),
             valuetype = "regex") %>%
  dfm_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                         "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                         "u", "v", "w", "x", "y", "z", "é"),
             valuetype = "fixed") %>%
  dfm_trim(min_termfreq = 0.75, termfreq_type = "quantile", min_docfreq = 0.5, docfreq_type = "quantile") %>%
  dfm_tfidf()

meine.dfm.trim.weight <- meine.dfm.Maga.trim.weight %>%
  dfm_lookup(dictionary = mft.lexikon)


DF.MAGAweight.mtf <- convert(meine.dfm.trim.weight, "data.frame")

DF.MAGAweight.mtf <- melt(DF.MAGAweight.mtf, id.vars=c("doc_id"))

DF.MAGAweight.mtf <- DF.MAGAweight.mtf %>%
  filter(value >= 1)


plot.MAGA.mtf.weight <- DF.MAGAweight.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #MAGA Korpus",
       subtitle = "Moral Foundations Theory - Gewichtungsschema: TF IDF - Term/ Docfreq: 0.75/ 0.5",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.MAGA.mtf.weight  



meine.dfm.maga.mft <- dfm((tokens.org),
                          dictionary = mft.lexikon)


DF.MAGA.mtf <- convert(meine.dfm.maga.mft, "data.frame")

DF.MAGA.mtf <- melt(DF.MAGA.mtf, id.vars=c("doc_id"))

DF.MAGA.mtf <- DF.MAGA.mtf %>%
  filter(value >= 1)

plot.maga.mtf <- DF.MAGA.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #MakeAmericaGreatAgain Korpus",
       subtitle = "Moral Foundations Theory ",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.maga.mtf  



# MAGA Topic Modelling ----

# groben LDA-Themenmodells


anzahl.themen <- 8
dfm2topicmodels1 <- convert(meine.dfm.Maga.org, to = "topicmodels")
lda.modell1 <- LDA(dfm2topicmodels1, anzahl.themen)
lda.modell1

as.data.frame(terms(lda.modell1, 8))

ldatuning.metriken1 <- FindTopicsNumber(dfm2topicmodels1, topics = seq(from = 2, to = 15, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
)

FindTopicsNumber_plot(ldatuning.metriken1)

lda.themen.aehnlichkeit1 <- as.data.frame(lda.modell1@beta) %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
dpar(mar = c(0, 4, 4, 2))
plot(lda.themen.aehnlichkeit1, main = "LDA-Themenähnlichkeit nach Features", xlab = "", sub = "")

# Topicmodeling anhand von Bigrams

dfm2topicmodels2 <- convert(meine.dfm.Maga.org.bi, to = "topicmodels")
lda.modell2 <- LDA(dfm2topicmodels2, anzahl.themen)
lda.modell2

as.data.frame(terms(lda.modell2, 2))


data.frame(Thema = topics(lda.modell2))


ldatuning.metriken2 <- FindTopicsNumber(dfm2topicmodels2, topics = seq(from = 2, to = 15, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
)

FindTopicsNumber_plot(ldatuning.metriken2)

lda.themen.aehnlichkeit2 <- as.data.frame(lda.modell2@beta) %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
dpar(mar = c(0, 4, 4, 2))
plot(lda.themen.aehnlichkeit2, main = "LDA-Themenähnlichkeit nach Features", xlab = "", sub = "")

# stm Topic Modelling
# https://juliasilge.com/blog/sherlock-holmes-stm/

topic_model_maga <- stm(meine.dfm.Maga.org, K = 5, 
                        verbose = FALSE, init.type = "Spectral")
                    
summary(topic_model_maga)

td_beta <- tidy(topic_model_maga)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")


td_gamma_maga <- tidy(topic_model_maga, matrix = "gamma",                    
                 document_names = rownames(meine.dfm.Maga.org))
scipen(999)
ggplot(td_gamma_maga, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))





# Stop the Steal----
# Datenframes vorverarbeiten

load("StealTweets1.Rda")
Steal1 <- tweets.df.Steal1

load("2020-12-23 Steal.Rda")
Steal2 <- tweets.df.Steal

load("2020-12-24 Steal.Rda")
Steal3 <- tweets.df.Steal

load("2020-12-27 Steal.Rda")
Steal4 <- tweets.df.Steal

load("2020-12-28 Steal.Rda")
Steal5 <- tweets.df.Steal

load("2020-12-30 Steal.Rda")
Steal6 <- tweets.df.Steal

load("2020-12-31 Steal.Rda")
Steal7 <- tweets.df.Steal

load("2021-01-02 Steal.Rda")
Steal8 <- tweets.df.Steal

load("2021-01-03 Steal.Rda")
Steal9 <- tweets.df.Steal

load("2021-01-05 Steal.Rda")
Steal10 <- tweets.df.Steal

load("2021-01-06 Steal.Rda")
Steal11 <- tweets.df.Steal

load("2021-01-09 Steal.Rda")
Steal12 <- tweets.df.Steal

load("2021-01-11 Steal.Rda")
Steal13 <- tweets.df.Steal

# Dataframe aufbereiten (Spalten ggf entfernen, neu hinzufügen)----

Steal1 <- Steal1 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Steal2 <- Steal2 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1341322659280216065)

Steal3 <- Steal3 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1341694734805446656)

Steal4 <- Steal4 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Steal5 <- Steal5 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1343116853695152128)

Steal6 <- Steal6  %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1343549460320825344)

Steal7 <- Steal7  %>%
  select(screenName, text, created, isRetweet, retweetCount, id) 

Steal8 <- Steal8 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1344573673827684353)

Steal9 <- Steal9 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1345331565992280065)

Steal10 <- Steal10 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1345748867988541441)

Steal11 <- Steal11 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1346364388765216770)

Steal12 <- Steal12 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1346945343653638147)

Steal13 <- Steal13 %>%
  select(screenName, text, created, isRetweet, retweetCount, id) %>%
  filter(id >= 1347889801324687362)

# Ueberschneidungen rausfiltern!

Steal.Sammlung <- rbind(Steal13, Steal12, Steal11, Steal10, Steal9, Steal8, Steal7, Steal6, Steal5, Steal4, Steal3, Steal2, Steal1)


Steal.organic <- Steal.Sammlung %>%
  filter(isRetweet == FALSE)

Steal.retweet <- Steal.Sammlung %>%
  filter(isRetweet == TRUE)

write.csv2(Steal.Sammlung, "Stealtweets.Sammlung.csv")
write.csv2(Steal.organic, "Stealtweets.org.csv")
write.csv2(Steal.retweet, "Stealtweets.re.csv")

save(Steal.organic, file = "StealOrganic.Rda")
# CSV neu reinladen
file.choose()
Steal.organic <-  read.csv2("D://R Scripte//Scripte//Social Movements//IT 1 - Social movements, digital network and transnational solidarity//Twitter Mining//Stealtweets.org.csv",
                           header = T, check.names = F)
Steal.organic$text <- Steal.organic$text %>%
  tolower()

Steal.organic$text <- gsub("http.*", "", Steal.organic$text)
Steal.organic$text <- gsub("https.*", "", Steal.organic$text)
Steal.organic$text <- gsub("&amp;", "&", Steal.organic$text)
Steal.organic$text <- gsub("[\\U+2E80-\\U+2EFF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+0F00-\\U+0FDA]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+1000-\\U+109F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+0980-\\U+09FF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+0A00-\\U+0A7F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+0F00-\\U+0FFF]", "", Steal.organic$text)
Steal.organic$text <- gsub("[\\U+FE00-\\U+FE0F]", "", Steal.organic$text)
Steal.organic$text <- gsub("[\\U+1000-\\U+109F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+3040-\\U+309F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+30A0-\\U+30FF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+3130-\\U+318F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+31A0-\\U+31BF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+31C0-\\U+31EF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+31F0-\\U+31FF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+3300-\\U+33FF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+3400-\\U+4DBF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+4E00-\\U+9FFF]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+A000-\\U+A48F]", "", Steal.organic$text) 
Steal.organic$text <- gsub("[\\U+AC00-\\U+D7AF]", "", Steal.organic$text)

# Korpus Analyse Steal----
# korpus organic tweets
korpus_Steal_org <- corpus(Steal.organic, docid_field = "screenName", text_field = "text", unique_docnames = FALSE)
docvars(korpus_Steal_org, "Tweetnummer") <- sprintf("%02d", 1:ndoc(korpus_Steal_org)) # Variable Textnummer generieren

Stealkorpus.stats.org <- summary(korpus_Steal_org, n = 3000000) #Das Funktionsargument n = 1000000 wird hier nur deshalb verwendet, weil die Funktion summary ansonsten nur maximal 100 Texte zusammenfasst. In diesem Fall reicht das zwar aus, aber bei gr??eren Datens?tzen ist das eher unpraktisch
Stealkorpus.stats.org$Text <- reorder(Stealkorpus.stats.org$Text, 1:ndoc(korpus_Steal_org), order = T)


Stealkorpus.saetze.org <- corpus_reshape(korpus_Steal_org, to = "sentences")


# Steal Tokens----

Stealtokens.org <- tokens(korpus_Steal_org, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(pattern = c("just", "like", "now", "uf", "gt", "cc", "will", "ele",
                             "many", "u", "de", "says", "one", "see", "la", "el", "elec",
                            "ali", "say", "even", "yes", "can", "need", "go", "yo", "hi", 
                            "fo", "lin", "#a", "ya", "ppl", "early", "nd", "lo", "por", "per",
                            "rid", "vo", "let", "mr", "rt", "oh", "wow", "en", 
                            "re", "twitter", "tweets", "wi", "li", "co", "mi", "mo", 
                            "retweet", "dbongino", "week", "total", "via", "bs", "rd", "asking", 
                            "le", "y'all", "vs", "al", "giving", "less", "today", "ever", "man",
                            "look", "given", "said", "give", "lol", "ha", "pr", "don"),
                valuetype = "regex") %>%
  tokens_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                            "u", "v", "w", "x", "y", "z", "é"),
                valuetype = "fixed") %>%
  tokens_compound(pattern = phrase(c("stop the steal", "fake news", "new world order", 
                                     "make america great again", "blue lives matter",
                                     "january 6th", "jan 6th", "washington d.c", "voter fraud", 
                                     "electoral college", "united states", "president trump")))

Stealtokens.org.bi <- tokens_ngrams(Stealtokens.org, n = 2)
Stealtokens.org.tri <- tokens_ngrams(Stealtokens.org, n = 3)



# Steal DFM via Tokens----

meine.dfm.Steal.org <- dfm(Stealtokens.org)
meine.dfm.Steal.org.bi <- dfm(Stealtokens.org.bi)
meine.dfm.Steal.org.tri <- dfm(Stealtokens.org.tri)



# Steal Visualisierung Tokens - Wordcloud----


worthaeufigkeiten.Steal.org1 <- textstat_frequency(meine.dfm.Steal.org)
worthaeufigkeiten.Steal.org2 <- textstat_frequency(meine.dfm.Steal.org.bi)
worthaeufigkeiten.Steal.org3 <- textstat_frequency(meine.dfm.Steal.org.tri)

# Worthaeufigkeiten reduzieren

worthaeufigkeiten.Steal.org1 <- worthaeufigkeiten.Steal.org1 %>%
  filter(frequency >= 200)

worthaeufigkeiten.Steal.org2 <- worthaeufigkeiten.Steal.org2 %>%
  filter(frequency >= 100)

worthaeufigkeiten.Steal.org3 <- worthaeufigkeiten.Steal.org3 %>%
  filter(frequency >= 50)


wordcloud2(worthaeufigkeiten.Steal.org1,
           color = "random-light",
           backgroundColor = "#414141",
           size = 1, minSize = 0.5,
           shuffle = FALSE, rotateRatio = 0.1,
           shape = "cloud",
           ellipticity = 0.375)

# Steal DIspersionsplot----
# Keyword in context (kwic) muss/ kann angepasst werden

# textplot_xray(kwic(korpus_Steal_org, "fraud", valuetype = "regex", case_insensitive = FALSE)) + 
#  ggtitle("Lexikalische Dispersion von \"fraud\" über den StopTheSteal Korpus")



# Steal Kollokationen----


kollokationen.steal1 <- textstat_collocations(Stealtokens.org, min_count = 100, size = 2)
arrange(kollokationen.steal1, desc(count))
arrange(kollokationen.steal1, desc(lambda))
view(kollokationen.steal1)


kollokationen.steal2 <- textstat_collocations(Stealtokens.org.bi, min_count = 20, size = 2)
arrange(kollokationen.steal2, desc(count))
arrange(kollokationen.steal2, desc(lambda))
view(kollokationen.steal2)


kollokationen.steal3 <- textstat_collocations(Stealtokens.org.tri, min_count = 10, size = 2)
arrange(kollokationen.steal3, desc(count))
arrange(kollokationen.steal3, desc(lambda))
view(kollokationen.steal3)

write_delim(kollokationen.steal1, file = "KollokateSteal1.csv", delim = ";") # Datei ist Excel-kompatibel
write_delim(kollokationen.steal2, file = "KollokateSteal2,2.csv", delim = ";") # Datei ist Excel-kompatibel
write_delim(kollokationen.steal3, file = "KollokateSteal3,3.csv", delim = ";") # Datei ist Excel-kompatibel

# Steal Netzwerkdarstellung der Topfeatures/ Keywords-----
# http://quanteda.io/articles/pkgdown/examples/twitter.html

hashtag.steal <- dfm_select(meine.dfm.Steal.org, pattern = ("#*"))
tophastag.steal <- names(topfeatures(hashtag.steal, 25))

meine.dfm.keywordssteal <- dfm_select(meine.dfm.Steal.org, pattern = tophastag.steal, selection = "keep")


## Berechnung einer FCM (Feature Cooccureance Matrix) 

meine.fcmsteal <- fcm(meine.dfm.keywordssteal)

textplot_network(meine.fcmsteal, edge_size = 1, min_freq = 1,  omit_isolated = TRUE,
                 edge_color = "#64a4c4",
                 edge_alpha = 0.5,
                 vertex_color = "#b19c8c",
                 vertex_size = 3,
                 vertex_labelcolor = "#293e5b",
                 vertex_labelfont = NULL,
                 vertex_labelsize = 2.5,
                 offset = NULL)


# Steal Sentiment Analysis/ spezialisierte Lexika----

# Moral Foundations Lexikon https://moralfoundations.org/
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WJXCT8


mft.lexikon <- dictionary(file = "lexika/moral_foundations_dictionary.dic", format = "LIWC")



meine.dfm.Steal.trim.weight <- meine.dfm.Steal.org %>%
  dfm_remove(stopwords("english")) %>%
  dfm_remove(stopwords("german")) %>%
  dfm_remove(pattern = c("just", "like", "now", "get", "let", "today", "eua", "#e", "tare", 
                         "uf", "ha", "uffauff", "hey", "th", "gt", "är", "jizarif", "range", "scream",
                         "här", "av", "liv","city", "några", "tagits", "på", "ok", "ir", 
                         "will", "can", "also", "got", "say", "got", "using", "rglu", "azzez",
                         "watch", "go", "de", "man", "new", "still", "year", "signsu", "rae",
                         "see", "continue", "last", "pe", "even", "say", "said", "says",
                         "really", "much", "en", "yet", "christmas", "et", "always", "jzarif",
                         "dr", "lot", "put", "la", "yes", "twitter", "ever", "oh", "in", "to",
                         "read", "head", "som", "y'all", "since", "que", "dec", "sure", "coribush",
                         "scream", "sign", "range", "glo", "pfp", "supply", "fanno", "billion", 
                         "uae", "uuuuuuuuuu", "ans", "bio", "un", "confli", "het", "two", 
                         "ou", "le", "je", "van", "mai-ka", "far", "dat", "ask", "come", "btw",
                         "rt", "gi", "im", "wow", "een", "les", "st", "lol", "det", "is",
                         "sat", "week", "mot", "förde", "männisK", "else", "al", "es", "for",
                         "uu", "se", "sun", "ya", "ik", "jul", "etc", "ur", "da", "pepper",
                         "udu", "als", "op", "te", "der", "lt", "el", "ma", "hi", "gfx", "and", 
                         "wh", "smh", "list", "plus", "das", "di", "er", "uuu", "bit", "rip"),
             valuetype = "regex") %>%
  dfm_remove(pattern = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                         "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", 
                         "u", "v", "w", "x", "y", "z", "é"),
             valuetype = "fixed") %>%
  dfm_trim(min_termfreq = 0.6, termfreq_type = "quantile", min_docfreq = 0.3, docfreq_type = "quantile") %>%
  dfm_tfidf()

meine.dfm.trim.weight <- meine.dfm.Steal.trim.weight %>%
  dfm_lookup(dictionary = mft.lexikon)


DF.stealweight.mtf <- convert(meine.dfm.trim.weight, "data.frame")

DF.stealweight.mtf <- melt(DF.stealweight.mtf, id.vars=c("doc_id"))

DF.stealweight.mtf <- DF.stealweight.mtf %>%
  filter(value >= 1)


plot.Steal.mtf.weight <- DF.stealweight.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #StopTheSteal Korpus",
       subtitle = "Moral Foundations Theory - Gewichtungsschema: TF IDF - Term/ Docfreq: 0.75/ 0.5",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.Steal.mtf.weight  







#

meine.dfm.steal.mft <- dfm((Stealtokens.org),
                           dictionary = mft.lexikon)


DF.Steal.mtf <- convert(meine.dfm.steal.mft, "data.frame")

DF.Steal.mtf <- melt(DF.Steal.mtf, id.vars=c("doc_id"))

DF.Steal.mtf <- DF.Steal.mtf %>%
  filter(value >= 1)

plot.steal.mtf <- DF.Steal.mtf %>%
  ggplot(aes(x = variable, y = value, color = variable, fill = variable)) +
  geom_col() +
  labs(title = "Themen für den #StopTheSteal Korpus",
       subtitle = "Moral Foundations Theory ",
       x = "",
       y = "kumulierte Einstufung der Tweets", 
       caption = "eigene Darstellung", 
       color = "variable") +
  theme(axis.ticks.x=element_blank())


plot.steal.mtf  







# Steal Topic Modelling-----

# groben LDA-Themenmodells


anzahl.themen <- 5
dfm2topicmodels1 <- convert(meine.dfm.Steal.org, to = "topicmodels")
lda.modell1 <- LDA(dfm2topicmodels1, anzahl.themen)
lda.modell1

as.data.frame(terms(lda.modell1, 2))

ldatuning.metriken1 <- FindTopicsNumber(dfm2topicmodels1, topics = seq(from = 2, to = 15, by = 1), metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), method = "Gibbs", control = list(seed = 77), mc.cores = 2L, verbose = TRUE
)

FindTopicsNumber_plot(ldatuning.metriken1)

lda.themen.aehnlichkeit1 <- as.data.frame(lda.modell1@beta) %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
dpar(mar = c(0, 4, 4, 2))
plot(lda.themen.aehnlichkeit1, main = "LDA-Themenähnlichkeit nach Features", xlab = "", sub = "")


# stm Topic Modelling
# https://juliasilge.com/blog/sherlock-holmes-stm/

topic_model_steal <- stm(meine.dfm.Steal.org, K = 4, 
                        verbose = FALSE, init.type = "Spectral")

summary(topic_model_steal)

td_beta <- tidy(topic_model_steal)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")


td_gamma_maga <- tidy(topic_model_steal, matrix = "gamma",                    
                      document_names = rownames(topic_model_steal))
scipen(999)
ggplot(td_gamma_maga, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))

# Bestimmung der idealen Themenanzahl 

mein.stm.idealK <- searchK(topic_model_steal$documents, topic_model_steal$vocab, K = seq(4, 20, by = 2), max.em.its = 75)
plot(mein.stm.idealK)




# Merging der Daten und simultane Betrachtung----
file.choose()

#

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\StealOrganic.Rda")
Steal.organic <- Steal.organic %>% 
  select(screenName, text, created, id)
#

load("D:\\R Scripte\\Scripte\\Social Movements\\IT 1 - Social movements, digital network and transnational solidarity\\Twitter Mining\\MagaOrganic.Rda")
MAGA.organic <- MAGA.organic %>% 
  select(screenName, text, created, id)
#

load("BLM.organic.Rda")
BLM.organic <- BLM.organic %>% 
  select(screenName, text, created, id)


#
##
#

blm <- c("BLM")

stopthesteal <- c("StopTheSteal")

maga <- c("MakeAmericaGreatAgain")

#
##
#

Steal.organic <- cbind(Steal.organic, Hashtag = stopthesteal)

MAGA.organic <- cbind(MAGA.organic, Hashtag = maga)

BLM.organic <- cbind(BLM.organic, Hashtag = blm)

#

AllTweetsDF <- rbind(Steal.organic, MAGA.organic, BLM.organic)

AllTweetsDF <- AllTweetsDF %>% 
  arrange(desc(created))

#


# remove duplicates from AllTweets Dataframe

AllTweetsDF <- AllTweetsDF[!duplicated(AllTweetsDF), ]

#


save(AllTweetsDF, file = "AllTweets.Rda")
load("AllTweets.Rda")

# Frequency Plot

color1 <- "#2596be"
color2 <- "#165a72"

#


FreqsPlot <- AllTweetsDF %>% 
  ts_plot(AllTweetsDF, by = "1 days") +
  ggplot2::geom_line(colour = color1, size = 0.75) +
  ggplot2::geom_point(colour = color2, size = 1.5) +
  ggthemes::theme_economist() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets",
    subtitle = "Twitter status (#BLM, #MAGA, #StoptheSteal) counts aggregated using daily intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet,
         Retweets excluded"
  )
 
FreqsPlot

#

AllTweetsDF$text <- AllTweetsDF$text %>%
  tolower()


BLM.organic$text <- gsub("http.*", "", BLM.organic$text)
BLM.organic$text <- gsub("https.*", "", BLM.organic$text)
BLM.organic$text <- gsub("&amp;", "&", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+2E80-\\U+2EFF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0F00-\\U+0FDA]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+1000-\\U+109F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0980-\\U+09FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0A00-\\U+0A7F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+0F00-\\U+0FFF]", "", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+FE00-\\U+FE0F]", "", BLM.organic$text)
BLM.organic$text <- gsub("[\\U+3040-\\U+309F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+30A0-\\U+30FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3130-\\U+318F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31A0-\\U+31BF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31C0-\\U+31EF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+31F0-\\U+31FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3300-\\U+33FF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+3400-\\U+4DBF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+4E00-\\U+9FFF]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+A000-\\U+A48F]", "", BLM.organic$text) 
BLM.organic$text <- gsub("[\\U+AC00-\\U+D7AF]", "", BLM.organic$text)
####                                          ###

#korpus organische tweets
####                                          ###

AllTweetsKorpus <- corpus(AllTweetsDF, docid_field = "screenName", text_field = "text", unique_docnames = FALSE)
docvars(AllTweetsKorpus, "Tweetnummer") <- sprintf("%02d", 1:ndoc(AllTweetsKorpus)) # Variable Textnummer generieren

korpus.stats.org <- summary(AllTweetsKorpus, n = 3000000) #Das Funktionsargument n = 1000000 wird hier nur deshalb verwendet, weil die Funktion summary ansonsten nur maximal 100 Texte zusammenfasst. In diesem Fall reicht das zwar aus, aber bei gr??eren Datens?tzen ist das eher unpraktisch
korpus.stats.org$Text <- reorder(korpus.stats.org$Text, 1:ndoc(AllTweetsKorpus), order = T)


korpus.saetze.org <- corpus_reshape(AllTweetsKorpus, to = "sentences")

