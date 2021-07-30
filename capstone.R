library(readxl)
library(tidyverse)
library(quanteda)
library(sjPlot)

readership <- data.frame(paper = c("Telegraph", "Express", "Mail", "Sun", "Times", "FT", "Star", "Mirror", "Independent", "Guardian"),
                         con_reader = c(79, 77, 74, 59, 58, 40, 38, 19, 15, 8))

endorsement <- data.frame(paper = c("Express", "Mail", "Mirror", "Telegraph", "FT", "Guardian", "Independent", "Star", "Sun", "Times"),
                          endorsement = c("Con", "Con", "non-Con", "Con", "non-Con", "non-Con", "non-Con", "non-Con", "Con", "Con"))

tabloid <- data.frame(paper = c("Express", "Mail", "Mirror", "Telegraph", "FT", "Guardian", "Independent", "Star", "Sun", "Times"),
                      tabloid = c("y", "y", "y", "n", "n", "n", "n", "y", "y", "n"))

covid_cases <- read.csv("./Downloads/owid-covid-data.csv")

uk_covid_cases <- covid_cases %>% filter(iso_code == "GBR") %>%
  select(date, new_cases)

excel_file_names <- list.files("./Downloads/", pattern = "XLSX", full.names = T)

news <- excel_file_names %>% map(read_excel) %>% bind_rows

sections <- news$Section %>% str_split(";") %>% map_chr(`[`, c(1))
sections <- sections[!is.na(sections)]

sections <- table(sections) %>% data.frame %>% arrange(desc(Freq))

oped <- news[str_detect(news$Section, "VOICES|COMMENT|OPINION|LETTER|EDITORIAL"), ]

oped <- oped[!is.na(oped$Section), ]

oped_publications <- oped$Publication...4 %>% table %>% data.frame %>% arrange(desc(Freq))

news_corpus <- corpus(news, text_field = "Hlead")

news_tokens <- tokens(news_corpus)

kwic(news_tokens, pattern = c("johnson", "downing street", "no10"),
     valuetype = "glob", window = 20) %>% head(100) %>% View

table(news$Publication...4[is.na(news$Section)]) %>% data.frame %>% arrange(desc(Freq)) %>% View

news_dfm <- dfm(news_tokens, remove_punct = TRUE, remove = stopwords("english"))

duplicates <- c()

for(i in 30317:length(news$Title)) {
  if (sum(news$Title[-i] == news$Title[i]) > 0 & !i %in% duplicates) {
    idx <- which(news$Title == news$Title[i])
    idx <- idx[idx != i]
    similarity <- data.frame(textstat_simil(news_dfm[idx, ], news_dfm[i, ], margin = "documents"))
    duplicates <- c(duplicates, as.numeric(str_remove(as.character(similarity$document1[similarity$correlation > 0.95]), "text")))
  }
  print(i)
}


news_unique <- news[!seq_along(news$Title) %in% duplicates, ]

news_without_oped <- news_unique[str_detect(news_unique$Section, "VOICES|COMMENT|OPINION|LETTER|EDITORIAL",
                                            negate = T) | is.na(news_unique$Section), ]

news_without_oped <- news_without_oped %>% 
  select(title = Title, date = `Published date`, publication = Publication...4, article = Hlead)

publication <- data.frame(paper = c("Mirror", "Telegraph", "Independent", "Sun", "Express", "Mail", "Guardian", "Telegraph", "Express", "FT", "Star", "Mirror", "Guardian", "Times", "Times", "Sun", "Telegraph", "Times", "Mail", "FT", "Star"), 
                          publication = c("mirror.co.uk", "telegraph.co.uk", "The Independent (United Kingdom)", "thesun.co.uk", "Express Online", "MailOnline", "The Guardian (London)", "The Daily Telegraph (London)", "The Express", "Financial Times Online", "Daily Star Online", "The Mirror (The Daily Mirror and The Sunday Mirror)", "The Observer (London)", "The Times (London)", "thetimes.co.uk", "The Sun (England)", "The Sunday Telegraph (London)", "The Sunday Times (London)", "The Daily Mail and Mail on Sunday (London)", "Financial Times (London, England)", "Daily Star"))

news_cleaned <- inner_join(news_without_oped, publication) %>% select(-publication) %>%
  select(paper, date, title, article)

news_cleaned$date <- lubridate::mdy(news_cleaned$date) %>% as.character
news_cleaned <- news_cleaned %>% inner_join(uk_covid_cases)

news_title <- news_cleaned %>% select(paper, date, title, new_cases)
news_lead <- news_cleaned %>% mutate(lead = substr(article, 1, 500)) %>%
  select(paper, date, lead, new_cases)
news_article <- news_cleaned %>% select(paper, date, article, new_cases)

mydict <- data_dictionary_LSD2015[1:2]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "positiv")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "confirm")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "care$")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "like")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "expert")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "sage")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "essential")]
mydict$positive <- mydict$positive[!str_detect(mydict$positive, "guardian")]

mydict$negative <- mydict$negative[!str_detect(mydict$negative, "too$")]
mydict$negative <- mydict$negative[!str_detect(mydict$negative, "^ire")]
mydict$negative <- mydict$negative[!str_detect(mydict$negative, "^strain")]
mydict$negative <- mydict$negative[!str_detect(mydict$negative, "opposition")]
mydict$negative <- mydict$negative[!str_detect(mydict$negative, "cancer")]

title_sent <- news_title %>% corpus(text_field = "title") %>% dfm(dictionary = mydict)
lead_sent <- news_lead %>% corpus(text_field = "lead") %>% dfm(dictionary = mydict)
article_sent <- news_article %>% corpus(text_field = "article") %>% dfm(dictionary = mydict)

news_title$score <- as.numeric(title_sent[,2]) - as.numeric(title_sent[,1])
news_lead$score <- as.numeric(lead_sent[,2]) - as.numeric(lead_sent[,1])
news_article$score <- as.numeric(article_sent[,2]) - as.numeric(article_sent[,1])

positive_tokens <- news_article %>% corpus(text_field = "article") %>%
  tokens %>% tokens_keep(pattern = mydict$positive) %>% dfm

negative_tokens <- news_article %>% corpus(text_field = "article") %>%
  tokens %>% tokens_keep(pattern = mydict$negative) %>% dfm

View(textstat_frequency(positive_tokens))
View(textstat_frequency(negative_tokens))

article_tokens <- news_article %>% corpus(text_field = "article") %>% tokens

kwic(article_tokens, pattern = c("against"),
     valuetype = "glob", window = 20) %>% head(100) %>% View

news_title %>% group_by(paper) %>% summarise(sent = mean(score)) %>% arrange(desc(sent)) %>% View
news_lead %>% group_by(paper) %>% summarise(sent = mean(score)) %>% arrange(desc(sent)) %>% View
news_article %>% group_by(paper) %>% summarise(sent = mean(score)) %>% arrange(desc(sent)) %>% View

title_null <- lm(score ~ new_cases, news_title)

title_paper <- lm(score ~ paper + new_cases, news_title)
summary(title_paper)

title_f <- anova(title_null, title_paper, test = "F")

lead_null <- lm(score ~ new_cases, news_lead)

lead_paper <- lm(score ~ paper + new_cases, news_lead)
summary(lead_paper)

lead_f <- anova(lead_null, lead_paper, test = "F")

article_null <- lm(score ~ new_cases, news_article)

article_paper <- lm(score ~ paper + new_cases, news_article)
summary(article_paper)

article_f <- anova(article_null, article_paper, test = "F")

tab_model(title_f, lead_f, article_f)

title_reader <- lm(score ~ con_reader + new_cases, inner_join(news_title, readership))
summary(title_reader)

tab_model(title_reader)

title_endorse <- lm(score ~ endorsement + new_cases, inner_join(news_title, endorsement))
summary(title_endorse)

tab_model(title_endorse)

title_tabloid <- lm(score ~ tabloid + new_cases, inner_join(news_title, tabloid))
summary(title_tabloid)

tab_model(title_tabloid)

title_reader_tabloid <- lm(score ~ con_reader + tabloid + new_cases, inner_join(news_title, tabloid) %>% inner_join(readership))
summary(title_reader_tabloid)

title_endorse_tabloid <- lm(score ~ endorsement + tabloid + new_cases, inner_join(news_title, tabloid) %>% inner_join(endorsement))
summary(title_endorse_tabloid)



title_paper_chart <- broom::tidy(lead_paper, conf.int = TRUE)
title_paper_chart <- title_paper_chart[-c(1, nrow(title_paper_chart)), ] %>%
  mutate(term = str_remove_all(term, "paper"))
title_paper_chart <- title_paper_chart %>%
  mutate(term = factor(term, levels = title_paper_chart[order(title_paper_chart$estimate), ]$term))

ggplot(title_paper_chart, aes(term, estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 12))

modified_part <- data.frame(
  word = c("positiv", "confirm", "care", "like", "expert", "sage", "essential", "guardian", "too", "ire", "strain", "opposition", "cancer"),
  category = c("positive", "positive", "positive", "positive", "positive", "positive", "positive", "positive", "negative", "negative", "negative", "negative", "negative"),
  reason = c("mostly means test 'positive' or Covid 'positive'", 
             "mostly used in the context of 'confirmed' cases", 
             "mostly used in the context of 'care' workers", 
             "mostly used as preposition",
             "mostly used to refer to health 'experts'",
             "mostly used to refer to the scientific advisory group", 
             "mostly used to refer to 'essential' industry and workers", 
             "mostly used to refer to the 'Guardian'", 
             "mostly used as adverb", 
             "most results are 'Ireland' instead of the 'ire' as a separate word", 
             "mostly used to refer to Covid 'strain'", 
             "mostly used in the context of the 'opposition' party", 
             "mostly used to refer to cancer patients affected by Covid hospitalization"))

tab_df(modified_part)

publication_table <- inner_join(readership, endorsement) %>%
  inner_join(group_by(news_cleaned, paper) %>% summarise(`number of articles` = n())) %>%
  select(name = paper, `number of articles`, `Conservative readership` = con_reader,
         `past endorsement` = endorsement)

tab_df(publication_table, col.header = names(publication_table))



