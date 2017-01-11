library(tidyverse)
library(tidytext)
library(stringr)

load('www.nvic.org.Rdata')

## Remove empty documents
parsed_site = parsed_site %>% 
	filter(!is.na(title), !is.na(text))

## For development purposes, work with a much smaller set of documents
# parsed_site = parsed_site %>% sample_frac(size = .25)

## The simplest way to get a count of document lengths is to go through token_counts once first? 
tokens = parsed_site %>%
	unnest_tokens(token, text)
token_counts = tokens %>%
	group_by(path, title, token) %>%
	summarize(token_n = n()) %>%
	ungroup() #%>%
	# bind_tf_idf(term_col = token, document_col = path, n_col = token_n)
docs = token_counts %>%
	group_by(path, title) %>%
	summarize(length = sum(token_n)) %>%
	ungroup()
## Difference between docs and parsed_site due to un-OCRed PDFs
## ECDF of document length
ggplot(docs, aes(length)) + stat_ecdf() + geom_rug()

## Manually remove some problem documents
problem_docs = c('BLF-vs--Offit---Plaintiff-s-Opposition-to-Defendan.aspx')
docs = docs %>% filter(!str_detect(path, problem_docs))

token_counts = token_counts %>%
	filter(path %in% docs$path) %>%
	bind_tf_idf(term_col = token, document_col = path, n_col = token_n)

vocabulary = token_counts %>%
	group_by(token) %>%
	summarize(idf = first(idf), 
			  tf_idf = max(tf_idf))
ggplot(vocabulary, aes(idf, tf_idf)) + geom_point() + stat_smooth()
ggplot(vocabulary, aes(idf)) + stat_ecdf()
ggplot(vocabulary, aes(tf_idf)) + stat_ecdf()



dtm = token_counts %>%
	filter(tf_idf >= quantile(vocabulary$tf_idf, probs = .8)) %>%
	cast_dtm(path, token, token_n)
# dist = proxy::dist(as.matrix(tdm), method = 'cosine')
## Use this instead: http://stackoverflow.com/a/29755756/3187973
library(slam)
cosine_sim_dtm <- crossprod_simple_triplet_matrix(t(dtm))/(sqrt(row_sums(dtm^2) %*% t(row_sums(dtm^2))))
