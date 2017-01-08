library(tidyverse)
library(tidytext)
library(stringr)

load('www.nvic.org.Rdata')

## Remove empty documents
parsed_site = parsed_site %>% 
	filter(!is.na(title), !is.na(text))

## The simplest way to get a count of document lengths is to go through token_counts once first? 
tokens = parsed_site %>%
	unnest_tokens(token, text)
token_counts = tokens %>%
	group_by(path, title, token) %>%
	summarize(token_n = n()) #%>%
	# bind_tf_idf(term_col = token, document_col = path, n_col = token_n)
docs = token_counts %>%
	group_by(path, title) %>%
	summarize(length = sum(token_n))
ggplot(docs, aes(length)) + stat_ecdf() + geom_rug()

"
There are a lot of documents that are very short on this run — length 54 or 68 — 
because they only contain the footer and copyright information.  These docs
don't have their content in <p> tags, but instead in every other kinds of 
places.  The comments `<!-- END SOCIAL SHARING -->` and `<!-- start footer -->`
may be a better way to parse the files.  
"

problem_docs = c('BLF-vs--Offit---Plaintiff-s-Opposition-to-Defendan.aspx')


vocabulary = token_counts %>%
	group_by(token) %>%
	summarize(idf = first(idf), 
			  tf_idf = max(tf_idf))
ggplot(vocabulary, aes(idf)) + stat_ecdf()
ggplot(vocabulary, aes(tf_idf)) + stat_ecdf()

token_counts %>%
	filter(!str_detect(token, '[0-9]'))