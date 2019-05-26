## -----------------------------------------------------------------------------
## get set up - load packages
## -----------------------------------------------------------------------------

usethis::use_git_ignore(c("*.csv", "*.rds"))

library(tidyverse)  # for data manipulation; includes library(dplyr); library(ggplot2); library(stringr)
library(anytime)
library(lubridate)  # for working with dates
library(sentimentr)  # for sentiment analysis https://github.com/trinker/sentimentr
library(igraph)  # for processing social network
library(ggraph)  # for visualizing social network


## -----------------------------------------------------------------------------
## load, clean, view data
## -----------------------------------------------------------------------------

## assumes filenames are of the form "r_SUBREDDIT.csv" and stored in same directory
file_list <- str_extract_all(dir(), "^r_\\S+", simplify=TRUE)
file_list <- file_list[file_list[,1] != "", ]

#reddit_all <- lapply(file_list, read.csv, header=TRUE, colClasses='character')  # subreddit_og[[i]] = sample i  |  subreddit_og[[i]][1,] = row 1 of sample i
#reddit_all %>% sapply(dim)


## -----------------------------------------------------------------------------
## Select which subreddit to analyze:
## 1: r/education
## 2: r/Teachers
## 3: r/teaching
## 4: r/teachingresources

case <- 4
## -----------------------------------------------------------------------------

index <- c((2*case)-1, 2*case)
reddit_og <- lapply(file_list[index], read.csv, header=TRUE, colClasses='character')  
        # subreddit_og[[i]] = sample i  |  subreddit_og[[i]][1,] = row 1 of sample i

subreddit_posts <- reddit_og[[1]] %>% 
        as.data.frame() %>%
        rename(post_id = id, 
               post_author = author,
               voting_score = score
               ) %>%
        mutate(date_time = created_utc %>% as.numeric() %>% anytime(asUTC=TRUE) %>% as_datetime,
               voting_score = voting_score %>% as.numeric()
               ) %>%
        distinct(post_id, .keep_all = TRUE) %>%
        filter(post_id != "",
               !is.na(post_id),
               post_author != "",
               post_author != "[deleted]",
               selftext != "[deleted]",
               selftext != "[removed]"
               )


## -----------------------------------------------------------------------------
subreddit_responses <- reddit_og[[2]] %>% 
        as.data.frame() %>% 
        rename(response_id = id, 
               response_author = author,
               voting_score = score) %>%
        mutate(thread_id = str_remove(link_id, pattern="t[0-9]_"),
               parent_id = str_remove(parent_id, pattern="t[0-9]_"),
               date_time = created_utc %>% as.numeric() %>% anytime(asUTC=TRUE) %>% as_datetime,
               post_id = thread_id,
               voting_score = voting_score %>% as.numeric()
               ) %>% 
        distinct(response_id, .keep_all = TRUE) %>%
        filter(response_id != "",
               !is.na(response_id),
               response_author != "",
               response_author != "[deleted]",
               body != "[deleted]",
               body != "[removed]"#, 
               #parent_id %in% c(subreddit_posts$post_id, thread_id, response_id)  # filter the data by responses to posts and threads which started within the collection time period
        )

subreddit_responses <- subreddit_responses %>% 
        filter(parent_id %in% c(subreddit_posts$post_id, subreddit_responses$thread_id, subreddit_responses$response_id))


## -----------------------------------------------------------------------------
subreddit_merged <- subreddit_posts %>% 
        inner_join(subreddit_responses, by=c('post_id'))
#subreddit_merged %>% dim
##subreddit_merged$parent_id %in% subreddit_merged$post_id %>% which %>% length
#subreddit_merged$parent_id %in% subreddit_merged$response_id %>% which %>% length


## -----------------------------------------------------------------------------
## Contributor Activity: Descriptive Statistics
## -----------------------------------------------------------------------------

## posts; nonzero-response posts; percentage of posts with some responses
n_posts <- subreddit_posts %>% distinct(post_id) %>% nrow()
n_nonzero <- subreddit_merged %>% distinct(post_id) %>% nrow()
p_nonzero <- {100 * n_nonzero / n_posts} %>% round(2)

## responses; threads; responses/thread
n_responses <- subreddit_merged %>% distinct(response_id) %>% nrow()  # responses
n_threads <- subreddit_merged %>% distinct(thread_id) %>% nrow()  # threads
responses_per_thread <- {n_responses / n_threads} %>% round(2)

## total contributors (unique posters + responders)
n_posters <- subreddit_merged %>% distinct(post_author) %>% nrow()
n_responders <- subreddit_merged %>% distinct(response_author) %>% nrow()
n_contributors <- c(subreddit_merged$post_author, subreddit_merged$response_author) %>% 
        unique() %>% length()

## print results
paste(n_posts, "posts",
      "(", n_nonzero, "or",  p_nonzero, "% with at least one response )")
paste(n_responses, "responses in", 
      n_threads, "threads",
      "(", responses_per_thread, "responses per thread )")
paste(n_contributors, "contributors",
      "(", n_posters, "posters and",
      n_responders, "responders )")


## -----------------------------------------------------------------------------
## posts per poster
posters <- subreddit_merged$post_author %>%
        table() %>% 
        as.data.frame() %>%
        rename(user = '.', posts='Freq') %>%
        arrange(desc(posts))
#posters %>% nrow()

## proportion posting more than one time
p_multi_posters <- (100 * nrow(filter(posters, posts > 1)) / nrow(posters)) %>% round(2)
paste(p_multi_posters, "% of contributers posted more than once")


## -----------------------------------------------------------------------------
## responses per responder
responders <- subreddit_merged$response_author %>%
        table() %>% 
        as.data.frame() %>%
        rename(user = '.', responses='Freq') %>%
        arrange(desc(responses))
#responders %>% nrow()

## proportion responding more than one time
p_multi_responders <- (100 * nrow(filter(responders, responses > 1)) / nrow(responders)) %>% round(2)
paste(p_multi_responders, "% of contributers responded more than once")

## calculating stats: responses per responder
responses_mean <- responders$responses %>% mean() %>% round(2)
responses_sd <- responders$responses %>% sd() %>% round(2)
responses_median <- responders$responses %>% median() %>% round(2)
responses_min <- responders$responses %>% min() %>% round(2)
responses_max <- responders$responses %>% max() %>% round(2)

paste("Responses per Responder:")
paste("Mean =", responses_mean)
paste("Standard Deviation =", responses_sd)
paste("Median =", responses_median)
paste("Range =", responses_min, "to", responses_max)


## histogram of responses
ggplot(responders, aes(x=responses)) + 
        geom_histogram(binwidth=1, fill="#00BFFF") +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=0) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5)
              )
#ggsave("r-output/responses_histogram3.png", width = 1 * 10, height = 1 * 10)



## -----------------------------------------------------------------------------
## Network Analysis
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## retrieve the name of the parent author
subreddit_merged <- subreddit_merged %>%
        mutate(parent_author = ifelse(parent_id %in% subreddit_merged$post_id,
                                      post_author,
                                      response_author
                                      )
               )
#saveRDS(subreddit_merged, "r-output/subreddit_full_stats4.rds")
#subreddit_merged <- readRDS("r-output/subreddit_full_stats4.rds")

## -----------------------------------------------------------------------------
## create edgelist and then the network graph

subreddit_graph <- subreddit_merged %>% 
        select(response_author, parent_author) %>%
        as.matrix() %>%
        graph_from_edgelist(directed=TRUE) %>% 
        set_vertex_attr(name='degree', value=degree(., mode='total', loops=FALSE))
#subreddit_graph %>% summary()


## -----------------------------------------------------------------------------
## network statistics

## size
subreddit_graph %>% V() %>% length()  # number of vertices/nodes
subreddit_graph %>% gsize()  # number of edges
#subreddit_graph %>% diameter(directed=TRUE, unconnected=TRUE)  # the length of the longest geodesic (max distance between two vertices)

(subreddit_graph %>% transitivity("global") * 100) %>% round(2)  
## The balance of connections. Also called the clustering coefficient.
## The probability that the adjacent vertices of a vertex are connected. 
## When the clustering coefficient is large it implies that a graph is highly clustered around a few nodes, 
## When it is low it implies that the links in the graph are relatively evenly spread among all the nodes. (Hogan, 2017)


(subreddit_graph %>% reciprocity * 100) %>% round(2)  
## The proportion of mutual connections (in a directed network).
## The probability that the opposite counterpart of a directed edge is also included in the graph.


## average connectedness
subreddit_graph %>% vertex_attr('degree') %>% mean() %>% round(2)
subreddit_graph %>% vertex_attr('degree') %>% sd() %>% round(2)
subreddit_graph %>% vertex_attr('degree') %>% median()
subreddit_graph %>% vertex_attr('degree') %>% min()
subreddit_graph %>% vertex_attr('degree') %>% max()

## visualization
subreddit_graph_viz <- subreddit_merged %>% 
        select(response_author, parent_author) %>%
        filter(response_author != parent_author) %>% 
        as.matrix() %>% 
        graph_from_edgelist(directed=TRUE) %>% 
        set_vertex_attr(name='degree', value=degree(., mode='total', loops=FALSE))
#subreddit_graph_viz %>% summary()
#subreddit_graph %>% summary()


## percentage of contributors connected to other users:
connected_users <- ( (subreddit_graph_viz %>% V %>% length) / 
                (subreddit_graph %>% V %>% length) * 100) %>% round(2)
paste(connected_users, "% of users conntected with other users")

layout <- subreddit_graph_viz %>% create_layout(layout = 'drl') 

ggraph(layout) +
        geom_edge_link(alpha = .2, width=.3) +
        geom_node_point(alpha=.2, size=1) +
        theme_bw() +  # makes background white (not gray)
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position="none"
        )
#ggsave("r-output/subreddit_network_visualization4.png", width = 1 * 10, height = 1 * 10)


## -----------------------------------------------------------------------------
## For analysis with Gephi
subreddit_edgelist <- subreddit_merged %>% 
        select(response_author, parent_author) %>%
        filter(response_author != parent_author) %>% 
        rename(Source = response_author,
               Target = parent_author) %>%
        as.matrix() 
#write.csv(subreddit_edgelist, "subreddit_edgelist4.csv", row.names=FALSE)
#dim(subreddit_edgelist)












## -----------------------------------------------------------------------------
## Voting Scores Analysis
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## voting scores: posts

## calculating stats: posts
post_voting_mean <- subreddit_merged$voting_score.x %>% mean() %>% round(2)
post_voting_sd <- subreddit_merged$voting_score.x %>% sd() %>% round(2)
post_voting_median <- subreddit_merged$voting_score.x %>% median() %>% round(2)
post_voting_min <- subreddit_merged$voting_score.x %>% min() %>% round(2)
post_voting_max <- subreddit_merged$voting_score.x %>% max() %>% round(2)

paste("Posts Voting Scores:")
paste("Mean =", post_voting_mean)
paste("Standard Deviation =", post_voting_sd)
paste("Median =", post_voting_median)
paste("Range =", post_voting_min, "to", post_voting_max)

## histogram of voting scores: posts voting
ggplot(subreddit_merged, aes(x=voting_score.x)) + 
        geom_histogram(binwidth=1, fill="#00BFFF") +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=0, linetype="dashed") +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5)
              )
#ggsave("r-output/posts_voting_histogram4.png", width = 1 * 10, height = 1 * 10)


## -----------------------------------------------------------------------------
## voting scores: responses

## calculating stats: responses voting
response_voting_mean <- subreddit_merged$voting_score.y %>% mean() %>% round(2)
response_voting_sd <- subreddit_merged$voting_score.y %>% sd() %>% round(2)
response_voting_median <- subreddit_merged$voting_score.y %>% median() %>% round(2)
response_voting_min <- subreddit_merged$voting_score.y %>% min() %>% round(2)
response_voting_max <- subreddit_merged$voting_score.y %>% max() %>% round(2)

paste("Responses Voting Scores:")
paste("Mean =", response_voting_mean)
paste("Standard Deviation =", response_voting_sd)
paste("Median =", response_voting_median)
paste("Range =", response_voting_min, "to", response_voting_max)

## histogram of voting scores: responses
ggplot(subreddit_merged, aes(x=voting_score.y)) + 
        geom_histogram(binwidth=1, fill="#00BFFF") +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=0, linetype="dashed") +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5)
        )
#ggsave("r-output/responses_voting_histogram4.png", width = 1 * 10, height = 1 * 10)



## -----------------------------------------------------------------------------
## Sentiment Analysis
## -----------------------------------------------------------------------------

subreddit_post_sentiment <- subreddit_merged$selftext %>% 
        as.character() %>%
        sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker)
#saveRDS(subreddit_post_sentiment, "r-output/subreddit_post_sentiment4.rds")
#subreddit_post_sentiment <- readRDS("r-output/subreddit_post_sentiment4.rds")

subreddit_title_sentiment <- subreddit_merged$title %>% 
        sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker)
#saveRDS(subreddit_title_sentiment, "r-output/subreddit_title_sentiment4.rds")
#subreddit_title_sentiment <- read_rds("r-output/subreddit_title_sentiment4.rds")

subreddit_response_sentiment <- subreddit_merged$body %>% 
        as.character() %>%
        sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker)
#saveRDS(subreddit_response_sentiment, "r-output/subreddit_response_sentiment4.rds")
#subreddit_response_sentiment <- readRDS("r-output/subreddit_response_sentiment4.rds")

subreddit_merged <- subreddit_post_sentiment %>%
        mutate(word_count = word_count %>% as.double %>% if_else(is.na(.), 0, .)) %>%
        group_by(element_id) %>%
        summarize(sentiment_post = sum(sentiment),
                  word_count_post = sum(word_count)
                  ) %>%
        cbind(subreddit_merged, .) %>%
        select(-element_id)

subreddit_merged <- subreddit_title_sentiment %>%
        mutate(word_count = word_count %>% as.double %>% if_else(is.na(.), 0, .)) %>%
        group_by(element_id) %>%
        summarize(sentiment_title = sum(sentiment),
                  word_count_title = sum(word_count)
                  ) %>%
        cbind(subreddit_merged, .) %>%
        mutate(sentiment_post = sentiment_post + sentiment_title,
               word_count_post = word_count_post + word_count_title
               ) %>% 
        select(-element_id, -sentiment_title, -word_count_title)

subreddit_merged <- subreddit_response_sentiment %>%
        mutate(word_count = word_count %>% as.double %>% if_else(is.na(.), 0, .)) %>%
        group_by(element_id) %>%
        summarize(sentiment_response = sum(sentiment),
                  word_count_response = sum(word_count)
                  ) %>%
        cbind(subreddit_merged, .) %>%
        select(-element_id)

## calculating stats: posts sentiment
post_word_count <- subreddit_merged %>% 
        distinct(post_id, .keep_all=TRUE) %>% 
        pull(word_count_post) %>% 
        mean() %>% 
        round(2)
post_sentiment_mean <- subreddit_merged$sentiment_post %>% mean() %>% round(2)
post_sentiment_sd <- subreddit_merged$sentiment_post %>% sd() %>% round(2)
post_sentiment_median <- subreddit_merged$sentiment_post %>% median() %>% round(2)
post_sentiment_min <- subreddit_merged$sentiment_post %>% min() %>% round(2)
post_sentiment_max <- subreddit_merged$sentiment_post %>% max() %>% round(2)

paste("Posts had a mean of", post_word_count, "words.")
paste("Posts Sentiment Scores:")
paste("Mean =", post_sentiment_mean)
paste("Standard Deviation =", post_sentiment_sd)
paste("Median =", post_sentiment_median)
paste("Range =", post_sentiment_min, "to", post_sentiment_max)


## calculating stats: responses
response_word_count <- subreddit_merged %>% 
        distinct(response_id, .keep_all=TRUE) %>% 
        pull(word_count_response) %>% 
        mean() %>% 
        round(2)
response_sentiment_mean <- subreddit_merged$sentiment_response %>% mean() %>% round(2)
response_sentiment_sd <- subreddit_merged$sentiment_response %>% sd() %>% round(2)
response_sentiment_median <- subreddit_merged$sentiment_response %>% median() %>% round(2)
response_sentiment_min <- subreddit_merged$sentiment_response %>% min() %>% round(2)
response_sentiment_max <- subreddit_merged$sentiment_response %>% max() %>% round(2)

paste("Responses had a mean of", response_word_count, "words.")
paste("Responses Sentiment Scores:")
paste("Mean =", response_sentiment_mean)
paste("Standard Deviation =", response_sentiment_sd)
paste("Median =", response_sentiment_median)
paste("Range =", response_sentiment_min, "to", response_sentiment_max)


## histogram of sentiment: posts
ggplot(subreddit_merged, aes(x=sentiment_post)) + 
        geom_histogram(binwidth=.1, fill="#00BFFF") +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=0, linetype="dashed") +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5)
        )
#ggsave("r-output/posts_sentiment_histogram3.png", width = 1 * 10, height = 1 * 10)


## histogram of sentiment: responses
ggplot(subreddit_merged, aes(x=sentiment_response)) + 
        geom_histogram(binwidth=.1, fill="#00BFFF") +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=0, linetype="dashed") +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5)
        )
#ggsave("r-output/responses_sentiment_histogram3.png", width = 1 * 10, height = 1 * 10)



## -----------------------------------------------------------------------------
## plot: posts voting vs sentiment (voting score as a function of sentiment score)
ggplot(data=subreddit_merged,
       aes(x=sentiment_post, y=voting_score.x)) + 
        geom_point(alpha=.2, color="gray40") + 
        geom_hline(yintercept=0, linetype="dashed") +
        geom_vline(xintercept=0, linetype="dashed") +
        xlim(-12,12) +
        ylim(-500,500) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5),
              axis.text=element_text(size=18),
              axis.title=element_text(size=24)  # face="bold"
        ) + 
        labs(x="Sentiment Score", y="Voting Score")
#ggsave("r-output/posts_voting_v_sentiment_plot4.png", width = 1 * 10, height = 1 * 10)


## -----------------------------------------------------------------------------
## plot: responses voting vs sentiment (voting score as a function of sentiment score)
ggplot(data=subreddit_merged,
       aes(x=sentiment_response, y=voting_score.y)) + 
        geom_point(alpha=.2, color="gray40") + 
        geom_hline(yintercept=0, linetype="dashed") +
        geom_vline(xintercept=0, linetype="dashed") +
        xlim(-12,12) +
        ylim(-500,500) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5),
              axis.text=element_text(size=18),
              axis.title=element_text(size=24)  # face="bold"
        ) + 
        labs(x="Sentiment Score", y="Voting Score")
#ggsave("r-output/responses_voting_v_sentiment_plot4.png", width = 1 * 10, height = 1 * 10)
