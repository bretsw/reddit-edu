## --------------------------------------------------------------
## load libraries
## --------------------------------------------------------------

library(tidyverse)  # for data manipulation; includes library(dplyr); library(ggplot2)
library(stringr)  # for ease of working with string and character varbiables
library(anytime); library(lubridate)  # for working with dates
library(moments)  # for skewness and kurtosis statistics
library(sentimentr)  # for sentiment analysis https://github.com/trinker/sentimentr
library(igraph)  # for processing social network
library(ggraph)  # for visualizing social network


## --------------------------------------------------------------
## load, clean, view data
## --------------------------------------------------------------

## assumes filenames are of the form "r_SUBREDDIT.csv" and stored in same directory
file_list <- stringr::str_extract_all(dir(), "^r_\\S+", simplify=TRUE)
file_list <- file_list[file_list[,1] != "", ]

#reddit_all <- lapply(file_list, read.csv, header=TRUE, colClasses='character')  # subreddit_og[[i]] = sample i  |  subreddit_og[[i]][1,] = row 1 of sample i
#reddit_all %>% sapply(dim)


## --------------------------------------------------------------
## Select which subreddit to analyze:
## 1: r/education
## 2: r/Teachers
## 3: r/teaching
## 4: r/teachingresources

case <- 3
## --------------------------------------------------------------

index <- c((2*case)-1, 2*case)
reddit_og <- lapply(file_list[index], read.csv, header=TRUE, colClasses='character')  # subreddit_og[[i]] = sample i  |  subreddit_og[[i]][1,] = row 1 of sample i

subreddit_posts <- reddit_og[[1]] %>% 
        as.data.frame %>%
        dplyr::rename(., post_id = id, post_author = author) %>%
        mutate(date_time = created_utc %>% as.numeric %>% anytime(asUTC=TRUE) %>% as_datetime,
               voting_score = score %>% as.numeric) %>%
        distinct(post_id, .keep_all = TRUE) %>%
        filter(post_id != "",
               !is.na(post_id),
               post_author != "",
               post_author != "[deleted]",
               selftext != "[deleted]",
               selftext != "[removed]"
               )

## --------------------------------------------------------------
subreddit_responses <- reddit_og[[2]] %>% 
        as.data.frame %>% 
        dplyr::rename(., 
                      response_id = id, 
                      response_author = author) %>%
        mutate(thread_id = str_remove(link_id, pattern="t[0-9]_"),
               parent_id = str_remove(parent_id, pattern="t[0-9]_"),
               parent_author = "",
               date_time = created_utc %>% as.numeric %>% anytime(asUTC=TRUE) %>% as_datetime,
               voting_score = score %>% as.numeric,
               post_id = thread_id
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


## --------------------------------------------------------------
subreddit_merged <- subreddit_posts %>% 
        inner_join(subreddit_responses, by=c('post_id'))

subreddit_merged %>% nrow
subreddit_merged$parent_id %in% subreddit_merged$post_id %>% which %>% length
subreddit_merged$parent_id %in% subreddit_merged$response_id %>% which %>% length



## --------------------------------------------------------------
## Descriptive Statistics
## --------------------------------------------------------------

## posts; nonzero-response posts; percentage of posts with some responses
n_posts <- subreddit_posts %>% distinct(post_id) %>% nrow
n_nonzero <- subreddit_merged %>% distinct(post_id) %>% nrow
p_nonzero <- {100 * n_nonzero / n_posts} %>% round(2)

## responses; threads; responses/thread
n_responses <- subreddit_merged %>% distinct(response_id) %>% nrow  # responses
n_threads <- subreddit_merged %>% distinct(thread_id) %>% nrow  # threads
responses_per_thread <- {n_responses / n_threads} %>% round(2)

## total contributors (unique posters + responders)
n_posters <- subreddit_merged %>% distinct(post_author) %>% nrow
n_responders <- subreddit_merged %>% distinct(response_author) %>% nrow
n_contributors <- c(subreddit_merged$post_author, subreddit_merged$response_author) %>% 
        unique %>% length

## print results
n_posts; n_nonzero; p_nonzero
n_responses; n_threads; responses_per_thread
n_contributors; n_posters; n_responders


## --------------------------------------------------------------
## responses per responder
responders <- subreddit_merged$response_author %>% 
        table %>% 
        as.data.frame %>%
        dplyr::rename(user = '.', responses='Freq') %>%
        arrange(desc(responses))
#responders %>% head
responders %>% nrow
subreddit_merged %>% distinct(response_id) %>% nrow # responses
subreddit_merged %>% distinct(post_id) %>% nrow  # threads


## calculating stats
responders$responses %>% mean %>% round(2)
responders$responses %>% sd %>% round(2)
responders$responses %>% median %>% round(2)
responders$responses %>% range %>% round(2)
responders$responses %>% moments::skewness(.) %>% round(2)
responders$responses %>% moments::kurtosis(.) %>% round(2)

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



## --------------------------------------------------------------
## voting scores: posts

## calculating stats: posts
subreddit_merged$voting_score.x %>% mean %>% round(2)
subreddit_merged$voting_score.x %>% sd %>% round(2)
subreddit_merged$voting_score.x %>% median %>% round(2)
subreddit_merged$voting_score.x %>% range %>% round(2)
subreddit_merged$voting_score.x %>% moments::skewness(.) %>% round(2)
subreddit_merged$voting_score.x %>% moments::kurtosis(.) %>% round(2)

## histogram of voting scores: posts
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
#ggsave("r-output/posts_voting_histogram3.png", width = 1 * 10, height = 1 * 10)


## --------------------------------------------------------------
## voting scores: responses

## calculating stats: responses
subreddit_merged$voting_score.y %>% mean %>% round(2)
subreddit_merged$voting_score.y %>% sd %>% round(2)
subreddit_merged$voting_score.y %>% median %>% round(2)
subreddit_merged$voting_score.y %>% range %>% round(2)
subreddit_merged$voting_score.y %>% moments::skewness(.) %>% round(2)
subreddit_merged$voting_score.y %>% moments::kurtosis(.) %>% round(2)

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
#ggsave("r-output/responses_voting_histogram3.png", width = 1 * 10, height = 1 * 10)



## --------------------------------------------------------------
## Sentiment Analysis
## --------------------------------------------------------------

subreddit_post_sentiment <- subreddit_merged$selftext %>% 
        sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker)
#saveRDS(subreddit_post_sentiment, "r-output/subreddit_post_sentiment3.rds")
#subreddit_post_sentiment <- readRDS("r-output/subreddit_post_sentiment3.rds")

subreddit_response_sentiment <- subreddit_merged$body %>% 
        sentiment(polarity_dt = lexicon::hash_sentiment_jockers_rinker)
#saveRDS(subreddit_response_sentiment, "r-output/subreddit_response_sentiment3.rds")
#subreddit_response_sentiment <- readRDS("r-output/subreddit_response_sentiment3.rds")

subreddit_merged <- subreddit_post_sentiment %>%
        group_by(element_id) %>%
        summarize(sentiment_post = sum(sentiment)) %>%
        cbind(subreddit_merged, .) %>%
        select(-element_id)

subreddit_merged <- subreddit_response_sentiment %>%
        group_by(element_id) %>%
        summarize(sentiment_response = sum(sentiment)) %>%
        cbind(subreddit_merged, .) %>%
        select(-element_id)


## calculating stats: posts
subreddit_merged$sentiment_post %>% mean %>% round(2)
subreddit_merged$sentiment_post %>% sd %>% round(2)
subreddit_merged$sentiment_post %>% median %>% round(2)
subreddit_merged$sentiment_post %>% range %>% round(2)
subreddit_merged$sentiment_post %>% moments::skewness(.) %>% round(2)
subreddit_merged$sentiment_post %>% moments::kurtosis(.) %>% round(2)

## calculating stats: responses
subreddit_merged$sentiment_response %>% mean %>% round(2)
subreddit_merged$sentiment_response %>% sd %>% round(2)
subreddit_merged$sentiment_response %>% median %>% round(2)
subreddit_merged$sentiment_response %>% range %>% round(2)
subreddit_merged$sentiment_response %>% moments::skewness(.) %>% round(2)
subreddit_merged$sentiment_response %>% moments::kurtosis(.) %>% round(2)


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



## --------------------------------------------------------------
## plot: posts voting vs sentiment (voting score as a function of sentiment score)
ggplot(data=subreddit_merged,
       aes(x=sentiment_post, y=voting_score.x)) + 
        geom_point(alpha=.2, color="black") + 
        geom_hline(yintercept=0, linetype="dashed") +
        geom_vline(xintercept=0, linetype="dashed") +
        xlim(-12,12) +
        ylim(-500,500) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5) 
        ) + 
        labs(x="Sentiment Score", y="Voting Score")
#ggsave("r-output/posts_voting_v_sentiment_plot3.png", width = 1 * 10, height = 1 * 10)


## --------------------------------------------------------------
## plot: responses voting vs sentiment (voting score as a function of sentiment score)
ggplot(data=subreddit_merged,
        aes(x=sentiment_response, y=voting_score.y)) + 
        geom_point(alpha=.2, color="black") + 
        geom_hline(yintercept=0, linetype="dashed") +
        geom_vline(xintercept=0, linetype="dashed") +
        xlim(-12,12) +
        ylim(-500,500) +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border= element_blank(),
              axis.line = element_line(color="black", size = .5) 
              ) + 
        labs(x="Sentiment Score", y="Voting Score")
#ggsave("r-output/responses_voting_v_sentiment_plot3.png", width = 1 * 10, height = 1 * 10)



## --------------------------------------------------------------
## Network Analysis
## --------------------------------------------------------------

subreddit_merged %>% distinct(post_id) %>% nrow  # posts
subreddit_merged %>% distinct(thread_id) %>% nrow  # threads
subreddit_merged %>% distinct(response_id) %>% nrow  # responses
c(subreddit_merged$post_author, subreddit_merged$response_author) %>% unique %>% length  # these are the unique contributors

subreddit_merged %>% distinct(post_author) %>% nrow  # these are the different posters
subreddit_merged %>% distinct(response_author) %>% nrow  # these are the different responders


## --------------------------------------------------------------
## retrieve the name of the parent author
for (i in 1:nrow(subreddit_merged)) {
        subreddit_merged$parent_author[i] <-
                ifelse(subreddit_merged$parent_id[i] %in% subreddit_merged$post_id, 
                       subreddit_merged$post_author[i],
                       subreddit_merged$response_author[i]
                )
}
#saveRDS(subreddit_merged, "r-output/subreddit_full_stats3.rds")
#subreddit_merged <- readRDS("r-output/subreddit_full_stats3.rds")

## --------------------------------------------------------------
## create edgelist and then the network graph
subreddit_graph <- subreddit_merged %>% 
        select(response_author, parent_author) %>%
        as.matrix %>% graph_from_edgelist(directed=TRUE) %>% 
        set_vertex_attr(name='degree', value=degree(., mode='total', loops=FALSE))
subreddit_graph %>% summary


## --------------------------------------------------------------
## network statistics

## size
subreddit_graph %>% V %>% length  # number of vertices/nodes
subreddit_graph %>% gsize  # number of edges
#subreddit_graph %>% diameter(directed=TRUE, unconnected=TRUE)  # the length of the longest geodesic (max distance between two vertices)

## connectedness
subreddit_graph %>% vertex_attr('degree') %>% mean %>% round(2)
subreddit_graph %>% vertex_attr('degree') %>% sd %>% round(2)
subreddit_graph %>% vertex_attr('degree') %>% median
subreddit_graph %>% vertex_attr('degree') %>% range
subreddit_graph %>% vertex_attr('degree') %>% moments::skewness(.) %>% round(2)
subreddit_graph %>% vertex_attr('degree') %>% moments::kurtosis(.) %>% round(2)

## visualization
subreddit_graph_viz <- subreddit_merged %>% 
        select(response_author, parent_author) %>%
        filter(response_author != parent_author) %>% 
        as.matrix %>% graph_from_edgelist(directed=FALSE) %>% 
        set_vertex_attr(name='degree', value=degree(., mode='total', loops=FALSE))
subreddit_graph_viz %>% summary
subreddit_graph %>% summary


## percentage of contributors connected to other users:
{ (subreddit_graph_viz %>% V %>% length) / 
                (subreddit_graph %>% V %>% length) * 100} %>% round(2)



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
#ggsave("r-output/subreddit_network_visualization3.png", width = 1 * 10, height = 1 * 10)