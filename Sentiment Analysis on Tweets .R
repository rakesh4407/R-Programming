# Step 1: Install required packages
install.packages(c( "tidytext", "dplyr", "ggplot2", "tidyr"))

# Step 2: Load packages
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 3: Search for recent tweets (no academic access needed!)
tweets <- search_tweets("technology", n = 200, lang = "en", include_rts = FALSE)

# Step 4: Clean and tokenize the text
data("stop_words")

tweet_words <- tweets %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Step 5: Use Bing lexicon for sentiment analysis
bing_sentiment <- tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

# Step 6: Plot the sentiment distribution
ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Sentiment of Tweets on 'Technology'",
    x = "Sentiment",
    y = "Number of Words"
  ) +
  theme_minimal()
