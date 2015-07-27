# load required libraries
options(java.parameters='-Xmx4g')
library(readr)
library(doParallel)
library(wordcloud)
library(tm)
library(RWeka)
library(slam)
library(data.table)
library(stringr)
library(ggplot2)
library(gridExtra)
library(printr)

# load the data
docs <- list(
  blogs=read_lines('en_US.blogs.txt'),
  twitter=read_lines('en_US.twitter.txt'),
  news=read_lines('en_US.news.txt')
)

# get basic statistics from the data

# function to clean the files for summary statistics
cleanText <- function(x) {
  stripWhitespace(
    iconv(
      removePunctuation(
        removeNumbers(
          tolower(
            x
          )
        )
      ), to='ASCII', sub=''
    )
  )
}

# function to compute basic summary statistics
computeStats <- function(doc, p) {
  s <- sample(doc, p*length(doc))
  results <- list()
  # total number of documents
  results[[1]] <- length(s)
  # words in each document
  words <- str_split(cleanText(s), ' ')
  # total number of words
  results[[2]] <- length(unlist(words))
  # average number of words per document
  results[[3]] <- mean(sapply(words, length))
  # unique words
  u_words <- unique(unlist(words))
  # total number of unique words
  results[[4]] <- length(u_words)
  # word counts
  dt <- data.table(word=unlist(words), count=1)
  dt <- dt[, sum(count), by=c('word')]
  results[[5]] <- data.table(word=dt$word, count=dt$V1)
  results
}

# compute basic summary statistics
statistics <- list()
for(i in 1:length(docs)) {
  s_size <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  results <- list()
  for(j in 1:length(s_size)) {
    results[[j]] <- computeStats(docs[[i]], s_size[j])
    cat('completed doc', i, 'size', j, '\n')
  }
  statistics[[i]] <- results
}

# print basic summary statistics for entire files
data_files <- c('en_US.blogs', 'en_US.twitter', 'en_US.news')
number_of_documents <- formatC(c(statistics[[c(1,10,1)]], statistics[[c(2,10,1)]], statistics[[c(3,10,1)]]), format='d', big.mark=',')
number_of_words <- formatC(c(statistics[[c(1,10,2)]], statistics[[c(2,10,2)]], statistics[[c(3,10,2)]]), format='d', big.mark=',')
avg_words_per_document <- formatC(round(c(statistics[[c(1,10,3)]], statistics[[c(2,10,3)]], statistics[[c(3,10,3)]]), 2), format='d', small.mark='.')
number_of_unique_words <- formatC(c(statistics[[c(1,10,4)]], statistics[[c(2,10,4)]], statistics[[c(3,10,4)]]), format='d', big.mark=',')
summary_data <- data.frame(data_files, number_of_documents, number_of_words, avg_words_per_document, number_of_unique_words)
names(summary_data) <- c(' Data File', 'Number of Documents', 'Number of Words', 'Average Number of Words per Document', 'Number of Unique Words')
print(summary_data)

# plot word frequency distribution for entire files
wc_blogs <- data.frame(statistics[[c(1,10,5)]][1:500][order(-count)])
wc_twitter <- data.frame(statistics[[c(2,10,5)]][1:500][order(-count)])
wc_news <- data.frame(statistics[[c(3,10,5)]][1:500][order(-count)])
p_blogs <- ggplot(wc_blogs, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='blue') +
  geom_area(fill='blue') +
  labs(x='Words', y='Count') +
  annotate('text', x=400, y=1500000, label='Blogs', color='blue')
p_twitter <- ggplot(wc_twitter, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='red') +
  geom_area(fill='red') +
  labs(x='Words', y='Count') +
  annotate('text', x=400, y=750000, label='Twitter', color='red')
p_news <- ggplot(wc_news, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='darkgreen') +
  geom_area(fill='darkgreen') +
  labs(x='Words', y='Count') +
  annotate('text', x=400, y=1500000, label='News', color='darkgreen')
grid.arrange(p_blogs, p_twitter, p_news, ncol=1, main = "Word Distribution for the Top 500 Most Frequent Words")

# sample 1% of each dataset
s1_blogs <- sample(docs[[1]], 0.01*length(docs[[1]]))
s1_twitter <- sample(docs[[2]], 0.01*length(docs[[2]]))
s1_news <- sample(docs[[3]], 0.01*length(docs[[3]]))

# plot word clouds for file samples of 1% without stop words
# function to clean the sampled files for wordcloud plotting
cleanTextWC <- function(x) {
  stripWhitespace(
    removeWords(
      iconv(
        removePunctuation(
          removeNumbers(
            tolower(
              x
            )
          )
        ), to='ASCII', sub=''
      ), stopwords('english')
    )
  )
}
wordcloud(cleanTextWC(s1_blogs), min.freq=50, max.words=75, scale=c(3, 0.3), colors=brewer.pal(8, "Dark2"), random.order=F, rot.per=0)
wordcloud(cleanTextWC(s1_twitter), min.freq=50, max.words=75, scale=c(3, 0.3), colors=brewer.pal(8, "Dark2"), random.order=F, rot.per=0)
wordcloud(cleanTextWC(s1_news), min.freq=50, max.words=75, scale=c(3, 0.3), colors=brewer.pal(8, "Dark2"), random.order=F, rot.per=0)

# plot vocabulary growth for all files
dt <- c(rep('Blogs', 10), rep('Twitter', 10), rep('News', 10))
ss <- rep(seq(10, 100, 10), 3)
vs <- NULL
for(i in 1:3) {
  for(j in 1:10) {
    vs <- c(vs, statistics[[c(i,j,4)]])
  }
}
vg_data <- data.frame(dt, ss, vs)
names(vg_data) <- c('Data Type', 'Sample Size (%)', 'Vocabulary Size')
ggplot(vg_data, aes(x=`Sample Size (%)`, y=`Vocabulary Size`, color=`Data Type`)) +
  geom_line(size=1)

# plot word frequency distribution for difference between 10%-60% samples and entire data
dt_10 <- statistics[[c(2,1,5)]][order(-count)]
dt_20 <- statistics[[c(2,2,5)]][order(-count)]
dt_30 <- statistics[[c(2,3,5)]][order(-count)]
dt_40 <- statistics[[c(2,4,5)]][order(-count)]
dt_50 <- statistics[[c(2,5,5)]][order(-count)]
dt_60 <- statistics[[c(2,6,5)]][order(-count)]
dt_100 <- statistics[[c(2,10,5)]][order(-count)]
dt1 <- subset(dt_100, !word %in% dt_10$word)[order(-count)][1:500]
dt2 <- subset(dt_100, !word %in% dt_20$word)[order(-count)][1:500]
dt3 <- subset(dt_100, !word %in% dt_30$word)[order(-count)][1:500]
dt4 <- subset(dt_100, !word %in% dt_40$word)[order(-count)][1:500]
dt5 <- subset(dt_100, !word %in% dt_50$word)[order(-count)][1:500]
dt6 <- subset(dt_100, !word %in% dt_60$word)[order(-count)][1:500]
p1 <- ggplot(dt1, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='blue') +
  geom_area(fill='blue') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 10%')
p2 <- ggplot(dt2, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='darkgreen') +
  geom_area(fill='darkgreen') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 20%')
p3 <- ggplot(dt3, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='red') +
  geom_area(fill='red') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 30%')
p4 <- ggplot(dt4, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='orange') +
  geom_area(fill='orange') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 40%')
p5 <- ggplot(dt5, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='brown') +
  geom_area(fill='brown') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 50%')
p6 <- ggplot(dt6, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='purple') +
  geom_area(fill='purple') +
  labs(x='Distinct Words', y='Count') +
  ggtitle('Sample of 60%')
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, main='Word Distribution for the Top 500 Most Frequent Words\nWords in Twitter Dataset not Covered by the Sample')

# sample 10% of each dataset
s10_blogs <- sample(docs[[1]], 0.1*length(docs[[1]]))
s10_twitter <- sample(docs[[2]], 0.1*length(docs[[2]]))
s10_news <- sample(docs[[3]], 0.1*length(docs[[3]]))

# function to tokenize the data and compute word counts
tokenize <- function(x, n) {
  dt <- data.table(phrase=NGramTokenizer(x, Weka_control(min=n, max=n)), count=1)
  dt <- dt[, sum(count), by=c('phrase')]
  data.table(phrase=dt$phrase, count=dt$V1)
}

# compute 1-gram, 2-gram, 3-gram, and 4-gram word counts
counts_1 <- tokenize(cleanText(c(s10_blogs, s10_twitter, s10_news)), 1)
counts_2 <- tokenize(cleanText(c(s10_blogs, s10_twitter, s10_news)), 2)
counts_3 <- tokenize(cleanText(c(s10_blogs, s10_twitter, s10_news)), 3)
counts_4 <- tokenize(cleanText(c(s10_blogs, s10_twitter, s10_news)), 4)

# plot word frequency distribution for n-grams
wc_1 <- data.frame(counts_1[1:500][order(-count)])
wc_2 <- data.frame(counts_2[1:500][order(-count)])
wc_3 <- data.frame(counts_3[1:500][order(-count)])
wc_4 <- data.frame(counts_4[1:500][order(-count)])
p_1 <- ggplot(wc_1, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='blue') +
  geom_area(fill='blue') +
  labs(x='Distinct 1-Grams', y='Count')
p_2 <- ggplot(wc_2, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='red') +
  geom_area(fill='red') +
  labs(x='Distinct 2-Grams', y='Count')
p_3 <- ggplot(wc_3, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='darkgreen') +
  geom_area(fill='darkgreen') +
  labs(x='Distinct 3-Grams', y='Count')
p_4 <- ggplot(wc_4, aes(x=c(1:length(count)), y=count)) +
  geom_line(color='orange') +
  geom_area(fill='orange') +
  labs(x='Distinct 4-Grams', y='Count')
grid.arrange(p_1, p_2, p_3, p_4, ncol=2, main = "N-Gram Distribution for the Top 500 Most Frequent N-Grams")
