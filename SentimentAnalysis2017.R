# Read file
apple <- read.csv(file.choose(), header = T)
str(apple)
#it has 1000 tweets and 16 colns
# but we are only interested in first coln, which contains all the tweets

# Build corpus
# corpus is a collection of documents and each tweet will be treated as a document
library(tm)     #for text mining

corpus <- iconv(apple$text, to = "utf-8-mac")
#we are interested in only first column, whcih is text column

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5]) #inspect first 5 tweets

# --------------------------------------------------------
# Clean text
# first step is to make everything to lower case
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

# --------------------------------------------------------
# Next remove all the punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# --------------------------------------------------------
#Next, remove all the numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# --------------------------------------------------------
# Next remove english words, which will not be adding much values, like  'on' etc, called stopwords
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

#to see what all are stored in stopwords
stopwords('english')

# --------------------------------------------------------
# Remove http
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

# --------------------------------------------------------
# Remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


# -------------------------------------------------------------------------------
# Term Document Matrix
# tweets are unstructured data
# for further analysis, we need to convert them into strucrtured data into rows and columns
# which is achieved by term document matrix
# Term document matrix
# Sparcity is 99% as per resutl, meaning 99% i will see 0's
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] #i want first 10 words and 20 columns
#i see aapl appears almost in every tweet, common word, we have to remove this word

# -------------------------------------------------------------------------------
#Removing the common word 'aapl'
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))  #here i am specifying our own words

#dont run below code unless the line mentions some where below
cleanset <- tm_map(cleanset, gsub, pattern = 'stocks', 
                   replacement = 'stock')
#after
#remove white spaces (stripwhitepsace line)
#then re run the TDM codes again



cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')


# -------------------------------------------------------------------------------
# Bar plot, how many words appear how many times, for each word it has given its frequency
w <- rowSums(tdm)

#since our data is huge, i mean the tweet
#lets use the words whose frequency is more than 25
w <- subset(w, w>=25)
barplot(w,
        las = 2,    #all the words printed vertically
        col = rainbow(50)) 
#from above results, we see the tweets are about more of earnings
#we also see stocks and stock
# go back to the code where cleanset was created
# after creating another cleanset, re run bar plot commands
# now stocks replaced by stock


# -------------------------------------------------------------------------------
# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w, #upto here, the words shown are too cluttered
          max.words = 150,
          random.order = F,
          min.freq = 5, #anytime a word appears 5 times should be printed
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3), #maximum size of the highest frequency word should be 5 and the lowest one should be 0.3
          rot.per = 0.7) #i want 30 percentage of words to be rotated

# ------------------------------------------------------------------------------------------
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',   #default size is circle
           rotateRatio = 0.5,   #meaning 50%
           minSize = 1)

# you can also create letter cloud
letterCloud(w,
            word = "apple",  #i can get size in 'A', 'a' etc
            size=1)

---------------------------------------------
# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = 'utf-8-mac') #we are interested only in text column

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s) #we get 10 different columns
#sentiment ranges from anger to positive
# for 1st tweet, it scores everything 0 excpet positive, so socres first tweet as positive

#tweet number 4 has various entries
#it has score for anger 1, scores for disgust 2 and so on
tweets[4]
get_nrc_sentiment('delay')
#so score for the word 'delay' has 1 for anger, 1 for disgust and so on

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')

#before earnings report by apple, the highest bar is for negative, sentiment is negative 
#people are thinking that they might miss their estimates, 
#there is some amount of fear also
# joy is almost on a smaller size

#after this, get tweets after earnings, which is attached to this 
#and re run all the codes from 'Obtain sentiment scores'
#and create this bar plot once again
#now you will get reports after earnings report, highest bar is for positive
#bse the data was available to the investors
#and they see that apple haas done very well
#now sentiment has changed to positive and negative is much lower compare to the positive one
#earlier we had smallest bar for joy and now it has increased significantly
#disgust is the lowest one now
