# Analysis on Federalist Papers



# # Libraries need for text processing
# library(openNLP) ## Loads the package for use in the task
# library(openNLPmodels.en) ## Loads the model files for the English language
# library(NLP)



corpus <- Corpus(DirSource("./essays/"), readerControl = list(language="english"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# corpus <- tm_map(corpus, stemDocument)
sort(stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("will", "may", "can", "might", "shall",
                                        "must", "one", "upon"))
inspect(corpus[85])

library(wordcloud)
wordcloud(corpus, scale=c(5,0.5), max.words=25, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

dtm <- DocumentTermMatrix(corpus)
vocab <- dtm_to_vocab(dtm)

freq <- colSums(as.matrix(dtm))


documents <- 


doc.list <- vector("list", length=85)

for(i in 1:85) {
  doc <- as.matrix(dtm)[i,]
  indices <- which(doc!=0)
  doc <- doc[indices]
  doc.list[i] <- lexicalize(paste(fed.papers[[i]], collapse=""), lower=TRUE, vocab=vocab)
}

require("reshape2")

K <- 5
result <- lda.collapsed.gibbs.sampler(doc.list, K, vocab, 50, 0.1, 0.1, compute.log.likelihood=TRUE) 

top.words <- top.topic.words(result$topics, 5, by.score=TRUE)

## Number of documents to display
N <- 9

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)

sample <- sample(1:dim(topic.proportions)[1], N)
topic.proportions <-
  topic.proportions[sample,]
topic.proportions[is.na(topic.proportions)] <-  1 / K

colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
                                   document=factor(1:N)),
                             variable.name="topic",
                             id.vars = "document")  



qplot(topic, value, fill=document, ylab="proportion",
      data=topic.proportions.df, stat="identity") +
  coord_flip() +
  facet_wrap(~ document, ncol=3)



+
  opts(axis.text.x = theme(angle=90, hjust=1)) 






library(ggplot2)
load("federalist.RData")
n.essays <- length(fed.papers)


library(tm)
get_corpus <- function(path) {
  old_directory <- getwd()
  new_directory <- as.character(path) # Making sure it's a string
  setwd(new_directory)
  corpus <- Corpus(DirSource(getwd()), readerControl = list(language="english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument, language="english")
  corpus
}
corpus <- get_corpus("./essays/")
dtm <- get_dtm_matrix(corpus)



#
# Focus for now on 69 out of 85 essays whose authorship is not disputed
#
disputed <- 
  which(as.character(authors.array$hamilton) != as.character(authors.array$madison))
undisputed <- setdiff(1:n.essays, disputed)

hamilton <- intersect(undisputed, which(authors.array$hamilton=="HAMILTON"))
madison <- intersect(undisputed, which(authors.array$hamilton=="MADISON"))
jay <- intersect(undisputed, which(authors.array$hamilton=="JAY"))

# re-order undisputed
undisputed <- c(hamilton, madison, jay)
authors <- c(rep("Hamilton", length(hamilton)), 
             rep("Madison", length(madison)),
             rep("Jay", length(jay)))
initials <- c(rep("H", length(hamilton)), 
              rep("M", length(madison)),
              rep("J"), length(jay))
hamilton.index <- which(authors=="Hamilton")
madison.index <- which(authors=="Madison")
jay.index <- which(authors=="Jay")


#
# Per essay metrics.  Nothing sexy except John Jay didn't do shit
#
n.par.per.essay <- unlist(par.count[undisputed]) 
boxplot(n.par.per.essay ~ authors, horizontal=TRUE, 
        xlab="# of paragraphs per essay")

n.sen.per.essay <- lapply(sen.count[undisputed], unlist)
n.sen.per.essay <- lapply(n.sen.per.essay, sum)
n.sen.per.essay <- unlist(n.sen.per.essay)
boxplot(n.sen.per.essay ~ authors, horizontal=TRUE, 
        xlab="# of sentences per essay")

n.word.per.essay <- lapply(word.count[undisputed], function(x){lapply(x, unlist)})
n.word.per.essay <- lapply(n.word.per.essay, unlist)
n.word.per.essay <- lapply(n.word.per.essay, sum)
n.word.per.essay <- unlist(n.word.per.essay)
boxplot(n.word.per.essay ~ authors, horizontal=TRUE, xlab="# of words per essay")

n.char.per.essay <- lapply(char.count[undisputed], function(x){lapply(x, unlist)})
n.char.per.essay <- lapply(n.char.per.essay, unlist)
n.char.per.essay <- lapply(n.char.per.essay, sum)
n.char.per.essay <- unlist(n.char.per.essay)
boxplot(n.char.per.essay ~ authors, horizontal=TRUE, 
        xlab="# of (non-space) characters per essay")

# # of total words and sentences
n.sen <- sum(n.sen.per.essay)
n.word <- sum(n.word.per.essay)
n.char <- sum(n.char.per.essay)

# Obviously highly correlated
n.per.essay <- data.frame(n.par=n.par.per.essay, n.sen=n.sen.per.essay, 
                        n.word=n.word.per.essay)
round(cor(n.per.essay), 3)

# Lower correlations for non-Hamilton essays.  what is variance of correlations?
round(cor(n.per.essay[hamilton.index, ]), 3)
round(cor(n.per.essay[-hamilton.index, ]), 3)

#
# Per sentence metrics:  average sentence length
#
# Words per sentence.  Hard to figure out how to keep the exchangeable units 
# intact
n.word.per.sen <- unlist(word.count[undisputed])
n.word.per.sen <- unlist(n.word.per.sen)
# sanity check:  check number of sentences
stopifnot(n.sen == length(n.word.per.sen))

# For each sentence associate author
authors.sen <- rep(authors, times=n.sen.per.essay)

# Compare average # of words per sentence
word.per.sen <- data.frame(n.word=n.word.per.sen, author=authors.sen)
# ggplot(word.per.sen, aes(x=n.word)) + geom_histogram(aes(y=..density..), binwidth=5) + 
#   facet_grid(author ~ .)
# ggplot(word.per.sen, aes(x=n.word, fill=author)) + 
#   geom_histogram(aes(y=..density..), binwidth=5, alpha=.5, position="identity")
boxplot(n.word.per.sen ~ authors.sen, horizontal=TRUE, 
        xlab="# of words per sentence")
ggplot(word.per.sen, aes(n.word)) +
  geom_freqpoly(aes(y=..density.., group = author, colour = author), binwidth=10) +
  xlim(0,100) + ylab("proportion of sentences") + xlab("words per sentence")
model.word.per.sen <- lm(n.word.per.sen ~ authors.sen)
summary(model.word.per.sen)



#
# Per word metrics:  average char per word.  Is this useful?
#
n.char.per.word <- lapply(char.count[undisputed], function(x){lapply(x, unlist)})
n.char.per.word <- lapply(n.char.per.word, unlist)
n.char.per.word <- unlist(n.char.per.word)
# sanity check:  check number of sentences
stopifnot(n.char == sum(n.char.per.word))

# For each word associate author
authors.word <- rep(authors, times=n.word.per.essay)
boxplot(n.char.per.word ~ authors.word, horizontal=TRUE, 
        xlab="# of characters per word")
char.per.word <- data.frame(n.char=n.char.per.word, author=authors.word)
ggplot(char.per.word, aes(n.char)) +
  geom_freqpoly(aes(y=..density.., group = author, colour = author), binwidth=1) +
  xlim(0,15) + ylab("proportion of words") + xlab("word length")
model.char.per.word <- lm(n.char.per.word ~ authors.word)
summary(model.char.per.word)



#
# Figure out which words were used by each author
#
ham.words <- NULL
mad.words <- NULL
jay.words <- NULL
all.words <- NULL

# Compile all words
n.word.per.essay.2 <- rep(0,length(undisputed))
for (i in 1:length(undisputed)) {
  essay.num <- undisputed[i]
  words <- unlist(fed.papers[[essay.num]])
  words <- tolower(words)
  words <- gsub("[[:punct:]]", " ", words)
  words <- unlist(strsplit(words, " "))
  words <- words[!words == ""]
  words <- words[!words == " "]
  
  if (is.element(i, hamilton.index))
    ham.words <- c(ham.words, words)
  
  if (is.element(i, madison.index))
    mad.words <- c(mad.words, words) 

  if (is.element(i, jay.index))
    jay.words <- c(jay.words, words)
  
  n.word.per.essay.2[i] <- length(words)
  all.words <- c(all.words, words)
}

# Sanity check number of words per essay
stopifnot(all(n.word.per.essay.2 == n.word.per.essay))
rm(n.word.per.essay.2)

stopifnot(all(
  c(length(ham.words), length(jay.words), length(mad.words))
  == tapply(n.word.per.essay, authors, sum)
))

# Compute frequencies
ham.freq <- sort(table(ham.words), decreasing=TRUE)
n.words.ham <- sum(ham.freq)
ham.freq <- ham.freq/n.words.ham

mad.freq <- sort(table(mad.words), decreasing=TRUE)
n.words.mad <- sum(mad.freq)
mad.freq <- mad.freq/n.words.mad

jay.freq <- sort(table(jay.words), decreasing=TRUE)
n.words.jay <- sum(jay.freq)
jay.freq <- jay.freq/n.words.jay

all.freq <- sort(table(all.words), decreasing=TRUE)
n.words.all <- sum(all.freq)
all.freq <- all.freq/n.words.all



# Create list of "uninteresting" words.  Some judgement calls here.  Have to
# also consider those words I left in, like "we"
uninteresting.words <- 
  c("the", "of", "to", "and", "in", "a", "be", "that", "it", "is", "by", 
    "which", "as", "on", "have", "for", "not", "this", "will", "their",
    "or", "with", "are", "been", "from", "they", "may", "an", "would", "other",
    "has", "its", "these", "them", "than", "so", "such", "if", "any", "at",
    "into", "was", "had", "were", "who", "those", "each", "but", "upon",
    "only", "too", "when", "though", "much", "even", "also", "therefore",
    "very", "what", "without",
    # Dicier words to remove. Focus in on topics/subjects/themes
    "we", "all", "no", "more", "most", "his", "he", "either", "there",
    "can", "most", "every", "under", "could", "some")

mad.freq <- mad.freq[!is.element(names(mad.freq), uninteresting.words)]
ham.freq <- ham.freq[!is.element(names(ham.freq), uninteresting.words)]
jay.freq <- jay.freq[!is.element(names(jay.freq), uninteresting.words)]
all.freq <- all.freq[!is.element(names(all.freq), uninteresting.words)]


n.words <- 50
top.ham.words <- names(ham.freq)[1:n.words]
top.mad.words <- names(mad.freq)[1:n.words]
top.jay.words <- names(jay.freq)[1:n.words]
top.words <- names(all.freq)[1:n.words]


words <- top.ham.words
words.author <- "hamilton "

use.author <- "hamilton "
interest.words <- ham.words
non.interest.words <- c(jay.words, mad.words)


log.odds.ratio <- rep(0, n.words)
se <- rep(0, n.words)

for(i in 1:length(top.words)) {
  word <- words[i]
   
  p.interest <- sum(interest.words==word)/length(interest.words)
  p.non.interest <-  sum(non.interest.words==word) /
    length(non.interest.words)
  
  odds.ratio <- p.interest/(1-p.interest)
  odds.ratio <- odds.ratio / (p.non.interest/(1-p.non.interest))
  
  log.odds.ratio[i] <- log(odds.ratio)
  
  se[i] <- 
    1/sum(interest.words == word) + 
    1/sum(interest.words != word) +
    1/sum(non.interest.words == word) + 
    1/sum(non.interest.words != word)
  se[i] <- sqrt(se[i])
}

top.words.data <- data.frame(words=words, log.odds.ratio=log.odds.ratio,
                             se=se)
top.words.data$words <- 
  factor(top.words.data$words, levels=rev(top.words.data$words))

limits <- aes(ymax = log.odds.ratio + 1.96*se, ymin=log.odds.ratio - 1.96*se)
dodge <- position_dodge(width=0.9)

ggplot(top.words.data, aes(x=words,y=log.odds.ratio)) + 
  geom_bar(stat="identity") + 
  theme(text = element_text(size=20), 
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_errorbar(limits, position=dodge, width=0.25, col="red") + 
  geom_point(col="red", size=3) + 
  labs(
    x=sprintf("top %i %snon-uninteresting words (in order of use)", n.words, words.author),
    y=sprintf("log odds ratio of %suse vs rest use", use.author),
    title=sprintf("Use of Non-Uninteresting Words in %i Authorship-Undisputed Federalist Papers", 
                  length(undisputed))
    ) + coord_flip()



test.kind <- lapply(fed.papers[undisputed], function(x){grepl("kind of", x)})
test.kind <- lapply(fed.papers[undisputed], function(x){grepl("this kind", x)})
test.kind <- lapply(fed.papers[undisputed], function(x){grepl("a kind", x)})
test.kind <- lapply(fed.papers[undisputed], function(x){grepl("a kind of", x)})

test.kind <- lapply(fed.papers[undisputed], function(x){grepl(" kind ", x)})

authors[which(unlist(lapply(test.kind, any)))]



which(unlist(lapply(test.kind, any)))
i <- 11
fed.papers[[undisputed[i]]][which(test.kind[[i]])]


