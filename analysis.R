#
# Analysis on Federalist Papers
#

# Load libraries needed for language processing + data
library(openNLP) ## Loads the package for use in the task
library(openNLPmodels.en) ## Loads the model files for the English language
library(ggplot2)
load("federalist.RData")
n.essays <- length(fed.papers)


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
# Store in nested tree/list structures the
# paragraph <- sentence <- word <- character counts
# Note:
# -sentences are cut using sentDetect function from package
# -all words dropped to lower case for counting purposes
# -words are assumed to be delineated by spaces
# -all punctuation dropped to count words.  This is an issue for Hamilton in
#  particular since he used words like "well-behaved" a few times
par.count <- sen.count <- word.count <- char.count <- 
  vector(length=n.essays, mode="list")

for (i in 1:n.essays) {
  essay <- fed.papers[[i]]
  n.par <- length(essay)
  par.count[[i]] <- n.par
  
  # Further nested lists
  sen.count[[i]] <- word.count[[i]] <- char.count[[i]] <-
    vector(length=n.par, mode="list")  
  
  for (j in 1:n.par) {
    par <- essay[[j]]
    sentences <- sentDetect(par, language="en")
    n.sen <- length(sentences)
    sen.count[[i]][[j]] <- n.sen
    
    # Further nested lists
    word.count[[i]][[j]] <- char.count[[i]][[j]] <- 
      vector(length=n.sen, mode="list") 
    
    for (k in 1:n.sen) {
      sen <- sentences[[k]]
      sen <- tolower(sen)
      sen <- gsub("[[:punct:]]", "", sen)
      words <- unlist(strsplit(sen, " "))
      n.words <- length(words)
      word.count[[i]][[j]][[k]] <- n.words
      
      # Further nested lists
      char.count[[i]][[j]][[k]] <- 
        vector(length=n.words, mode="list")
      
      for (l in 1:n.words){
        char.count[[i]][[j]][[k]][[l]] <- nchar(words[l])
      }
    }
  }
}


#
# Per essay metrics.  Nothing sexy except John Jay didn't do shit
#
n.par <- unlist(par.count[undisputed]) 
boxplot(n.par ~ authors, horizontal=TRUE, xlab="# of paragraphs per essay")

n.sen <- lapply(sen.count[undisputed], unlist)
n.sen <- lapply(n.sen, sum)
n.sen <- unlist(n.sen)
boxplot(n.sen ~ authors, horizontal=TRUE, xlab="# of sentences per essay")

n.word <- lapply(word.count[undisputed], function(x){lapply(x, unlist)})
n.word <- lapply(n.word, function(x){lapply(x, sum)})
n.word <- lapply(n.word, unlist)
n.word <- lapply(n.word, sum)
n.word <- unlist(n.word)
boxplot(n.word ~ authors, horizontal=TRUE, xlab="# of words per essay")

# Character per essay part too much useless info
# lapply(char.count, function(x){lapply(x, function(y){lapply(y, unlist)})})

# Obviously highly correlated
n.per.essay <- data.frame(n.par=n.par, n.sen=n.sen, n.word=n.word)
round(cor(n.per.essay), 3)

round(cor(n.per.essay[hamilton.index, ]), 3)
round(cor(n.per.essay[-hamilton.index, ]), 3)


#
# Per paragraph metrics.  Are these really interesting?  Seems kind of arbitray
# as to how people cut up paragraphs
#


#
# Per sentence metrics:  average sentence length
#
# Words per sentence.  Hard to figure out how to keep the exchangeable units 
# intact
n.word <- unlist(word.count[undisputed])
n.word <- unlist(n.word)

n.sen <- sen.count[undisputed]
n.sen <- lapply(n.sen, unlist)
n.sen <- lapply(n.sen, sum)
n.sen <- unlist(n.sen)
authors.sen <- rep(authors, times=n.sen)

boxplot(n.word ~ authors.sen, horizontal=TRUE, xlab="# of words per sentence")

temp <- data.frame(n.word=n.word, author=authors.sen)
ggplot(temp, aes(x=n.word)) +  geom_histogram(aes(y=..density..), binwidth=5) + 
  facet_grid(author ~ .)
ggplot(temp, aes(x=n.word, fill=author)) + 
  geom_histogram(aes(y=..density..), binwidth=5, alpha=.5, position="identity")


#
# Per word metrics:  average char per word
#
n.char <- unlist(unlist(unlist(char.count[undisputed])))

n.word <- word.count[undisputed]
n.word <- unlist(n.word)

authors.word <- 
  rep(authors, 
      times=unlist(lapply(lapply(word.count[undisputed], unlist), sum))
  )

boxplot(n.char ~ authors.word, horizontal=TRUE, xlab="# of characters per word")

temp <- data.frame(n.char=n.char, author=authors.word)
ggplot(temp, aes(x=n.char)) +  geom_histogram(aes(y=..density..), binwidth=5) + 
  facet_grid(author ~ .)
ggplot(temp, aes(x=n.char, fill=author)) + 
  geom_histogram(aes(y=..density..), binwidth=5, alpha=.5, position="identity")


#
# Figure out which words used in each
#
hamilton.text <- NULL
madison.text <- NULL

for (i in 1:n.essays) {
  text <- unlist(fed.papers.list[[i]])
  text <- tolower(text)
  text <- gsub("[[:punct:]]", "", text)
  text <- unlist(strsplit(text, " "))
  
  if (is.element(i, hamilton))
    hamilton.text <- c(hamilton.text, text)
  
  if (is.element(i, madison))
    madison.text <- c(madison.text, text) 
}

hamilton.freq <- sort(table(hamilton.text), decreasing=TRUE)
n.words.hamilton <- sum(hamilton.freq)
hamilton.freq <- hamilton.freq/n.words.hamilton
madison.freq <- sort(table(madison.text), decreasing=TRUE)
n.words.madison <- sum(madison.freq)
madison.freq <- madison.freq/n.words.madison


uninteresting.words <- 
  c("the", "of", "to", "and", "in", "a", "be", "that", "it", "is", "by", 
    "which", "as", "on", "have", "for", "not", "this", "will", "their",
    "or", "with", "are", "been", "from", "they", "may", "an", "would", "other",
    "has", "its", "these", "them", "than", "so", "such", "if", "any", "at",
    "into", "was", "had", "were", "who", "those", "each", "but", "upon",
    "only", "too", "when", "though", "much", "even", "also", "therefore",
    "very")

madison.freq <- madison.freq[!is.element(names(madison.freq), uninteresting.words)]
hamilton.freq <- hamilton.freq[!is.element(names(hamilton.freq), uninteresting.words)]

madison.freq[1:50]*100
hamilton.freq[1:50]*100

plot(hamilton.freq, log='xy', type='n', xlab="ith word",
     ylab="word frequency")
lines(madison.freq, col="black", pch='.')
lines(hamilton.freq, col="red", pch='.')
legend("topright", 
       legend=c("Madison", "Hamilton"), 
       col=c("black", "red"),
       lty=c(1,1),
       bty='n')

n.words <- 100
top.hamilton.words <- names(hamilton.freq)[1:n.words]
top.madison.words <- names(madison.freq)[1:n.words]

# hamilton used but not madison
setdiff(top.hamilton.words, top.madison.words)
# madison used but not hamilton
setdiff(top.madison.words, top.hamilton.words)


# Observations
# Hamilton used "well-grounded"