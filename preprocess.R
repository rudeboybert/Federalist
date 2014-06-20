# Preprocess the Federalist Papers contained in pg1404.txt from the Project
# Gutenberg


# Input document: multiple line document
fed.papers <- scan("pg1404.txt", "character", sep="\n", 
                   blank.lines.skip = FALSE)
n.lines <- length(fed.papers)


# Identify starting and ending line of each essay
n.essays <- 85
essay.index <- data.frame(start=rep(0, n.essays), end=rep(0, n.essays))
counter <- 1

query <- "FEDERALIST No."
for (i in 1:n.lines) {
  if (substr(fed.papers[i], 1, nchar(query)) == query) {    
    essay.index$start[counter] <- i
    counter <- counter + 1
  }
}
essay.index$end[1:(n.essays-1)] <- essay.index$start[2:n.essays] - 1
essay.index$end[n.essays] <- n.lines


# For each essay:
# -ID author
# -Cut all text upto & including the "To the People of the State of New York:"
# line and including and after the "PUBLIUS" line. We leverage the fact that all
# 85 essays started with this NY line, and all but essay 37 ended with the
# PUBLIUS signature.  Note all footnotes are therefore removed.
NY.text <- "To the People of the State of New York:"
fed.papers.list <- vector(n.essays, mode="list")
listed.authors <- rep("", n.essays)
possible.authors <- c("HAMILTON", "JAY", "MADISON", "HAMILTON OR MADISON", 
                      "MADISON, with HAMILTON")

for (i in 1:n.essays) {
  # Get text corresponding to that essay
  essay.text <- fed.papers[essay.index[i, 1]:essay.index[i, 2]]
  
  # ID listed author and starting and ending indices of the essay
  for(j in 1:length(essay.text)) {
    if(is.element(essay.text[j], possible.authors)) {
      listed.authors[i] <- essay.text[j]  
    }
    if(substr(essay.text[j], 1, nchar(NY.text)) == NY.text) {
      start.index <- j
    }
    if(length(grep("PUBLIUS", essay.text[j], fixed=TRUE)) != 0) {
      end.index <- j
    }   
  }
  # Note essay 37 was not signed PUBLIUS
  if(i == 37) {
    end.index <- length(essay.text)
  } 
  
  # Drop "To the People of the State of New York" and "PUBLIUS" lines and 
  # leading/trailing whitespace lines
  if (substr(essay.text[start.index], 1, nchar(NY.text)) == NY.text ) {
    start.index <- start.index + 1
  } 
  while(essay.text[start.index] == "")
    start.index <- start.index + 1
  if (substr(essay.text[end.index], 1, nchar("PUBLIUS")) == "PUBLIUS" ) {
    end.index <- end.index - 1
  } 
  while(essay.text[end.index] == "")
    end.index <- end.index - 1

  # Pare down essay
  essay.text <- essay.text[start.index:end.index]
  
  # Write to file and save to list
  write(essay.text, file=sprintf("./essays/essay%02d.txt", i))
  fed.papers.list[[i]] <- essay.text
}


# #
# # Store in nested tree/list structures the
# # paragraph <- sentence <- word <- character counts
# # Note:
# # -sentences are cut using sentence token annotator function from package
# # -all words dropped to lower case for counting purposes
# # -words are assumed to be delineated by spaces
# # -all punctuation dropped to count words.  This is an issue for Hamilton in
# #  particular since he used words like "well-behaved" a few times
# #
# # Rename variable
# fed.papers <- fed.papers.list
# 
# par.count <- sen.count <- word.count <- char.count <- 
#   vector(length=n.essays, mode="list")
# 
# # Sentence tokenizer
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# 
# for (i in 1:n.essays) {
#   essay <- fed.papers[[i]]
#   n.par <- length(essay)
#   par.count[[i]] <- n.par
#   
#   # Further nested lists
#   sen.count[[i]] <- word.count[[i]] <- char.count[[i]] <-
#     vector(length=n.par, mode="list")  
#   
#   for (j in 1:n.par) {
#     par <- as.String(essay[[j]])
#     annotation <- annotate(par, sent_token_annotator)
#     sentences <- par[annotation]
#     n.sen <- length(sentences)
#     sen.count[[i]][[j]] <- n.sen
#     
#     # Further nested lists
#     word.count[[i]][[j]] <- char.count[[i]][[j]] <- 
#       vector(length=n.sen, mode="list") 
#     
#     for (k in 1:n.sen) {
#       sen <- sentences[[k]]
#       sen <- tolower(sen)
#       sen <- gsub("[[:punct:]]", " ", sen)
#       words <- unlist(strsplit(sen, " "))
#       words <- words[!words == ""]
#       words <- words[!words == " "]
#       
#       n.words <- length(words)
#       word.count[[i]][[j]][[k]] <- n.words
#       
#       # Further nested lists
#       char.count[[i]][[j]][[k]] <- 
#         vector(length=n.words, mode="list")
#       
#       for (l in 1:n.words){
#         char.count[[i]][[j]][[k]][[l]] <- nchar(words[l])
#       }
#     }
#   }
# }


# Create data.frame of authors, disputed authors, and undisputed authors. Page 5
# of the following document contains the two conflicting list of authors of the
# papers
# http://writing.mit.edu/sites/writing.mit.edu/files/Who%20Wrote%20the%20Federalist%20Papers.pdf
hamilton.list <- 
  c("HAMILTON", "JAY", "JAY", "JAY", "JAY", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON AND MADISON", 
    "HAMILTON AND MADISON", "HAMILTON AND MADISON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "MADISON", "MADISON", "MADISON", "MADISON", 
    "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", 
    "MADISON", "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "JAY", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON")
madison.list <-
  c("HAMILTON", "JAY", "JAY", "JAY", "JAY", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", "MADISON", "MADISON", 
    "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "MADISON", 
    "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", 
    "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", 
    "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", "MADISON", 
    "MADISON", "MADISON", "MADISON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "MADISON", "MADISON", "JAY", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", 
    "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON", "HAMILTON"
  )
authors <- data.frame(hamilton=hamilton.list, madison=madison.list, 
                      listed=listed.authors)


# Save objects of interest
fed.papers <- fed.papers.list
save(file="federalist.RData", authors, fed.papers)
