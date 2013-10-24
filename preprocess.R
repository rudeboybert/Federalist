#
# This R script preprocesses the following document:
# http://thomas.loc.gov/home/histdox/fedpaper.txt
# containing the Federalist Papers for analysis in R
#
# Page 5 of 
# http://writing.mit.edu/sites/writing.mit.edu/files/Who%20Wrote%20the%20Federalist%20Papers.pdf
# contains the two conflicting list of authors of the papers
#

# Input document: multiple line document
fed.papers <- scan("pg1404.txt", "character", sep="\n", blank.lines.skip = FALSE)


#
# First pass to identify starting and ending line of each essay
#
essay.starts <- NULL
for (i in 1:length(fed.papers)) {
  if (substr(fed.papers[i], 1, nchar("FEDERALIST No.")) == "FEDERALIST No.") {    
    essay.starts <- c(essay.starts, i)
  }
}
essay.ends <- essay.starts - 1
essay.ends <- essay.ends[-1]
essay.ends <- c(essay.ends, length(fed.papers))
essay.index <- cbind(essay.starts, essay.ends)

# 85 essays total
n.essays <- nrow(essay.index)


#
# Identify 
# -Which line has "To the People of the State of New York" intro (if at all)
# -Which line has Publius signature
# -Listed author
#
new.york.text <- "To the People of the State of New York"
has.new.york.text <- rep(0, n.essays)
has.publius <- rep(0, n.essays)

listed.authors <- rep("", n.essays)
possible.authors <- c("HAMILTON", "JAY", "MADISON", "HAMILTON OR MADISON", 
                      "MADISON, with HAMILTON")

for (i in 1:n.essays) {
  indices <- essay.index[i, 1] : essay.index[i, 2]
  essay.text <- fed.papers[indices]
  
  for (j in 1:length(essay.text)) {
    # ID intro to people of NY line
    if(substr(essay.text[j], 1, nchar(new.york.text)) == new.york.text) {
      has.new.york.text[i] <- 1
    }
    
    # ID Publius line
    if(length(grep("PUBLIUS", essay.text[j], fixed=TRUE)) != 0) {
      has.publius[i] <- 1
    } 
    
    # ID Listed author
    if(is.element(essay.text[j], possible.authors)) {
      listed.authors[i] <- essay.text[j]  
    }
  }  
}


#
# Cut all text
# -Prior to "To the People of the State of New York:" line
# -After Publius signature
# We leverage the fact that all 85 essays started with this intro, and all but
# essay 37 ended with the Publius signature.  Note all footnotes are therefore 
# removed.  
#
# We then cut up each essay into paragraphs, and store each paragraphs 
# separately.  Then store in new list 'fed.papers.list'. 
#
fed.papers.list <- vector(n.essays, mode="list")

for (i in 1:n.essays) {
  # Get text corresponding to that essay
  indices <- essay.index[i, 1]:essay.index[i, 2]
  essay.text <- fed.papers[indices]
  
  # ID starting and ending indices of the essay
  start.index <- 0
  end.index <- 0
  for(j in 1:length(essay.text)) {
    if(substr(essay.text[j], 1, nchar(new.york.text)) == new.york.text) {
      start.index <- j + 1
    }
    if(length(grep("PUBLIUS", essay.text[j], fixed=TRUE)) != 0) {
      end.index <- j
    }   
  }

  # Essay 37 was not signed
  if(i == 37) {
    end.index <- length(essay.text)
  } 
  
  # Drop Publius line
  if (substr(essay.text[end.index], 1, nchar("PUBLIUS")) == "PUBLIUS" ) {
    end.index <- end.index - 1
  } 
  
  # Pare down text
  essay.text <- essay.text[start.index:end.index]
  
  # Remove all possible leading and trailing blank lines
  while(essay.text[1] == "")
    essay.text <- essay.text[-1]
  
  while(essay.text[length(essay.text)] == "")
    essay.text <- essay.text[-length(essay.text)]  
  
  # Determine number of paragraphs
  n.paragraphs <- sum(essay.text == "") + 1  
  
  # Determine which lines start and end each paragraph
  paragraph.starts <- c(1, which(essay.text == "") + 1)
  paragraph.ends <- c(which(essay.text == "") - 1, length(essay.text))
  
  paragraph.ends <- paragraph.starts - 1
  paragraph.ends <- c(paragraph.ends, length(essay.text))
  paragraph.ends <- paragraph.ends[-1]

  # Store paragraphs in a list
  paragraphs <- vector(n.paragraphs, mode="list")
  for(j in 1:n.paragraphs) {
    paragraphs[j] <- 
      paste(essay.text[paragraph.starts[j]:paragraph.ends[j]], collapse=" ")
  }
  fed.papers.list[[i]] <- paragraphs
}


#
# Create data.frame of authors, disputed authors, and undisputed authors
#
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
authors.array <- data.frame(essay=1:n.essays, hamilton=hamilton.list, 
                            madison=madison.list, listed=listed.authors)


#
# Save objects of interest
#
fed.papers <- fed.papers.list
save(file="federalist.RData", authors.array, fed.papers)
