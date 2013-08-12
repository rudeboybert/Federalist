## This R script preprocesses the file federalist_papers.txt
## http://thomas.loc.gov/home/histdox/fedpaper.txt


fed.papers <- scan("federalist_papers.txt", "character", sep="\n")


# First pass
essay.starts <- NULL
for (i in 1:length(fed.papers)) {
  if (substr(fed.papers[i], 1, nchar("FEDERALIST.")) == "FEDERALIST.") {
    fed.papers[i] <- 
      paste("FEDERALIST No.", strsplit(fed.papers[i], "No. ")[[1]][2])
  } 
  if (substr(fed.papers[i], 1, nchar("FEDERALIST")) == "FEDERALIST") {    
    essay.starts <- c(essay.starts, i)
  }
}
essay.ends <- essay.starts - 1
essay.ends <- essay.ends[-1]
essay.ends <- c(essay.ends, length(fed.papers))
essay.index <- cbind(essay.starts, essay.ends)

# Two versions of 70
remove.row <- which(fed.papers[essay.starts] == "FEDERALIST No. 70")[2]
remove.indices <- essay.index[remove.row, 1] : essay.index[remove.row, 2] 

essay.index <- essay.index[-remove.row, ]
fed.papers <- fed.papers[-remove.indices]

# Second pass
essay.starts <- NULL
for (i in 1:length(fed.papers)) {
  if (substr(fed.papers[i], 1, nchar("FEDERALIST")) == "FEDERALIST") {    
    essay.starts <- c(essay.starts, i)
  }
}
essay.ends <- essay.starts - 1
essay.ends <- essay.ends[-1]
essay.ends <- c(essay.ends, length(fed.papers))
essay.index <- cbind(essay.starts, essay.ends)


n.essays <- nrow(essay.index)


authors <- rep("", n.essays)
new.york.text <- "To the People of the State of New York"
has.new.york.text <- rep(0, n.essays)

has.publius <- rep(0, n.essays)

possible.authors <- c("HAMILTON", "JAY", "MADISON", "HAMILTON OR MADISON", 
                      "HAMILTON AND MADISON")

for (i in 1:n.essays) {
  indices <- essay.index[i, 1] : essay.index[i, 2]
  essay.text <- fed.papers[indices]
  
  for (j in 1:length(essay.text)) {
    # given authors
    if( is.element(essay.text[j], possible.authors) ) {
      authors[i] <- essay.text[j]  
    }
    
    # preambles
    if(substr(essay.text[j], 1, nchar(new.york.text)) == new.york.text) {
      has.new.york[i] <- 1
    }
    
    # closers
    if(length(grep("PUBLIUS", essay.text[j], fixed=TRUE)) != 0) {
      has.publius[i] <- 1
    } 
  }  
}


# Clean up text
fed.papers.list <- vector(n.essays, mode="list")
for (i in 1:n.essays) {
  indices <- essay.index[i, 1] : essay.index[i, 2]
  essay.text <- fed.papers[indices]

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
  
  # Drop PUBLIUS
  if (substr(essay.text[end.index], 1, nchar("PUBLIUS")) == "PUBLIUS" ) {
    end.index <- end.index - 1
  } else {
    essay.text[end.index] <- strsplit(essay.text[end.index], "PUBLIUS")[[1]][1]
  }
  
  
  # pare down text
  essay.text <- essay.text[start.index:end.index]
  
  
  paragraph.starts <- NULL
  
  for(j in 1:length(essay.text)) {
    if(substr(essay.text[j], 1, 1) != " ") {
      paragraph.starts <- c(paragraph.starts, j)
    }  
  }
  
  paragraph.ends <- paragraph.starts - 1
  paragraph.ends <- c(paragraph.ends, length(essay.text))
  paragraph.ends <- paragraph.ends[-1]
  
  n.paragraphs <- length(paragraph.starts)
  
  paragraphs <- vector(n.paragraphs, mode="list")

  for(j in 1:n.paragraphs) {
    paragraphs[[j]] <- paste(essay.text[paragraph.starts[j]:paragraph.ends[j]], collapse="")
  }
    
  fed.papers.list[[i]] <- paragraphs
}




n.paragraphs <- unlist(lapply(fed.papers.list,length))
which(n.paragraphs > 100)
