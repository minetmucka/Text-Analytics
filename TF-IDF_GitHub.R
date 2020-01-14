
#########################################################################################################################################
## Objective: Perform Text Analysis preprocessing tasks and evaluate documentsimilarity via TF-IDF.                                     #
## Data source: crude in r                                                                                                              #
## Please install "tm" package: install.packages("tm") for Text Mining                                                                  #
## Please install "lsa" package: install.packages("lsa") for latent semantic analysis                                                   #
## Please install "qdap" package: install.packages("qdap") for bridging gap between qual and quant data                                 #
#########################################################################################################################################

install.packages("tm") 
install.packages("lsa")

# Load the library
library(tm)
library(lsa) 

# Load the test dataset
data(crude)


# Look at the help file of the dataset
?crude


# Display the raw text of the first document in the corpus and display.
writeLines(as.character(crude[[1]]))


# Transform document words to lower case
crude <- tm_map(crude, content_transformer(tolower))
writeLines(as.character(crude[[1]]))


# Remove punctuation from documents
crude <- tm_map(crude, removePunctuation)
writeLines(as.character(crude[[1]]))


#If see words like U.S., it thinks is us, then just search and replace those words... see below for code

# Remove stopwords from the corpus. You can add it to the stopword list if not there like said
crude <- tm_map(crude, removeWords, c(stopwords("english"), 'said'))
writeLines(as.character(crude[[1]]))

X <- stopwords("english")
# Remove numbers from the corpus
crude <- tm_map(crude, removeNumbers, x[x!='not']) #dont remove not, im doing sentiment analysis
writeLines(as.character(crude[[1]]))


# Stem the corpus
crude <- tm_map(crude, stemDocument, language = "english")
writeLines(as.character(crude[[1]]))

#lemmatize?
#crude <- tm_map(crude, lematize???, language = "english")
#writeLines(as.character(crude[[1]]))

# Build a document-term matrix using TF-IDF and inspect
crude.dt <- DocumentTermMatrix(crude, control=list(weighting=weightTfIdf))
crude.dt
inspect(crude.dt[1:8, 1:8]) #just showing first 3 examples


# Compute a matrix of cosine similarity scores between each document pair 
# and inspect
crude.cos <- cosine(as.matrix(t(crude.dt)))
crude.cos[1:8, 1:8]

# Save your text files in a directory and feed them into tm function Corpus
crude <-Corpus(DirSource("C:/Users/muckam/Desktop/DataScienceBootcamp/Text Analytics Fundamentals"), readerControl = list(language="en"))
writeLines(as.character(crude[[1]]))

# Improve the analysis! Re-run the analysis after further finessing your data

# Add to stopword list
crude <- tm_map(crude, removeWords, c(stopwords("english"), "said"))
writeLines(as.character(crude[[1]]))

# Check for mispellings/non-standard English words and replace with the correct words
#install.packages("qdap")
library(qdap) 
# You might have to specify your path to Java:
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.6.0_24') #Replace with path to your Java
for(i in 1:length(crude)){
  mispell <- which_misspelled(as.character(crude[[i]]), suggest=FALSE);
  print(i)
  print(mispell);
}

pattern <- c("copany", "organiaation", "US")
replacement <- c("company","organization")
for(i in 1:length(pattern)){
  crude <- tm_map(crude, content_transformer(gsub), pattern = pattern[i], replacement = replacement[i])
}
writeLines(as.character(crude[[1]]))

# Read csv file with a column storing the docs
some_text <- read.csv("C:/Users/muckam/Desktop/DataScienceBootcamp/Text Analytics Fundamentals/mytext.txt")
some_text <- data.frame(lapply(some_text, as.character), stringsAsFactors=FALSE)
some_data <-VectorSource(some_text[,1])
some_data_corp <- VCorpus(some_data)
writeLines(as.character(some_data_corp[[1]]))

