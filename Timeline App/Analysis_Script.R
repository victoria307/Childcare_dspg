
# Install Packages --------------------------------------------------------

library(tidyverse) 
library(tidytext)
library(tm) #textmining
library(quanteda) #quantitative analysis of text data
library(textdata) 
library(syuzhet) #sentiment modeling
library(RColorBrewer)
library(topicmodels) #topic modeling
library(SnowballC) #stemming


# Load Text ---------------------------------------------------------------

corpus_df <- list.files("/Users/gracemullins/DPSG/DPSG dir/Text/Text Analysis", full.names = TRUE)
view(corpus_df)
document_ids <- tools::file_path_sans_ext(basename(corpus_df))
print(document_ids)
