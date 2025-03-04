install.packages("tm")
install.packages("tidytext")

library(readr)
library(stringi)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)

#Import text - Source = 'https://hindi.blog' - add more to improve analysis reliance, then create Anki flashcards!
blogs<-read.csv("C:/Users/adamw/OneDrive - Centre for Sustainable Energy/Personal Projects/Hindi Conjunct Character Analysis/HIndi Blog Posts.csv")

#Remove numbers
blogs$clean_text <- gsub("[[:digit:]]", "", blogs$TEXT)

#Remove Latin characters (as source files were messy with some english phrases)
blogs$clean_text <- gsub("[A-z]","",blogs$clean_text)

#Remove white spaces
blogs$clean_text <- gsub("\\s+", "", blogs$clean_text)

#Remove matras and lone vowels - only constanant conjuncts wanted
blogs$characters_clean<- gsub("[\u0900-\u0914\u093E-\u094C]","",blogs$clean_text)

#Split by visible characters
blogs$characters<-str_split(blogs$characters_clean, "")

#Create fq dictionary and unlist individual columns
frequency_df <- as.data.frame(table(unlist(blogs$characters)))

#Isolate conjuncts only
conjuncts_only <- frequency_df[grep("\u094D", frequency_df$Var1), ]

#CReate a function for decomposing conjuncts ready for translation
decompose_hindi <- function(conjunct) {
  parts <- unlist(strsplit(conjunct, "\u094D"))
  combined<-paste(parts,collapse="+")
  return(combined)
}

#Order prior to printing
conjuncts_only<-conjuncts_only[order(conjuncts_only$Freq,decreasing = TRUE),]%>%
  mutate(decomposed=sapply(as.character(Var1),decompose_hindi))

#Export to excel and preserve Devangari
write_excel_csv(conjuncts_only, file="C:/Users/adamw/OneDrive - Centre for Sustainable Energy/Personal Projects/Hindi Conjunct Character Analysis/Conjunct Frequency Table.csv", col_names = F)
