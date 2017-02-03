---
title: "R Notebook"
output: html_notebook
author: "KE HAN kh2793"
---

# Step 0: load data and data cleaning

The step 0 is to prepare the data, including data collecting and data cleaning. Also, I counted all the words in the speech, get the somekind panel data. I find that working with numerical data would tell us something. What's more, numerical representitive of the speech would include more tools in analyzing.

```{r}


```

# Step 1: analysis of the whole speech & visualization

The goal of this part is to integrate the analysis of sentiment/length. Some of the thought is derived from the Smart Data with R webpage. And the financial data is attached in the cvs file in the data folder.

```{r}
library(rJava)
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(ggrepel)
library(rio)

# Import Data
folder.path="../data/InauguralSpeeches/"
speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)

############
pres_party <- import("./data/further_files/Presidents_Party.csv")

files <- list.files("./data/")
files <- files[-length(files)]
lng_files <- length(files)

sentences <- data.frame(speech = rep(NA, lng_files),
            year_president = rep(NA, lng_files))

for (i in 1 : lng_files) {
    # import each individual speech
    file_path <- paste0("./data/", files[i])
    temp <- readLines(file_path)
    temp <- paste(temp, collapse = " ")  # concatenate all characters to one string
    sentences$speech[i] <- temp
    sentences$year_president[i] <- strsplit(files[i], split = ".", fixed = T)[[1]][1]
}

sentences <- sentSplit(sentences, "speech", verbose = F)

#polarity() is a function that provides sentiment analysis
pol <- polarity(sentences$speech, sentences$year_president)
pol_df <- pol$all
pol_df <- pol_df %>% dplyr::filter(!is.na(year_president))
pol_df$year_president <- as.factor(pol_df$year_president)
pol_df$pos.words <- NULL
pol_df$neg.words <- NULL

pol_group <- pol$group

# get party information
#################### not yet##########################
pol_group <- left_join(pol_group, pres_party, by = "year_president")
pol_group$party <- as.factor(pol_group$party)

pol_group$year_president <- gsub("_", " ", pol_group$year_president)

# Polarity vs. Mean Sentence Length
color_party <- c("blue", "green", "orange", "red", "grey", "brown")
g <- ggplot(pol_group, aes(x =total.words / total.sentences, 
               y = stan.mean.polarity))
g <- g + geom_point(aes(color = party,
            size = total.words/250),
            alpha = .9)
g <- g + geom_text_repel(aes(x =total.words / total.sentences, 
                 y = stan.mean.polarity,
                 label = factor(year_president)))
g <- g + scale_color_manual(values = color_party)
g <- g + xlab ("Mean Words in Sentence [-]")
g <- g + ylab ("Sentiment [-]")
g <- g + ggtitle ("Sentiment, Average Sentence Length, Speech Length and Party of US Inaugurations")
g <- g + theme_bw()
g
```

# Step 2: analysis of the speech style

Here, I define the speech as the style, which is represented as stopwords.

What mainly in this part, is the comparision between the centroal/trump/others. And the graph in the attached file show the clear distinction between Trump and others.

Actually, Trump enjoyed using WE and OUR, I think this is a way to gain trust from the others. And  it is somehow an exaggeration. Since, there is some many fighting back voice, WE and OUR is a way to get near to others, showing that everyone is on the same boat. Just like Obama, "Together we can!", closer relationship with everyone.

```{r}
stopword<-readline('stopwords_300.txt') 
stopwords = set([]) 
for line in file: 
  line = line.strip() 
stopwords.add(line) 
file.close() 
filenames = os.listdir('Speeches_Text') 
counts = {) centroid = () 
for filename in filenames: 
  file = open('Speeches_Text/' + filename, 'r') 
  counts[filename] = {} 
  length = 0 
  for line in file: 
    line = line.strip() 
  line = line.lower() 
  words = re.split('W+', line) 
  for word in words: 
    if len(word) > 0 and word in stopwords: 
    counts[filename][word] = counts[filename].get(word,0) + 1 
  length += 1 
  file.close() 
  for word in counts[filename]: 
    counts[filename][word] /= float(length) 
  # normalization centroid[word] = centroid.get(word,0) + counts[filename][word] 
centroid list = [] 
for word in sorted(centroid, key=centrold.get, reverse=True): 
  centroidlist.append(centrold[word]) 
cosine = {) 
for filename in counts: 
  speech_list = [] 
  for word in sorted(centroid, key=centrold.get, reverse=True): 
  if word in counts[filename]: speech_list.append(counts[filename][word]) 
else:
  speech_list.append(0.0) 
cosine[filename] = 1 - spatial.distance.cosine(speech_list, centroid list) 
for filename in sorted(cosine, key=cosine.get): 
  print filename, cosine[filename] 

```

# Step 3: analysis of the speech content (words)

Here, the analysis of the speech mostly work with words. 

## time trend

In the tutorial, teacher analysis the wording and find the time trend behind it. For example, the wording in 1920s maybe more harsh, about war. And nowadays, focused more on economy.

Here, I dig deepper into the area of speech's correlation with economy. Do the data mining work, and do the feature selection with all the words we have.

```{r}
#using the preprocess data with financial data(GDP, YoY, Financial market data, dollar price)
data<-read.csv("step2.csv")

```

## not about time

I know, there must be something across time, which is classic american, not changing with time trend.

From the tutorial, the length of sentence, the emotion of paragrph and the clustering of working are all about content. Here, I analyzed the us

```{r}
#using the same one from step1(using the word counting method for the non- stoping words)


```


# Step 4: summary

In the R Notebook, the analysis found out that  

After analysing the word data and the whole speech with text mining techniques, I think more work or comparsion might be the one between candidata and between candidata/presidental one. Since, it is more currently related. And it might be helpful for us to find out whether Trump is trust worth or not. For example, Trump has not change wording in candidata domination and presidental speech, then might some regulation would come soon, which is more relative to the people.






# reference
http://smartdatawithr.com/en/

http://avalon.law.yale.edu/subject_menus/inaug.asp

