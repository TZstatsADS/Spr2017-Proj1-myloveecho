---
title: "Project 1"
output: html_notebook
author: "KE HAN kh2793"
---
## Summary

Here, this notbook include 4 parts:

##### - data preprocessing

##### - sentimental and length analysis

##### - wording analysis

######     ---- speech style 

######     ---- wording to economy 

##### - summary

In the sentimental analysis and the length, I ploted all the information into a same plot. This plot shows the sentiment in the speech, and it generated some ideas for the further review.

Then, it comes to the wording analysis. It is quite similary to word cloud, since they both using the freqency of the words. And two parts are in it, the first is using stop words to analyzing speech style.  And the second is using other data, like yearly GDP, GDP growth, financial market data and CPI to crosss analyze the presidental inaugration speech. The inuition is the inaugration speech would reflect the president's wish and goal which would benefit or feeled by Americans. And what's more, this topic interests me because in clustering it would show sentimental anlysis's value. 

And in the wording to economy, some of the variable I used are derived from papers and articile I read before, some of the url link is in the reference.

Party/Change of party: one of the article I read analyzed the relationship  of party change to stock trends(that part is really interesting, since market trends tell us ahead of time, which party would win). So, I used the SP500 index to indicate the stock market. Also, I know that some student use NYSE or other index, but that is not reflecting overall American. The diviation of small company/ small market might because of how Trump promised to SMB (small-midium size business), so I looked at NYSE but not included here.

## Step 0: load data and data cleaning

The step 0 is to prepare the data, including data collecting and data cleaning. Also, I counted words in the speech, get the somekind panel data. I find that working with numerical data would tell us something. What's more, numerical representitive of the speech would include more tools in analyzing.

And the methodology here, inculding left_join varibales and speech data, descriptive summary of speeches, reshape the data into data.frame which would be easy handle.

Counting the words with table function:
```{r}
#get the word count for further analysis
#to avoid reproductivity problem, use "file.choose()"
Test<-readLines(file.choose())
# to lower
Test<-tolower(Test)
#process, using , . and space to seperate the sentence
Test.words <- strsplit(Test, split = "[ |. |, |  ]")
Test_count<-table(Test.words)
Test_count<-sort(Test_count,decreasing = TRUE)
head(Test_count,10)
tail(Test_count)
```

Left join the tables with financial data:
```{r}
Test <-readLines(file.choose())
Financials<-readLines(file.choose())
Test <- left_join(Test, Financials, by = "year_president")
#some of the errors/unmatch in president speech time was corrected in the finance file, so the following analysis would use step2.csv, which is a processed data set, covering from 1930s, by when there is GDP, CPI and financial market data
```

Combine all speeches into one file, making it easy to compare others with Trump:
```{r}
#files <- list.files("./data/InauguralSpeeches/")
#sometimes the previous line would not work, using file.choose would avoid problems
files <- list.files(file.choose())
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
}
```

## Step 1: analysis of the whole speech & visualization

The goal of this part is to integrate the analysis of sentiment/length. Some of the thought is derived from the Smart Data with R webpage. And the financial data is attached in the cvs file in the data folder.

```{r}
library(rJava)
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(ggrepel)
library(rio)

# Import Data
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

## Step 2: analysis of the speech style

Here, I define the speech as the style, which is represented as stopwords.

What mainly in this part, is the comparision between the centroal/trump/others. And the graph in the attached file show the clear distinction between Trump and others.

Actually, Trump enjoyed using WE and OUR, I think this is a way to gain trust from the others. And  it is somehow an exaggeration. Since, there is some many fighting back voice, WE and OUR is a way to get near to others, showing that everyone is on the same boat. Just like Obama, "Together we can!", closer relationship with everyone.

```{r,warning=FALSE}
# all about Trump Speech style
# if needed, truning sentences into one long string
# HC <- paste(HC, collapse = " ")
# load
Trump<-readLines("Trump.txt")
# to lower
Trump<-tolower(Trump)
#process
Trump.words <- strsplit(Trump, split = "[ |. |, |  ]")
Trump_count<-table(Trump.words)
Trump_count<-sort(Trump_count,decreasing = TRUE)
head(Trump_count,10)
tail(Trump_count)
############ comparision########
Others<-readLines("Others.txt")
Others<- paste(Others, collapse = " ")
Others<-tolower(Others)
Others.words<-strsplit(Others,split = "[ |. |, |  ]")
Others_count<-table(Others.words)
Others_count<-sort(Others_count,decreasing = TRUE)
head(Others_count,10)
tail(Others_count)
```

Here, the top words already differ from Trump to others. The number here is the absolute time the word mentioned. But the usage of words, and their rank do show something. Here, he mentioned WILL, AMERICA, ALL, YOU, and the words he did not frequently mentioned are THAT, BE, IT, BY.

The interesting thing I found is that, Trump is using less relative clause since there is less that. One of the reason might be the people who vote for him is different from the one used to be. In some new, they mentioned that, the segmentation of the audience is different. See reference: [Reality Check: Who Voted for Donald Trump?](http://www.bbc.com/news/election-us-2016-37922587). Also, Trump is using shorter sentences in his speech (not listed here, see attachedment referals). And he is not using as much BE as others. One of the reason might be he is using WILL instead, another way to express future and deliver it to the Americans.

It is similiar to the discussion in class, that media is changing the way people speak. And evidence is that, people are using shorter sentences.

```{r}
## now, comparing only the stopwords & the GRAPHICS
# stopword<-readline('stopwords_300.txt') this is a trying methods, after analyzing the stopwords, it is not reproductable here. So I only list out the top 10 stopwords


# style comparision to other presidents

```

![image](https://media.licdn.com/mpr/mpr/AAEAAQAAAAAAAAfPAAAAJDhiOWQ5ZjYzLTUzYTctNDNkMy04M2FmLTIzY2RiZmY2NjZjZg.jpg)


## Step 3: analysis of the speech content (words)

Here, the analysis of the speech mostly work with words. 

### time trend

In the tutorial, teacher analysis the wording and find the time trend behind it. For example, the wording in 1920s maybe more harsh, about war. And nowadays, focused more on economy. And to me, I think this part is very important, since it is most relevant part to people's daily life.

Here, I dig deepper into the area of speech's correlation with economy. Do the data mining work, and do the feature selection with all the words we have.

```{r}
#in the data processing part, I use let_join to get the GDP/YoY/Financial Market/CPI data into one single sheet
# since the data of GDP only from 1929 great depression, so only analyze part of the data after 1930s
step2<-read.csv("step2.csv")
str(step2)
# lots of the data in the sheet would turned into factor variable
```

## Topic model 15 topics!!!!!

```{r}
#using the preprocess data with financial data(GDP, YoY, Financial market data, dollar price)
#due to data constrain, the analysis only include 1929-2017

data<-read.csv("step2.csv")

```

### not about time

I know, there must be something across time, which is classic american, not changing with time trend.

From the tutorial, the length of sentence, the emotion of paragrph and the clustering of working are all about content. Here, I analyzed the us

```{r}
#using the same one from step1(using the word counting method for the non- stoping words)


```


## Step 4: summary

The most interesting result I have see is that, Trump do differ from others. And hi


Also, from  

After analysing the word data and the whole speech with text mining techniques, I think more work or comparsion might be the one between candidata and between candidata/presidental one. Since, it is more currently related. And it might be helpful for us to find out whether Trump is trust worth or not. For example, Trump has not change wording in candidata domination and presidental speech, then might some regulation would come soon, which is more relative to the people.






# reference
http://smartdatawithr.com/en/
http://avalon.law.yale.edu/subject_menus/inaug.asp

