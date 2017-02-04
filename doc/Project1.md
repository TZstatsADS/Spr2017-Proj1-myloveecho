---
title: "Project 1"
output: html_notebook
author: "KE HAN kh2793"
---
## Summary

Here, this notbook include 4 parts:

*  Data preprocessing

*  Sentimental and length analysis

*  Wording analysis

*  *  **A**. Speech style 

*  *  **B**. Wording to economy 

*  Summary

In the sentimental analysis and the length, I ploted all the information into a same plot. This plot shows the sentiment in the speech, and it generated some ideas for the further review.

Then, it comes to the wording analysis. It is quite similary to word cloud, since they both using the freqency of the words. And two parts are in it, the first is using stop words to analyzing speech style.  And the second is using other data, like yearly GDP, GDP growth, financial market data and CPI to crosss analyze the presidental inaugration speech. The inuition is the inaugration speech would reflect the president's wish and goal which would benefit or feeled by Americans. And what's more, this topic interests me because in clustering it would show sentimental anlysis's value. 

And in the wording to economy, some of the variable I used are derived from papers and articile I read before, some of the url link is in the reference.

Party/Change of party: one of the article I read analyzed the relationship  of party change to stock trends(that part is really interesting, since market trends tell us ahead of time, which party would win). So, I used the SP500 index to indicate the stock market. Also, I know that some student use NYSE or other index, but that is not reflecting overall American. The diviation of small company/ small market might because of how Trump promised to SMB (small-midium size business), so I looked at NYSE but not included here.

(Notice: Some function takes more than 10 minutes to run, recommend using the test instead of the step1&step2 real file)

## Step 0: load data & data cleaning & exploratory analysis

The step 0 is to prepare the data, including data collecting and data cleaning. Also, I counted words in the speech, get the somekind panel data. I find that working with numerical data would tell us something. What's more, numerical representitive of the speech would include more tools in analyzing.

And the methodology here, inculding left_join varibales and speech data, descriptive summary of speeches, reshape the data into data.frame which would be easy handle.

Counting the words with table function:
```{r,warning=F}
getwd()
#setwd("")
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
Using this trunk of code, select file of speech, this function can give out any top words of any president's speech.

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
#plot(step2$GDP)
#plot(step2$CPI)
```

A preview of the data we have, after those processing steps.

And some view of the GDP YoY can be seen here.

![image](https://img1.doubanio.com/view/note/large/public/p40447868.jpg)

![image](https://img1.doubanio.com/view/note/large/public/p40447727.jpg)
![image](https://img1.doubanio.com/view/note/large/public/p40447729.jpg)

### Some findings:

*  The GDP YoY can help us to devide all the speeches into 4 category.

*  The speech data can be analyzed using polarity, which is a direct way to get information.

*  Comparision of wording, cross analysis of data from other sources to further analyze

## Step 1: analysis of the whole speech & visualization

The goal of this part is to integrate the analysis of sentiment and length. Some of the thought is derived from the Smart Data with R webpage. Also, join the information from presidentinfo, to get the party infomation.

Here, all the steps would help us to get to the speech basic text analysis, including average number of words per sentence, sentiment, party membership and length of speech.

The steps are:

*  Library
*  Data Import
*  Sentence split & left_join of difference data source& sentimental analysis
*  Data visulization

Also, the first trunk is the analysis of Trump only. And the next includ all presidents.

```{r}
library(rJava)
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(ggrepel)
library(rio)
# test part, using small file to run the whole process 
#if this one not working, please use all<-readLines(file.choose())
t <- data.frame(speech = rep(NA, 1),
                 year_president = rep(NA, 1))
t$speech[1]=Trump
t$year_president[1]=2017
tt <- sentSplit(t, "speech", verbose = F)
pol <- polarity(tt$speech, tt$year_president)
pol_df <- pol$all
pol_df <- pol_df %>% dplyr::filter(!is.na(year_president))
pol_df$year_president <- as.factor(pol_df$year_president)
pol_df$pos.words <- NULL
pol_df$neg.words <- NULL
pol_group <- pol$group
pol_group$party <- as.factor(1)

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

![image](https://img1.doubanio.com/view/note/large/public/p40447627.jpg)

The previous image is the one with Trump only, and it is all about sentiment, average sentence/speech.

This is the whole process, which would help to draw the informative picture.

polarity() can be found here. [polarity score](https://www.rdocumentation.org/packages/qdap/versions/2.2.5/topics/polarity)


```{r}
#real process, with the whole speech and the whole run
# Import Data
# using the speech data from step0
# sentences
all<-readLines("all.txt")
sentences <- data.frame(speech = rep(NA, 58),
            year_president = rep(NA, 58))
for(i in 1:58){
  sentences$year_president[i]=2017-4*(i-1)
  sentences$speech[i]=all[i]
}
sentences <- sentSplit(sentences, "speech", verbose = F)

#polarity() is a function that provides sentiment analysis
# polarity function would last for a few minutes
pol <- polarity(sentences$speech, sentences$year_president)
pol_df <- pol$all
pol_df <- pol_df %>% dplyr::filter(!is.na(year_president))
pol_df$year_president <- as.factor(pol_df$year_president)
pol_df$pos.words <- NULL
pol_df$neg.words <- NULL
pol_group <- pol$group
```

Here, it is quite clear Sentiment numerical result can be used afterwards in trend analysis. Package polarity() is very useful here.

```{r}
# get party information
#pol_group <- left_join(pol_group, pres_party, by = "year_president")
#pol_group$party <- as.factor(pol_group$party)
# the join here, sometimes not work out, so, can use step0, which is already processed
pol_group$year_president <- gsub("_", " ", pol_group$year_president)

# Polarity vs. Mean Sentence Length
pol_group<-read.csv("step0.csv")
color_party <- c("blue", "green", "orange", "red", "grey", "brown")
g <- ggplot(pol_group, aes(x =total.words / total.sentences, 
               y = stan.mean.polarity))
g <- g + geom_point(aes(color = party,
            size = total.words/250),
            alpha = .9)
g <- g + geom_text_repel(aes(x =total.words / total.sentences, 
                 y = stan.mean.polarity,
                 label = factor(president)))
g <- g + scale_color_manual(values = color_party)
g <- g + xlab ("Mean Words in Sentence [-]")
g <- g + ylab ("Sentiment [-]")
g <- g + ggtitle ("Sentiment, Average Sentence Length, Speech Length and Party of US Inaugurations")
g <- g + theme_bw()
g
# if the first time it not working, try again. The missing values would be moved the second time.
```
Here, the text is a little bit large to fit in.
So, the following 2 plot shows the adjusted graphics.

![image](https://img3.doubanio.com/view/note/large/public/p40448385.jpg)
![iamge](https://img3.doubanio.com/view/note/large/public/p40448383.jpg)

### Some findings:

*  For different party, the difference is quite obvious. Especially the sentimental ones, since polarity() shows the sentiment (polarity) of text by grouping variable(s). Actucally  Maybe clustering would make sense here. Also, this is a proof to one of the articles I read. [Democrat vs. Republican](http://www.diffen.com/difference/Democrat_vs_Republican) And I think this is part of the common point I am looking for, that is althongh time change, president stayed same. Further analysis see part 2.

*  Trump uses the shortest sentences of all presidents, and his speech has a higher numerical sentiment compared to both Obama speeches. His speech is very short(but not the shortest one). 

*  Time trend exists. Adams speech in 1797 used longest sentneces. As the discussion in class, media trend and how people comunicate evolve. I also read some paper about this, one of the reason is that new words are coming up, some words mean more than one concept. See the link for more information. It also analyzed the complexity of the sentence, the structure and the visulization of sentences., showing same pattern. [Stylistic analysis](http://cdmd.cnki.com.cn/Article/CDMD-10487-2009036843.htm) [Writer use shorter sentences](https://www.reference.com/education/writers-use-short-sentences-740f5aad12fb3b24)

*  Lastly, this is a inuitively grapgic to further analysis on interesting topics for business purpose, and thus further test is needed here. Such as sentences getting shorter, Twitter can switch to shorter text. Instagram getting popular, since people are more interested in pictures and shorter sentences, so there is no need for long sentences. 


## Step 2: analysis of the speech style

Here, I define the speech as the style, which is represented as stopwords. As we start to learn English, we were taught that WOULD WILL SHOULD SHALL COULD CAN all differ. And one of the feeling I have here, is that people here in US are really polite and respective to people around, thus using less pushing words. Finding on how stopword differ is interesting in this sense.

What mainly in this part, is the comparision (using cosine similarity) between the central to all president and how Trump is different from others. And the graph in the attached file show the clear distinction between Trump and others.

Actually, Trump enjoyed using WE and OUR, I think this is a way to gain trust from the others. And  it is somehow an exaggeration. Since, there is some many fighting back voice, WE and OUR is a way to get near to others, showing that everyone is on the same boat. Just like Obama, "Together we can!", closer relationship with everyone. Obama and Trump both use WE and OUR more than other presidents.

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

The interesting thing I found is that, Trump is using less relative clause since there is less THAT. One of the reason might be the people who vote for him is different from the one used to be. Also the segmentation of the audience is different. See reference: [Reality Check: Who Voted for Donald Trump?](http://www.bbc.com/news/election-us-2016-37922587). Also, Trump is not using as much BE as others. One of the reason might be he is using WILL instead, another way to express future and deliver it to the Americans.

It is similiar to the discussion in class, that media is changing the way people speak. And evidence is that, people are using shorter sentences.

```{r}
## now, comparing only the stopwords & the GRAPHICS
# stopword<-readline('stopwords_300.txt') After trying the stopwords, I only list out the top 42 stopwords here.
stopwords<-c("the","of","and","to","in","our","a","we","that","is","be","it","for","have","by","will","which","with","as","not","this","i","are","all","their","but","us","on","has","from","my","its","or","can","no","they","been","must","so","who","an","at")
step2stop<-matrix(NA,ncol=2,nrow=length(stopwords))
j=1
for( i in stopwords){
  step2stop[j,1]=Trump_count[i]/2103
  step2stop[j,2]=Others_count[i]/130231
  j=j+1
}
colnames(step2stop) <- c("Trump", "Others")
rownames(step2stop) <- stopwords
barplot(t(step2stop), beside=T,horiz = T, ylab="usage of words", cex.names=0.8, las=2,col=c("darkblue","red"),legend.text = c("Trump", "Others") )
# then plot the as bar chart, it is quite distinct between Trump and all other president in using stopwords
barplot(t(step2stop), beside=T,horiz = T, ylab="usage of words", cex.names=0.8, las=2,col=c("darkblue","red"),legend.text = c("Trump", "Others") )
```
![image](https://img3.doubanio.com/view/status/median/public/5bdac5338dda38e.jpg)

Finding is that, first thing everyone would notice is that Trump use extremely less THE/OF/TO/A/THAT/IT. And extremely more WILL/WE. So, street language, more future tense, more close relationship are the thing define Trump.

Here, from the perspective of cosine similarity and comparison to centroid. The following chart showed the top 5 distinction president.

```{r}
# style comparision to other presidents, here we use the cosine similarity to compar major difference and similarity of presidents
library(proxy)
dist(m, method="cosine")
# the finding is not that interesting, so I just attached the plot here
```
![image](https://img3.doubanio.com/view/status/median/public/2775e430efec244.jpg)

### Some finding: 
*  Very interesting in stop words Trump is using. From the perspective of data science, it is clear to show that Trump’s speech turned out to be the second farthest from the centroid, right behind George W. Bush’s address of 2001. 

*  Trump was strong in using OUR and WE – and almost never said “I”. I think this is a pattern shown about exaggeration, and this is similar to Obama's 2013 speech

*  Trump is the all-time winner in using WILL (and interestingly, a large portion of his speech is in the future tense). Also, if looking at the actural sentences, Trump use WE’VE, the oral language. And no people did this before.

*  For the part of comparision between centriod to president, Trump is in top5. Although not Top1, it is different enough. 

*  Further analysis and underlying business usage would be, president speech is a way to reflect common people's life. 

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


Interesting methods are the one discussed in class, using categorical and sentimental analysis to get the sense of how president care about economy. And also, the word cloud also show the relationship of speech to time. The following graphics shows the time trend of word cloud.

![image](https://img3.doubanio.com/view/status/median/public/e14c6940a5fc26f.jpg)

But my model, I would use simple linear regression model on the data. I have a concern on this, since there is limited data, some regression issue might be hard to resolve.

If possible, maybe tree and logistic regression can be introduced.

```{r}
#using the preprocess data with financial data(GDP, YoY, Financial market data, dollar price)
#due to data constrain, the analysis only include 1940s till now
# run a linear model of these data


```

### not about time

I know, there must be something across time, which is classic american, not changing with time trend.

From the tutorial, the length of sentence, the emotion of paragrph and the clustering of working are all about content. Here, I analyzed the us

```{r}
#using the same one from step1(using the word counting method for the non- stoping words)
stopwords<-c("people","government","world","nation","great","country","new","peace","own","america","states","time","citizen","public","")
step2stop<-matrix(NA,ncol=2,nrow=length(stopwords))
j=1
for( i in stopwords){
  step2stop[j,1]=Trump_count[i]/2103
  step2stop[j,2]=Others_count[i]/130231
  j=j+1
}
colnames(step2stop) <- c("Trump", "Others")
rownames(step2stop) <- stopwords
barplot(t(step2stop), beside=T,horiz = T, ylab="usage of words", cex.names=0.8, las=2,col=c("darkblue","red"),legend.text = c("Trump", "Others") )

```


## Step 4: summary

The most interesting result I have see is that, Trump do differ from others. And hi


Also, from  

After analysing the word data and the whole speech with text mining techniques, I think more work or comparsion might be the one between candidata and between candidata/presidental one. Since, it is more currently related. And it might be helpful for us to find out whether Trump is trust worth or not. For example, Trump has not change wording in candidata domination and presidental speech, then might some regulation would come soon, which is more relative to the people.






# reference
http://smartdatawithr.com/en/
http://avalon.law.yale.edu/subject_menus/inaug.asp

