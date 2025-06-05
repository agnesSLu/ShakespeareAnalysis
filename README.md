# Analysis on Shakespeare
# Shihua Lu
# This project was completed in 2022 when I was at the University of Chicago.

1. packages used:
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(wordcloud)
library(ggplot2)
library(dplyr)

2. data source: https://www.gutenberg.org
I downloaded texts from https://www.gutenberg.org. Meanwhile, I made some modification to the text, i.e: I deleted the scanner's note, to ensure my data visualization to be as accurate as possible. To get the same output, please use the data I upload, or you can get yours and perform similar steps to get your conclusions.

2. Analysis:
For HW09, I am doing a sentiment analysis for Shakespeare's works. I have chosen three tragedies: Macbeth, Romeo and Juliet, Hamlet and three comedies: Midnight Summer's Dream, As you Like it, The Merchant of Venice. The goal of this project is to see whether the results of my sentiment analysis can show if a play is a tragedy or comedy. The definition of what a tragedy is is always under debate in the field of Art, film studies, Aesthetics and philosophy. Sometimes I get a little bored, just for reading too much theoretical content. Let's see if the debate can be solved by data analysis. 

From the graph "Sentiment Analysis Score, Using BING Package," we can see that Hamlet, Macbeth and Romeo have the lowest sentiment score, by where romeo and macbeth have negative score, meaning that numbers of negative words surpass that of positive words. This result, we can say, coincides with people's understandings of these plays. Interestingly, the ONLY play that can be grouped as a comedy by the scholars and general public is As you Like it, which has the highest sentiment score. 

From the NRC plots, we can see that for every play, there are still more positive words than negative ones. It is a little hard to make a conclusion only from the graphs. Same as the AFinn package. That said, to achieve what I hope to do, the BING package is the most helpful one. 

However, what is tricky is that, for some plays such as the Merchant of Venice, can be considered as a comedy, only if considered together with the historical background - to make fun of the jewish people. However, this play still gets a high sentiment score from our analysis, and more ironically, this play has a lot~ of words that are related to "trust." Only from this graph, we do not know how these words are used specifically - this may lead people to misunderstand the play.

3. For more information, please see: https://cfss.uchicago.edu/homework/text-analysis/

4. Reflection:
I failed to use the glue() package (I tried several times and it just did not work), so some of my plots failed to have a very instructional titles. However, at the same time, even though I may have successfully used the glue() package, since I did not use the full name of the plays when I import data, the output may look very awkward as well. 

