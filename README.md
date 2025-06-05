# Analysis on Shakespeare

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

3. Analysis Overview

For this project, I conducted a sentiment analysis on six of Shakespeare’s plays:
	•	Tragedies: Macbeth, Romeo and Juliet, Hamlet
	•	Comedies: A Midsummer Night’s Dream, As You Like It, The Merchant of Venice

Objective

The goal was to determine whether sentiment analysis can help classify a play as a tragedy or comedy — a question debated in the fields of art, film studies, aesthetics, and philosophy. Instead of relying solely on theoretical discourse, this project explores what the data says.

Key Findings
	•	The BING sentiment analysis shows that Hamlet, Macbeth, and Romeo and Juliet have the lowest sentiment scores — with Macbeth and Romeo and Juliet even scoring negative. This aligns with the public perception of these as tragic plays.
	•	As You Like It, widely accepted as a comedy, has the highest positive sentiment score, supporting its classification.
	•	Analysis using NRC and AFINN lexicons is less conclusive, often showing more positive words even in tragedies.
	•	The Merchant of Venice presents complexity — often labeled a comedy in its historical context (e.g., mocking Jewish stereotypes), it scores high in “trust” words, which may mislead interpretation without deeper textual understanding.

