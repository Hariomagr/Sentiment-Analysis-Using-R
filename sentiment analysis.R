install.packages('twitteR')
install.packages('curl')
install.packages('tm')
install.packages('wordcloud')
install.packages(stringr)
require('twitteR')
require('curl')
require('tm')
require('wordcloud')
require('stringr')
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
#Use own keys
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
data <- searchTwitter("US+Trump",lang="en",n=1000,resultType="recent")
negative <- 0
positive <- 0
neutral <- 0
for(i in 1:1000){
	data_text <- sapply(data[i],function(x) x$getText())
	data_text <- removePunctuation(data_text)
	data_text <- tolower(data_text)
	data_text <- stripWhitespace(data_text)
	data_text <- removeNumbers(data_text)
	data_text <- removeWords(data_text,c("trump"))
	data_text <- removeWords(data_text,stopwords())
	wordcloud(data_text,random.order=F,colors=rainbow(50))
	data_text=str_split(data_text,pattern="\\s+")
	data_text <- unlist(data_text)
	pos_words <- scan('positive.txt',what='character',comment.char=';')
	neg_words <- scan('negative.txt',what='character',comment.char=';')
	positive_sentiment <- sum(!is.na(match(data_text,pos_words)))
	negative_sentiment <- sum(!is.na(match(data_text,neg_words)))
	score <- positive_sentiment - negative_sentiment
	if(score>0){
		positive <- positive+1
	}
	else if(score<0){
		negative <- negative+1
	}
	else{
		neutral <- neutral+1
	}
}
message("Positive: ",positive)
message("Negative: ",negative)
message("Neutral: ",neutral)