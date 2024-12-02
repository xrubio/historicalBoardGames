library(RSQLite)
library(ggplot2)
library(dplyr)
library(gridExtra)

library(tidyverse)
# process corpus
library(tm)
# stemming
library(SnowballC)
# find optimal number of topics
library(ldatuning)
# data manipulation
library(reshape2)
library(topicmodels)
library(tidytext)
library(Rtsne)
library(reshape2)    

conn <- dbConnect(RSQLite::SQLite(), "historical_games.db")

############################## FIGURE 5 - History as a theme 

# historical
historical <- dbGetQuery(conn, "select * from historical")
# scifi
scifi <- dbGetQuery(conn, "select * from game where game.type=='game' and owned>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Science Fiction'))")
# zombies
zombies <- dbGetQuery(conn, "select * from game where game.type=='game' and owned>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Zombies'))")
# fantasy
fantasy <- dbGetQuery(conn, "select * from game where game.type=='game' and owned>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Fantasy'))")



scifi$genre <- "scifi"
zombies$genre <- "zombies"
fantasy$genre <- "fantasy"
historical$genre <- "historical"
genres <- rbind(scifi, zombies, fantasy, historical)
freqGenres <- genres %>% group_by(year,genre)  %>% summarise(freq=n())

pdf("05_production.pdf", width=12, height=4)
ggplot(freqGenres, aes(x=year, y=freq, col=genre)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(values=c("skyblue3","indianred2","palegreen4", "goldenrod2")) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + ylab("games published")+ annotate("label", x = 2022, y=freqGenres[freqGenres$genre=="fantasy" & freqGenres$year==2021,]$freq, label = "fantasy", fill="skyblue3", col="white", size=5, hjust=0) + annotate("label", x = 2022, y=freqGenres[freqGenres$genre=="scifi" & freqGenres$year==2021,]$freq, label = "scifi", fill="palegreen4", col="white", size=5, hjust=0) + annotate("label", x = 2022, y=freqGenres[freqGenres$genre=="zombies" & freqGenres$year==2021,]$freq, label = "zombies", fill="goldenrod2", col="white", size=5, hjust=0) + annotate("label", x = 2022, y=freqGenres[freqGenres$genre=="historical" & freqGenres$year==2021,]$freq, label = "historical", fill="indianred2", col="white", size=5, hjust=0) + ylab("games published") + scale_x_continuous(breaks=seq(1970,2021,5), limits=c(1970, 2025)) + theme(legend.position="none") 
dev.off()

############################## FIGURE 6 - Temporal Relevance Index

gamesPeriods <- dbGetQuery(conn, "select * from ( select gamePeriod.*, period.* from gamePeriod inner join period on gamePeriod.id_period=period.id) as periodDetails inner join historical on historical.id=periodDetails.id_game")
gamesPeriods$id <- NULL

weighted <- gamesPeriods %>% 
  # Need to operate by row, so group by row number
  group_by(r=row_number()) %>% 
  # Create nested list column containing the sequence for each pair of Start, End values
  mutate(custom = list(begin:end)) %>% 
  # Remove the row-number column, which is no longer needed
  ungroup %>% select(-r) %>% 
  # Unnest the list column to get the desired "long" data frame
  unnest()

weighted$weight <- 1/(1+weighted$end-weighted$begin)

# define periods
a <- subset(weighted, custom>=-500 & custom<1453)
a$periodSplit <- "<1453" 
b <- subset(weighted, custom>1453 & custom<1789)
b$periodSplit <- "1453-1789"
c <- subset(weighted, custom>=1789 & custom<1900)
c$periodSplit <- "1789-1900"
d <- subset(weighted, custom>=1900 & custom<2022)
d$periodSplit <- "1900-2022"
weighted <- rbind(a,b,c,d)

weightedSum <- weighted %>% group_by(custom,periodSplit) %>% summarise_at(vars(weight), list(sum = sum)) 


g1 <- ggplot(weightedSum, aes(x=custom, y=sum, col=periodSplit, fill=periodSplit)) + geom_bar(stat="identity") + theme_bw() + xlab("year") + ylab("Temporal Relevance Index") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+ scale_fill_manual(values=c("skyblue3","indianred2","palegreen4","goldenrod2")) + scale_color_manual(values=c("skyblue3","indianred2","palegreen4","goldenrod2")) + scale_x_continuous(breaks=c(-500,0,500,1000,1500,2000)) + theme(legend.position="none") 

g2 <- ggplot(weightedSum, aes(x=custom, y=sum, col=periodSplit, fill=periodSplit)) + geom_bar(stat="identity") + theme_bw() + xlab("") + ylab("") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + scale_color_manual(values=c("skyblue3","indianred2","palegreen4","goldenrod2"))+ scale_fill_manual(values=c("skyblue3","indianred2","palegreen4","goldenrod2")) + theme(legend.position="none") + facet_wrap(~periodSplit, scales="free", ncol=2)+ theme(plot.margin = unit(c(0.3, 1, 0.3, 0.3), "cm"))

pdf("06_tri.pdf", width=18, height=9)
grid.arrange(g1,g2, ncol=1)
dev.off()

############################### Spatial Relevance Index 

# get all games and their periods

gamesLocation <- dbGetQuery(conn, "select historical.id, historical.title, historical.year, gameLocation.id_place, gameLocation.country, gameLocation.region, gameLocation.city from historical inner join (select gamePlace.*, place.* from gamePlace inner join place on place.id==gamePlace.id_place) as gameLocation on historical.id==gameLocation.id_game")

write.csv(gamesLocation, "gamesLocation.csv", row.names=F)


############################### Topic Modelling

## PARAMETERS

# minimum number of times a term needs to be present in the corpus to be taken into account
minimumFrequency <- 5

## 1 - collect the 4 periods from the database (ancient, medieval & modern, 19th, 20th)
       
p1 <- dbGetQuery(conn, "select * from historical where historical.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>-501 and end<1453))")
p1$period <- "<1453" 
p2 <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and cast(year as integer)>=1970 and cast(year as integer)<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>=1453 and end<1789))")
p2$period <- "1453-1789"
p3 <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and cast(year as integer)>=1970 and cast(year as integer)<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>=1789 and end<1900))")
p3$period <- "1789-1900"
p4 <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and cast(year as integer)>=1970 and cast(year as integer)<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>=1900 and end<2022))")
p4$period <- "1900-2021"

# here we add the required column names for the preprocessing (doc_id & text)  

createRequiredColumns <- function( documents )
{
	# the dataframe needs 2 columns: doc_id and text
	documents$doc_id <- documents$id
	documents$id <- NULL
	documents$text <- documents$description
	documents$description <- NULL
	return(documents)
}
p1 <- createRequiredColumns(p1)
p2 <- createRequiredColumns(p2)
p3 <- createRequiredColumns(p3)
p4 <- createRequiredColumns(p4)

## 2 - preprocessing of corpus

# custom list of words to ignore
wordsToIgnore <- c("game","play","use", "includ", "player", "point", "action", "one", "round", "end", "phase", "hand", "board", "turn", "two", "piec", "color", "will", "gain", "take", "becom", "can", "dice", "roll", "differ", "also", "die", "rule", "base", "system", "set", "must", "complet", "first", "publish", "new", "even", "version", "design", "edit", "mechan", "compon", "like", "—", "type", "three", "four", "five", "six", "deck", "show", "best", "win", "tri", "need", "offer", "make", "may", "score", "draw", "start", "number", "valu", "anoth", "draw", "token", "seri", "veri", "book", "possibl", "level", "direct", "either", "reach", "help", "get", "mani", "befor", "come", "way", "now", "back", "just", "good", "time", "age", "call", "page", "ani", "choos", "next", "special", "abil", "part", "skill", "lost", "determin", "featur", "order", "begin", "add", "onli", "event", "year", "much", "find", "allow", "well", "scenario", "follow", "•", "activ", "chit", "great", "cover", "centuri", "dure", "hour", "day", "print", "mile", "scale", "hex", "area", "card", "counter", "provid", "easi", "wwii", "learn", "present", "second", "chart", "sheet", "booklet", "full", "map", "repres", "contain", "simpl", "tabl", "—descript", "victori", "box", "face", "onc", "option", "reveal", "bonus", "last", "winner", "around", "long", "everi", "howev", "want", "bring", "right", "keep", "look", "solitair", "basic", "quick", "per", "standard", "diecut", "size", "half", "entir", "updat", "suitabl", "chang", "attempt", "war", "wargam", "unit", "combat", "miniatur", "fought", "histor", "period")


createCorpus <- function( documents )
{
	# final list is custom words + standard english words
	stopWords <- tm::stopwords("en")
	stopWords <- c(stopWords, wordsToIgnore)

	corpus <- Corpus(DataframeSource(documents))
	processedCorpus <- tm_map(corpus, content_transformer(tolower))
	removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
	processedCorpus <- tm_map(processedCorpus, removeSpecialChars) 
	processedCorpus <- tm_map(processedCorpus, removeNumbers)
	processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
	processedCorpus <- tm_map(processedCorpus, removeWords, stopWords)
	# remove dashes after deletion of words with dashes (some times dashes were stil present)
#	processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = FALSE)
	processedCorpus <- tm_map(processedCorpus, stripWhitespace)
	return(processedCorpus)
}

p1Corpus <- createCorpus(p1)
p2Corpus <- createCorpus(p2)
p3Corpus <- createCorpus(p3)
p4Corpus <- createCorpus(p4)

# creation of the Document-Term Matrix
createDTM <- function( corpus )
{
	DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
	return(DTM)
}

getPresentIdx <- function(DTM)
{
	presentIdx <- slam::row_sums(DTM) > 0
	return(presentIdx)
}

removeEmptyRows <- function( presentIdx, dataset )
{
	# because some games do not have any term repeated in other documents we have empty rows (e.g. rows with no terms)
	# LDA will not like this so we remove them from the analysis
	return(dataset[presentIdx,])
}

p1DTM <- createDTM(p1Corpus)
p1Idx <- getPresentIdx(p1DTM)    
p1DTM <- removeEmptyRows(p1Idx, p1DTM)
p1 <- removeEmptyRows(p1Idx, p1)

p2DTM <- createDTM(p2Corpus)
p2Idx <- getPresentIdx(p2DTM)    
p2DTM <- removeEmptyRows(p2Idx, p2DTM)
p2 <- removeEmptyRows(p2Idx, p2)

p3DTM <- createDTM(p3Corpus)
p3Idx <- getPresentIdx(p3DTM)    
p3DTM <- removeEmptyRows(p3Idx, p3DTM)
p3 <- removeEmptyRows(p3Idx, p3)

p4DTM <- createDTM(p4Corpus)
p4Idx <- getPresentIdx(p4DTM)    
p4DTM <- removeEmptyRows(p4Idx, p4DTM)
p4 <- removeEmptyRows(p4Idx, p4)

## 3 - explore metrics to choose optimal number of topìcs

plotMetrics <- function( df )
{
	g1 <- ggplot(df, aes(x=topics, y=CaoJuan2009)) + geom_line() + geom_point() + ggtitle("minimize") + theme_bw() + scale_x_continuous(breaks=seq(min(df$topics), max(df$topics,1)))

	g2 <- ggplot(df, aes(x=topics, y=Deveaud2014)) + geom_line() + geom_point() + ggtitle("maximize") + theme_bw()+ scale_x_continuous(breaks=seq(min(df$topics), max(df$topics,1)))
	grid.arrange(g1, g2)
}

p1TopicsFit <- ldatuning::FindTopicsNumber(p1DTM, topics = seq(from = 4, to = 10, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )
p2TopicsFit <- ldatuning::FindTopicsNumber(p2DTM, topics = seq(from = 4, to = 10, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )
p3TopicsFit <- ldatuning::FindTopicsNumber(p3DTM, topics = seq(from = 4, to = 10, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )
p4TopicsFit <- ldatuning::FindTopicsNumber(p4DTM, topics = seq(from = 4, to = 10, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )

plotMetrics(p1TopicsFit)
plotMetrics(p2TopicsFit)
plotMetrics(p3TopicsFit)
plotMetrics(p4TopicsFit)

## based on previous runs
p1NumTopics <- 7
p2NumTopics <- 7
p3NumTopics <- 5
p4NumTopics <- 8

## 4 - fit LDA with optimal num topics
p1LDA <- LDA(p1DTM, p1NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/p1NumTopics, seed=77))
p2LDA <- LDA(p2DTM, p2NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/p2NumTopics, seed=77))
p3LDA <- LDA(p3DTM, p3NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/p3NumTopics, seed=77))
p4LDA <- LDA(p4DTM, p4NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/p4NumTopics, seed=77))

## 5 - interactive visualization to identify clusters of topics or too large topics
library(LDAvis)
topicmodels2LDAvis <- function(x, ...){
    post <- topicmodels::posterior(x)
    if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
    mat <- x@wordassignments
    LDAvis::createJSON(
        phi = post[["terms"]], 
        theta = post[["topics"]],
        vocab = colnames(post[["terms"]]),
        doc.length = slam::row_sums(mat, na.rm = TRUE),
        term.frequency = slam::col_sums(mat, na.rm = TRUE)
    )
}
# remove dirs if they exist
unlink("visP1", recursive=TRUE)
unlink("visP2", recursive=TRUE)
unlink("visP3", recursive=TRUE)
unlink("visP4", recursive=TRUE)

serVis(topicmodels2LDAvis(p1LDA), out.dir = 'visP1', open.browser = TRUE)
serVis(topicmodels2LDAvis(p2LDA), out.dir = 'visP2', open.browser = TRUE)
serVis(topicmodels2LDAvis(p3LDA), out.dir = 'visP3', open.browser = TRUE)
serVis(topicmodels2LDAvis(p4LDA), out.dir = 'visP4', open.browser = TRUE)


## 6 - get top 10 terms with highest beta per topic and period 
p1Beta <- tidy(p1LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
p1Beta$topic <- paste("Topic n.",as.character(p1Beta$topic))
p1Beta$period <- "Ancient & Medieval" 
p2Beta <- tidy(p2LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
p2Beta$topic <- paste("Topic n.",as.character(p2Beta$topic))
p2Beta$period <- "Medieval & Renaissance"
p3Beta <- tidy(p3LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
p3Beta$topic <- paste("Topic n.",as.character(p3Beta$topic))
p3Beta$period <- "19th century"
p4Beta <- tidy(p4LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
p4Beta$topic <- paste("Topic n.",as.character(p4Beta$topic))
p4Beta$period <- "20th century"

betas <- rbind(p1Beta, p2Beta, p3Beta, p4Beta)


g1 <- ggplot(p1Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", nrow=1) + theme(legend.position="none") + theme_bw() + ggtitle("Ancient and Medieval") + scale_y_reordered()+ ylab("top terms") + xlab("term weight for topic")

g2 <- ggplot(p2Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", nrow=1) + theme(legend.position="none") + theme_bw() + ggtitle("Renaissance and Modern") + scale_y_reordered()+ ylab("top terms") + xlab("term weight for topic")

g3 <- ggplot(p3Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", nrow=1) + theme(legend.position="none") + theme_bw() + ggtitle("19th century") + scale_y_reordered()+ ylab("top terms") + xlab("term weight for topic")

g4 <- ggplot(p4Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", nrow=1) + theme(legend.position="none") + theme_bw() + ggtitle("20th century") + scale_y_reordered()+ ylab("top terms") + xlab("term weight for topic")

pdf("barplots.pdf", width=16, height=12)
grid.arrange(g1,g2,g3,g4, ncol=1)
dev.off()



# list top 10 terms
terms(p1LDA, 10)
terms(p2LDA, 10)
terms(p3LDA, 10)
terms(p4LDA, 10)

# 8 - temptative name for topics based on top10 terms
p1TopicNames <- c('Medieval Warfare','Alexander the Great','Greek-Persian Wars','England','Roman Empire','Ancient Warfare','Punic Wars')
p2TopicNames <- c('City building','North America','Trade','Japan','Politics','Gunpowder battles','Empire management')
p3TopicNames <- c('Napoleonic Wars','Naval warfare','Colonialism','Tactics','Operational')
p4TopicNames <- c('Eastern Front','Tactics','Operational','Western Front','Pacific Front','Naval Warfare','Air Warfare','Grand Strategy')

# 9 - Identify top games per topic (1 example per corpus)
p1Gamma <- tidy(p1LDA, matrix ="gamma")
p2Gamma <- tidy(p2LDA, matrix ="gamma")
p3Gamma <- tidy(p3LDA, matrix ="gamma")
p4Gamma <- tidy(p4LDA, matrix ="gamma")

p1ExampleTopic <- which(p1TopicNames=='Medieval Warfare')
p1Top <- p1Gamma  %>% filter(topic==p1ExampleTopic) %>% slice_max(gamma, n=10)
subset(p1, doc_id %in% p1Top$document)$title
subset(p1, doc_id %in% p1Top$document)$text

p2ExampleTopic <- which(p2TopicNames=='Empire management')
p2Top <- p2Gamma  %>% filter(topic==p2ExampleTopic) %>% slice_max(gamma, n=20)
subset(p2, doc_id %in% p2Top$document)$title
subset(p2, doc_id %in% p2Top$document)$text

p3ExampleTopic <- which(p3TopicNames=='Colonialism')
p3Top <- p3Gamma  %>% filter(topic==p3ExampleTopic) %>% slice_max(gamma, n=20)
subset(p3, doc_id %in% p3Top$document)$title
subset(p3, doc_id %in% p3Top$document)$text

p4ExampleTopic <- which(p4TopicNames=='Pacific Front')
p4Top <- p4Gamma  %>% filter(topic==p4ExampleTopic) %>% slice_max(gamma, n=20)
subset(p4, doc_id %in% p4Top$document)$title
subset(p4, doc_id %in% p4Top$document)$text

# 10 t-SNE

p1Matrix <- acast(p1Gamma, document ~ topic, value.var="gamma")
p1TSNE <- Rtsne(p1Matrix, check_duplicates=FALSE)
p1MainTopic <- apply(p1Matrix, MARGIN = 1, FUN = which.max)
p1DF <- data.frame(x=p1TSNE$Y[,1], y=p1TSNE$Y[,2], topic=p1TopicNames[p1MainTopic])
p1NamePos <- p1DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
g1 <- ggplot(p1DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=p1NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE Ancient & Medieval") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") + ggtitle("Ancient & Medieval")

p2Matrix <- acast(p2Gamma, document ~ topic, value.var="gamma")
p2TSNE <- Rtsne(p2Matrix, check_duplicates=FALSE)
p2MainTopic <- apply(p2Matrix, MARGIN = 1, FUN = which.max)
p2DF <- data.frame(x=p2TSNE$Y[,1], y=p2TSNE$Y[,2], topic=p2TopicNames[p2MainTopic])
p2NamePos <- p2DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
g2 <- ggplot(p2DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=p2NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE Renaissance & Modern") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") + ggtitle("Renaissance & Modern")

p3Matrix <- acast(p3Gamma, document ~ topic, value.var="gamma")
p3TSNE <- Rtsne(p3Matrix, check_duplicates=FALSE)
p3MainTopic <- apply(p3Matrix, MARGIN = 1, FUN = which.max)
p3DF <- data.frame(x=p3TSNE$Y[,1], y=p3TSNE$Y[,2], topic=p3TopicNames[p3MainTopic])
p3NamePos <- p3DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
g3 <- ggplot(p3DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=p3NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE 19th century") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") + ggtitle("19th century")

p4Matrix <- acast(p4Gamma, document ~ topic, value.var="gamma")
p4TSNE <- Rtsne(p4Matrix, check_duplicates=FALSE)
p4MainTopic <- apply(p4Matrix, MARGIN = 1, FUN = which.max)
p4DF <- data.frame(x=p4TSNE$Y[,1], y=p4TSNE$Y[,2], topic=p4TopicNames[p4MainTopic])
p4NamePos <- p4DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
g4 <- ggplot(p4DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.3, size=2) + geom_label(data=p4NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE 20th century") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") + ggtitle("20th century")


pdf("tsne.pdf", width=12, height=12)
grid.arrange(g1,g2,g3,g4, ncol=2)
dev.off()



## finish DB connection

dbDisconnect(conn)

