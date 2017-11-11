housing <- read.csv('C:/Users/awasthi/Desktop/landdata-states.csv', header = TRUE, sep = ",",na.strings = c("NA", "#N/A","" ))

#checking null
colSums(is.na(housing))
#saving nulls in the df
nullhousing <- subset(housing, is.na(housing$region))
head(nullhousing)

#checking the State for which region is null
unique(nullhousing$State)
#all the NAs are from State = DC
subset(housing, housing$State == 'DC')[,2]
#setting DC to south region as per https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
housing[housing$State=='DC',2] <- 'South'
#check NULL to verify NULL#

colnames(housing)
#aggregating the data based home value - by state
Hvalue_by_state <- aggregate(Home.Value ~ State, housing, FUN = sum)
sortedd <- Hvalue_by_state[order(Hvalue_by_state$Home.Value, decreasing = TRUE),]
top5 <- as.list(sortedd[1:5,1])
top10states <- as.list(sortedd[1:10,1])
#checking the top 3 states
#scatter plot
ggplot(subset(housing, State %in% (unlist(top5))), aes(x=Date, y=Home.Value, color=State))+geom_point()
#box-plot
#bplot <- ggplot(subset(housing, State %in% (unlist(top5))), aes(x=Date, y=Home.Value, color=State))+geom_boxplot()
#ggplot(subset(housing, State %in% (unlist(top5))), aes(x=State, y=Home.Value, color=State))+geom_boxplot()+geom_jitter(alpha=0.3, color='tomato')
#box plot for top 10 State vs HomeValue
ggplot(subset(housing, State %in% (unlist(top10states))), aes(x=State, y=Home.Value, color=State))+geom_boxplot()+geom_jitter(alpha=0.3, color='tomato')

hp.2007Q1 <- subset(housing, Date == '2007.25')

#let us take a closer look on scatter plot for 2007 1st Quarter - Land value vs Structure Cost
ggplot(hp.2007Q1, aes(x= Land.Value, y = Structure.Cost))+geom_point()
#since our x axis has a high variance - lets try to fit against a log
ggplot(hp.2007Q1, aes(x= log(Land.Value), y = Structure.Cost))+geom_point()
#color coding
ggplot(hp.2007Q1, aes(x= log(Land.Value), y = Structure.Cost, color = log(Land.Value)))+geom_point()
#Let us try to fit a line based on our X and Y and add it to your plot
hp.2007Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp.2007Q1))
p1 <- ggplot(hp.2007Q1, aes(x=log(Land.Value), y = Structure.Cost ))
#adding points and line
p1 + geom_point(aes(color = Home.Value))+geom_line(aes(y=pred.SC))
#observing states on the plot
p1 + geom_text(aes(label = State), size = 2, color = 'red')
#install.packages("ggrepel") #library("ggrepel")
#points with text
p1 + geom_point(color='red') + geom_text_repel(aes(label=State), size = 3)

#mapping variable to other aesthetics
p1 + geom_point(aes(color=Home.Value, shape = region))
#mapping variable to other aesthetics
p1 + geom_point(aes(size=Home.Value, shape = region, color = Home.Value))


#Statistical transformations
p2 <- ggplot(housing, aes(x=Home.Value))+geom_histogram(stat = 'bin', binwidth = 4000)
#top10states
top10 <- (sortedd[1:10,])
ggplot(top10, aes(x=State, y=Home.Value)) + geom_bar(stat = 'identity', color=Home.Value)
#all
ggplot(Hvalue_by_state, aes(x=State, y=Home.Value)) + geom_bar(stat = 'identity')

#Home value by Date and State
p3 <- ggplot(housing, aes(x=State, y=Home.Price.Index)) + theme(legend.position = 'top', axis.text = element_text(size = 6))
p3+geom_point(aes(color=Date), alpha=0.5,size=1.5)
#to avoid overlapping
p4 <- p3+geom_point(aes(color=Date), alpha=0.5,size=1.5, position = position_jitter(width = 0.25, height = 0))
#color coding
p4+scale_color_continuous(low = 'green', high = 'red')
#Facets
p5 <- ggplot(housing, aes(x=Date, y=Home.Value))
p5 + geom_line(aes(color=State))  #Difficult to distinguish???
#Facets can help here
p5 + geom_line()+facet_wrap(~State, ncol = 10)




###********###
#converting factor to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
hxls$Structure.Cost <- as.numeric.factor(hxls$Structure.Cost)
hxls$Land.Price.Index <- as.numeric.factor(hxls$Land.Price.Index)
#rounding off the cols
#hxls2 <- data.frame(lapply(hxls, function(y) if(is.numeric(y)) round(y, 0) else y))
#replacing cols values
hxls$Date <- gsub('Q2', '.50', hxls$Date)
#check string part
#grepl('Q2', hxls$Date[2])
#t1 <- hxls %>% filter(str_detect(Date, "Q4"))
#t2<- (as.integer(substring(t1$Date,1,4)))
#grepl('Q4', hxls$Date)
#getting all Q4 head(subset(hxls$Date,grepl('Q4', hxls$Date)),10)
#decimal -> format(t2, nsmall = 2)



