#   -----------------------------------------------------------------------------------
# Analyze Facebook-fCC-data



# PREPARE WORKSPACE  ------------------------------------------------------------------


# Read in packages
library(tidyverse)
library(RCurl)
library(lubridate)
library(modelr)

# Load this Facebook dataset from FreeCodeCamp on github
gitURL <- getURL("https://raw.githubusercontent.com/freeCodeCamp/open-data/master/facebook-fCC-data/data/freeCodeCamp-facebook-page-activity.csv")
(fb <- read_csv(gitURL))



# FORMAT DATA -------------------------------------------------------------------------

# Address (convert) variable class
fb <- fb %>% 
  mutate_each(mdy, "date") %>%
  mutate_each(as.numeric, "reactions")



# Check where the warning is coming from (ok - na value only for reactions)
fb %>%
  group_by(type, reactions) %>%
  filter(is.na(reactions) | is.nan(reactions)) %>% 
  summarize(n())

  
# Add proportion of clicks per users reached & proporiton of reactions per user clicks
(fb <- mutate(fb,
             propClicks = clicks / reach,
             propReactions = reactions / reach))

  
  
# EXPLORATORY DATA ANALYSIS  ----------------------------------------------------------
  
## Explore how interactions (clicks and reactions) vary by type
  
# Check sample size by type
# status and public should be removed due to very few observations
type_n <- fb %>% 
  group_by(type) %>% 
  summarize(
    Qty_PostType = n()
  )
fb <- left_join(fb, type_n)
fb <- fb %>% 
  mutate(Type = paste0(type, "\nn = ", Qty_PostType))


# Visualize - Clicks by type
fb %>% 
  filter(!type %in% c("Status","Public")) %>% 
  ggplot() +
  geom_boxplot(aes(type, propClicks), varwidth = T)


fb %>% 
  filter(!type %in% c("Status","Public")) %>% 
  ggplot(aes(fill = type)) +
  geom_violin(aes(Type, propReactions)) + 
  labs(x = "", y = "Reaction Rate") + 
  theme(legend.position = "none", text = element_text(size=rel(4.5)))



# Visualize - Reactions by type for those that were clicked on
fb %>% 
  filter(!type %in% c("Status","Public")) %>% 
  ggplot() +
  geom_boxplot(aes(type, propReactions), varwidth = T)

fb %>% 
  filter(!type %in% c("Status","Public")) %>% 
  ggplot() +
  geom_violin(aes(type, propReactions))



## Explore how interactions vary by date and time

# Date Range
range(fb$date)


# Visulize the timeframe for these articles
# most of the video articals were run oct 2016 to jan. 2017
# number of articles posted with link increases over time
# number of articles posted with photo has stayed steady
# number of articles posted with video have declined

ggplot(fb) +
  geom_freqpoly(aes(date)) +
  ylab("Number of posts")


fb %>% 
  filter(!type %in% c("Public", "Status")) %>% 
  ggplot() +
  geom_histogram(aes(date, fill = type)) +
  facet_wrap(~ type, nrow=1)
fb %>% 
  filter(!type %in% c("Public", "Status")) %>% 
  ggplot(aes(x = date, y = ..density..)) +
  geom_freqpoly(aes(color = type), bins = 20)


fb %>% 
  filter(!type %in% c("Public", "Status")) %>% 
  ggplot() +
  geom_histogram(aes(date))

fb %>% 
  filter(!type %in% c("Public", "Status")) %>% 
  group_by(date) %>% 
  mutate(n_date = n()) %>% 
  ggplot(aes(x = date, y = n_date)) +
  geom_point()


# add variables
fb <- fb %>% 
  mutate(week = week(date),
         year = year(date),
         month = month(date, label = T, abbr = T),
         day = day(date),
         hour = as.numeric(substr(time, 1, 2)),
         min = as.numeric(substr(time, 4, 5)),
         datetime = make_datetime(year, month, day, hour, min),
         label = paste0(month, "-", year))



fb %>% 
  group_by(type) %>% 
  summarize(Average_Click = mean(clicks), Average_Reach = mean(reach))

fb %>% 
  ggplot(aes(date, reach, col=type)) +
  geom_point() + 
  labs(y="Users Reached", x = "")





# Visualize Time
# most posts occure between [0900 & 1900]
# all types (link, photo, video) occure throughout the 24-hour day, somewhat
# no need to subset to time period; don't want to loose video data

# Aggregate by week
fb_week <- fb %>% 
  filter(!type %in% c("Public", "Status") & reach < 100000) %>% 
  group_by(week, year) %>% 
  summarize(n_posts = n(),
            sum_reach = sum(reach),
            sum_click = sum(clicks),
            mean_clicks = mean(clicks),
            mean_propClicks = mean(propClicks),
            mean_propReaction = mean(propReactions),
            mean_reachedPerPost = sum_reach / n_posts) %>% 
  arrange(year, week)
print(fb_week, width = Inf)

fb_week$weekNo <- seq(1, nrow(fb_week))

# look at a linear model
summary(lm(n_posts ~ weekNo, data = fb_week))

labels <- unique(fb[,c("week","month")])
labels <- left_join(labels, fb_week[,c("week", "weekNo")])
labels <- labels[order(labels$weekNo), ]
label <- NULL
for(i in unique(labels$month)){
  labelx <- labels[labels$month == i, ]
  labelx <- labelx[labelx$weekNo == min(labelx$weekNo), ]
  label <- rbind(label, labelx)
}

# fb %>% 
#   filter(!type %in% c("Public", "Status")) %>% 
#   ggplot(aes(datetime)) + 
#   geom_freqpoly(binwidth = 604800) +  # second in a week
#   geom_abline(intercept = 4.857, slope = 0.19) +
#   labs(y = "Posts per week", x = "")

fb_week %>% 
  mutate(reached1000 = sum_reach/1000) %>% 
  ggplot(aes(weekNo, reached1000)) + 
  geom_line() +  
  geom_smooth(method = "lm") +  
  labs(y = "Total thousand users reached", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                     labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))

fb_week %>% 
  mutate(clicked1000 = sum_click/1000) %>% 
  ggplot(aes(weekNo, clicked1000)) + 
  geom_line() +  
  geom_smooth(method = "lm") +  
  labs(y = "Total thousand user clicks", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                     labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))


ggplot(fb_week, aes(weekNo, n_posts)) + 
  geom_line() +  # second in a week
  geom_smooth(method = "lm") +  # second in a week
  labs(y = "Posts per week", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                   labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))
 
  
fb_week %>% 
  ggplot(aes(weekNo, mean_reachedPerPost)) + 
  geom_line() +  
  geom_smooth(method = "lm") +  # second in a week
  labs(y = "Average users reached per post per week", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                     labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))

fb_week %>% 
  ggplot(aes(weekNo, mean_propClicks)) + 
  geom_line() +  
  geom_smooth(method = "lm") +  
  labs(y = "Average number of clicks per post per week", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                     labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))


## look at this by type
ggplot(fb_week, aes(weekNo, n_posts, col = type)) + 
  geom_line() +  # second in a week
  geom_smooth(method = "lm") +  # second in a week
  labs(y = "Posts per week", x = "") +
  scale_x_continuous(breaks = label$weekNo,
                     labels = label$month) +
  theme(axis.line = element_line(),
        text = element_text(size=rel(4)))



fb_week_type <- fb %>% 
  filter(!type %in% c("Public", "Status") & reach < 100000) %>% 
  group_by(type, week, year) %>% 
  summarize(n_posts = n(),
            sum_reach = sum(reach),
            mean_propClicks = mean(propClicks),
            mean_propReaction = mean(propReactions),
            mean_reachedPerPost = sum_reach / n_posts) %>% 
  arrange(year, week)
print(fb_week, width = Inf)

fb_week_type <- left_join(fb_week_type, fb_week[,c("week", "weekNo")])

fb_week_type %>% 
  ggplot(aes(weekNo, mean_propClicks)) + 
  # geom_smooth(method = "lm") +  # second in a week
  geom_line() +
  labs(y = "Average proportion of clicks per week", x = "")




# Inspect the outlier in reach
# Suspect this is an error since it is so different from the rest
# Without being able to understand the reason, it will be dropped from analysis. 
fb %>% 
  filter(reach > 100000)


# Visualize how many people were reached by date posted
fb %>% 
  filter(reach < 100000 & !type %in% c("Status","Public")) %>% 
  ggplot() +
  geom_point(aes(date, reach, color = type))


## Remove data from analysis dataset
fbdat <- fb %>% 
  filter(reach < 100000 &  # outlier          
           !type %in% c("Public", "Status")  # too few observations from these groups (not sure what they are)
  )


## Initial conclusions
# articals with videos and photos get more clicks than those without
# users are not influenced by photos or videos when providing reactions
# I suspect that subject matter has something to do with user click

## Assumptions
# I assume 'time' is when the article was posted
# I assume that reactions were calculated over the same length of time (perhaps 24-hours) for
# each article


## Look at buzzwords

# Remove quotes in titles and make lowercase
str_replace(fbdat$title, '"', '')     # replaces the first match

str_replace(fbdat$title[4], '"', '')
fbdat$title[8]
"An interaction designer explains how a \"homeless iPhone\" might work."  

fb <- fb %>% 
  mutate(title = str_to_lower(gsub('"', '', title)))
fb$title

# Find important key words in artical titles that might lead to user clicks
eachword <- fb %>% 
  separate_rows(title)

eachword %>% 
  group_by(title) %>% 
  summarize(
    Count = n()
  ) %>% 
  arrange(desc(Count)) %>% 
  print(n=150)

eachword %>% 
  filter(title == "his")

# # Common subject words
# buzzwords <- c("developer", "https", "code", "free", "open", "coding","job", "developers",
#                "google", "programming", "javascript", "source", "data", "help",
#                "freecodecamp", "courses", "his", "people", "learn", "science", "amazon",
#                "interview","learning", "fun", "good", "her")

library(tm)
library(wordcloud)
library(SnowballC)
article_titles <- str_to_lower(fbdat$title) #pulls the titles into a chr vector
article_corpus <- tm::VectorSource(article_titles)
article_corpus <- tm::Corpus(article_corpus) #these two commands prepare the word matrix
article_corpus <- tm::tm_map(article_corpus, removePunctuation)     #removes Punctuation
article_corpus <- tm::tm_map(article_corpus, removeWords, stopwords('english')) # remove stopwords
article_corpus <- tm::tm_map(article_corpus, removeNumbers)
article_corpus <- tm::tm_map(article_corpus, stemDocument) #turns verb conjugations into their stems
article_corpus <- tm::tm_map(article_corpus, stripWhitespace)
# article_corpus <- tm::tm_map(article_corpus, stemCompletion, commonWords$word,
#                              type = c("shortest")) #
inspect(article_corpus)
wordcloud::wordcloud(article_corpus, max.words = 80, random.order = FALSE)

summary(article_corpus)
as.tibble(article_corpus)
as.character(commonWords$word)
typeof(commonWords$word)
article_corpus[[1]]$content
article_corpus$cont
fbdat$title[364]


# Isolate common terms
commonWords_tdm <- article_corpus %>% 
  tm::TermDocumentMatrix() 
commonWords_tdm <- removeSparseTerms(commonWords_tdm, 0.99)
commonWords <- commonWords_tdm %>% as.matrix()
commonWords <- tibble(Terms = rownames(commonWords),
                      freq = rowSums(commonWords)) %>% 
  arrange(desc(freq))
  

# Explore
head(commonWords)
tail(commonWords)
commonWords_tdm
inspect(commonWords_tdm)
findFreqTerms(commonWords_tdm, 10)
findAssocs(commonWords_tdm, "data", 0.25)


# Get the average proportion of clicks for articals containing each buzzword
wordClicks <- NULL
for(word in findFreqTerms(commonWords_tdm, 12)){
  modeldf <- fbdat %>% 
    filter(str_detect(str_to_lower(title), word))
  wordclicksx <- tibble(Buzzword = word,
                        avg.prop.clicks = mean(modeldf$propClicks),
                        sd = sd(modeldf$propClicks),
                        n = nrow(modeldf))
  wordClicks <- rbind(wordClicks, wordclicksx)
}


# Sample size range for these buzzwords
range(wordClicks$n)


# Visualize clicks by buzzword
ggplot(wordClicks, aes(x = reorder(Buzzword, avg.prop.clicks), y =avg.prop.clicks)) +
  geom_point() +
  coord_flip() + 
  labs(y = "Average proportion of clicks for articals containing term", x = "Term")


# Visualize buzzword article sample size
ggplot(wordClicks, aes(x = reorder(Buzzword, avg.prop.clicks), y = n)) +
  geom_point() +
  coord_flip()


# Visualize what time of day artical with buzzword was posted
word <- "freecodecamp"
fbdat %>% 
  filter(str_detect(str_to_lower(title), word)) %>% 
  mutate(Time = as.numeric(gsub(":","",time)) / 100,
         hour =  Time %/% 100,
         min = Time %% 100,
         decimalHour = hour + min / 60  # decimal hour
  ) %>% 
  ggplot(aes(x = decimalHour, y = ..density..)) +
  geom_histogram()

# Visualize what time of day artical with buzzword was posted
buzzword <- "https"
fb %>% 
  filter(!type %in% c("Public", "Status") &
           grepl(buzzword, title)) %>% 
  mutate(Time = as.numeric(gsub(":","",time)) / 100,
         hour =  Time %/% 100,
         min = Time %% 100,
         decimalHour = hour + min / 60  # decimal hour
  ) %>% 
  ggplot(aes(x = decimalHour, y = ..density..)) +
  geom_histogram()


# findbuzz <- function(x, searchwords){
#   wordsx <- separate_rows(x)
#   
# }

# Words with greatest proportion of clicks
(buzzwords <- wordClicks %>% 
    filter(avg.prop.clicks > 0.08))


## Initial conclusions
# Articals containing the words https, code, free, developers, 
# freecodecamp, & science (though science has n=8 only) may get more clicks
# Deeper analysis should look at multiple buzzwords in a single artical


# analysis

# logistic regression
# does it contain a buzzword
# buzzword is any word included in the artical that gets at least x% of clicks
buzzThresh <- 0.08
fbdat <- fbdat %>% 
  mutate(hasBuzz = ifelse(any(strsplit(title, " ") %in% buzzwords$Buzzword), 1, 0))

fbdat %>% 
  mutate(words = str)

summarize(fbdat, hasBuzz,
            n())
table(fbdat$hasBuzz)

