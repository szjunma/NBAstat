library(jsonlite)
library(ggplot2)
# shot data for Stephen Curry


shot.df$SHOT_CLOCK <- as.numeric(as.character(shot.df$SHOT_CLOCK))
shot.df$SHOT_DIST<- as.numeric(as.character(shot.df$SHOT_DIST))
shot.df$CLOSE_DEF_DIST <- as.numeric(as.character(shot.df$CLOSE_DEF_DIST))
# shot.df$FGM <- as.numeric(as.character(shot.df$FGM))

train <- shot.df[1:686400,c(9, 12, 17, 18)]
test <- shot.df[686401:686435,c(9, 12, 17, 18)]

mod <- glm(FGM ~ SHOT_CLOCK + SHOT_DIST + CLOSE_DEF_DIST,family=binomial(link = "logit"),data=train[!is.na(train[,1]), ])

summary(mod)

fitted.results <- predict(mod,newdata=subset(test,select=c(1,2,3)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$FGM)
print(paste('Accuracy',1-misClasificError))

