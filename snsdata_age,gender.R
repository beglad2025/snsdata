# 4. snsdata의 결측값을 적절하게 대체하여 kmeans를 적용해보세요

# 1) 결측값 존재하는 열 확인

teens <- read.csv("snsdata.csv")
str(teens)
missmap(teens, col=c("red","gray")) #gender, age열에 결측값 존재
dev.off()

# 2) age열 결측값 처리

# 2-1) 정상범위 외의 값을 가지는 데이터들 삭제

summary(teens$age)  #min과 max의 값이 이상함
box.age <- boxplot(teens$age)
length(box.age$out)  #361, 정상범위 넘어간 개수
box.age$stats  #13.396~21.158이 정상범위

new.teens <- teens %>% 
  filter((age>=13.396 & age<=21.158) | is.na(age))


# 2-2) 같은 졸업연도를 가진 age의 중위수와 평균의 평균으로 결측값 넣기

aggregate(data=new.teens, age~gradyear, mean, na.rm=T)
#    gradyear  age
# 1     2006 18.66807
# 2     2007 17.71148
# 3     2008 16.78087
# 4     2009 15.82594

aggregate(data=new.teens, age~gradyear, median, na.rm=T)
#    gradyear  age
# 1     2006 18.6750
# 2     2007 17.6905
# 3     2008 16.7340
# 4     2009 15.7860

ave_age <- ave(new.teens$age, new.teens$gradyear, 
               FUN = function(x){(mean(x, na.rm=T)+median(x, na.rm=T))/2})
new.teens$age <- ifelse(is.na(new.teens$age), ave_age, new.teens$age)

summary(new.teens$age)  #더 이상 결측값 없음


# 3) gender열 결측값 처리

str(new.teens)
table(new.teens$gender, useNA = "ifany")  #NA 2692

# 연령과 관심사를 통해 gender을 유추해보자(knn)

st.data <- as.data.frame(scale(new.teens[,-c(1:2)]))

na.idx <- which(is.na(new.teens$gender))
na.data <- st.data[na.idx,]  #gender가 NA인 데이터
train <- st.data[-na.idx,]  #gender가 NA아닌 데이터

# train 데이터를 통한 모델링

set.seed(123)
nrow(train)*0.7  #18862.9
tr_idx <- sample(1:nrow(train), 18863)
train_tr <- train[tr_idx,]
train_te <- train[-tr_idx,]

labels <- as.character(new.teens[-na.idx,2])
train_labels <- labels[tr_idx]
test_labels <- labels[-tr_idx]

library(class)
train_te_pred <- knn(train = train_tr,
                     test = train_te,
                     cl = train_labels,
                     k=23)

sum(train_te_pred==test_labels) / length(test_labels)  #정확도 0.8197674

# k=21        k=23       k=24       k=25       k=27      
# 0.8186541   0.8197674  0.8185304  0.81952    0.8198911


# na.data에 모델 적용

na.data_pred <- knn(train = train_tr,
                    test = na.data,
                    cl = train_labels,
                    k=23)

new.teens$gender[na.idx] <- as.character(na.data_pred)
new.teens$gender[na.idx]

table(new.teens$gender, useNA = "ifany")

#   F     M 
# 24446  5193 



# 4) kmeans 적용

# gender 원인핫코딩(연속형 자료로 볼 수 없으므로)

level <- unique(new.teens[[2]]) #"M" "F"
for(j in level){
  new <- paste("gender", j, sep = ".")
  new.teens[new] <- ifelse(new.teens[[2]]==j,1,0)
}


str(new.teens)
features <- new.teens[,-c(1:2)] 
str(features)


# 표준화

features_z <- as.data.frame(lapply(features, scale))
str(features_z)

# 군집화

set.seed(210311)
teens_clusters <- kmeans(features_z,5)
teens_clusters

teens_clusters$size
# [1]   843  1165 17350  5012  5269
teens_clusters$centers
teens_clusters$cluster

teens_clusters$withinss
# [1]  58210.79 211292.66 260625.94 130787.87 358568.34
teens_clusters$tot.withinss  #1019486

teens_clusters$betweenss #166034.4