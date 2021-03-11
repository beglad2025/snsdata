# kmeans 머신러닝 알고리즘
# knn과 비슷하지만 혼동하면 안됨
# 집단을 만드는 역할(클러스터링 한다고 한다)


# kmeans 기반 10대 학생 시장 세분화

teens <- read.csv("snsdata.csv")
str(teens)

table(teens$gender)  #NA 있는데 M과 F밖에 안나옴
table(teens$gender, useNA = "ifany")  #NA도 나온다

summary(teens$age)  #NA's로 NA개수 알려줌
# NA, 3살, 106살 -> 처리
# 13세 이상 20세 미만 : 정상 범위
# 정상범위 외에 있는 데이터는 모두 NA로 취급하자

teens$age <- ifelse(teens$age>=13 & teens$age<20,
                    teens$age,
                    NA)
summary(teens$age)


# 성별 NA는 어떻게 처리?

teens$female <- ifelse(teens$gender=="F" & !is.na(teens$gender),
                       1,
                       0)
# 1: 여성, 0: 남성 혹은 NA

teens$nogender <- ifelse(is.na(teens$gender),
                         1,
                         0)
# 1: 성별 없음 0:여성or남성

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$nogender, useNA = "ifany")



# age열 처리

mean(teens$age)
mean(teens$age, na.rm = TRUE)

# quiz
# 졸업연도별 나이평균 출력

str(teens)
library(dplyr)
teens %>% 
  group_by(gradyear) %>% 
  summarise(mean(age, na.rm = T))
#혹은
tapply(teens$age, teens$gradyear, mean, na.rm=T)
#혹은
aggregate(data=teens, age~gradyear, mean, na.rm=T)


# 결측값 대체
# teens$age <- ifelse(is.na(teens$age), 함수 호출, teens$age)

# ave()
# ave(데이터, 그룹, 함수) #함수는 mean이 defualt값

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm=T))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
teens$age
summary(teens$age)  #NA 더 이상 없음


# kmeans 클러스터링

str(teens)
ncol(teens)
interests <- teens[5:40]

# 표준화

interests_z <- as.data.frame(lapply(interests, scale))
interests_z


# 군집화

set.seed(2345)
teen_clusters <- kmeans(interests_z,5)
teen_clusters
# 표준화 진행하고 얻은 값이므로 0에 가까울수록 평균에 가까운 것이고
# 멀수록 독특한 경우인 것이므로 더 관심가지고 본다

teen_clusters$size  #각 클러스터에 할당된 데이터 개수
teen_clusters$centers  #각 크러스터의 중심점 행렬
teen_clusters$cluster  #각 데이터별 할당된 클러스터 번호

teen_clusters
teen_clusters$withinss
teen_clusters$tot.withinss

# withinss : 각 크러스터 내의 테이터간 거리의 제곱 합
# 값이 작을수록 클러스터링이 잘 된 것

teen_clusters$betweenss  #클러스터 간 중심의 거리 제곱합
# 값이 클수록 클러스터링이 잘 된 것



teens$cluster <- teen_clusters$cluster
str(teens)


# 클러스터 나이의 평균

aggregate(data = teens, age ~ cluster, mean)
# 클러스터별 나이의 평균이 거의 비슷 -> 아.. 별로 영향을 주지 못하는군!


# 클러스터 성비

aggregate(data = teens, female ~ cluster, mean)


# 클러스터 친구수

aggregate(data = teens, female ~ cluster, mean)







