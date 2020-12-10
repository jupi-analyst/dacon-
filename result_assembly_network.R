getwd()
setwd('~/sh_R/open')
library(tidyverse)
library(reshape2)
library(extrafont)
library(scales)
library(ggplot2)
library(gridExtra)
loadfonts()
suggest <- read_csv('open/suggest.csv')

# 20대의원 중 어떤의원이 가결, 폐기, 대안이 많은가?
## 1. 데이터 전처리 및 EDA
### 1.1. 법안발의한 역대 의원 결과 비교(현 21대의원 제외)
suggest %>%
  filter(AGE >= 16 & AGE != 21) %>%
  group_by(AGE, PROC_RESULT) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = AGE, y = n, color = PROC_RESULT, fill = PROC_RESULT)) +
  geom_bar(stat = 'identity', position = 'dodge', color ='black') +
  theme_bw() +
  ggtitle("")
  labs(x = '16대 ~ 20대', y = '발의 수')
  


## 2000년도 21세기(16대)부터 폐기, 가결, 대안반영만 따로 보기로 하자, 21대는 진행중인 국회이므로 제외
suggest %>%
  filter(AGE >= 16 & AGE != 21) %>%
  filter(PROC_RESULT == '임기만료폐기'|PROC_RESULT == '수정안반영폐기' | 
           PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결' | PROC_RESULT == '대안반영폐기') %>%
  group_by(AGE, PROC_RESULT) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = AGE, y = n, color = PROC_RESULT, fill = PROC_RESULT)) +
  geom_bar(stat = 'identity', position = 'dodge')

### 16~20대의원, 각 대별 전체 발의수
suggest %>%
  filter(AGE >= 16 & AGE != 21) %>%
  group_by(AGE) %>%
  summarise(발의수 = n()) %>%
  ggplot(aes(x = AGE, y = 발의수)) +
  geom_bar(stat = 'identity', fill = 'skyblue', color = 'black') +
  xlab("의원대수") +
  ylab("발의 수") + 
  theme_minimal(base_family = 'NanumGothicExtraBold')

### 16~20대의원, 각 대별로 전체 발의수 데이터 추출
suggest  %>%
  filter(AGE >= 16, AGE != 21) %>%
  group_by(AGE) %>%
  summarise(법안발의수 = n()) -> age_total

### 폐기수 및 폐기율(수정안반영폐기, 임기만료폐기) 
### 폐기관련 처리만 추출
suggest %>%
  filter(AGE >= 16, AGE != 21) %>%
  filter(PROC_RESULT == '임기만료폐기'|PROC_RESULT == '수정안반영폐기') %>%
  group_by(AGE) %>%
  summarise(폐기수 = n()) -> age_trash_total

trash_df <- inner_join(age_total, age_trash_total, by = 'AGE') # 대 기준으로 조인

#### 대별 폐기수
trash_df %>%
  ggplot(aes(x = AGE, y = 폐기수)) +
  geom_bar(stat='identity', fill = "#F8766D", color = 'black') +
  geom_text(aes(label=round(폐기수,1)), vjust=1.5, colour="white") +
  theme_bw(base_family = 'NanumGothic')

#### 대별 폐기 비율
trash_df %>%
  mutate(폐기_비율 = (폐기수 / 법안발의수) *100) %>%
  ggplot(aes(x = AGE, y = 폐기_비율)) +
  geom_bar(stat = 'identity', fill = "#F8766D" , color = 'black') +
  geom_text(aes(label= paste0(round(폐기_비율,1), "%")), vjust=1.5, colour="white") +
  ggtitle("< 대별 폐기 비율 >") +
  labs(y = "폐기비율(%)", x = "의원대수") + 
  theme(plot.title = element_text(hjust=0.5, family = 'NanumGothicExtraBold', size = 14),
        axis.title = element_text(family = 'NanumGothicExtraBold', size = 11),
        panel.grid.major = element_line(colour = "white", size = 0.2),
        panel.grid.minor = element_blank()) +
  ylim(c(0,100))
  
  

### 가결수 및 가결율(수정 + 원안)
suggest %>%
  filter(AGE >= 16, AGE != 21) %>%
  filter(PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결') %>%
  group_by(AGE) %>%
  summarise(가결수 = n()) -> age_success_total 

#### 대별 가결수(수정+원안)
success_df <- inner_join(age_total, age_success_total, by = 'AGE')
success_df %>%
  ggplot(aes(x = AGE, y = 가결수)) +
  geom_bar(stat='identity', fill = '#7CAE00', color = 'black') +
  geom_text(aes(label=가결수), vjust=1.5, colour="black") +
  theme_bw()

#### 대별 가결율(수정+원안)
success_df %>%
  mutate(가결_비율 = (가결수 / 법안발의수) *100) %>%
  ggplot(aes(x = AGE, y = 가결_비율)) +
  geom_bar(stat = 'identity', fill = '#7CAE00', color = 'black') +
  geom_text(aes(label= paste0(round(가결_비율, 1), "%")), vjust=1.5, colour="black") +
  ylim(c(0,100)) +
  theme_bw()

### 대안반영 및 대안반영율
### 대안반영은 폐기가 아니라 이미 비슷한 법이나 대체할 수 있는 법이 있으므로 그 법으로 수정,추가하여 대체하는 것이므로 폐기와 따로 분리
suggest %>%
  filter(AGE >= 16, AGE != 21) %>%
  filter(PROC_RESULT == '대안반영폐기') %>%
  group_by(AGE) %>%
  summarise(대안반영수 = n()) -> age_alter_total 

alter_df <- inner_join(age_total, age_alter_total, by = 'AGE')
#### 대안반영수
alter_df %>%
  ggplot(aes(x = AGE, y = 대안반영수)) +
  geom_bar(stat='identity', fill = "#619CFF" , color = 'black') +
  geom_text(aes(label= 대안반영수), vjust=1.5, colour="black") +
  theme_bw()
### 대안반영율
alter_df %>%
  mutate(대안반영_비율 = (대안반영수 / 법안발의수) *100) %>%
  ggplot(aes(x = AGE, y = 대안반영_비율)) +
  geom_bar(stat = 'identity', fill = "#619CFF", color = 'black') +
  geom_text(aes(label= paste0(round(대안반영_비율, 1), "%")), vjust=1.5, colour="black") +
  ylim(c(0,100)) +
  theme_bw()

### 각 법안처리결과 조인후 테이블
df <- inner_join(trash_df, success_df, by = c('AGE', '법안발의수'))
age_df <- inner_join(df, alter_df,  by = c('AGE', '법안발의수'))
age_df %>%
  mutate(폐기율 = 폐기수 / 법안발의수,
            가결율 = 가결수 / 법안발의수,
            대안반영율 = 대안반영수 / 법안발의수) -> age_df
age_df

### 각 폐기, 가결, 대안반영수 및 비율 비교
melt_age_df2 <- melt(age_df, id.vars = 'AGE', measure.vars = c('폐기수','가결수','대안반영수'))
melt_age_df2 %>%
  ggplot(aes(x = AGE, y =value, fill = variable)) +
  geom_bar(stat='identity', positio = 'dodge')

melt_age_df <- melt(age_df, id.vars = 'AGE', measure.vars = c('폐기율','가결율','대안반영율'))
melt_age_df %>%
  ggplot(aes(x = AGE, y = value, fill = variable)) +
  geom_bar(stat='identity', position = 'fill') +
  scale_y_continuous(labels = percent)

### 20대의원 중 어떤 의원이 폐기, 가결, 대안반영을 많이 했는지, 그 대표의원과 공동발의 의원을 네트워크 시각화
#### 전체 발의중 20대의원의 발의만 추출
suggest %>%
  filter(AGE == 20) -> suggest_age20

### 20대의원 법안처리결과
suggest_age20 %>%
  group_by(PROC_RESULT) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(PROC_RESULT, n), y= n))+
  geom_bar(stat='identity') + 
  theme_bw() +
  coord_flip()

### 20대의원 중 발의 TOP10
suggest_age20 %>%
  group_by(RST_PROPOSER) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%  head(10) %>%
  ggplot(aes(x = reorder(RST_PROPOSER, n), y = n)) +
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw()

suggest_age20 %>%
  group_by(PROC_RESULT) %>%
  summarise(n = n())
### 폐기,가결,대안반영만 추출
suggest_age20 %>%
  filter(PROC_RESULT == '임기만료폐기'|PROC_RESULT == '수정안반영폐기' | 
           PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결' | PROC_RESULT == '대안반영폐기') %>%
  select(PROC_RESULT, RST_PROPOSER, PUBL_PROPOSER) -> age20_result_proposer

## 본격 임기만료폐기, 대안방안, 가결(수정+원안) 네트워크 시각화
### 임기만료폐기만 추출
age20_result_proposer %>%
  filter(PROC_RESULT == '임기만료폐기') -> age20_trash_df

### 임기만료폐기 TOP10 시각화
age20_trash_df %>%
  group_by(RST_PROPOSER) %>%
  summarise(임기만료폐기수 = n()) %>% 
  arrange(desc(임기만료폐기수)) %>%
  head(10) %>% 
  ggplot(aes(x = reorder(RST_PROPOSER, 임기만료폐기수), y = 임기만료폐기수)) +
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw() +
  xlab('대표발의자')

### 대안반영만 추출
age20_result_proposer %>%
  filter(PROC_RESULT == '대안반영폐기') -> age20_alter_df

### 대안반영 TOP10 시각화
age20_alter_df %>%
  group_by(RST_PROPOSER) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(10) %>% 
  ggplot(aes(x = reorder(RST_PROPOSER, n), y = n)) +
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw()

### 가결(수정+원안)만 추출
age20_result_proposer %>%
  filter(PROC_RESULT == '수정가결'|PROC_RESULT == '원안') -> age20_success_df

### 가결 TOP10 시각화
age20_success_df %>%
  group_by(RST_PROPOSER) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(10) %>% 
  ggplot(aes(x = reorder(RST_PROPOSER, n), y = n)) +
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw()

### 대안반영을 누가 많이 했고 누구와 같이 많이 했는가?
#### 전체를 살펴보기엔 데이터가 크고 복잡해서 TOP5만 뽑아서 살펴봄
age20_alter_df %>%
  group_by(RST_PROPOSER) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(5) -> alter_index

top5_alter_proposer <- age20_alter_df[age20_alter_df$RST_PROPOSER %in% alter_index$RST_PROPOSER,]
top5_alter_proposer %>%
  group_by(RST_PROPOSER) %>%
  summarise(n = n()) %>% arrange(desc(n))

age20_alter_split <- data.frame(RST_PROPOSER = top5_alter_proposer $RST_PROPOSER, 
                                   str_split(top5_alter_proposer $PUBL_PROPOSER, ",",  simplify = TRUE))
age20_alter_split[age20_alter_split == ""] <- NA

tmp <- data.frame()
test <- data.frame()
for(i in 1:dim(age20_alter_split)[1]){
  print(i)
  tmp <- data.frame()
  for(j in 2:dim(age20_alter_split)[2]){
    if(!is.na(age20_alter_split[i, j])){
      tmp[j-1, 1] = paste0(age20_alter_split[i, 1], ',', age20_alter_split[i, j])
    }
  }
  test <- rbind(test, tmp)
}
# View(test)
age20_alter_net <- test
alter_from_to <- data.frame(str_split(age20_alter_net$V1, ",",  simplify = TRUE))
colnames(alter_from_to) <- c('from','to')

alter_from_to %>%
  group_by(from, to) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) -> alter_network

library(igraph)
library(networkD3)
library(ggraph)
library(network)
library(sna)
G <- graph_from_data_frame(alter_network, directed = FALSE);
E(G) # 엣지정보
V(G) # 노드정보
l = layout_with_fr(G) # layout
plot(G,
     vertex.label.dist=0,
     vertex.shape='circle',
     vertex.size = igraph::degree(G, v =V(G), mode = 'all') / 3, #연결성중심으로 정점 크기 (조절필요)
     edge.width = E(G)$n / mean(E(G)$n), #조절할 필요가 있음
     edge.color="orange",
     edge.arrow.size=1,
     edge.curved=.3,
     vertex.label.cex = .5,
     main="Top5 대안반영 - 대표발의자",
     layout = l) 

nodes <- data.frame(node = (V(G)$name)) 
size <- data.frame(size = igraph::degree(G, v =V(G), mode = 'all')); 
rownames(size) <- 1:dim(size)[1]
nodes <- bind_cols(nodes,size)
nodes$node <- factor(nodes$node, levels=nodes$node)
nodes$idx <- 1:dim(nodes)[1]

# 링크 데이터 생성
links <- data.frame(from = as.numeric(as.factor(alter_network$from))-1,
                    to = as.numeric(as.factor(alter_network$to))-1,
                    width = alter_network$n)
str(links)

# 시각화
forceNetwork(Nodes = nodes, Links = links,  Source = 'from', Target = 'to',
             NodeID = 'node', Group = 'node',
             zoom = TRUE, fontSize = 20,
             Nodesize = 'size',
             linkDistance = 200, 
             radiusCalculation = JS("d.nodesize + 30 "),
             Value = 'width', 
             linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
             opacity = 0.8, opacityNoHover = TRUE,
             charge=-900, fontFamily = 'NanumGothic Bold')


