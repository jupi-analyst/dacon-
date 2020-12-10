getwd()
setwd('~/sh_R/open')
library(tidyverse)
library(reshape2)
library(scales)
library(extrafont)
library(plotly)
library(gridExtra)
loadfonts()
new_people <- read.csv('open/new_people.csv', fileEncoding = 'euc-kr')
process <- read_csv('open/process.csv')
suggest <- read_csv('open/suggest.csv')

# 특별위원회 제거 및 20대 국회만 추출
suggest_df <- suggest[-grep(suggest$COMMITTEE, pattern = "특별위원회"),]
age20_suggest <- suggest_df %>%
  filter(AGE == 20)

# 소관위원회별 발의된 수
age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(COMMITTEE, n), y= n)) +
  geom_bar(stat='identity') +
  coord_flip()


# 소관위별 처리결과
age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>%
  group_by(COMMITTEE, PROC_RESULT) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(COMMITTEE, n), y= n,  fill = PROC_RESULT)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme(legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

# 소관위별 가결
age20_suggest %>%
  filter(PROC_RESULT == '수정가결' | PROC_RESULT =='원안가결') %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(COMMITTEE, n), y= n)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle("소관위별 가결 수") +
  labs(x = "소관위", y = "가결수") +
  theme(plot.title = element_text(family = "NanumGothicExtraBold", hjust = 0.5, size =13))

# 소관위별 폐기
age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>%
  filter(PROC_RESULT == '임기만료폐기' | PROC_RESULT =='폐기' | PROC_RESULT == '수정안반영폐기') %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(COMMITTEE, n), y= n)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle("소관위별 폐기") +
  labs(x = "소관위", y = "폐기수") +
  theme(plot.title = element_text(family = "NanumGothicExtraBold", hjust = 0.5, size =13))

# 소관위별 대안반영
age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>%
  filter(PROC_RESULT == '대안반영폐기') %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(COMMITTEE, n), y= n)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle("소관위별 대안반영") +
  labs(x = "소관위", y = "대안반영수") +
  theme(plot.title = element_text(family = "NanumGothicExtraBold", hjust = 0.5, size =13))

# 가결, 폐기, 대안반영 추출
age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>% 
  filter(PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결') %>%
  group_by(COMMITTEE) %>% 
  summarise(가결 = n()) -> success_df

age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>% 
  filter(PROC_RESULT == '임기만료폐기' | PROC_RESULT == '수정안반영폐기' |
           PROC_RESULT == '폐기') %>%
  group_by(COMMITTEE) %>% 
  summarise(폐기= n()) -> trash_df

age20_suggest %>%
  filter(!is.na(COMMITTEE)) %>% 
  filter(PROC_RESULT == '대안반영폐기') %>%
  group_by(COMMITTEE) %>% 
  summarise(대안반영 = n()) -> alter_df

# 조인 및 처리결과별 비율
result_df <- left_join(inner_join(alter_df, success_df), trash_df)
result_df[is.na(result_df)] = 0 # NA를 0으로 처리
result_df %>%
  mutate(대안반영총합 = sum(대안반영),            # 각 처리결과 전체 합
               폐기총합 = sum(폐기),
               가결총합 = sum(가결)) %>%
  group_by(COMMITTEE) %>%
  mutate(대안반영율 = 대안반영 / 대안반영총합,    # 전체 중 각 처리 결과 비율
              폐기율 = 폐기 / 폐기총합,
              가결율 = 가결 / 가결총합) -> result_df

count_result_df <- melt(result_df, id.vars = "COMMITTEE", measure.vars = c("가결","대안반영","폐기"))
count_result_df %>%
  ggplot(aes(x = reorder(COMMITTEE, value), y = value, fill = variable)) +
  geom_bar(stat='identity', color = 'black', position = 'dodge') +
  coord_flip()

per_result_df <- melt(result_df, id.vars = "COMMITTEE", measure.vars = c("가결율","대안반영율","폐기율"))
per_result_df %>%
  mutate(COMMITTEE = factor(COMMITTEE)) %>%
  mutate(value = value * 100) %>%
  ggplot(aes(x = fct_reorder(COMMITTEE, value), y = value, fill = variable)) +
  geom_bar(stat='identity', position = 'fill', color = 'black') +
  coord_flip() +
  scale_y_continuous(labels = percent)

# per_result_df %>% 
#   filter(variable == '가결율') %>%
#   group_by(COMMITTEE) %>%
#   summarise(비율 = round(value * 100, 1)) %>%
#   ggplot(aes(x = "", y = 비율, fill = COMMITTEE)) +
#   geom_bar(stat='identity', width = 1, color = 'white') +
#   coord_polar("y", start = 0) +
#   theme_void() +
#   ggtitle("발의 법안 가결처리 결과") +
#   labs(x = "", y = "") +
#   theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
#   geom_text(aes(label = paste0(비율, "%")), 
#             color = "white", size=3, position = position_stack(vjust = 0.5))




# 각 의원이 어떤 법안에 관심을 가지고 있는지 (한 법안은 한 상임위원회를 가지고 있음) ex)황주홍
committee_df <- data.frame(COMMITTEE = levels(as.factor(age20_suggest$COMMITTEE)))
age20_suggest %>%
  filter(RST_PROPOSER == '황주홍') %>% #name
  filter(!is.na(COMMITTEE)) %>%
  group_by(RST_PROPOSER, COMMITTEE, PROC_RESULT) %>%
  summarise(n = n()) -> proposer_committee

proposer_committee_df <- full_join(proposer_committee,committee_df)
proposer_committee_df$RST_PROPOSER = '황주홍' #name
na.omit(proposer_commproposer_committee_dfittee_df)

proposer_committee_df %>%
  filter(PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결') %>%
  group_by(COMMITTEE) %>%
  summarise(가결 = sum(n)) -> proposer_success_df
proposer_success_df <- left_join(committee_df, proposer_success_df )

proposer_committee_df %>%
  filter(PROC_RESULT == '임기만료폐기' | PROC_RESULT == '수정안반영폐기' | PROC_RESULT == '폐기') %>%
  group_by(COMMITTEE) %>%
  summarise(폐기 = sum(n)) -> proposer_trash_df
proposer_trash_df <- left_join(committee_df, proposer_trash_df )

proposer_committee_df %>%
  filter(PROC_RESULT == '대안반영폐기') %>%
  group_by(COMMITTEE) %>%
  summarise(대안반영 = sum(n)) -> proposer_alter_df
proposer_alter_df <- left_join(committee_df, proposer_alter_df )



proposer_committee_result <- inner_join(inner_join(proposer_success_df, proposer_trash_df, by ='COMMITTEE'), proposer_alter_df, by = 'COMMITTEE')
proposer_committee_result[is.na(proposer_committee_result)] = 0 # NA을 0으로 변경

### -----------------------------------------------------정적그래프 ----------------------------------------------
# 전체 비율을 구하기위한 변수
result_totalSum <- sum(proposer_committee_result$가결 + proposer_committee_result$폐기 + proposer_committee_result$대안반영)
# 전체 발의법안 처리 결과율
proposer_committee_result %>%
  group_by(COMMITTEE) %>%
  mutate(가결율 = 가결 / result_totalSum,
            폐기율 = 폐기 / result_totalSum,
            대안반영율 = 대안반영 / result_totalSum) %>%
  melt(id.vars = 'COMMITTEE', measure.vars = c('가결율', '폐기율', '대안반영율')) -> melt_proposer_comm_result_ratio


## 위에 코드로 대체
melt_proposer_comm_result_ratio %>%
  group_by(variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("황주홍의원, 발의 법안 처리 결과") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5))

# 각각 의원별, 소관위별 처리결과 ex) 황주홍의원의 농림축산식품해양수산위원회에서의 처리 결과
age20_suggest %>%
  filter(RST_PROPOSER =='황주홍') %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(5) -> top5_index

proposer_committee_result %>%
  filter(COMMITTEE %in% top5_index$COMMITTEE) %>%
  group_by(COMMITTEE) %>%
  mutate(가결율 = 가결 / sum(가결,폐기,대안반영),
            폐기율 = 폐기 / sum(가결,폐기,대안반영),
            대안반영율 = 대안반영 / sum(가결,폐기,대안반영)) -> proposer_committee_result_ratio
tmp <- list()
for(i in 1:5) {
  tmp[[i]] <- data.frame(melt(proposer_committee_result_ratio[i, ],
       id.vars = 'COMMITTEE', 
       measure.vars = c('가결율', '폐기율', '대안반영율')))
}

tmp[[1]] %>%
  group_by(COMMITTEE,variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle(paste0("황주홍의원,",tmp[[1]]$COMMITTEE, "\n 관련 법안 처리 결과율")) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5)) -> p1 

tmp[[2]] %>%
  group_by(COMMITTEE,variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle(paste0("황주홍의원,",tmp[[2]]$COMMITTEE, "\n 관련 법안 처리 결과율")) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5)) -> p2

tmp[[3]] %>%
  group_by(COMMITTEE,variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle(paste0("황주홍의원,",tmp[[3]]$COMMITTEE, "\n 관련 법안 처리 결과율")) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5)) -> p3

tmp[[4]] %>%
  group_by(COMMITTEE,variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle(paste0("황주홍의원,",tmp[[4]]$COMMITTEE, "\n 관련 법안 처리 결과율")) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5)) -> p4

tmp[[5]] %>%
  group_by(COMMITTEE,variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle(paste0("황주홍의원,",tmp[[5]]$COMMITTEE, "\n 관련 법안 처리 결과율")) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5)) -> p5

# melt(proposer_committee_result_ratio, 
#      id.vars = 'COMMITTEE', measure.vars = c('가결율', '폐기율', '대안반영율')) -> proposer_committee_ratio

grid.arrange(p1,p2,p3,p4,p5, nrow = 3, ncol = 2)

tmp
## 아래코드로 대체
proposer_committee_ratio %>%
  group_by(variable) %>%
  summarise(비율 = round(sum(value) * 100, 1)) %>%
  ggplot(aes(x = "", y = 비율, fill = variable)) +
  geom_bar(stat='identity', width = 1, color = 'white') +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("황주홍의원, 농림축산식품해양수산위원회 \n관련 법안 발의 처리 결과율") +      ## {name}, {committee}
  labs(x = "", y = "") +
  theme(plot.title = element_text(size = 13, hjust = 0.5, family = "NanumGothicExtraBold")) +
  geom_text(aes(label = paste0(비율, "%")),
            color = "white", size=3, position = position_stack(vjust = 0.5))
### --------------------------------------------------------------------------------------
###  ------------------------------------------------------인터랙티브그래프
library(plotly)
proposer_committee_result %>%
  group_by(COMMITTEE) %>%
  melt(id.vars = 'COMMITTEE', measure.vars = c('가결', '폐기', '대안반영')) -> melt_proposer_comm_result_count

melt_proposer_comm_result_count %>%
  group_by(variable) %>%
  summarise(value = sum(value)) %>% 
  plot_ly(labels = ~variable, values = ~value, type = 'pie', 
          textinfo='label+percent')%>%
  layout(title = '황주홍의원, 발의한 전체 법안 처리 결과율',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# 각 의원이 가장많이 발의한 소관위원 법안 TOP5만의 처리결과만 전체적으로 나타내기
age20_suggest %>%
  filter(RST_PROPOSER =='황주홍') %>%
  group_by(COMMITTEE) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(5) -> top5_index

melt_proposer_comm_result_count %>%
  filter(COMMITTEE %in% top5_index$COMMITTEE) -> top5_result_df
top5_result_df %>%
  ggplot(aes(x = fct_reorder(COMMITTEE, value), y = value, fill = variable)) +
  geom_bar(stat='identity', position = 'dodge', color = 'black') +
  coord_flip()

# top5_result_df %>%
#   filter(COMMITTEE == '농림축산식품해양수산위원회') %>%
#   plot_ly(labels = ~variable, values = ~value, type = 'pie',
#           textinfo='label+percent') %>%
#   layout(title = '황주홍의원, 농림축산식품해양수산위원회 \n관련 법안 발의 처리 결과율',
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


fig <- plot_ly(textinfo = 'label + percent', 
               textposition = 'inside',
               insidetextfont = list(color = 'white'),
               marker = list(line = list(color ='white', width = 1)))
fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[1]), 
                       labels = ~variable, values = ~value, name = "", domain = list(row = 0, column = 0))
fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[2]), 
                        labels = ~variable, values = ~value, name = "", domain = list(row = 0, column = 1))
fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[3]), 
                       labels = ~variable, values = ~value, name = "", domain = list(row = 1, column = 0))
fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[4]), 
                       labels = ~variable, values = ~value, name = "", domain = list(row = 1, column = 1))
fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[5]), 
                       labels = ~variable, values = ~value, name = "", domain = list(row = 2, column = 0))
fig <- fig %>% layout(title = "황주홍의원 발의한 법안관련 TOP5 소관위, 법안 처리결과", showlegend = T,
                      grid=list(rows=3, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

# 데이터 추출 및 전처리 함수
each_proposer <- function(age, name) {
  # n대의원 추출, 특별위제외
  suggest_df <- suggest[-grep(suggest$COMMITTEE, pattern = "특별위원회"),]
  age_suggest <- suggest_df %>%
    filter(AGE == age)
  
  # 소관위원회 추출
  committee_df <- data.frame(COMMITTEE = levels(as.factor(age_suggest$COMMITTEE)))
  
  # 
  age_suggest %>%
    filter(RST_PROPOSER == name) %>% #name
    filter(!is.na(COMMITTEE)) %>%
    group_by(RST_PROPOSER, COMMITTEE, PROC_RESULT) %>%
    summarise(n = n()) -> proposer_committee
  
  # 소관위와 특정위원이 발의법안 관련 소관위를 조인
  proposer_committee_df <- full_join(proposer_committee,committee_df)
  proposer_committee_df$RST_PROPOSER = name #name
  na.omit(proposer_committee_df)
  
  # 법안 처리 가결건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결') %>%
    group_by(COMMITTEE) %>%
    summarise(가결 = sum(n)) -> proposer_success_df
  proposer_success_df <- left_join(committee_df, proposer_success_df )
  
  # 법안 처리 폐기건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '임기만료폐기' | PROC_RESULT == '수정안반영폐기' | PROC_RESULT == '폐기') %>%
    group_by(COMMITTEE) %>%
    summarise(폐기 = sum(n)) -> proposer_trash_df
  proposer_trash_df <- left_join(committee_df, proposer_trash_df )
  
  # 법안 처리 대안반영건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '대안반영폐기') %>%
    group_by(COMMITTEE) %>%
    summarise(대안반영 = sum(n)) -> proposer_alter_df
  proposer_alter_df <- left_join(committee_df, proposer_alter_df )
  
  # 법안 처리들을 모두 조인
  proposer_committee_result <- inner_join(inner_join(proposer_success_df, proposer_trash_df, by ='COMMITTEE'), proposer_alter_df, by = 'COMMITTEE')
  proposer_committee_result[is.na(proposer_committee_result)] = 0 # NA을 0으로 변경
  
  # 전처리 함수에서 받은 데이터를 시각화를 위해 metl시킴
  proposer_committee_result%>%
    group_by(COMMITTEE) %>%
    melt(id.vars = 'COMMITTEE', measure.vars = c('가결', '폐기', '대안반영')) -> melt_proposer_comm_result_count
  
  melt_proposer_comm_result_count %>%
    group_by(variable) %>%
    summarise(value = sum(value)) %>% 
    plot_ly(labels = ~variable, values = ~value, type = 'pie', 
            textinfo='label+percent')%>%
    layout(title = paste0(name, '의원이 발의한 법안 전체 처리 결과율'),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # 의원 발의법안 관련 top5 소관위 추출
  age20_suggest %>%
    filter(RST_PROPOSER == name) %>%
    group_by(COMMITTEE) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(5) -> top5_index
  
  # 시각화를 위해 melt
  melt_proposer_comm_result_count %>%
    filter(COMMITTEE %in% top5_index$COMMITTEE) -> top5_result_df
  
  # 소관위 TOP5 인터랙티브 시각화 
  fig <- plot_ly(textinfo = 'label + percent', 
                 textposition = 'inside',
                 insidetextfont = list(color = 'white'),
                 marker = list(line = list(color ='white', width = 1)))
  fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[1]), 
                         labels = ~variable, values = ~value, name = "", domain = list(row = 0, column = 0))
  fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[2]), 
                         labels = ~variable, values = ~value, name = "", domain = list(row = 0, column = 1))
  fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[3]), 
                         labels = ~variable, values = ~value, name = "", domain = list(row = 1, column = 0))
  fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[4]), 
                         labels = ~variable, values = ~value, name = "", domain = list(row = 1, column = 1))
  fig <- fig %>% add_pie(data = top5_result_df %>% filter(COMMITTEE == COMMITTEE[5]), 
                         labels = ~variable, values = ~value, name = "", domain = list(row = 2, column = 0))
  fig <- fig %>% layout(title = paste0(name,"의원 발의한 법안관련 TOP5 소관위별 법안 처리결과"), 
                        showlegend = T,
                        grid=list(rows=3, columns=2),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  fig
}
each_proposer(20, '황주홍')

top5_total <- function(age, name){
  # n대의원 추출, 특별위제외
  suggest_df <- suggest[-grep(suggest$COMMITTEE, pattern = "특별위원회"),]
  age_suggest <- suggest_df %>%
    filter(AGE == age)
  
  # 소관위원회 추출
  committee_df <- data.frame(COMMITTEE = levels(as.factor(age_suggest$COMMITTEE)))
  
  # 
  age_suggest %>%
    filter(RST_PROPOSER == '박광온') %>% #name
    filter(!is.na(COMMITTEE)) %>%
    group_by(RST_PROPOSER, COMMITTEE, PROC_RESULT) %>%
    summarise(n = n()) -> proposer_committee
  
  # 소관위와 특정위원이 발의법안 관련 소관위를 조인
  proposer_committee_df <- full_join(proposer_committee,committee_df)
  proposer_committee_df$RST_PROPOSER = name  #name
  na.omit(proposer_committee_df)
  
  # 법안 처리 가결건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '수정가결' | PROC_RESULT == '원안가결') %>%
    group_by(COMMITTEE) %>%
    summarise(가결 = sum(n)) -> proposer_success_df
  proposer_success_df <- left_join(committee_df, proposer_success_df )
  
  # 법안 처리 폐기건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '임기만료폐기' | PROC_RESULT == '수정안반영폐기' | PROC_RESULT == '폐기') %>%
    group_by(COMMITTEE) %>%
    summarise(폐기 = sum(n)) -> proposer_trash_df
  proposer_trash_df <- left_join(committee_df, proposer_trash_df )
  
  # 법안 처리 대안반영건만 추출
  proposer_committee_df %>%
    filter(PROC_RESULT == '대안반영폐기') %>%
    group_by(COMMITTEE) %>%
    summarise(대안반영 = sum(n)) -> proposer_alter_df
  proposer_alter_df <- left_join(committee_df, proposer_alter_df )
  
  # 법안 처리들을 모두 조인
  proposer_committee_result <- inner_join(inner_join(proposer_success_df, proposer_trash_df, by ='COMMITTEE'), proposer_alter_df, by = 'COMMITTEE')
  proposer_committee_result[is.na(proposer_committee_result)] = 0 # NA을 0으로 변경
  
  # 전처리 함수에서 받은 데이터를 시각화를 위해 metl시킴
  proposer_committee_result%>%
    group_by(COMMITTEE) %>%
    melt(id.vars = 'COMMITTEE', measure.vars = c('가결', '폐기', '대안반영')) -> melt_proposer_comm_result_count
  
  age_suggest %>%
    filter(RST_PROPOSER == '박광온') %>%
    filter(!is.na(COMMITTEE)) %>%
    group_by(COMMITTEE) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(5) -> top5_index
  
  melt_proposer_comm_result_count %>%
    filter(COMMITTEE %in% top5_index$COMMITTEE) -> top5_result_df
  
  top5_result_df %>%
    ggplot(aes(x = fct_reorder(COMMITTEE, value), y = value, fill = variable)) +
    geom_bar(stat='identity', position = 'dodge', color = 'black') +
    coord_flip() -> p2
  p2
  return(p2)
}
top5_total(20, '박광온')
