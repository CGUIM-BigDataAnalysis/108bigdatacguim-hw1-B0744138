
library(jsonlite)
library(dplyr)
library(readr)
library(data.table)
options(encoding = "UTF-8")



# knitr::kable(head(iris))
S104 <- read_csv("C:/Users/USER/Desktop/RProject/104profession.csv")
S107 <- read_csv("C:/Users/USER/Desktop/RProject/107profession.csv")


colnames(S104) <- gsub('及以上', '' ,colnames(S104))
S_total <- merge(x = S104, y = S107, by = "大職業別")
colnames(S_total) <- gsub('x', '104' ,colnames(S_total))
colnames(S_total) <- gsub('y', '107' ,colnames(S_total))




#-------------------------------第一題-----------------------------------------

Ans_1 <- S_total%>%
  select(`大職業別`,`大學-薪資.104`,`大學-薪資.107`)%>%
  subset(`大學-薪資.107` != "—")%>%
  mutate(`大學-薪資.107` = as.numeric(`大學-薪資.107`),
         `大學-薪資.104` = as.numeric(`大學-薪資.104`))


Ans_1_a <- Ans_1%>%
  mutate(`平均大學薪資` = Ans_1$`大學-薪資.107`/Ans_1$`大學-薪資.104`)%>%
  filter(`大學-薪資.107`>`大學-薪資.104`) %>%
  arrange(desc(平均大學薪資))


Ans_1_b <- Ans_1_a %>%
  filter(平均大學薪資 >1.05)%>%
  select(`大職業別`,平均大學薪資)%>%
  arrange(desc(平均大學薪資))


Ans_1_c <- table(sapply( (strsplit (Ans_1_b$大職業別,"-") ), "[" ,  1))
View(sort(Ans_1_c, decreasing = T))



#-------------------------------第二題-----------------------------------------


Ans_2 <- S_total%>%
  select(`大職業別`,`大學-女/男.104`, `大學-女/男.107`)%>%
  subset(`大學-女/男.107` != "—")%>%
  subset(`大學-女/男.104` != "—")%>%
  mutate(`大學-女/男.107` = as.numeric(`大學-女/男.107`),
         `大學-女/男.104` = as.numeric(`大學-女/男.104`))

Ans_2_a_104 <- Ans_2%>%
  filter(`大學-女/男.104`<100) %>%
  select(`大職業別`,`大學-女/男.104`)%>%
  arrange(`大學-女/男.104`)

Ans_2_a_107 <- Ans_2%>%
  filter(`大學-女/男.107`<100) %>%
  select(`大職業別`,`大學-女/男.107`)%>%
  arrange(`大學-女/男.107`)


Ans_2_b_104 <- Ans_2%>%
  filter(`大學-女/男.104`<100) %>%
  select(`大職業別`,`大學-女/男.104`)%>%
  arrange(desc(`大學-女/男.104`))

Ans_2_b_107 <- Ans_2%>%
  filter(`大學-女/男.107`<100) %>%
  select(`大職業別`,`大學-女/男.107`)%>%
  arrange(desc(`大學-女/男.107`))


#-------------------------------第三題-----------------------------------------


Ans_3 <- S_total%>%
  select(`大職業別`,`大學-薪資.107`,`研究所-薪資.107`)%>%
  subset(`大學-薪資.107` != "—")%>%
  subset(`研究所-薪資.107` != "—")%>%
  mutate(`大學-薪資.107` = as.numeric(`大學-薪資.107`),
         `研究所-薪資.107` = as.numeric(`研究所-薪資.107`),
         `成長率` = `研究所-薪資.107`/`大學-薪資.107`)%>%
  arrange(desc(`成長率`))


Ans_3 <- Ans_3[-grep("其他+",Ans_3$大職業別,value=F), ]

#------------------------------第四題-----------------------------------------


grep("資訊+",S_total$大職業別,value=T)






















