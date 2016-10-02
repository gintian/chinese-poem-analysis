---
title: "中国古典诗词分析"
author: "罗炜"
date: "2016年10月2日"
output: word_document
---
对中国诗词数据库进行数据挖掘，分析历代诗词之间的潜在关系

#1. 数据来源

本研究使用的数据来源于互联网上流传的“诗词汇总”软件。该软件由**李自力**开发，是网站“诗词总汇”使用的数据库的一部分，共收录自先秦到现代近10万条诗词数据包括题目、作者、朝代、类型、考证、出处等多项信息。尽管后三者数据多有缺失且并未结构化，但依然能为我们的分析提供一些重要数据，例如可考诗人的生卒年份，这个数据将有助于我们分析不同年代诗人风格的特征。

#2.研究问题

钱钟书在《宋词选注》序言部分曾指出，宋人承续唐统，在诗歌方面继续向深入发展，在词的方面则进行了新的探索。这很大程度上出于唐人已经在诗歌方面取得了令后人难以超越的成就——唐人将诗歌中能写的几乎都写了。

这或许能在文本层面找到证据。如果唐人果真把“能写的都写了”，那么显然，后人在进行诗歌创作时会多有“借鉴”。“借鉴”之一便是“意象”——说的更简略些，就是特定的词汇。假若我们掌握了历朝历代的所有诗歌，把它们全部放在一起进行对比分析，尤其对比它们的用词特征，那么此类问题就有可能获得解答。

本研究使用的是量化方法，采取统计学手段对诗歌进行分析。与国内的既有研究不同，我们不是依据诗歌的外部信息（被选本引用次数、相关研究论文的数量）来分析历代诗歌的特征，而是希望借助**文本挖掘**技术，深入诗歌内部，从用词角度来分析诗歌的历时性特征。

本研究受统计之都的“统计词话”影响，参考了Hughes等人对英语文本的写作风格的历时性特征研究，希望回答以下几个问题：

1. 不同朝代的诗歌风格之间是否有明显的区别（操作化为，用词特征是否有明显区别）

2. 诗歌用词特征是否存在时间维度上的特征——相近时间段的相似度更大（所谓的时代特征），而较大时间差距的诗歌的风格明显不同（所谓的时代差异）

3. 情感分析？

4. 

#连接数据库
非常奇怪，在Rsutido中RODBC无法连接到指定数据库，但在32位的R中却可以读取。我们从R中导出csv，再在Rsutdio中导入。

```{r}
library(readxl)
poem<-read_excel("poem.xlsx");poem<-poem[,-1]
poet<-read.csv("poet.csv");poet<-poet[,-1]
```

提取简介中的诗人生卒年

1. 把中文数字转化成阿拉伯数字
```{r}
poet$简介=gsub(pattern = "○",replacement = "0",x =poet$简介)
poet$简介=gsub(pattern = "〇",replacement = "0",x =poet$简介)
poet$简介=gsub(pattern = "零",replacement = "0",x =poet$简介)
poet$简介=gsub(pattern = "一",replacement = "1",x =poet$简介)
poet$简介=gsub(pattern = "二",replacement = "2",x =poet$简介)
poet$简介=gsub(pattern = "三",replacement = "3",x =poet$简介)
poet$简介=gsub(pattern = "四",replacement = "4",x =poet$简介)
poet$简介=gsub(pattern = "五",replacement = "5",x =poet$简介)
poet$简介=gsub(pattern = "六",replacement = "6",x =poet$简介)
poet$简介=gsub(pattern = "七",replacement = "7",x =poet$简介)
poet$简介=gsub(pattern = "八",replacement = "8",x =poet$简介)
poet$简介=gsub(pattern = "九",replacement = "9",x =poet$简介)
f<-function(p,r){gsub(pattern = p,replacement = r,x =poet$简介)}
poet$简介<-f("０","0")
poet$简介<-f("１","1")
poet$简介<-f("２","2")
poet$简介<-f("３","3")
poet$简介<-f("４","4")
poet$简介<-f("５","5")
poet$简介<-f("６","6")
poet$简介<-f("７","7")
poet$简介<-f("８","8")
poet$简介<-f("９","9")
poet$简介<-f("?|？","")
```
2. 提取括号内的年份数据
```{r}
poet$简介=gsub(pattern = "\\(",replacement = "（",x =poet$简介)
poet$简介=gsub(pattern = "\\)",replacement = "）",x =poet$简介)
poet$简介=gsub(pattern = "（",replacement = "\\[",x =poet$简介)
poet$简介=gsub(pattern = "）",replacement = "\\]",x =poet$简介)
poet$简介=gsub(pattern = "--|--|－|——|～",replacement = "-",x =poet$简介)
poet$简介=gsub(pattern = " ",replacement = "",x =poet$简介)
a=stri_match_all_regex(poet$简介,"[0-9-()（）]{7,18}")
b=sapply(a,function(i)is.na(i))
b=sapply(b,function(i)sum(i))
poet$b<-b==0
poetbd<-subset(poet,b==1)#x选定有生卒年份的诗人
poetbd$b<-unlist(stri_match_all_regex(poetbd$简介,"[0-9-()（）]{7,18}"))
```

3. 朝代预处理

```{r}
f<-function(p,r){gsub(pattern = p,replacement = r,x =poem$年代)}
poem$年代<-f("战国","先秦")
poem$年代<-f("南朝","南北")
poem$年代<-f("梁","南北")
poem$年代<-f("南唐","五代")
poem$年代<-f(" ","")
poem$年代<-f("三国","魏晋")
as.data.frame(table(poem$年代))
dynasty<-read.csv("dynasty.csv")
dynasty$med<-(dynasty$end-dynasty$establish)/2+dynasty$establish
```

4.分词和建立矩阵

```{r}
library(jiebaR)
library(slam)
library(tm)
library(tmcn)
mixseg<-worker()
system.time(x <- lapply(poem$内容, function(x) segment(x,mixseg)))
msgWords <- as.data.frame(cbind(rep(poem[,"年代"], unlist(lapply(x, length))), 
                                     unlist(x),
                                     names(unlist(x))), stringsAsFactors = F)
     names(msgWords) <- c("Docid", "Term")
creat_tdm<-function(sentence,msg,cut.var){
     msgWords <- as.data.frame(cbind(rep(msg[,cut.var], unlist(lapply(x, length))), 
                                     unlist(x),
                                     names(unlist(x))), stringsAsFactors = F)
     names(msgWords) <- c("Docid", "Term")
     msgWords <- msgWords[which(nchar(msgWords$Term) >= 1),]
     msgWords$Term <- gsub(" ", "", msgWords$Term)
     msgWords$Term <- gsub("[/r/n]", "", msgWords$Term)
     #清理数据
     x <- msgWords[, 1:2]
     x <- split(x$Term, x$Docid)#数据框转化为list
     wordcorpus <- Corpus(VectorSource(x)) # 组成语料库格式
     #生成矩阵
     library(slam)
     tdm_year=TermDocumentMatrixCN (wordcorpus,
                                  control = list(
                                    wordLengths=c(1, Inf), # 限制词长
                                    bounds = list(global = c(1,Inf)), # 设置词的最小频率
                                    removeNumbers = TRUE, 
                                    weighting = weightTf, 
                                    encoding = "UTF-8"))
}
tdm_dynasty<-creat_tdm(poem$内容,poem,"年代")
tdm_dynasty1<-removeSparseTerms(tdm_dynasty,sparse = 0.8)
tdm_dynastym<-as.matrix(tdm_dynasty1)
d=t(tdm_dynastym)/col_norms(tdm_dynastym,1)#L1正则化
dist(d,"divergence")
```



