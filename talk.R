### R code from vignette source '/home/ken/teaching/rug/talk.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: talk.Rnw:98-100
###################################################
migraine=read.table("migraine.txt",header=T)
migraine


###################################################
### code chunk number 2: talk.Rnw:117-119
###################################################
library(tidyr)
migraine2=gather(migraine,drug,pain,DrugA:DrugC)


###################################################
### code chunk number 3: talk.Rnw:126-127
###################################################
migraine2


###################################################
### code chunk number 4: talk.Rnw:144-145
###################################################
boxplot(pain~drug,data=migraine2)


###################################################
### code chunk number 5: talk.Rnw:190-192
###################################################
adhd=read.table("adhd.txt",header=T)
spread(adhd,When,Score)


###################################################
### code chunk number 6: talk.Rnw:230-233
###################################################
disease=read.table("disease.txt",header=T)
tmp=gather(disease,dis.loc,frequency,-Species)
tmp


###################################################
### code chunk number 7: talk.Rnw:245-246
###################################################
separate(tmp,dis.loc,c("disease","location"),1)


###################################################
### code chunk number 8: talk.Rnw:270-272
###################################################
adhd
library(dplyr)


###################################################
### code chunk number 9: talk.Rnw:282-283
###################################################
filter(adhd,Subject==2)


###################################################
### code chunk number 10: talk.Rnw:288-289
###################################################
filter(adhd,When=="t3")


###################################################
### code chunk number 11: talk.Rnw:300-301
###################################################
filter(adhd,Score>25)


###################################################
### code chunk number 12: talk.Rnw:306-307
###################################################
filter(adhd,Subject==2 | Score>25)


###################################################
### code chunk number 13: talk.Rnw:319-321
###################################################
select(adhd,c(Subject,
  Score))


###################################################
### code chunk number 14: talk.Rnw:324-325
###################################################
select(adhd,-When)


###################################################
### code chunk number 15: talk.Rnw:337-339
###################################################
tmp=mutate(adhd,pct=Score/30*100)
head(tmp)


###################################################
### code chunk number 16: talk.Rnw:347-350
###################################################
tmp=mutate(adhd,pct=Score/30*100)
tmp2=select(tmp,-Score)
head(tmp2)


###################################################
### code chunk number 17: talk.Rnw:364-368
###################################################
adhd %>% 
  mutate(pct=Score/30*100) %>%
  select(-Score) %>%
  head()


###################################################
### code chunk number 18: talk.Rnw:378-381
###################################################
adhd %>% 
  mutate(pct=Score/30*100) %>%
  boxplot(pct~Subject,data=.)


###################################################
### code chunk number 19: talk.Rnw:391-395
###################################################
adhd %>% 
  mutate(pct=Score/30*100) %>%
  select(-Score) -> adhd.2
head(adhd.2)


###################################################
### code chunk number 20: talk.Rnw:409-410
###################################################
summarize(adhd.2,mean=mean(pct))


###################################################
### code chunk number 21: talk.Rnw:415-419
###################################################
adhd.2 %>%
  group_by(Subject) %>%
  summarize( pct.mean=mean(pct),
             pct.sd=sd(pct) )


###################################################
### code chunk number 22: talk.Rnw:465-468
###################################################
library(ggplot2)
p=ggplot(adhd.2,aes(x=pct))
p+geom_histogram()


###################################################
### code chunk number 23: talk.Rnw:475-477
###################################################
p=ggplot(adhd.2,aes(x=pct))
p+geom_histogram(binwidth=10)


###################################################
### code chunk number 24: talk.Rnw:484-487
###################################################
p=ggplot(adhd.2,aes(x=pct))
p+geom_histogram(binwidth=10,aes(y=..density..))+
  geom_density(col="blue")


###################################################
### code chunk number 25: talk.Rnw:495-497
###################################################
p=ggplot(adhd.2,aes(x=When,y=pct))
p+geom_boxplot()


###################################################
### code chunk number 26: talk.Rnw:504-506
###################################################
p=ggplot(adhd.2,aes(x=factor(Subject),y=pct))
p+geom_boxplot()


###################################################
### code chunk number 27: talk.Rnw:513-515
###################################################
p=ggplot(adhd.2,aes(x=When,y=pct,group=Subject))
p+geom_line()


###################################################
### code chunk number 28: talk.Rnw:522-524
###################################################
p=ggplot(adhd.2,aes(x=When,y=pct,group=Subject))
p+geom_line(aes(colour=factor(Subject)))


###################################################
### code chunk number 29: fred
###################################################
adhd %>% 
  mutate(pct=Score/30*100) %>%
  select(-Score) %>%
  ggplot(aes(x=When,y=pct,group=Subject)) +
    geom_line(aes(colour=factor(Subject)))


###################################################
### code chunk number 30: fred
###################################################
adhd %>% 
  mutate(pct=Score/30*100) %>%
  select(-Score) %>%
  ggplot(aes(x=When,y=pct,group=Subject)) +
    geom_line(aes(colour=factor(Subject)))


###################################################
### code chunk number 31: talk.Rnw:579-582
###################################################
disease %>% gather(dis.loc,frequency,-Species) %>%
  separate(dis.loc,c("disease","location"),1) -> dis.2
head(dis.2,4)


###################################################
### code chunk number 32: talk.Rnw:592-595
###################################################
dis.2 %>%
  ggplot(aes(x=disease,weight=frequency)) + 
    geom_bar() 


###################################################
### code chunk number 33: talk.Rnw:602-605
###################################################
dis.2 %>%
  ggplot(aes(x=disease,weight=frequency)) + 
  geom_bar() + facet_grid(Species ~ location)


###################################################
### code chunk number 34: talk.Rnw:612-617
###################################################
dis.2 %>% 
  ggplot(aes(x=factor(1),weight=frequency,fill=disease)) +
    geom_bar(aes(y=(..count..)/
    tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(Species~location)


###################################################
### code chunk number 35: talk.Rnw:626-632
###################################################
dis.2 %>% 
  ggplot(aes(x=factor(1),weight=frequency,fill=disease)) +
    geom_bar(aes(y=(..count..)/
    tapply(..count..,..PANEL..,sum)[..PANEL..])) +
    facet_grid(Species~location) + 
    coord_polar(theta="y")


###################################################
### code chunk number 36: talk.Rnw:646-648
###################################################
premier=read.csv("premier.csv",header=T)
tail(premier)


###################################################
### code chunk number 37: talk.Rnw:659-662
###################################################
premier %>%
  separate(score,c("home","away")," - ") -> goals
tail(goals)


###################################################
### code chunk number 38: talk.Rnw:670-672
###################################################
goals %>%
  ggplot(aes(x=home,y=away)) + geom_point()


###################################################
### code chunk number 39: talk.Rnw:679-680
###################################################
goals %>% ggplot(aes(x=home, y=away)) + geom_jitter() 


###################################################
### code chunk number 40: talk.Rnw:708-714
###################################################
library(lubridate)
goals %>% 
  mutate(thedate=as.Date(date)) %>%
  mutate(themonth=month(thedate,label=T)) %>%
  ggplot(aes(x=home,y=away,colour=themonth)) + 
    geom_jitter()


###################################################
### code chunk number 41: talk.Rnw:721-727
###################################################
goals %>% 
  mutate(thedate=as.Date(date)) %>%
  mutate(themonth=month(thedate,label=T)) %>%
  ggplot(aes(x=home,y=away,
    colour=themonth)) + 
    geom_jitter()


###################################################
### code chunk number 42: talk.Rnw:735-737
###################################################
goals %>% ggplot(aes(x=home,y=away)) +
  stat_sum(aes(size = factor(..n..)), geom = "point") 

goals %>% ggplot(aes(x=home,y=away)) +
  stat_sum(aes(colour = factor(..n..), size=factor(..n..)), geom = "point") 

attach(goals)
table(home,away)
diff=as.numeric(home)-as.numeric(away)
detach(goals)
str(goals)
diff
table(cut(diff,breaks=c(-10,-0.5,0.5,10)))
