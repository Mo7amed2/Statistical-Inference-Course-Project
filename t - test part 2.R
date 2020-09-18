## EDA 
data("ToothGrowth")
head(ToothGrowth)
summary(ToothGrowth$len)

unique(ToothGrowth$supp)
table(ToothGrowth$supp)

summary(ToothGrowth$dose)
unique(ToothGrowth$dose)

nrow(ToothGrowth)
sapply(ToothGrowth, class)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

## Ploting the dose fig1 
ggplot(aes(x=dose, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=dose)) + xlab("Dose Amount") + ylab("Tooth Length") + facet_grid(~ supp) + ggtitle("Tooth Length vs. Dose Amount \nby Delivery Method") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))


## Ploting the Supply fig2
ggplot(aes(x=supp, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=supp)) + xlab("Supplement Delivery") + ylab("Tooth Length") + facet_grid(~ dose) + ggtitle("Tooth Length vs. Delivery Method \nby Dose Amount") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

## t-test 
t.test(len~supp,data=ToothGrowth)

ToothGrowth_sub1 <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0,0.5))
t.test(len~dose,data=ToothGrowth_sub1)

ToothGrowth_sub2 <- subset(ToothGrowth, ToothGrowth$dose %in% c(0.5,2.0))
t.test(len~dose,data=ToothGrowth_sub2)

ToothGrowth_sub3 <- subset(ToothGrowth, ToothGrowth$dose %in% c(2.0,1.0))
t.test(len~dose,data=ToothGrowth_sub2)


