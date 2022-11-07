
####libs####
library(vegan)
library(reshape2)
library(cluster)
library(factoextra)
library(ggfortify)
library(bipartite)
library(igraph)
library(sna)
library(d3network)
library(graphviz)
library(qgraph)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(agricolae)


####data preparation####

options(scipen=999)

t11<-read.csv("t1_30VIII_MD.csv", sep=";", dec=",")
sect.trees2020<-as.data.frame(subset(t11, t11$year=="2020"))#subset, year 2020
sect.trees2021<-as.data.frame(subset(t11, t11$year=="2021"))# 2021

sect.trees2020[,c(1:5)]==sect.trees2021[,c(1:5)]

colnames(sect.trees2021)
sect.deco.2020<-decostand(sect.trees2020[,c(6:53)], "pa")#transformation for year�
sect.deco.2021<-decostand(sect.trees2021[,c(6:53)], "pa")

ptaki.sumy<-decostand(sect.deco.2020+sect.deco.2021, "pa")#suming years

dane.sect<-as.data.frame(cbind(sect.trees2020[,3], ptaki.sumy))#a sector must be added to the attendance data


dane.melt<-melt(dane.sect, id.vars="sect.trees2020[, 3]")#we melt down to group by sector                    
names(dane.melt)<-c("sektor", "gatunek", "frekwencja")

dane.cast<-dcast(dane.melt, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast, "dane.cast.csv", sep=";") 

dane.sieci<-read.csv("dane.cast.csv", sep=";", dec=",")




####Fig.1. in MS -> map
#### Fig.2. in MS -> methods






#### Fig.3. mds for tree-independent sectors

#### NMDs####

colnames(dane.sieci)
dane.deco<-decostand(dane.sieci[-c(13,14),], "normalize")
mds.sektory<-metaMDS(dane.deco)#analiza MDS

decorana(dane.deco)

spec.scores<-as.data.frame(cbind(mds.sektory$species[,1], mds.sektory$species[,2]))
site.scores<-as.data.frame(cbind(mds.sektory$points[,1], mds.sektory$points[,2]))

rownames(site.scores)
kolorki<-c(spec1<-rep("spec", 48),sit1<-rep("site", 16))

rownames(sajty)
sajty<-as.data.frame(cbind(rbind(spec.scores, site.scores), kolorki))
names(sajty)<-c("NMDS1", "NMDS2", "kolorki")

rownames(sajty)


ggplot(sajty, aes(x=NMDS1, y=NMDS2, col=factor(kolorki)))+
  geom_point()+
  geom_text(data=sajty, aes(x=NMDS1, y=NMDS2, label=rownames(sajty)))+
  theme_few()+
  theme(legend.position = "none")







#Fig. 4 mds for sectors within each of the 6 tree species

####mds alnus####

mds2<-metaMDS(dane.sieci.alnus[-14,])
spec.scores.alnus<-as.data.frame(cbind(mds2$species[,1], mds2$species[,2]))
site.scores.alnus<-as.data.frame(cbind(mds2$points[,1], mds2$points[,2]))
rownames(spec.scores.alnus)
rownames(site.scores.alnus)

decorana(dane.sieci.alnus)

kolorki.alnus<-c(spec1<-rep("spec", 48),sit1<-rep("site", 17))


sajty.alnus<-as.data.frame(cbind(rbind(spec.scores.alnus, site.scores.alnus), kolorki.alnus))
names(sajty.alnus)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.alnus, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.alnus)))+
  geom_point()+
  geom_text(data=sajty.alnus, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.alnus)))+
  theme_few()+
  theme(legend.position = "none")

####mds carpinus####
colSums(dane.sieci.carpinus)
rowSums(dane.carp)

dane.carp<-dane.sieci.carpinus[-c(1,3,4,5,9,12,16,23,24,25,33,34,43,48)]

dane.carpinus.deco<-decostand(dane.carp[-c(13,14),], "normalize")
mds3<-metaMDS(dane.carpinus.deco)#z transformacją lepiej
spec.scores.carpinus<-as.data.frame(cbind(mds3$species[,1], mds3$species[,2]))
site.scores.carpinus<-as.data.frame(cbind(mds3$points[,1], mds3$points[,2]))
rownames(spec.scores.carpinus)
rownames(site.scores.carpinus)
decorana(dane.carpinus.deco)

kolorki.carpinus<-c(spec1<-rep("spec", 34),sit1<-rep("site", 16))


sajty.carpinus<-as.data.frame(cbind(rbind(spec.scores.carpinus, site.scores.carpinus), kolorki.carpinus))
names(sajty.carpinus)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.carpinus, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.carpinus)))+
  geom_point()+
  geom_text(data=sajty.carpinus, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.carpinus)))+
  theme_few()+
  theme(legend.position = "none")


####mds tilia####
colSums(dane.sieci.tilia)
rowSums(dane.tilia)

dane.tilia<-dane.sieci.tilia[-c(2:5,9,19,23:27,34,40,43,45,47,48)]

dane.tilia.deco<-decostand(dane.tilia[-c(13,14),], "normalize")# z transformacją lepiej
mds4<-metaMDS(dane.tilia.deco)
spec.scores.tilia<-as.data.frame(cbind(mds4$species[,1], mds4$species[,2]))
site.scores.tilia<-as.data.frame(cbind(mds4$points[,1], mds4$points[,2]))
rownames(spec.scores.tilia)
rownames(site.scores.tilia)
decorana(dane.tilia.deco)

kolorki.tilia<-c(spec1<-rep("spec", 31),sit1<-rep("site", 16))


sajty.tilia<-as.data.frame(cbind(rbind(spec.scores.tilia, site.scores.tilia), kolorki.tilia))
names(sajty.tilia)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.tilia, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.tilia)))+
  geom_point()+
  geom_text(data=sajty.tilia, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.tilia)))+
  theme_few()+
  theme(legend.position = "none")


####mds acer####
colSums(dane.sieci.acer)
rowSums(dane.acer)

dane.acer<-dane.sieci.acer[-c(2,3,5,9,10,12,14,15,19,20,23:25,27,29,30,32:35,37,39,41,44,46,49)]


dane.acer.deco<-decostand(dane.acer[-c(13,14,17),], "normalize") #z transformacją lepiej
mds5<-metaMDS(dane.acer.deco)
spec.scores.acer<-as.data.frame(cbind(mds5$species[,1], mds5$species[,2]))
site.scores.acer<-as.data.frame(cbind(mds5$points[,1], mds5$points[,2]))
rownames(spec.scores.acer)
rownames(site.scores.acer)

decorana(dane.acer.deco)

kolorki.acer<-c(spec1<-rep("spec", 23),sit1<-rep("site", 15))


sajty.acer<-as.data.frame(cbind(rbind(spec.scores.acer, site.scores.acer), kolorki.acer))
names(sajty.acer)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.acer, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.acer)))+
  geom_point()+
  geom_text(data=sajty.acer, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.acer)))+
  theme_few()+
  theme(legend.position = "none")


####mds quercus####
colSums(dane.sieci.quercus)
rowSums(dane.quercus)

dane.quercus<-dane.sieci.quercus[-c(2,4,5,9,10,19,22:27,29,31:36,40,43:45,48)]

rownames(dane.sieci.quercus)
dane.quercus.deco<-decostand(dane.quercus[-c(11,13,14,17),], "normalize")
mds6<-metaMDS(dane.quercus.deco)
spec.scores.quercus<-as.data.frame(cbind(mds6$species[,1], mds6$species[,2]))
site.scores.quercus<-as.data.frame(cbind(mds6$points[,1], mds6$points[,2]))
rownames(spec.scores.quercus)
rownames(site.scores.quercus)

decorana(dane.sieci.quercus)
kolorki.quercus<-c(spec1<-rep("spec", 24),sit1<-rep("site", 14))


sajty.quercus<-as.data.frame(cbind(rbind(spec.scores.quercus, site.scores.quercus), kolorki.quercus))
names(sajty.quercus)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.quercus, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.quercus)))+
  geom_point()+
  geom_text(data=sajty.quercus, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.quercus)))+
  theme_few()+
  theme(legend.position = "none")



####mds picea#### z transformacją lepiej
colSums(dane.sieci.picea)
rowSums(dane.picea)

dane.picea<-dane.sieci.picea[-c(1,3:5,14,15,23:25,27,31,32,43,45,48)]

dane.picea.deco<-decostand(dane.picea[-c(13,14),], "normalize")
mds7<-metaMDS(dane.picea.deco)
spec.scores.picea<-as.data.frame(cbind(mds7$species[,1], mds7$species[,2]))
site.scores.picea<-as.data.frame(cbind(mds7$points[,1], mds7$points[,2]))
rownames(spec.scores.picea)
rownames(site.scores.picea)

kolorki.picea<-c(spec1<-rep("spec", 33),sit1<-rep("site", 16))


sajty.picea<-as.data.frame(cbind(rbind(spec.scores.picea, site.scores.picea), kolorki.picea))
names(sajty.picea)<-c("NMDS1", "NMDS2", "kolorki")


ggplot(sajty.picea, aes(x=NMDS1, y=NMDS2, col=factor(kolorki.picea)))+
  geom_point()+
  geom_text(data=sajty.picea, aes(x=NMDS1, y=NMDS2, label=rownames(sajty.picea)))+
  theme_few()+
  theme(legend.position = "none")







#### Fig. 5. a network for sectors independent of trees
# a5 landscape

####sector level network independent of trees ####

colSums(dane.sieci)
rowSums(dane.sieci)
rownames(dane.sieci)

webs1 <- as.matrix(t(dane.sieci))
lapply(webs1, head, n = 2L)
plotweb(t(webs1), text.rot=90, col.low = "red", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")







### Fig.6 webs for all tree species

#Fig. 6, networks for sectors within each of the 6 tree species
#a5 landscape

####ALNUS GLUTINOSA####

alnus<-as.data.frame(subset(t11, t11$tree.spec=="AG"))

colnames(alnus)

sect.alnus2020<-as.data.frame(subset(alnus, alnus$year=="2020"))
sect.alnus2021<-as.data.frame(subset(alnus, alnus$year=="2021"))

colnames(sect.alnus2020)

sect.deco.2020.alnus<-decostand(sect.alnus2020[,c(6:53)], "pa")
sect.deco.2021.alnus<-decostand(sect.alnus2021[,c(6:53)], "pa")

ptaki.sumy.alnus<-decostand(sect.deco.2020.alnus+sect.deco.2021.alnus, "pa")

dane.sect.alnus<-as.data.frame(cbind(sect.alnus2020[,3], ptaki.sumy.alnus))


dane.melt.alnus<-melt(dane.sect.alnus, id.vars="sect.alnus2020[, 3]")                     
names(dane.melt.alnus)<-c("sektor", "gatunek", "frekwencja")

dane.cast.alnus<-dcast(dane.melt.alnus, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.alnus, "dane.cast.alnus.csv", sep=";")

dane.sieci.alnus<-read.csv("dane.cast.alnus.csv", sep=";", dec=",")

rowSums(dane.sieci.alnus)

webs2 <- as.matrix(t(dane.sieci.alnus))
lapply(webs2, head, n = 2L)
plotweb(t(webs2), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")


####CARPINUS####

carpinus<-as.data.frame(subset(t11, t11$tree.spec=="CB"))

colnames(carpinus)

sect.carpinus2020<-as.data.frame(subset(carpinus, carpinus$year=="2020"))
sect.carpinus2021<-as.data.frame(subset(carpinus, carpinus$year=="2021"))

colnames(sect.carpinus2020)

sect.deco.2020.carpinus<-decostand(sect.carpinus2020[,c(6:53)], "pa")
sect.deco.2021.carpinus<-decostand(sect.carpinus2021[,c(6:53)], "pa")

ptaki.sumy.carpinus<-decostand(sect.deco.2020.carpinus+sect.deco.2021.carpinus, "pa")

dane.sect.carpinus<-as.data.frame(cbind(sect.carpinus2020[,3], ptaki.sumy.carpinus))


dane.melt.carpinus<-melt(dane.sect.carpinus, id.vars="sect.carpinus2020[, 3]")                     
names(dane.melt.carpinus)<-c("sektor", "gatunek", "frekwencja")

dane.cast.carpinus<-dcast(dane.melt.carpinus, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.carpinus, "dane.cast.carpinus.csv", sep=";")

dane.sieci.carpinus<-read.csv("dane.cast.carpinus.csv", sep=";", dec=",")

webs3 <- as.matrix(t(dane.sieci.carpinus))
lapply(webs3, head, n = 2L)
plotweb(t(webs3), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")





####TILIA####

tilia<-as.data.frame(subset(t11, t11$tree.spec=="TC"))

colnames(tilia)

sect.tilia2020<-as.data.frame(subset(tilia, tilia$year=="2020"))
sect.tilia2021<-as.data.frame(subset(tilia, tilia$year=="2021"))

colnames(sect.tilia2020)

sect.deco.2020.tilia<-decostand(sect.tilia2020[,c(6:53)], "pa")
sect.deco.2021.tilia<-decostand(sect.tilia2021[,c(6:53)], "pa")

ptaki.sumy.tilia<-decostand(sect.deco.2020.tilia+sect.deco.2021.tilia, "pa")

dane.sect.tilia<-as.data.frame(cbind(sect.tilia2020[,3], ptaki.sumy.tilia))


dane.melt.tilia<-melt(dane.sect.tilia, id.vars="sect.tilia2020[, 3]")                     
names(dane.melt.tilia)<-c("sektor", "gatunek", "frekwencja")

dane.cast.tilia<-dcast(dane.melt.tilia, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.tilia, "dane.cast.tilia.csv", sep=";")

dane.sieci.tilia<-read.csv("dane.cast.tilia.csv", sep=";", dec=",")

webs4 <- as.matrix(t(dane.sieci.tilia))
lapply(webs4, head, n = 2L)
plotweb(t(webs4), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")


####ACER####

acer<-as.data.frame(subset(t11, t11$tree.spec=="AP"))

colnames(acer)

sect.acer2020<-as.data.frame(subset(acer, acer$year=="2020"))
sect.acer2021<-as.data.frame(subset(acer, acer$year=="2021"))

colnames(sect.acer2020)

sect.deco.2020.acer<-decostand(sect.acer2020[,c(6:53)], "pa")
sect.deco.2021.acer<-decostand(sect.acer2021[,c(6:53)], "pa")

ptaki.sumy.acer<-decostand(sect.deco.2020.acer+sect.deco.2021.acer, "pa")

dane.sect.acer<-as.data.frame(cbind(sect.acer2020[,3], ptaki.sumy.acer))


dane.melt.acer<-melt(dane.sect.acer, id.vars="sect.acer2020[, 3]")                     
names(dane.melt.acer)<-c("sektor", "gatunek", "frekwencja")

dane.cast.acer<-dcast(dane.melt.acer, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.acer, "dane.cast.acer.csv", sep=";")

dane.sieci.acer<-read.csv("dane.cast.acer.csv", sep=";", dec=",")

rowSums(dane.sieci.acer)



webs5 <- as.matrix(t(dane.sieci.acer))
lapply(webs5, head, n = 2L)
plotweb(t(webs5), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")


####QUERCUS####

quercus<-as.data.frame(subset(t11, t11$tree.spec=="Q"))

colnames(quercus)

sect.quercus2020<-as.data.frame(subset(quercus, quercus$year=="2020"))
sect.quercus2021<-as.data.frame(subset(quercus, quercus$year=="2021"))

colnames(sect.quercus2020)

sect.deco.2020.quercus<-decostand(sect.quercus2020[,c(6:53)], "pa")
sect.deco.2021.quercus<-decostand(sect.quercus2021[,c(6:53)], "pa")

ptaki.sumy.quercus<-decostand(sect.deco.2020.quercus+sect.deco.2021.quercus, "pa")

dane.sect.quercus<-as.data.frame(cbind(sect.quercus2020[,3], ptaki.sumy.quercus))


dane.melt.quercus<-melt(dane.sect.quercus, id.vars="sect.quercus2020[, 3]")                     
names(dane.melt.quercus)<-c("sektor", "gatunek", "frekwencja")

dane.cast.quercus<-dcast(dane.melt.quercus, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.quercus, "dane.cast.quercus.csv", sep=";")

dane.sieci.quercus<-read.csv("dane.cast.quercus.csv", sep=";", dec=",")

webs6 <- as.matrix(t(dane.sieci.quercus))
lapply(webs6, head, n = 2L)
plotweb(t(webs6), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")


####PICEA ABIES####

picea<-as.data.frame(subset(t11, t11$tree.spec=="PA"))

colnames(picea)

sect.picea2020<-as.data.frame(subset(picea, picea$year=="2020"))
sect.picea2021<-as.data.frame(subset(picea, picea$year=="2021"))

colnames(sect.picea2020)

sect.deco.2020.picea<-decostand(sect.picea2020[,c(6:53)], "pa")
sect.deco.2021.picea<-decostand(sect.picea2021[,c(6:53)], "pa")

ptaki.sumy.picea<-decostand(sect.deco.2020.picea+sect.deco.2021.picea, "pa")

dane.sect.picea<-as.data.frame(cbind(sect.picea2020[,3], ptaki.sumy.picea))


dane.melt.picea<-melt(dane.sect.picea, id.vars="sect.picea2020[, 3]")                     
names(dane.melt.picea)<-c("sektor", "gatunek", "frekwencja")

dane.cast.picea<-dcast(dane.melt.picea, sektor~gatunek, fun.aggregate = sum)

#write.table(dane.cast.picea, "dane.cast.picea.csv", sep=";")

dane.sieci.picea<-read.csv("dane.cast.picea.csv", sep=";", dec=",")

webs7 <- as.matrix(t(dane.sieci.picea))
lapply(webs7, head, n = 2L)
plotweb(t(webs7), text.rot=90, col.low = "springgreen4", col.high = "springgreen4",
        y.width.low=0.025,y.width.high=0.025, ybig=0.1, col.interaction="springgreen3",
        arrow="down.center", bor.col.interaction ="black")








### Fig. 7.boxplots and emmeansy from d.index and RR separately for each tree in sector frames

#### glmbeta for species specialization index d'

library(glmmTMB)
library(emmeans)
library(multcomp)
library(multcompView)

#speciale$tree = with(speciale, reorder(tree, d.index)) 
ss1<-ggplot(speciale, aes(x=tree, y=d.index, fill=tree))+geom_boxplot()+theme_few()

#write.table(speciale, "speciale.csv", sep=";", dec=",")
speciale.beta<-read.csv("speciale.csv", sep=";", dec=",")

mod.beta1<-glmmTMB(d.index~tree, data=speciale.beta, family=beta_family())
summary(mod.beta1)


mod.zero1<-glmmTMB(d.index~1, data=speciale.beta, family=beta_family())

AIC(mod.zero1, mod.beta1)

library(car)
Anova(mod.beta1)


emy.beta<-cld(emmeans(mod.beta1, ~tree, type="response"))


em.beta<-data.frame(tree=emy.beta$tree,
                    response =emy.beta$response ,
                    se=emy.beta$SE)

#em.beta$tree = with(em.beta, reorder(tree, response))
ss2<-ggplot(em.beta, aes(x=tree, y=response, fill=tree)) + 
  geom_bar(stat="identity", col="black") +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.7)+theme_few()

grid.arrange(ss1, ss2, ncol=2, nrow=1)

#### glm for resource range index RR

#write.table(resourcee, "resourcee.csv", sep=";", dec=",")
resourcee.beta<-read.csv("resourcee.csv", sep=";", dec=",")


#resourcee$tree = with(resourcee, reorder(tree, RR)) 
ss1.r<-ggplot(resourcee.beta, aes(x=tree, y=RR, fill=tree))+geom_boxplot()+theme_few()

mod.beta2<-glmmTMB(RR~tree, data=resourcee.beta, family=beta_family())
summary(mod.beta2)


mod.zero2<-glmmTMB(RR~1, data=resourcee.beta, family=beta_family())

AIC(mod.zero2, mod.beta2)

library(car)
Anova(mod.beta2)


emy.beta2<-cld(emmeans(mod.beta2, ~tree, type="response"))


em.beta2<-data.frame(tree=emy.beta2$tree,
                     response =emy.beta2$response ,
                     se=emy.beta2$SE)

#em.beta$tree = with(em.beta, reorder(tree, response))
ss2.r<-ggplot(em.beta2, aes(x=tree, y=response, fill=tree)) + 
  geom_bar(stat="identity", col="black") +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.7)+theme_few()

grid.arrange(ss1.r, ss2.r, ncol=2, nrow=1)



#####all in one
grid.arrange(ss1, ss2, ss1.r, ss2.r, ncol=2, nrow=2)








#Fig.  8
#specificization of distance from the trunk and from the ground

mod.beta31<-glmmTMB(d.index~pos.trunk+pos.ground, data=position[-c(1:17),], family=beta_family())
summary(mod.beta3)

emy.beta31<-cld(emmeans(mod.beta3, ~pos.trunk+pos.ground, type="response"))


em.beta31<-data.frame(pos.trunk=emy.beta3$pos.trunk,
                      pos.ground=emy.beta3$pos.ground,
                      response =emy.beta3$response ,
                      se=emy.beta3$SE)


mod.zero31<-glmmTMB(d.index~1, data=position[-c(1:17),], family=beta_family())

AIC(mod.zero31, mod.beta3)

library(car)
Anova(mod.beta31)


dist1<-ggplot(em.beta31, aes(x=pos.trunk, y=response, fill=pos.ground)) + 
  scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d"))+
  geom_bar(stat="identity", position=position_dodge(), col="black") +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.7, position=position_dodge(.9))+
  #coord_flip()+
  theme_few()+
  ggtitle("specialization d' index")

#RR 

mod.beta4<-glmmTMB(RR~pos.trunk+pos.ground, data=position[-c(1:17),], family=beta_family())
summary(mod.beta4)

emy.beta4<-cld(emmeans(mod.beta4, ~pos.trunk+pos.ground, type="response"))


em.beta4<-data.frame(pos.trunk=emy.beta4$pos.trunk,
                     pos.ground=emy.beta4$pos.ground,
                     response =emy.beta4$response ,
                     se=emy.beta4$SE)


null4<-glmmTMB(RR~1, data=position[-c(1:17),], family=beta_family())

AIC(mod.beta4, null4)
Anova(mod.beta4)


dist2<-ggplot(em.beta4, aes(x=pos.trunk, y=response, fill=pos.ground)) + 
  scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d"))+
  geom_bar(stat="identity", position=position_dodge(), col="black") +
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.7, position=position_dodge(.9))+
  #coord_flip()+
  theme_few()+
  ggtitle("resource range index")


grid.arrange(dist1, dist2, ncol=1, nrow=2)#a4 portrait









###Fig.9.dependencies between d.index and RR separately for each tree
position<-read.csv("position.csv", sep=";", dec=",")

library(ggeffects)

mod1<-glmmTMB(RR~d.index, data=subset(position, position$tree=="all"), family=beta_family())
mod2<-glmmTMB(RR~d.index, data=subset(position, position$tree=="AG"), family=beta_family())
mod3<-glmmTMB(RR~d.index, data=subset(position, position$tree=="AP"), family=beta_family())
mod4<-glmmTMB(RR~d.index, data=subset(position, position$tree=="CB"), family=beta_family())
mod5<-glmmTMB(RR~d.index, data=subset(position, position$tree=="PA"), family=beta_family())
mod6<-glmmTMB(RR~d.index, data=subset(position, position$tree=="QR"), family=beta_family())
mod7<-glmmTMB(RR~d.index, data=subset(position, position$tree=="TC"), family=beta_family())

summary(mod7)





mb1<-plot(ggpredict(mod1))$d.index
mb2<-plot(ggpredict(mod2))$d.index
mb3<-plot(ggpredict(mod3))$d.index
mb4<-plot(ggpredict(mod4))$d.index
mb5<-plot(ggpredict(mod5))$d.index
mb6<-plot(ggpredict(mod6))$d.index
mb7<-plot(ggpredict(mod7))$d.index

grid.arrange(mb1, mb2, mb3, mb4, mb5, mb6, mb7, ncol=4, nrow=2)#a5 landscape









# Fig. 10

dupa<-dcast(speciale, sektory.id~tree, value.var="d.index")

#write.table(dupa, "dupa.csv", sep=";", dec=",")
all<-read.csv("all1.csv", sep=";", dec=",")
alnus1<-read.csv("alnus1.csv", sep=";", dec=",")
carpinus1<-read.csv("carpinus1.csv", sep=";", dec=",")
tilia1<-read.csv("tilia1.csv", sep=";", dec=",")
quercus1<-read.csv("quercus1.csv", sep=";", dec=",")
picea1<-read.csv("picea1.csv", sep=";", dec=",")
acer1<-read.csv("acer1.csv", sep=";", dec=",")

#melt
all.m<-melt(all, id.vars="position")
carpinus.m<-melt(carpinus1, id.vars="position")
alnus.m<-melt(alnus1, id.vars="position")
tilia.m<-melt(tilia1, id.vars="position")
quercus.m<-melt(quercus1, id.vars="position")
picea.m<-melt(picea1, id.vars="position")
acer.m<-melt(acer1, id.vars="position")

all.m$tree<-rep("1 all tree species", 18)
carpinus.m$tree<-rep("4 Carpinus betulus", 18)
alnus.m$tree<-rep("3 Alnus glutinosa", 18)
tilia.m$tree<-rep("7 Tilia cordata", 18)
quercus.m$tree<-rep("6 Quercus robur", 18)
picea.m$tree<-rep("5 Picea abies", 18)
acer.m$tree<-rep("2 Acer platanoides", 18)


heat<-rbind(all.m, carpinus.m, alnus.m, tilia.m,
            quercus.m, picea.m, acer.m)

ggplot(heat, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  facet_wrap(~tree)+
  theme_few()



h1<-ggplot(all.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("all")
h2<-ggplot(carpinus.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("carpinus")
h3<-ggplot(alnus.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("alnus")
h4<-ggplot(tilia.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("tilia")
h5<-ggplot(quercus.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("quercus")
h6<-ggplot(picea.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("picea")
h7<-ggplot(acer.m, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("acer")

grid.arrange(h1, h7, h3, h2, h6, h5, h4, nrow=2, ncol=4)# all in one
# Fig. 10 landcsape 8x13







#Fig. 11


dupa.rr<-dcast(resourcee, sektory.id~tree, value.var="RR")

#write.table(dupa.rr, "dupa.rr.csv", sep=";", dec=",")
all.rr<-read.csv("all.rr.csv", sep=";", dec=",")
alnus.rr<-read.csv("alnus.rr.csv", sep=";", dec=",")
carpinus.rr<-read.csv("carpinus.rr.csv", sep=";", dec=",")
tilia.rr<-read.csv("tilia.rr.csv", sep=";", dec=",")
quercus.rr<-read.csv("quercus.rr.csv", sep=";", dec=",")
picea.rr<-read.csv("picea.rr.csv", sep=";", dec=",")
acer.rr<-read.csv("acer.rr.csv", sep=";", dec=",")

all.m.rr<-melt(all.rr, id.vars="position")
carpinus.m.rr<-melt(carpinus.rr, id.vars="position")
alnus.m.rr<-melt(alnus.rr, id.vars="position")
tilia.m.rr<-melt(tilia.rr, id.vars="position")
quercus.m.rr<-melt(quercus.rr, id.vars="position")
picea.m.rr<-melt(picea.rr, id.vars="position")
acer.m.rr<-melt(acer.rr, id.vars="position")

all.m.rr$tree<-rep("1 all tree species", 18)
carpinus.m.rr$tree<-rep("4 Carpinus betulus", 18)
alnus.m.rr$tree<-rep("3 Alnus glutinosa", 18)
tilia.m.rr$tree<-rep("7 Tilia cordata", 18)
quercus.m.rr$tree<-rep("6 Quercus robur", 18)
picea.m.rr$tree<-rep("5 Picea abies", 18)
acer.m.rr$tree<-rep("2 Acer platanoides", 18)


heat.rr<-rbind(all.m.rr, carpinus.m.rr, alnus.m.rr, tilia.m.rr,
               quercus.m.rr, picea.m.rr, acer.m.rr)

ggplot(heat.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  facet_wrap(~tree)+
  theme_few()



h1r<-ggplot(all.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("all")
h2r<-ggplot(carpinus.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("carpinus")
h3r<-ggplot(alnus.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("alnus")
h4r<-ggplot(tilia.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("tilia")
h5r<-ggplot(quercus.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("quercus")
h6r<-ggplot(picea.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("picea")
h7r<-ggplot(acer.m.rr, aes(x=variable, y=position, fill= value)) + 
  geom_tile(col="black")+
  scale_fill_gradient(low="#a6bddb", high="#023858")+
  ggtitle("acer")

grid.arrange(h1r, h7r, h3r, h2r, h6r, h5r, h4r, nrow=2, ncol=4)

#Fig. 11 landcsape 8x13