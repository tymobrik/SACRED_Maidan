#################################################################################
#################################################################################
# Replication code for:
#
# Spatial Analysis of Religious Diversity in Ukraine Before and After Euromaidan
#
# Chapter by Tymofii Brik and Stanislav Korolkov
# 
# Data: SACRED project, Brik, KSE 
# Comments, questions: tbrik@kse.org.ua 
#################################################################################
#################################################################################

#Set your working directory here:
setwd("/Users/tymo-brik/Documents/Brik and Korolkov 2018")

#Libraries to work with the data (youm might want to install them first!)
library(reshape) 
library(gplots)
library(ggplot2)
library(gridExtra)
library(plyr) 
library(xlsx)
library(pheatmap)

Sys.setlocale("LC_CTYPE", "ru_RU")
test = c("привіт")
test

#New function. Open a file, secelt a given religious group, select a status (active vs rejected), seelct a yaer

open_v1 <- function(dataname, name_community, status, sheet_year) {
  
  sheet <- read.xlsx(paste0(dataname, ".xlsx"),
                   sheetName =  sheet_year,
                   startRow = 1,
                   endRow = 28,
                   encoding = "UTF-8")
  
  colnames(sheet) <- gsub('[.]', '', colnames(sheet))
  
  sheet <- sheet[grep(name_community, colnames(sheet))]
  sheet <- sheet[grep(paste0("_", status), colnames(sheet))]
  
  colnames(sheet) <- sheet_year
  return(sheet)
}

#Apply the fanction to our data. Open and save six different datasets for respective religious groups

egov_r <- list()
hve_r <- list()
mp_r <- list()
kp_r <- list()
isl_r <- list()
gk_r <- list()

for (i in (1:5)) {
  
  years <- c(2013:2017)
  years <- as.character(years)
  
  egov_r[[i]] <- open_v1("data_sacred_euromadian_v1", "Єгови", "діючі", years[i])
  hve_r[[i]] <- open_v1("data_sacred_euromadian_v1", "ХВЄ", "діючі", years[i])
  mp_r[[i]] <- open_v1("data_sacred_euromadian_v1", "МП", "діючі", years[i])
  kp_r[[i]] <- open_v1("data_sacred_euromadian_v1", "КП", "діючі", years[i])
  isl_r[[i]] <- open_v1("data_sacred_euromadian_v1", "іслам", "діючі", years[i])
  gk_r[[i]] <- open_v1("data_sacred_euromadian_v1", "УГКЦ", "діючі", years[i])
  
}

#New function for plots

p1 <- list()
p2 <- list()
rate_all <- list()

APA.FORMAT = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))

for (i in 1:6) {
  
  selection <- list(mp_r, kp_r, 
                    egov_r, hve_r, 
                    gk_r, isl_r)
  plot_titles <- c("UOC MP", "UOC KP", "Jehovah witness", "EBC", "UGCH", "Islam")
  
  cmnt <- do.call(cbind, selection[[i]])
  cmnt <- apply(cmnt,2,as.character)
  cmnt <- apply(cmnt,2,as.numeric)
  cmnt <- as.data.frame(cmnt)

  cmnt[1, ] <- apply(cmnt[c(1,27),],2,sum)
  cmnt <- cmnt[-c(1,27), ]

#Sources:
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#https://www.r-graph-gallery.com/barplot/

oblast_names <- read.csv("oblast_names.csv")
oblast_names$Oblast <- gsub(" oblast" , '', oblast_names$Oblast)
cmnt$Oblast <- oblast_names$Oblast[-c(1,27)]

cmnt$rate <- round((100*cmnt$`2017`/cmnt$`2014` )-100,1)
rate_all[[i]] <- cmnt$rate

cmnt$dynamics <- NA
cmnt$dynamics[cmnt$rate>0] <- "dodgerblue4"
cmnt$dynamics[cmnt$rate<0] <- "brown"

cmnt$Oblast <- factor(cmnt$Oblast, levels=c(sort(cmnt$Oblast, decreasing=T)))

p1[[i]] <- ggplot(cmnt, aes(x=Oblast, y=rate, label=rate)) + 
  geom_point(stat='identity', color=cmnt$dynamics, size=8)  +
  geom_segment(aes(y = 0, 
                   x = Oblast, 
                   yend = rate, 
                   xend = Oblast), 
               color = cmnt$dynamics) + 
    geom_hline(yintercept = 0, linetype=1, col = "blue") +
  geom_text(color="white", size=2.5) +
  labs(title=plot_titles[i], 
       subtitle="Communities: rate of change in registration between 2014 and 2017, %") + 
  ylim(-120, 120) +
  APA.FORMAT +
  coord_flip() +
  theme(axis.title.x=element_blank()) 

cmnt$colour <- ifelse(cmnt$rate < 0, "firebrick1","steelblue")
cmnt$hjust <- ifelse(cmnt$rate > 0, 1.3, -0.3)

p2[[i]] <- ggplot(cmnt,aes(Oblast,rate,label="",hjust=hjust))+
  geom_text(aes(y=0,colour=colour))+
  geom_bar(stat="identity",position="identity",aes(fill = colour)) +
  geom_hline(yintercept = -25, linetype=5, col = "lightblue", alpha = 0.5) +
  geom_hline(yintercept =  25, linetype=5, col = "lightblue", alpha = 0.5) +
  geom_hline(yintercept = -50, linetype=5, col = "lightblue", alpha = 0.5) +
  geom_hline(yintercept =  50, linetype=5, col = "lightblue", alpha = 0.5) +
  geom_hline(yintercept =  -75, linetype=5, col = "lightblue", alpha = 0.5) +
  geom_hline(yintercept =  75, linetype=5, col = "lightblue", alpha = 0.5) +
  ylim(-110, 110) +
  APA.FORMAT +
  coord_flip() +
  labs(title=plot_titles[i], 
       subtitle="Communities: rate of change in registration between 2014 and 2017, %") +
  theme(legend.position="none") 

}

#New data for rates of change
data_rates <- as.data.frame(do.call(cbind,rate_all))
colnames(data_rates) <- plot_titles
rownames(data_rates) <- oblast_names$Oblast[-c(1,27)]
data_rates

summary(data_rates)

write.csv(data_rates, "data_rates.csv")

#Heat maps to plot rates


mtscaled = as.matrix(scale(as.matrix(data_rates[,c(1,2)])))
mt = as.matrix(as.matrix(data_rates[,c(1,2)]))

pheatmap(mtscaled, treeheight_row = 0, treeheight_col = 0)
pheatmap(mt, treeheight_row = 0, treeheight_col = 0)


#Plots
annotations <- data.frame(annotateText = "* - Controlled by Ukraine",
                          xpos =Inf,
                          ypos =  Inf,
                          hjustvar = 1,
                          vjustvar = 52)

annotations2 <- data.frame(annotateText2 = "** - Crimea is excluded",
                          xpos2 =Inf,
                          ypos2 =  Inf,
                          hjustvar2 = 1,
                          vjustvar2 = 54)

p2[[2]] <- p2[[2]]+theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
        geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=annotateText)) +
        geom_text(data = annotations2, aes(x=xpos2,y=ypos2,hjust=hjustvar2,
                                    vjust=vjustvar2,label=annotateText2))

p2[[4]] <- p2[[4]]+theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
        geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=annotateText)) +
        geom_text(data = annotations2, aes(x=xpos2,y=ypos2,hjust=hjustvar2,
                                     vjust=vjustvar2,label=annotateText2))

p2[[6]] <- p2[[6]]+theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
        geom_text(data = annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label=annotateText)) +
        geom_text(data = annotations2, aes(x=xpos2,y=ypos2,hjust=hjustvar2,
                                     vjust=vjustvar2,label=annotateText2))

#grid.arrange(p[[1]], p[[2]], ncol = 2, nrow = 1)
#grid.arrange(p[[3]], p[[4]], ncol = 2, nrow = 1)
#grid.arrange(p[[5]], p[[6]], ncol = 2, nrow = 1)

tiff("Figure3.tiff", width = 12, height = 7, units = 'in', res = 600)
grid.arrange(p2[[1]], p2[[2]], ncol = 2, nrow = 1)
dev.off()

tiff("Figure4.tiff", width = 12, height = 7, units = 'in', res = 600)
grid.arrange(p2[[3]], p2[[4]], ncol = 2, nrow = 1)
dev.off()

tiff("Figure5.tiff", width = 12, height = 7, units = 'in', res = 600)
grid.arrange(p2[[5]], p2[[6]], ncol = 2, nrow = 1)
dev.off()

#################################################################################
#################################################################################

#RAS data
#More info here: http://www.thearda.com/ras/

#Making heatmaps for two RAS indexex

ras <- read.csv("RAS.csv")

#VI. Regulation of and Restrictions on the Majority Religion or All Religions
majority_reg <- ras[, grep("N", colnames(ras))]
majority_reg <- majority_reg[, -grep("X14X", colnames(majority_reg))]
majority_reg <- majority_reg[, -grep("FUND", colnames(majority_reg))]
majority_reg <- majority_reg[, -grep("NXX", colnames(majority_reg))]
length(majority_reg)


#VII. Specific Types of Religious Legislation
leg_reg <- ras[, grep("L", colnames(ras))]
leg_reg <- leg_reg[, -grep("X14X", colnames(leg_reg))]
leg_reg <- data.frame(ras[, c(1:3)], leg_reg)
length(leg_reg)
#

years <- c(1990:2014)
leg_reg_tot <- list()
reg_reg_tot <- list()
for(i in 1:length(years)) {
  
  reg_reg_tot[[i]] <- rowSums(majority_reg[, grep(years[i], colnames(majority_reg))], na.rm=T)
  leg_reg_tot[[i]] <- rowSums(leg_reg[, grep(years[i], colnames(leg_reg))], na.rm=T)
}

maj_reg_f <- do.call(cbind, reg_reg_tot)
leg_reg_f <- do.call(cbind, leg_reg_tot)

rownames(maj_reg_f) <- ras$X.COUNTRY
colnames(maj_reg_f) <- paste0("majreg_", years)

rownames(leg_reg_f) <- ras$X.COUNTRY
colnames(leg_reg_f) <- paste0("legisl_", years)


maj_reg_f_long <- melt(maj_reg_f)
names(maj_reg_f_long) <- c("Country", "Year", "Index1")
maj_reg_f_long$Year <- gsub("majreg_", "", maj_reg_f_long$Year)

p_majr <- ggplot(maj_reg_f_long, aes(Year, Country)) + 
                    geom_tile(aes(fill = Index1), colour = "white") + 
                    scale_fill_gradient(low = "white", high = "steelblue") + APA.FORMAT +
                    scale_y_discrete(limits = rev(levels(maj_reg_f_long$Country)))+
                    labs(x = "", y = "Country") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_majr
#
leg_reg_f_long <- melt(leg_reg_f)
names(leg_reg_f_long) <- c("Country", "Year", "Index2")
leg_reg_f_long$Year <- gsub("legisl_", "", leg_reg_f_long$Year)

p_legr <- ggplot(leg_reg_f_long, aes(Year, Country)) + 
  geom_tile(aes(fill = Index2), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + APA.FORMAT +
  scale_y_discrete(limits = rev(levels(leg_reg_f_long$Country)))+
  labs(x = "", y = "Country") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_legr


tiff("Figure1.tiff", width = 12, height = 7, units = 'in', res = 600)
p_majr
dev.off()

tiff("Figure2.tiff", width = 12, height = 7, units = 'in', res = 600)
p_legr
dev.off()

#Summaries
do.call(cbind, egov_r)
do.call(cbind, hve_r)
do.call(cbind, mp_r)
do.call(cbind, kp_r)
do.call(cbind, isl_r)
do.call(cbind, gk_r)

# Thank you for your time
# Comments, questions: tbrik@kse.org.ua 
#################################################################################
#################################################################################