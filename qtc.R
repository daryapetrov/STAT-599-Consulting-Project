#load library
library(ggplot2)

#load data
data_cleaned <- read.csv("data_cleaned.csv",row.names = 1)


ekg_both_agit <- data_cleaned[which(data_cleaned$Indication=="Agitation" & data_cleaned$EKG.obtained.before.=="Y" & data_cleaned$EKG.obtained.after. == "Y"),]
n <- dim(ekg_both_agit)[1]
paired_samples_df <- data.frame(matrix(rep(1:n ,2),nrow = 2*n ))
colnames(paired_samples_df) <- c("id")
paired_samples_df$QTc <- c(ekg_both_agit$QTc..before.,ekg_both_agit$QTc..after.)
paired_samples_df$Time <- c(rep("Before",n) , rep("After",n ))
paired_samples_df$Time <- factor(paired_samples_df$Time, levels = c("Before","After"))
paired_samples_df$Arrhythmia <- as.factor(rep(c("Y","N","N","N","N"),2))

#FIGURE 4
ggplot(paired_samples_df, aes(x = Time, y = QTc)) + 
  geom_point(aes(color = Arrhythmia), alpha=1) +  
  geom_line(aes(color=Arrhythmia, group = id)) +
  scale_color_manual(values = c("palegreen4", "red4")) + 
  geom_hline(aes(yintercept = 500), col = "black",linetype="dotted")+
  labs(title = "Paired QTc Measurements for 5 Agitated Patients", x = "")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 15))  