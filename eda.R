#load libraries
library(ggplot2)
library(dplyr)

#load data
data_cleaned <- read.csv("data_cleaned.csv",row.names = 1)

#number of patients in the dataset
n = dim(data_cleaned)[1]

#dimension of data: 650 x 33
dim(data_cleaned) 

#number of patients with and without EKG before and after droperidol
table(data_cleaned$EKG.obtained.before.)
table(data_cleaned$EKG.obtained.after.)

#number of agitated patients that got an EKG before droperidol adminstration
which(data_cleaned$Indication=="Agitation" & data_cleaned$EKG.obtained.before.=="Y")

#5 datapoints for both EKG readings for people with agitation. I should take a look at these.
which(data_cleaned$Indication=="Agitation" & data_cleaned$EKG.obtained.before.=="Y" & data_cleaned$EKG.obtained.after. == "Y")

#how many patients recieved an EKG before and after. 
length(which(data_cleaned$EKG.obtained.after. == "Y" & data_cleaned$EKG.obtained.before. == "Y"))

#number of patients that experienced an arrythmia
length(which(data_cleaned$Any.arrythmia == "Y"))



####### absolute risk of arrhythmia among agitated#######
agitated_success <- length(which(data_cleaned$Indication=="Agitation" & data_cleaned$Any.arrythmia=="Y"))
agitated_total <- length(which(data_cleaned$Indication=="Agitation"))

nonagitated_success <- length(which(data_cleaned$Indication!="Agitation" & data_cleaned$Any.arrythmia=="Y"))
nonagitated_total <- length(which(data_cleaned$Indication!="Agitation"))

agitated_AR = length(which(data_cleaned$Indication=="Agitation" & data_cleaned$Any.arrythmia=="Y"))/length(which(data_cleaned$Indication=="Agitation"))
nonagitated_AR = length(which(data_cleaned$Indication!="Agitation" & data_cleaned$Any.arrythmia=="Y"))/length(which(data_cleaned$Indication!="Agitation"))

agitated.arrythmia <- table(data_cleaned$Indication=="Agitation", data_cleaned$Any.arrythmia)
fisher.test(agitated.arrythmia)




######### Figure 1 BEGIN ###########

idendication.count <- colSums(data_cleaned[,c("Abdominal.pain","Nausea","Agitation","Vomiting","Unknown","Headache")])
idendication.count.df <- data.frame(
  identification = c("Abdominal pain","Nausea","Agitation","Vomiting","Unknown","Headache"),
  count = idendication.count
)
idendication.count.df.ordered <- idendication.count.df[order(idendication.count.df$count,decreasing=TRUE), ]
indications.order <- idendication.count.df.ordered$identification
idendication.count.df.ordered <- idendication.count.df.ordered %>%
  mutate(Percentage = count / sum(count) * 100) %>%
  ungroup()
idendication.count.df.ordered$identification <- factor(idendication.count.df.ordered$identification, levels = indications.order)

#bar plot of number of people with each indication
ggplot(idendication.count.df.ordered, aes(x = identification, y = count)) +
  geom_bar(stat = "identity",fill = "grey") + 
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.1, position = position_dodge(0.9), size = 4) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.9, face = "bold",size=18),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 16),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 18))   +
  xlab("") +            
  ylab("Frequency")

######### Figure 1 END ###########


######### Figure 2 BEGIN ###########

#age vs. arrythmia 
ggplot(data_cleaned, aes(x = Any.arrythmia, y= Age,fill=Any.arrythmia)) +
  geom_boxplot() +
  labs(title = "Arrhythmia Occurence by Age", x = "", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold",size=19),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19))  +
  scale_fill_manual(values = c("palegreen4", "red4"))

#total dose vs. arrythmia 
ggplot(data_cleaned, aes(x = Any.arrythmia, y= Total.Amount.Given..mg.,fill=Any.arrythmia)) +
  geom_boxplot() +
  labs(title = "Arrhythmia Occurence by mg of Droperidol", x = "", y = "Total Administered Amount (mg)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1, face = "bold",size=19),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19))  +
  scale_fill_manual(values = c("palegreen4", "red4"))


#arrhythmia vs. sex exploratory plot
sex.arrythmia <- table(data_cleaned$Legal.Sex, data_cleaned$Any.arrythmia)
sex.arrythmia <- as.data.frame(sex.arrythmia)
colnames(sex.arrythmia) <- c("Sex","Arrhythmia", "Freq" )

sex.arrythmia <- sex.arrythmia %>%
  group_by(Sex) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("Female","Male","X \n(Legal \nNon-Binary \nDesignation)")

ggplot(sex.arrythmia, aes(x = Sex, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.2, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Arrhythmia Occurence Grouped by Patient Sex", x = "", y = "Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15) 
  )   +
  scale_x_discrete(labels = x_labels) 

#total dose vs. arrythmia 
ggplot(data_cleaned, aes(x = Any.arrythmia, y=QTc..before. ,fill=Any.arrythmia)) +
  geom_boxplot() +
  labs(title = "Arrhythmia Occurence by QTc (before)", x = "", y = "QTc (before)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1, face = "bold",size=19),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19))  +
  scale_fill_manual(values = c("palegreen4", "red4"))







######### Figure 2 END ###########


######### Figure 3 BEGIN ###########

#arrhythmia vs. headache - there are 0 people with headaches and arrythmia. 
headache.arrythmia <- table(data_cleaned$Headache, data_cleaned$Any.arrythmia)
headache.arrythmia <- as.data.frame(headache.arrythmia)
colnames(headache.arrythmia) <- c("Headache","Arrhythmia", "Freq")

headache.arrythmia <- headache.arrythmia %>%
  group_by(Headache) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("No Headache", "Headache")
ggplot(headache.arrythmia, aes(x = Headache, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Headache vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15))   +
  scale_x_discrete(labels = x_labels) 



#arrhythmia vs. agitation
agitation.arrythmia <- table(data_cleaned$Agitation, data_cleaned$Any.arrythmia)
agitation.arrythmia <- as.data.frame(agitation.arrythmia)
colnames(agitation.arrythmia) <- c("Agitation","Arrhythmia", "Freq" )

agitation.arrythmia <- agitation.arrythmia %>%
  group_by(Agitation) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("No Agitation", "Agitation")
ggplot(agitation.arrythmia, aes(x = Agitation, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Agitation vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15))   +
  scale_x_discrete(labels = x_labels) 

#arrhythmia vs. unknown
unknown.arrythmia <- table(data_cleaned$Unknown, data_cleaned$Any.arrythmia)
unknown.arrythmia <- as.data.frame(unknown.arrythmia)
colnames(unknown.arrythmia) <- c("Unknown","Arrhythmia", "Freq" )

unknown.arrythmia <- unknown.arrythmia %>%
  group_by(Unknown) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("Known", "Unknown")
ggplot(unknown.arrythmia, aes(x = Unknown, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Unknown vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15)) +
  scale_x_discrete(labels = x_labels) 


#arrhythmia vs. vomiting
vomiting.arrythmia <- table(data_cleaned$Vomiting, data_cleaned$Any.arrythmia)
vomiting.arrythmia <- as.data.frame(vomiting.arrythmia)
colnames(vomiting.arrythmia) <- c("Vomiting","Arrhythmia", "Freq" )

vomiting.arrythmia <- vomiting.arrythmia %>%
  group_by(Vomiting) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("No Vomiting", "Vomoting")
ggplot(vomiting.arrythmia, aes(x = Vomiting, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Vomiting vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15))  +
  scale_x_discrete(labels = x_labels) 

#arrhythmia vs. nausea
nausea.arrythmia <- table(data_cleaned$Nausea, data_cleaned$Any.arrythmia)
nausea.arrythmia <- as.data.frame(nausea.arrythmia)
colnames(nausea.arrythmia) <- c("Nausea","Arrhythmia", "Freq" )

nausea.arrythmia <- nausea.arrythmia %>%
  group_by(Nausea) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("No Nausea", "Nausea")
ggplot(nausea.arrythmia, aes(x = Nausea, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Nausea vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15))  +
  scale_x_discrete(labels = x_labels) 

#arrhythmia vs. abdominalpain
abdominalpain.arrythmia <- table(data_cleaned$Abdominal.pain, data_cleaned$Any.arrythmia)
abdominalpain.arrythmia <- as.data.frame(abdominalpain.arrythmia)
colnames(abdominalpain.arrythmia) <- c("Abdominalpain","Arrhythmia", "Freq" )

abdominalpain.arrythmia <- abdominalpain.arrythmia %>%
  group_by(Abdominalpain) %>%
  mutate(Percentage = Freq / sum(Freq) * 100) %>%
  ungroup()

x_labels <- c("No Abdominal \nPain", "Abdominal \npain")
ggplot(abdominalpain.arrythmia, aes(x = Abdominalpain, y = Freq, fill = Arrhythmia)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, position = position_dodge(0.9), size = 4) +  # Add counts above bars
  labs(title = "Abdominal Pain vs. Arrhythmia", x = "",y="Frequency") +
  scale_fill_manual(values = c("palegreen4", "red4")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.35, face = "bold",size=19),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 19),
        axis.title.y = element_text(size = 19),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 15))  +
  scale_x_discrete(labels = x_labels) 

######### Figure 3 END ###########

