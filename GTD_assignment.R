library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gridExtra)
GTD_12_15 = read.csv("D://Datasets//EDA//ASSIGNMENT 1//gtd_12to15_52134.csv")
GTD_92_11 = read.csv("D://Datasets//EDA//ASSIGNMENT 1//gtd_92to11_no 93_55072.csv")
GTD_70_91 = read.csv("D://Datasets//EDA//ASSIGNMENT 1//gtd_70to91_49566.csv")
GTD_93 = read.csv("D://Datasets//EDA//ASSIGNMENT 1//gtd1993_748.csv")

########################################################################################
#1]

terror_12_15 = GTD_12_15 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)



terror_92_11 = GTD_92_11 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)


terror_70_91 = GTD_70_91 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)


terror_93 = GTD_93 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)


# merging dataframes

q1 = data.frame(rbind(terror_70_91, terror_92_11, terror_12_15, terror_93))


# Plotting graph

q1_plot = ggplot(q1, aes(x=reorder(as.factor(iyear),-iyear), y=yearwise_attacks)) +
  geom_bar(stat = "Identity", aes(fill=as.factor(iyear)))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Attacks", x="Years", title="1]:Yearwise Attacks")+
  geom_text(aes(label = yearwise_attacks), position = position_dodge(width = .9), hjust=-.25)+
  coord_flip()
q1_plot



########################################################################################
#2]

terror1_12_15 = GTD_12_15 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_92_11 = GTD_92_11 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_70_91 = GTD_70_91 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_93 = GTD_93 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)




q2 = data.frame(rbind(terror1_70_91, terror1_92_11, terror1_93, terror1_12_15))
View(q2)

## Plotting graph for q2

q2_plot = ggplot(q2, aes(x=as.factor(iyear),y=yearwise_bombing))+
  geom_bar(stat = "Identity", aes(fill=as.factor(iyear)))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Bombings", x="Years", title="1]:Yearwise Bombing")+
  geom_text(aes(label = yearwise_bombing), position = position_dodge(width = .9), hjust=-.25)+
  coord_flip()
q2_plot


#########################################################################################

#3]

terror2_12_15 = GTD_12_15 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_92_11 = GTD_92_11 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_70_91 = GTD_70_91 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_93 = GTD_93 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



q3 = data.frame(rbind(terror2_70_91, terror2_92_11, terror2_12_15, terror2_93))
View(q3)

## Plotting graph for q3


q3_plot = ggplot(q3, aes(x=iyear,y=total_terr_att))+
  geom_bar(stat = "Identity", aes(fill=iyear))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Terrorist Attacks", x="Years", title="3]:Regionwise Total Terrorist Attacks")+facet_wrap(~region_txt)

q3_plot


############################################################################################

#4]


unraw_1 = GTD_70_91 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_2 = GTD_92_11 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_3 = GTD_93 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_4 = GTD_12_15 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())



raw4 = data.frame(rbind(unraw_1,unraw_2,unraw_3,unraw_4))

View(raw4)

q4 = raw4 %>% filter(!attacktype1_txt == "Unknown") %>%
  group_by(region_txt,attacktype1_txt) %>%
  summarise(Totall_Attacks = sum(Total_Attacks))%>%
  top_n(5)


View(q4)
#########################################################################################

# Plotting graph for q4

q4_plot = ggplot(q4, aes(x=attacktype1_txt,y=Totall_Attacks))+
  geom_bar(stat = "Identity", aes(fill=attacktype1_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of attacks", x="Types of Attacks", title="4]:Regionwise Top 5 types of Attacks")+facet_wrap(~region_txt)

q4_plot

##########################################################################################

#5]

terror5_12_15 = GTD_12_15 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_12_15$Total_wound = terror5_12_15$total_kill + terror5_12_15$total_wound


terror5_92_11 = GTD_92_11 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_92_11$Total_wound = terror5_92_11$total_kill + terror5_92_11$total_wound


terror5_70_91 = GTD_70_91 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_70_91$Total_wound = terror5_70_91$total_kill + terror5_70_91$total_wound


terror5_93 = GTD_93 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_93$Total_wound = terror5_93$total_kill + terror5_93$total_wound



raw_1 = data.frame(rbind(terror5_70_91, terror5_92_11, terror5_12_15, terror5_93))

q5 = raw_1 %>%group_by(targtype1_txt)%>% summarise(Total_cas = sum(Total_wound))%>% 
  arrange(-Total_cas) %>% head(10)
View(q5)

##########################################################################################

## Plotting graph for q5

q5_plot = ggplot(q5, aes(x=targtype1_txt, y=Total_cas))+
  geom_bar(stat = "Identity", aes(fill=targtype1_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of casualties", x="Types of Targets", title="5]:Heaviest heat target types")+
  geom_text(aes(label = Total_cas), position = position_dodge(width = .9), vjust = -0.25)



q5_plot


###########################################################################################

# 6]

terror6_12_15 = GTD_12_15 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())



terror6_92_11 = GTD_92_11 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())



terror6_70_91 = GTD_70_91 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())



terror6_93 = GTD_93 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())



raw_1 = data.frame(rbind(terror6_93, terror6_70_91, terror6_92_11, terror6_12_15))

raw_2 = raw_1 %>% arrange(-iyear)
View(raw_2)

sum(raw_2$Total_terr_att)

##########################################################################################

#7]

terror7_93 = GTD_93 %>% filter(country == 217 & doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
View(terror7_93)


terror7_70_91 = GTD_70_91 %>% filter(country == 217 & doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
View(terror7_70_91)


terror7_92_11 = GTD_92_11 %>% filter(country == 217 & doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
View(terror7_92_11)


terror7_12_15 = GTD_12_15 %>% filter(country == 217 & doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
View(terror7_12_15)


us_raw_1 = data.frame(rbind(terror7_93,terror7_92_11,terror7_70_91,terror7_12_15))
View(us_raw_1)

# for russia

terror7b_93 = GTD_93 %>% filter((country == 167 | country == 359)& doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
#View(terror7_93)


terror7b_70_91 = GTD_70_91 %>% filter((country == 167 | country == 359)& doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
#View(terror7_70_91)


terror7b_92_11 = GTD_92_11 %>% filter((country == 167 | country == 359)& doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
#View(terror7_92_11)


terror7b_12_15 = GTD_12_15 %>% filter((country == 167 | country == 359)& doubtterr == 0) %>%
  group_by(iyear) %>% summarise(Total_terr_att = n())
#View(terror7_12_15)


raw_1 = data.frame(rbind(terror7b_93,terror7b_92_11,terror7b_70_91,terror7b_12_15))
View(raw_1)

########################################################################################

#8]

# Grouping by countrywise


unraw_12_15 = GTD_12_15 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_70_91 = GTD_70_91 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_92_11 = GTD_92_11 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_93 = GTD_93 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))



raw = data.frame(rbind(unraw_70_91, unraw_12_15,unraw_92_11,unraw_93))

raw_12_15 = gather(raw,Casualty_type,Totall_casualties,"total_kill":"total_wound")
final1 = raw_12_15 %>% group_by(country_txt) %>%
  summarise(Total_casualties = sum(Totall_casualties)) %>% arrange(-Total_casualties)%>%
  head(10)


View(final1)

























## Targetwise



unraw1_12_15 = GTD_12_15 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_70_91 = GTD_70_91 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_92_11 = GTD_92_11 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_93 = GTD_93 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unrawr = data.frame(rbind(unraw1_12_15,unraw1_70_91,unraw1_92_11, unraw1_93))
View(unrawr)
ghp = gather(unrawr, Casualty_type, Totally_casu, "total_kill":"total_wound")
View(ghp)


finale = ghp %>%filter(!targtype1_txt == "Other")%>% group_by(targtype1_txt)%>%
  summarise(Total_casualties = sum(Totally_casu))%>%
  arrange(-Total_casualties)%>%head(15)
View(finale)






#########################################################################################

#9]


d1 = GTD_12_15 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d2 = GTD_70_91 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d3 = GTD_92_11 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d4 = GTD_93 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

df = data.frame(rbind(d1,d2,d3,d4))
View(df)

ght = gather(df, Casu_type, total_casu, "total_kill":"total_wound")
View(ght)

dp = ght %>% group_by(iyear) %>%
  summarise(Total_casualties = sum(total_casu)) %>%
  arrange(iyear)
View(dp)
#########################################################################################

#10]

g1 = GTD_12_15 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g2 = GTD_70_91 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g3 = GTD_92_11 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g4 = GTD_93 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


gh = data.frame(rbind(g1,g2,g3,g4))
View(gh)


sdf = gather(gh, Casu_type, total_casua, "total_kill":"total_wound")
View(sdf)


strong = sdf %>% group_by(weaptype1_txt) %>%
  summarise(Total_casualties = sum(total_casua)) %>%
  arrange(-Total_casualties)
View(strong)


############################################################################################


#11]

n1 = GTD_12_15 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n2 = GTD_70_91 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n3 = GTD_92_11 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n4 = GTD_93 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


nh = data.frame(rbind(n1,n2,n3,n4))

fn = nh %>% group_by(country_txt) %>%
  summarise(Total_attacks = sum(Total_attaks)) %>% 
  arrange(-Total_attacks) %>% head(15)
View(fn)
#######################################################################################3

#12]

# Less casualtywise
aw_12_15 = GTD_12_15 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_70_91 = GTD_70_91 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_92_11 = GTD_92_11 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_93 = GTD_93 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))



aw = data.frame(rbind(aw_70_91, aw_12_15,aw_92_11,aw_93))

aw_12_15 = gather(aw,Casualty_type,Totall_casualties,"total_kill":"total_wound")
finish = aw_12_15 %>% group_by(country_txt) %>%
  summarise(Total_casualties = sum(Totall_casualties)) %>% arrange(Total_casualties)%>%
  head(15)
View(finish)


# Success ratewise


ax1 = GTD_12_15 %>% group_by(country_txt) %>%
  filter(!success == 1)%>% summarise(total_defends = n())%>%
  arrange(total_defends) %>%head(15)
View(ax1)
