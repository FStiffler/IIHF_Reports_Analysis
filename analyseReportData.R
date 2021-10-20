# Load packages
source("Ressources/dependencies.R")

# Load data
reportData<-read.csv("IIHFSurveysCombined.csv")

# Load color palettes
Set3<-brewer.pal(name="Set3", n=12)

# Swiss Player Development
swissDev<-reportData%>%
  select(Year:Female)%>%
  filter(Country=="Switzerland")%>%
  pivot_longer(cols = Registered:Female, values_to = "Count", names_to = "Category")%>%
  ggplot(aes(Year, Count, color=Category))+
  geom_line(size=1)+
  geom_point()+
  geom_text(aes(label=Count), vjust=-1.2, show.legend =F, size=3)+
  theme_minimal()+
  scale_color_manual(values=c("black",Set3[4],Set3[5],Set3[6]),
                     labels=c("Gesamt", "Nachwuchs (U20)", "Aktive", "Frauen"),
                     limits=c("Registered","U20","Senior","Female"),
                     name="Legende")+
  scale_x_continuous(breaks = c(min(reportData$Year):max(reportData$Year)))+
  scale_y_continuous(breaks = seq(0,35000,5000), expand = c(0, 0), limits = c(0,max(reportData$Registered[reportData$Country=="Switzerland"])+2000))+
  ggtitle("Entwicklung des Spielerbestands in der Schweiz")+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, color="grey"),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_line(color="darkgrey"),
    axis.ticks = element_line(color="darkgrey"),
    
  )
swissDev

# Comparison with Germany not adjusted for population
compSuiGerUnajd<-reportData%>%
  select(Year:Registered,U20)%>%
  filter(Country %in% c("Switzerland","Germany"))%>%
  pivot_longer(cols = Registered:U20, values_to = "Count", names_to = "Category")

ggplot(compSuiGerUnajd, aes(Year, Count, color=Country, linetype=Category))+
         geom_line()

# Comparison with Germany adjusted for population (Players per 100000)
compSuiGerAdj<-reportData%>%
  select(Year:Registered,U20,Population)%>%
  filter(Country %in% c("Switzerland","Germany"))%>%
  pivot_longer(cols = Registered:U20, values_to = "Count", names_to = "Category")%>%
  mutate(Count=round(Count/Population*100000))

ggplot(compSuiGerAdj, aes(Year, Count, color=Country, linetype=Category))+
  geom_line()


# Comparison with Top6 nations not adjusted
compTop6Unadj<-reportData%>%
  select(Year:Registered)%>%
  filter(Country %in% c("Switzerland","Germany", "Russia", "United States", "Sweden", "Czech Republic", "Finland", "Canada"))

ggplot(compTop6Unadj, aes(Year, Registered, color=Country))+
  geom_line()

# Comparison with Top6 nations not adjusted
compTop6Adj<-reportData%>%
  select(Year:Registered, Population)%>%
  filter(Country %in% c("Switzerland","Germany", "Russia", "United States", "Sweden", "Czech Republic", "Finland", "Canada"))%>%
  mutate(Registered=Registered/Population*100000)
  

ggplot(compTop6Adj, aes(Year, Registered, color=Country))+
  geom_line()




