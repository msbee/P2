covid_df <- read_csv("coronavirus.csv")
#get the upto date from

# TOP CONFIRMED CASES -----------------------------------------------------


#TOP 20 countries with cases
top_confirmedcases <- covid_df %>% 
  filter(type=="confirmed") %>% #filter in data_type column
  group_by(country)%>%
  summarise(total_cases = sum(cases))%>%
  mutate(Global = cumsum(total_cases))%>%
  arrange(desc(total_cases))
y_label <- c(0,1,2,3)

library(tidyverse)
library(scales)

#plot top ten countries with confirmed cases
top_ten_confirmed <- top_confirmedcases %>% 
  head(10) %>%
  ggplot(aes(reorder(x=country,total_cases ),y=total_cases))+
  geom_bar(stat="identity",color = "black",fill= "coral")+
  labs(title = "Top Ten Countries with Confirmed cases",x = "Country", y="cases in Millions")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  geom_text(aes(label = total_cases),hjust = 1)
  
  #scale_y_continuous(labels = paste0(y_label, "M",breaks =y_label*10^7))

top_ten_confirmed+coord_flip()

#highest deaths
top_death<- covid_df %>% 
  filter(type=="death") %>% #filter in data_type column
  group_by(country)%>%
  summarise(Total_cases = sum(cases))%>%
  arrange(desc(Total_cases))
#plot top ten countries with confirmed cases
top_ten_death <- top_death %>% 
  head(10) %>%
  ggplot(aes(reorder(x=country,Total_cases ),y=Total_cases))+
  geom_bar(stat="identity",color = "black",fill= "navy")+
  labs(title = "Top Ten Countries with Death cases",x = "Country", y="cases in Thousands")+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  geom_text(aes(label = Total_cases),hjust = 1,color ="white")

top_ten_death+coord_flip()

#worldwide cases over time


#world_wide$group <- c(world_wide$active_total,world_wide$recovered_total,world_wide$death_total)
library(zoo)
ww_plot1 <- world_wide %>% ggplot(aes(x=date,y=active_total))+
  geom_line(color = "darkred",stat="identity")#+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(0,60000000,20000000),limits = c(0,6*10**7))+
  scale_x_date(date_labels = "%b%Y")+labs(y="number of active cases")
  
  
ww_plot1

ww_plot2 <- world_wide %>% ggplot(aes(x=date,y=recovered_total))+
  geom_line(color = "darkred",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_x_date(date_labels = "%b%Y")+labs(y="number of recovered cases")


ww_plot2+theme_bw()

ww_plot3 <- world_wide %>% ggplot(aes(x=date,y=death_total))+
  geom_line(color = "darkred",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y")+labs(y="number of deaths")

ww_plot3

#WORLDWIDE UPDATETO INCLUDE COUNTRY
#seperate from type to 
world_wide <-covid_df %>%
  group_by(type,date,country)%>%
  #sum the cases by type and date
  summarise(Total_cases = sum(cases))%>%
 #covert to wide format 
  pivot_wider(names_from = type,values_from= Total_cases)%>%
  #after summing cases group by date, to obtain total for each day
  arrange(date)%>%
  mutate(active = confirmed -death - recovered) %>%
  mutate(active_total = cumsum(active),#cumulative sum
         recovered_total = cumsum(recovered),
         death_total = cumsum(death))%>%arrange(desc(death_total))


world_wide1 <- covid_df %>%
  group_by(type,country)%>%
  summarise(Total_cases = sum(cases))%>%
  pivot_wider(names_from = type,values_from= Total_cases)%>%
  mutate(active = confirmed -death - recovered) 

world_wide1$death_rate <- world_wide1$death / world_wide1$confirmed
world_wide1$recovered_rate <- world_wide1$recovered / world_wide1$confirmed
world_wide1 <- world_wide1  %>%arrange(desc(world_wide1$confirmed))
arrange(desc(Total_cases))

# death rate 
top_death <- world_wide1 %>% head(10)%>%
  ggplot(aes(reorder(x=country,death_rate*100),y=death_rate*100))+
  geom_bar(stat='identity',color = "white" , fill='light blue')+
  geom_text(aes(label = round(death_rate*100,4),hjust = 1))+
  labs(title = 'Top Ten Countries Death Ratio',x ="Country", y="Death Rate") 

top_death + coord_flip()
  
#death rate over time
world_wide%>% ggplot(aes(x=date,y=death))+
  geom_line(color = "darkred",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths")

# death over time by countries --------------------------------------------
#death rate over time by countries
 world_wide%>%filter(country=='China')%>% ggplot(aes(x=date,y=death))+
  geom_line(color = "darkred",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths")
  
A <- world_wide %>%ggplot(aes(x=date,y=death))+
  geom_line(data =filter(world_wide,country=='US'),
            stat="identity",group =1,size=1,color="steelblue4")+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths")


B <- world_wide %>%ggplot(aes(x=date,y=death))+
  geom_line(data =filter(world_wide,country=='India'),
            stat="identity",group =1,size=1,color ='red')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths") 

C <- world_wide %>%ggplot(aes(x=date,y=death))+
  geom_line(data =filter(world_wide,country=='Germany'),
            stat="identity",group =1,size=1,color ='blue')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths") 

D <- world_wide %>%ggplot(aes(x=date,y=death))+
  geom_line(data =filter(world_wide,country=='Brazil'),
            stat="identity",group =1,size=1,color ='purple')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  labs(y="number of deaths")
#+theme(axis.text.x=element_text(angle=45,hjust = 1))
D
#arrange picture
library(ggpubr)
library(ggthemes)
figure <- ggarrange(A,B,
                    C,D,
                    labels = c("US", "India","Germany" ,"Brazil"), ncol = 2, nrow = 2)
annotate_figure(figure,top = text_grob(" Death cases", color = "black", face = "bold", size = 12))

figure

# recovery cases time series ----------------------------------------------
world_wide%>%filter(country=='US')%>% ggplot(aes(x=date,y=recovered))+
  geom_line(color = "steelblue4",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")


world_wide%>%filter(country=='India')%>% ggplot(aes(x=date,y=recovered))+
  geom_line(color = "steelblue4",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")

In <- world_wide %>%ggplot(aes(x=date,y=recovered))+
  geom_line(data =filter(world_wide,country=='India'),
            stat="identity",group =1,size=1,color ='red')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")

Gr <- world_wide %>%ggplot(aes(x=date,y=recovered))+
  geom_line(data =filter(world_wide,country=='Germany'),
            stat="identity",group =1,size=1,color ='blue')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")


world_wide %>%ggplot(aes(x=date,y=recovered))+
  geom_line(data =filter(world_wide,country=='Brazil'),
            stat="identity",group =1,size=1,color ='purple')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")

 world_wide %>%ggplot(aes(x=date,y=recovered))+
  geom_line(data =filter(world_wide,country=='United Kingdom'),
            stat="identity",group =1,size=1,color ='yellowgreen')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of cases")
#arrange four pictures into one page
figure1 <- ggarrange(In,bra,
                    Gr,uk,
                    labels = c("India","Germany","Brazil","United Kingdom"), ncol = 2, nrow = 2,vjust =2.5,hjust=-1)

# cases vs vaccine --------------------------------------------------------
world_wide%>%filter(country=='US')%>% ggplot(aes(x=date,y=confirmed))+
  geom_line(color = "darkred",stat="identity")+ 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+labs(y="number of deaths")
#us
usa <- ggplot()+
  geom_line(data =filter(world_wide,country=='US'),
            aes(x=date,y=confirmed,color ='black'),
            stat="identity",group =1,size=1)+
  geom_line(data =filter(covid_vaccines,country=='United States'),
            aes(x=date,y=daily_vaccinations/10,color ='red'),
            stat="identity",group =1,size=1)+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3),
                     name = "Daily Confirmed Cases (Thousands)",
                     sec.axis = sec_axis(~ ./100000, name = "Daily Vaccinations(in Millions)"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Cases vs Daily vaccination",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")
ggplotly(usa)
usa
#India
In <- ggplot()+
  geom_line(data =filter(world_wide,country=='India'),
            aes(x=date,y=confirmed,color ='black'),
            stat="identity",group =1,size=1)+
  geom_line(data =filter(covid_vaccines,country=='India'),
            aes(x=date,y=daily_vaccinations/10,color ='red'),
            stat="identity",group =1,size=1)+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                     breaks= seq(0,6500000,500000),name = "Daily Confirmed Cases (Thousands)",
                     sec.axis = sec_axis(~ ./100000, name = "Daily Vaccinations(in Millions)"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Cases vs Daily vaccination",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")

In
ggplotly(In)
#Brazil
Bra <- ggplot()+
  geom_line(data =filter(world_wide,country=='Brazil'),
            aes(x=date,y=confirmed,color ='black'),
            stat="identity",group =1,size=1)+
  #geom_hline(yintercept = mean(world_wide$confirmed),color="blue")+
  geom_line(data =filter(covid_vaccines,country=='Brazil'),
            aes(x=date,y=daily_vaccinations/10,color ='red'),
            stat="identity",group =1,size=1)+
   # geom_hline(yintercept = mean(covid_vaccines$daily_vaccinations,color="blue")+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3),
                     breaks= seq(0,1500000,150000),
                     sec.axis = sec_axis(~ ./100000, name = "Daily Vaccinations(in Millions)"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Cases vs Daily vaccination",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")
ggplotly(Bra)
Bra 
function(x)mean(x, na.rm=TRUE)

#Germany
Gr <- ggplot()+
  geom_line(data =filter(world_wide,country=='Germany'),
            aes(x=date,y=confirmed,color ='black'),
            stat="identity",group =1,size=1)+
  geom_line(data =filter(covid_vaccines,country=='Germany'),
            aes(x=date,y=daily_vaccinations/10,color ='red'),
            stat="identity",group =1,size=1)+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                     sec.axis = sec_axis(~ .*10, name = "Daily Vaccinations(in Thousands)"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Legend",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")

Gr



#united kingdom
uk <- ggplot()+
  geom_line(data =filter(world_wide,country=='United Kingdom'),
            aes(x=date,y=confirmed,color ='black'),
            stat="identity",group =1,size=1)+
  geom_line(data =filter(covid_vaccines,country=='United Kingdom'),
            aes(x=date,y=daily_vaccinations/10,color ='red'),
            stat="identity",group =1,size=1)+
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3),
                     sec.axis = sec_axis(~ .*10, name ="Daily Vaccinations(in Thousands)"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Legend",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")

uk
ggarrange(In,Bra,
          gr,uk,
          labels = c("India","Brazil","Germany","United Kingdom"), 
          ncol = 2, nrow = 2,vjust =2.5,hjust=-1)


covid_vaccines %>%ggplot(aes(x=date,y=daily_vaccinations))+
  geom_line(data =filter(covid_vaccines,country=='United States'),
            stat="identity",group =1,size=1,color ='red')+
  geom_line(data =filter(covid_vaccines,country=='India'),
            stat="identity",group =1,size=1,color ='tan')+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")
 


ggplot()+
  geom_line(data =filter(covid_vaccines,country=='United States'),
            aes(x=date,y=daily_vaccinations),
            stat="identity",group =1,size=1,color ='red')+
  geom_line(data =filter(world_wide,country=='US'),
            aes(x=date,y=confirmed*10),
            stat="identity",group =1,size=1,color ='black')+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     
                     sec.axis = sec_axis(~ ./10, name = "confirmation"))+
  scale_x_date(date_labels = "%b%Y",date_breaks = "2 months")+
  scale_color_identity(name = "Cases vs Daily vaccination",
                       breaks = c("black", "red"),
                       labels = c("Confirmed cases", "Vacination"),
                       guide = "legend")
#annotate_figure(figure1,top = text_grob("Recovery cases", color = "black", face = "bold", size = 10))
figure1
library(plotly) 
world_wide %>%plot_ly(x = ~ date,
        y = ~ active_total,
        name = 'Active', 
        fillcolor = '#1f77b4',
        type = 'scatter',
        mode = 'none', 
        stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Death",
            fillcolor = 'purple') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'yellow')%>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "date")) 
 
write.csv(world_wide,"Data science\\covid wide.csv", row.names = FALSE)

# vaccination -------------------------------------------------------------

 
#Daily vaccination
covid_vaccines <-read.csv("country_vaccinations1.csv")
#world_data <- read.csv("population_by_country_2020.csv")
#top highest vaccinated countries
top_vaccinated <- covid_vaccines %>% 
  group_by(country) %>%
  #select(COUNTRY,TOTAL_VACCINATIONS) %>%
  summarise(Total = sum(daily_vaccinations,na.rm = TRUE))%>% 
  arrange((-Total))
 
covid_vaccines$date <-as.Date(covid_vaccines$date)
top_vaccinated_plot <- top_vaccinated %>%head(10)%>%
  ggplot(aes(reorder(x=country,Total),y=Total,fill=Total))+
  geom_bar(stat="identity",color ="grey")+
  scale_fill_gradient(low="orange", high="blue")+
  labs(title = "Top Ten Vaccinated Countries", x= "Countries",y="Number of vaccinations in Billions", fill= "Total")+
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))

top_vaccinated_plot+coord_flip()


# fully vaccined countries ------------------------------------------------
full_vaccination <- covid_vaccines %>% 
  group_by(country)%>%
  summarise(total_full = max(people_fully_vaccinated,na.rm=TRUE))%>%
  arrange(desc(total_full))
full_vaccination <- full_vaccination[-8,]

#PLot most fully vaccinated countries
full_vaccination %>%head(10)%>%
  ggplot(aes(reorder(x=country,total_full),y=total_full))+
  geom_bar(stat="identity",color ="black",fill="paleturquoise")+
  labs(title="Top Ten countries with Full Vaccination",x="Country",y="numbers")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  coord_flip()

# top vaccinated country time trend ---------------------------------------
#change date to a date format
#covid_vaccines$date <- as.Date(covid_vaccines$date)
china <- covid_df%>%
  group_by(date, country) %>%
  filter(country == "China") %>% 
  filter(type=="confirmed")%>%
  group_by(type,date)%>%
  summarise(Total_cases = sum(cases))%>%
  group_by(date)%>%
  mutate(total_case=cumsum(Total_cases))%>%
  arrange(-total_case)%>%
  filter(date > "2020-12-18")

covid_vaccines %>%filter(country=="China")%>%
  ggplot(aes(x=date,y=daily_vaccinations,group =1))+
  geom_line(stat="identity",color ="blue")


#china cases
china %>%ggplot(aes(x=date,y=total_case))+
  geom_line(stat="identity",color ="purple")

India <- covid_df%>%
  group_by(date, country) %>%
  filter(country == "India") %>% 
  filter(type=="confirmed")%>%
  group_by(type,date)%>%
  summarise(Total_cases = sum(cases))%>%
  filter(date > "2020-12-18")
  #group_by(date)%>%
  #mutate(total_case=cumsum(Total_cases))%>%
  #arrange(-total_case)
#inida cases plot
India %>%ggplot(aes(x=date,y=Total_cases))+
  geom_line(stat="identity",color ="blue")
#india vaccination plot
covid_vaccines %>%filter(country=="United States")%>%
  drop_na()%>%
  ggplot(aes(x=date,y=daily_vaccinations))+
  geom_line(stat="identity",color ="dark red")

#USA
germany <- covid_df%>%
  group_by(date, country) %>%
  filter(country == "Germany") %>% 
  filter(type=="confirmed")%>%
  group_by(type,date)%>%
  summarise(Total_cases = sum(cases))%>%
  filter(date > "2020-12-18")
#brazil cases plot
germany %>%ggplot(aes(x=date,y=Total_cases))+
  geom_line(stat="identity",color ="blue")
#india vaccination plot
covid_vaccines %>%filter(country=="Germany")%>%
  drop_na()%>%
  ggplot(aes(x=date,y=daily_vaccinations))+
  geom_line(stat="identity",color ="dark red")

#brazil
Brazil <- covid_df%>%
  group_by(date, country) %>%
  filter(country == "Brazil") %>% 
  filter(type=="confirmed")%>%
  group_by(type,date)%>%
  summarise(Total_cases = sum(cases))%>%
  filter(date > "2020-12-18")
#brazil cases plot
Brazil %>%ggplot(aes(x=date,y=Total_cases))+
  geom_line(stat="identity",color ="blue")
#india vaccination plot
covid_vaccines %>%filter(country=="Brazil")%>%
  drop_na()%>%
  ggplot(aes(x=date,y=daily_vaccinations))+
  geom_line(stat="identity",color ="dark red")

#Germany
germany <- covid_df%>%
  group_by(date, country) %>%
  filter(country == "Germany") %>% 
  filter(type=="confirmed")%>%
  group_by(type,date)%>%
  summarise(Total_cases = sum(cases))%>%
  filter(date > "2020-12-18")
#brazil cases plot
germany %>%ggplot(aes(x=date,y=Total_cases))+
  geom_line(stat="identity",color ="blue")
#india vaccination plot
covid_vaccines %>%filter(country=="Germany")%>%
  drop_na()%>%
  ggplot(aes(x=date,y=daily_vaccinations))+
  geom_line(stat="identity",color ="dark red")



