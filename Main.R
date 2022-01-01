library(dplyr)
library(ggplot2)
data <- read.csv('export_narababy_20211130.csv')

# day v night
# catnap
# first long sleep of evening

sleep_df <- data %>% 
  filter(typeGroupKey=='SLEEP') %>% 
  transmute(
    EndTimestamp = as.POSIXct(SLEEP_endDatetimeLocal, format='%m/%d/%Y %H:%M'),
    End_Day=strftime(EndTimestamp,format='%Y/%m/%d'),        
    test_endtime= as.POSIXct(strftime(EndTimestamp, format = "%H:%M"),format = "%H:%M"),
         EndHour = as.POSIXct(test_endtime, format='%H:%M'),
         StartTimestamp = EndTimestamp-SLEEP_durationSeconds,
    Start_Day= as.Date(StartTimestamp),
    test_starttime = as.POSIXct(strftime(StartTimestamp, format = "%H:%M"),format = "%H:%M"),
         StartHour = as.numeric(strftime(StartTimestamp, format="%H")),
         durationsMinutes = round(SLEEP_durationSeconds/60),
         durationSeconds = SLEEP_durationSeconds,
         catnap = ifelse(durationsMinutes<90,1,0),
         TimeofDay = case_when(StartHour>=20 ~ "Evening/Night",
                               StartHour<=6 ~ "Early Morning",
                               StartHour<12 ~ 'Morning',
                               TRUE ~ "Afternoon"),
         TimeofDay = factor(TimeofDay, levels = c("Early Morning", "Morning", "Afternoon","Evening/Night")),
         DayNight = ifelse(TimeofDay %in% c('Morning','Afternoon'),'Day','Night'),
    Week=lubridate::floor_date(Start_Day,unit = 'week'))
  
#factor(sizes, levels = c("small", "medium", "large"))
ggplot(sleep_df, aes(x=durationsMinutes,fill=DayNight)) + geom_histogram()+labs(title="Maddie's Sleep Length in Minutes Histogram")
ggplot(sleep_df, aes(x=durationsMinutes,fill=TimeofDay)) + geom_histogram()+labs(title="Maddie's Sleep Length in Minutes By Time of Day")+facet_grid(TimeofDay~.)
timebreaks <- c(paste0('0',seq(0,9),':00'),paste0(seq(10,24),":00"))
paste0(seq(10,24),":00")
evenings_df <- sleep_df %>% filter(TimeofDay == 'Evening/Night') %>% 
  mutate(test_endtime = test_endtime)
ggplot(sleep_df, aes(x=test_endtime,y=durationsMinutes))+geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sleep_df, aes(x=test_starttime, y=durationsMinutes)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Maddie's Nap Durations by Nap Start Time",
       x = 'Nap Start Time',
       y = 'Nap Duration in Minutes')+
  scale_x_discrete(breaks = timebreaks,labels=timebreaks)

table(sleep_df$test_starttime)
class(evenings_df$test)
eat_df <- data %>% select()

#first long sleep of the night

firstlong <- sleep_df %>% 
  filter(catnap ==0 & TimeofDay == 'Evening/Night') %>% 
  arrange(StartTimestamp) %>% 
  group_by(Start_Day) %>% 
  summarise(StartTimestamp = first(StartTimestamp)) %>% 
  mutate(FirstLongNap = 1 )
  

new_data <- sleep_df %>% left_join(firstlong) %>% mutate(FirstLongNap = as.character(FirstLongNap))
ggplot(new_data, aes(x=test_starttime,y=durationsMinutes, color=FirstLongNap))+geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept=30)


ggplot(new_data %>% filter(FirstLongNap==1), aes(x=Start_Day, y=durationsMinutes))+geom_point()


#feeds per night