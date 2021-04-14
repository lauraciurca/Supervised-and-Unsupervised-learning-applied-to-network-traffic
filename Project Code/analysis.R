#libraries
library(tidyverse)
library(dplyr)

#import dataset
data <- read.csv2("data\android_traffic.csv")

#see how many benign and malicious we have -> information contained also in the summary
count(data, data$type)


#delete rows (NA) we don't need
data[12:14] <- list(NULL)


#check if udp and tcp_urg packets (col 6 and 7) have just zero values
all(data$tcp_urg_packet == 0)

data %>% 
  count(data$tcp_urg_packet !=0)

all(data$udp_packets==0)
data %>% 
  count(data$udp_packets !=0)

##considering that the tcp_urg_packet contains just 2 values, ill take it off
data[7] <- list(NULL)

#check if source.app.packets and source.app.packets1 contain the same data
if(data$source_app_packets == data$source_app_packets.1)
{
  print("Columns are identical")
}

#delete column source_app_packets.1 since it is identical to another one
data[11] <- list(NULL)

#summary
summary(data)



