library(dplyr)
library(readr)


B<-read_delim("c:users/lenovo/desktop/2008.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
b<-data.frame(B$Positive,B$Negative)
write.csv(b,"c:users/lenovo/desktop/2008.csv")