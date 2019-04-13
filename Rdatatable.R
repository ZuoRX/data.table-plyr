library(data.table)

#install.packages("bikeshare14")
library(bikeshare14)
# require(installr)
# updateR()
#=========================================================================#
#  %like%  #查找字符串
batrips
data("batrips")
head(batrips)
names(batrips)



# Filter all rows where end_station ends with "Market" 
start_markets <- batrips["start_station" %like% "Hall"]
end_markets

end_markets <- batrips['end_station' %like% "Market"]
end_markets


# Filter all rows where trip_id is 588841, 139560, or 139562
#多个条件的筛选
filter_trip_ids <- batrips[trip_id %in%  c(588841, 139560, 139562)]
filter_trip_ids

# Filtering with %between% and %chin%
#   Two other functions that can come in handy when filtering rows are %between% and %chin%.
# 
# %between% works only on numeric columns and can be used to filter values in the closed interval [val1, val2].
# %chin% works only on character columns and is an efficient version of %in%. 
# You can use it to look for specific strings in a vector.

# Filter all rows with specific start stations
two_stations <- batrips[start_station %chin% c("San Francisco City Hall", "Embarcadero at Sansome")]
two_stations

#=========================================================================#

dt_way <- batrips[,.(start_station,end_station)]
dt_way

#易错1
# Find the first and last ride for each start_station
first_last <- batrips[order(start_date), 
                      .(start_date=start_date[c(1,.N)]), 
                      by = .(start_station)]
first_last

#quzz2
# Find the total number of unique start stations and zip codes per month
unique_station_month <- batrips[, lapply(.SD, uniqueN), 
                                by = month(start_date), 
                                .SDcols = c("start_station", "zip_code")]
unique_station_month












#=========================================================================#
##---1.1---##
# The data.table package is preloaded

# Create my_first_data_table
my_first_data_table <- data.table(x = c("a", "b", "c", "d", "e"), 
                                  y = c(1, 2, 3, 4, 5))  

# Create a data.table using recycling
DT <- data.table(a = c(1L, 2L), b = LETTERS[1:4])

# Print the third row to the console
DT[3]

# Print the second and third row to the console without using commas
DT[2:3]


##---1.2---##
# DT and the data.table package are pre-loaded
DT
# Print the second to  last row of DT using .N
DT[.N-1] #语句歧义 the second to the last

# Print the column names of DT
names(DT)

# Print the number or rows and columns of DT
dim(DT)

# Print a new data.table containing rows 2, 2, and 3 of DT
DT[c(2,2,3)]


##---2.1---##
# DT and the data.table package are pre-loaded

# Subset rows 1 and 3, and columns B and C
DT[c(1,3),.(B,C)]

# Assign to ans the correct value
ans<-DT[,.(B,val=A*C)]

# Fill in the blanks such that ans2 equals target
target <- data.table(B = c("a", "b", "c", "d", "e", 
                           "a", "b", "c", "d", "e"), 
                     val = as.integer(c(6:10, 1:5)))
ans2 <- DT[, .(B, val =c(6:10, 1:5) )]


##---3.1---##
# iris is already available in your workspace

# Convert iris to a data.table: DT
DT<-data.table(iris)

# For each Species, print the mean Sepal.Length
DT[,mean(Sepal.Length),by=Species]

# Print mean Sepal.Length, grouping by first letter of Species
DT[,mean(Sepal.Length),by=substr(Species,1,1)]



##---3.2---##
# data.table version of iris: DT
DT <- as.data.table(iris)

# Group the specimens by Sepal area (to the nearest 10 cm2) and count how many occur in each group
DT[, .N, by = 10 * round(Sepal.Length * Sepal.Width / 10)]

# Now name the output columns `Area` and `Count`
DT[,.(Count= .N), by = .(Area=10 * round(Sepal.Length * Sepal.Width / 10))]






##---3.3---##

# Create the data.table DT
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8))

# Create the new data.table, DT2
DT2<-DT[,.(C=cumsum(C)),by=.(A,B)]


# Select from DT2 the last two values from C while you group by A
#DT2[,.(C=c(C,tail(C,2))),by=A]
DT2[,.(C=tail(C,2)),by=A]


#=========================================================================#
#第二大部分 第4小结视频

##---4.1---##
# The data.table package has already been loaded

# Build DT
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8)) 

# Combine the two steps in a one-liner
DT2 <- DT[, .(C = cumsum(C)), by = .(A, B)]
DT2[, .(C = tail(C, 2)), by = A]

DT[, .(C = cumsum(C)), by = .(A, B)][, .(C = tail(C, 2)), by = A]


##---4.2---##
# The data.table DT is loaded in your workspace

# Perform chained operations on DT
DT[, .(Sepal.Length = median(Sepal.Length), 
       Sepal.Width = median(Sepal.Width), 
       Petal.Length = median(Petal.Length),
       Petal.Width = median(Petal.Width)), 
   by = Species][order(-Species)]


##---5.1---##
# a special built-in variable .SD. 
# It refers to the subset of data for each unique value of the by argument. 
# A new data.table DT is available
DT
# Mean of columns
DT[,lapply(.SD,mean),by=x]

# Median of columns
DT[,lapply(.SD,median),by=(x)]

##---5.2---##
# A new data.table DT is available
DT
# Calculate the sum of the Q columns
DT[,lapply(.SD,sum),.SDcols=2:4]

# Calculate the sum of columns H1 and H2 
DT[,lapply(.SD,sum),.SDcols=c(paste0("H",1:2))]

# Select all but the first row of groups 1 and 2, returning only the grp column and the Q columns
DT[,.SD[-1],.SDcols=c(paste0("Q",1:3)),by=grp]














































