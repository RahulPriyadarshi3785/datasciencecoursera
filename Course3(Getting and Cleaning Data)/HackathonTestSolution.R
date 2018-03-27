mySolution_Hackathon <- function(){
        
        x <- c("dplyr","zoo","xlsx","readxl","bindrcpp","lubridate", "RCurl")
        install.packages(x)
        lapply(x, require, character.only = TRUE)
        
        df <- tbl_df(read_excel(text=getURL("https://github.com/RahulPriyadarshi3785/datasciencecoursera/blob/master/Course3(Getting%20and%20Cleaning%20Data)/TriathalonAnalyticalDataSet.xls"), sheet = 2))
        
        str(df)
        
        q1 <- df %>% select(Date, TRIPS) %>% group_by(Date) %>% summarise_all(funs(sum)) %>% head(5) %>% arrange(TRIPS)
        
        q2 <- df %>% select(Date, Time, TRIPS) %>% filter(Date == as.POSIXlt(as.Date("2017-02-11"))) %>% group_by(Time) %>% summarise_all(funs(sum)) %>% top_n(1)
        
        q3 <- df %>% select(Date, EYEBALLS, UNIQUE_DRIVERS) %>% group_by(Date) %>% summarise_all(funs(sum)) %>% arrange(EYEBALLS)
        
        q4 <- df %>% select(Date, EYEBALLS) %>% mutate(Date = weekdays(Date)) %>% group_by(Date) %>% summarise_all(funs(sum)) %>% mutate(EYEBALLS = EYEBALLS/sum(df[,"EYEBALLS"])*100) %>% filter(Date == "Friday")
        
        q5 <- dim(df)[1]
        
        q6 <- apply(df[,"REQUESTES"],2, sum)/apply(df[,"UNIQUE_DRIVERS"],2, sum)
        
        q7 <- df %>% select(Time, REQUESTS) %>% group_by(Time) %>% summarise_all(funs(sum)) %>% top_n(-1)
        
        q8 <- df %>% select(Time, UNIQUE_DRIVERS) %>% group_by(Time) %>% summarise_all(funs(sum)) %>% top_n(-1)
        
        q9 <- df %>% select(Time, REQUESTS, UNIQUE_DRIVERS) %>% group_by(Time) %>% summarise_all(funs(sum)) %>% mutate(Best_TIme = REQUESTS/UNIQUE_DRIVERS) %>% arrange(Best_TIme) %>% head(1)
        
        q10 <- df %>% select(Time, UNIQUE_DRIVERS) %>% filter(UNIQUE_DRIVERS != 0) %>% group_by(Time) %>% summarise(count = n()) %>% tail(15)
        
        q11 <- df %>% select(Time, REQUESTS) %>% group_by(Time) %>% summarise_all(funs(sum)) %>% mutate(val = rollsumr(REQUESTS, k = 8, fill = NA)) %>% filter(row_number()%%8 == 0) %>% top_n(1)
        
        q12 <- df %>% select(Date, TRIPS, REQUESTS) %>% group_by(Date) %>% summarise_all(funs(sum)) %>% mutate(TRIPs_PER_REQUEST = TRIPS/REQUESTS) %>% arrange(TRIPs_PER_REQUEST) %>% tail(1)

}


