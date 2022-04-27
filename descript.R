
#准备

rm(list = ls())
library(readxl)
library(dplyr)
library(survival)
library(splines)


  indic_names <- c("Systolic (mmHg)"
                 ,"Diastolic(mmHg)"
                 ,"60 sec. pulse (P)" 
                 ,"Triglycerides(mg/dL)"
                 ,"HDL Cholesterol(mg/dL)"
                 ,"LDL Cholesterol(mg/dL)" 
                 ,"Total Cholesterol(mg/dL)"
                 ,"Glucose(mg/dL)"
                 ,"Glycated Hemoglobin(%)" 
                 ,"White Blood Cell (in thousands)" 
                 ,"Hemoglobin(g/dL)" 
                 ,"Hematocrit(%)"
                 ,"Mean Corpuscular Volume(fl)"
                 ,"Platelets(10^9/L)"                    
                 ,"Creatinine(mg/dL)" 
                 ,"Blood Urea Nitrogen(mg/dL)"
                 ,"Urc Acid(mg/dL)"
                 ,"Cystatin C"
                 ,"Standing Height (cm)" 
                 ,"Weight (kg)"
                 ,"Waist Circumference (cm)")   
  
    a <- read_xlsx('~/Desktop/夹心No.3/range.xlsx',sheet = 'Sheet1')  
    mydata <- read_xlsx('~/Desktop/夹心No.3/data.xlsx'
                      , sheet = 'submydata')                                                                            
    data=mydata
    
    #数据变量转化#
    #factor
    for (i in names(data)[5:15]) { 
      data[[i]]<-as.factor(data[[i]])}        #for循环适合批量完成某种操作；function则是得到一个自定义的结果
    #int
    data$status <-as.integer(data$status)     #status不能转换为因子变量，否则会报“下标出错”

    #other
    indicators <- data %>% select(systolic:WC) 
    baseline <- data %>% select(age:sleepp)
    
    str(data)
    
    
    #---------baseline description---------#
    
    
    library(compareGroups)
   
    res <- createTable(compareGroups(
      data = baseline
      ,status~.
      ,method = c(age=NA,sleepp=NA,time=NA)
    ));res
    
    export2csv(res,file = 'baseline description.csv')
    
    
    #失访
    mylose <- read_xlsx('~/Desktop/夹心No.3/data.xlsx'
                        , sheet = 'mydata')   
    #数据变量转化#
    for (i in names(mylose)[5:16]) { 
      mylose[[i]]<-as.factor(mylose[[i]])}       
    mylose$lose <-as.integer(mylose$lose)     
    baseline_lose <- mylose %>% select(lose:sleepp)
    
    str(baseline_lose)
    
    res <- createTable(compareGroups(
      data = baseline_lose
      ,lose ~ .
      ,method = c(sleepp=NA)
    ));res
    export2csv(res,file = 'lose_description.csv')

    
    
    #---------indicators description---------#
    
    
  #表  
    
  fun <- function(namepi){
    
    min = as.numeric(summary(na.omit(indicators[[namepi]]))[1]) 
    max = as.numeric(summary(na.omit(indicators[[namepi]]))[6])         
    iqr = IQR(na.omit(indicators[[namepi]]))
    med = median(na.omit(indicators[[namepi]]))
    sd =  sd(na.omit(indicators[[namepi]])) 
    mean = mean(na.omit(indicators[[namepi]])) 
    c(round(iqr,1),round(med,1),round(sd,1),round(mean,1),min,max) 
    
    }
  pi <- sapply(names(indicators), fun) %>% t() 

  mediqr <- paste( pi[,2],'(',pi[,1],')' )
  meansd <- paste( pi[,4],'(',pi[,3],')' )

    table <- pi %>% 
      as_tibble() %>%
      bind_cols(mediqr,meansd,indic_names) %>%
      select(-c(1:4)) %>%
      rename('Mnimum'=1,'Maximum'=2,'Median(IQR)'=3,'Mean(SD)'=4,'Names'=5)%>%
      select('Names','Mnimum','Median(IQR)','Mean(SD)','Maximum') %>%
      mutate_if(is.numeric, ~round(., 1)) 

  
      print_table <- table %>%
      rbind(c('Blood Pressure and Pulse',rep(NA, length(table)))
            ,c('Blood Lipids Level',rep(NA, length(table)))
            ,c('Blood Glucose Level',rep(NA, length(table)))
            ,c('Blood Count',rep(NA, length(table)))
            ,c('Kidney Indicators',rep(NA, length(table)))
            ,c('Body Composition',rep(NA, length(table)))) 
      print_table <-print_table[c(22,1:3,23,4:7,24,8:9,25,10:14,26,15:18,27,19:21),]
    
  



    #图

    density <- function(namepi){
     
    library(ggplot2)
    p <-ggplot(indicators)+
        geom_density(
          mapping = aes_string(x=namepi)
          ,color = "pink"
          ,fill="#69b3a2"
          ,lwd = 0.5
          ,linetype = 2
          ,alpha=0.5)+
      labs(x=indic_names[which(names(indicators)==namepi)]
           ,y=NULL)+
      theme_bw()
    }
    
     p <- lapply(names(indicators), density)
     library(patchwork)
     efigure2 <- p[[1]]+p[[2]]+p[[3]]+plot_spacer()+plot_spacer()+
       p[[4]]+p[[5]]+p[[6]]+p[[7]]+plot_spacer()+ 
       p[[8]]+p[[9]]+plot_spacer()+plot_spacer()+plot_spacer() +
       p[[10]]+p[[11]]+p[[12]]+p[[13]]+p[[14]] +
       p[[15]]+p[[16]]+p[[17]]+p[[18]]+plot_spacer() +
       p[[19]]+p[[20]]+p[[21]]+plot_spacer()+plot_spacer()+
       plot_layout(nrow = 6)

      
       
     #变量对数转换 < 再带入上面作图表
     
     for (i in names(indicators)) {
       indicators[[i]] <- log(indicators[[i]])}
     
     
     ggsave(efigure2,file='log_density_plot.pdf',width=10, height=8,path = '~/Desktop/夹心No.3/result')

