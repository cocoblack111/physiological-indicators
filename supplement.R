
mymodel <- function(data,cov){
  
  #-------------预处理-------------#
  
  #数据变量转化#
  #factor
  for (i in names(data)[c(5:15)]) { 
    data[[i]]<-as.factor(data[[i]])}        #for循环适合批量完成某种操作；function则是得到一个自定义的结果
  #int
  data$status <-as.integer(data$status)     #status不能转换为因子变量，否则会报“下标出错”
  
  #other
  indicators <- data %>% select(systolic:WC) 
  baseline <- data %>% select(age:sleepp)
  
  
  
  
  
  #比较模型   
  mycompare <- function(namepi){
    
    
    #线性
    data_delete_na <- data %>% filter( !(is.na(  as.numeric(indicators[[namepi]]  )))  )
    pi_delete_na <- na.omit( as.numeric(indicators[[namepi]]) )
    library(infotheo)
    pi_int <-  as.factor ( discretize (pi_delete_na,"equalfreq" ,4)[["X"]])
    fit_lin <- coxph( formula(paste0("Surv(time,status)~pi_int+",cov)) ,x=T,y=T,data=data_delete_na)  
    
    
    #非线性
    fun <- function(degree){
      fit_non <- coxph( formula(paste0("Surv(time,status)~ ns(",namepi,",",degree,")+",cov)) ,x=T,y=T,data = data)
      AIC(fit_non) }
    degree = which.max(sapply(2:4, fun))
    fit_non <- coxph( formula(paste0("Surv(time,status)~ ns(",namepi,",",degree,")+",cov)) ,x=T,y=T,data = data)
    
    #指标
    AIC = round (AIC(fit_lin) - AIC(fit_non) ,1)
    p = if(anova(fit_lin,fit_non,test = 'LRT')[2,4]<0.001){print('<.001')
    }else {round(anova(fit_lin,fit_non,test = 'LRT')[2,4],2) }
    
    #样本量
    number <- length(na.omit(data[[namepi]]))
    
    
    cbind(number,AIC,p)
    
  }
  
  compare <- sapply( names(indicators), mycompare ) 
  
  
  AIC_select_optimal_model <- compare %>% 
    t() %>%
    as_tibble() %>%
    rename('No.of studies'= 1,'AIC Difference' = 2,'P lin-non' = 3)%>%
    mutate('Names' =indic_names) %>%
    mutate('Optimal Model' =  ifelse( compare[3,] > 0, 'Nonlinear', 'Linear')) %>%
    select('Names','No.of studies','AIC Difference','P lin-non','Optimal Model')   #重排列的顺序
  
  
  
  #非线性模型有意义的值
  mynonlinear <- function(namepi){
    
    
    library(boot)
    
    fun <- function(data,cov,indices){
      
      #随机索引行
      d <- data[indices,]
      
      #建模
      fit_non <- coxph( formula(paste0("Surv(time,status)~ ns(",namepi,",3)+",cov)) ,x=T,y=T,data = d)
      
      #预测（termplot）
      pre <- termplot(fit_non,se = T, plot = F)[[namepi]] %>% as_tibble() %>% mutate( hazard_ratio = exp(y) ) 
      
      #是否自定义指标区间范围
      if(1==1){
        #filter(between (x,lim))
        lim <- c("70,200","50,100","50,100",
                 "80,140","30,90","50,200","120,280",
                 "50,250","5,10",
                 "3,10","10,20","30,60","70,110","10,500",
                 "0,2","5,30","3,9","0.6,1.5",
                 "140,180","40,90","40,120")
        pre <- eval(parse(text = paste0( 
          "filter(pre,between (x,",lim[which(names(indicators)==namepi)],"))"
        )))
      }else{pre <- pre}
      
      
      #找最低值点
      lrx <-  pre$x[which.min(pre$hazard_ratio)]
      
    }
    
    set.seed(12345)
    boot <- boot( data = data, cov=cov, statistic = fun, R=1000)
    bootci <- boot.ci ( boot.out = boot, 
                        conf = 0.95, 
                        type="perc",
                        index = 1 )
    
    LR <-  c(round(bootci[['t0']],1),round(bootci[['percent']][4],1),round(bootci[['percent']][5],1))
    
  }
  
  LR <- sapply(names(indicators), mynonlinear)
  LR_nonlinear_result <- LR %>% t() %>% as_tibble() %>%
    rename('Lowest risk point' = 1,lower=2, upper=3) 
  
  
  
  #线性有意义的值
  mylinear <- function(namepi){
    
    #建模
    data_delete_na <- data %>% filter( !(is.na(  as.numeric(indicators[[namepi]]  )))  )
    pi_delete_na <- na.omit( as.numeric(indicators[[namepi]]) )
    library(infotheo)
    pi_int <-  as.factor ( discretize (pi_delete_na,"equalfreq" ,4)[["X"]])
    fit_lin <- coxph( formula(paste0("Surv(time,status)~pi_int+",cov)) ,x=T,y=T,data=data_delete_na)  
    
    #提取HR值
    summary(fit_lin)[["coefficients"]][1:3,] %>%
      as_tibble() %>% 
      rename(hr=2,se=3,P=5) %>%
      mutate(lower=hr-1.96*se
             ,upper=hr+1.96*se) %>%
      mutate(HR=paste0( round(hr,2),'(',round(lower,2),'-',round(upper,2),')' ) )%>%
      select('HR') %>%
      mutate_if(is.numeric, ~round(., 2)) %>% t() %>%
      as_tibble() %>%
      mutate( '1st Qu' = 'Ref.' )
  }
  
  HR <- sapply(names(indicators), mylinear) 
  
  HR_linear_result <-  HR  %>% t() %>% as_data_frame() %>%
    select('1st Qu','V1','V2','V3') %>%
    rename('Q2 HR(95%CI)' = 2, 'Q3 HR(95%CI)' = 3,"Q4 HR(95%CI)" = 4)
  
  
  
  table <- bind_cols(AIC_select_optimal_model,LR_nonlinear_result,HR_linear_result)
  
  #print
  print_table <- table %>%
    rbind(c('Blood Pressure and Pulse',rep(NA, length(table)))
          ,c('lood Lipids Level',rep(NA, length(table)))
          ,c('Blood Glucose Level',rep(NA, length(table)))
          ,c('Blood Count',rep(NA, length(table)))
          ,c('Kidney Indicators',rep(NA, length(table)))
          ,c('Body Composition',rep(NA, length(table)))) 
  print_table <-print_table[c(22,1:3,23,4:7,24,8:9,25,10:14,26,15:18,27,19:21),]
  
  
  
} 

myplot <- function(data,cov,prob){
  
  
  #-------------预处理-------------#
  
  #数据变量转化#
  #factor
  for (i in names(data)[c(5:16)]) { 
    data[[i]]<-as.factor(data[[i]])}        #for循环适合批量完成某种操作；function则是得到一个自定义的结果
  #int
  data$status <-as.integer(data$status)     #status不能转换为因子变量，否则会报“下标出错”
  data$sleepp <-as.integer(data$sleepp)
  
  #other
  indicators <- data %>% select(systolic:WC) 
  baseline <- data %>% select(age:sleepp)
  
  
  
  
  fit <- function(namepi){
    
    
    #删去极端值
    n <- quantile( indicators[[namepi]], probs=prob, na.rm=T, names=F )
    data <- eval(parse(text=paste0(
      "filter(data,between(",namepi,",n[1],n[2]))"
    )))
    
    
    #建模
    fit_non <- coxph( formula(paste0("Surv(time,status)~ ns(",namepi,",3)+",cov)) ,x=T,y=T,data = data)
    
    
    #预测（termplot）
    pre <- termplot(fit_non,se = T, plot = F)[[namepi]] %>% as_tibble() 
    
    
    #filter(between (x,lim))
    lim <- c("70,200","50,100","50,100",
             "80,140","30,90","50,200","120,280",
             "50,250","5,10",
             "3,10","10,20","30,60","70,110","10,500",
             "0,2","5,30","3,9","0.6,1.5",
             "140,180","40,90","40,120")
    pre <- eval(parse(text = paste0( 
        "filter(pre,between (x,",lim[which(names(indicators)==namepi)],"))"
      )))
    
    
    #设置回归线及置信区间
    pre <- pre %>%
      mutate( hazard_ratio = exp(y) ) %>%
      mutate( hazard_ratio_low_ci = exp(y-1.96*se) ) %>%
      mutate( hazard_ratio_high_ci =exp(y+1.96*se) )
    
    
    #找最低值点
    lry <-  pre$hazard_ratio[which.min(pre$hazard_ratio)]
    lry_vector <- rep(lry,nrow(pre)) %>% as_tibble()
    pre <- pre %>% bind_cols(value=lry_vector) 
    
    
    #设置点为参考值点
    pre <- pre %>%
      mutate( hazard_ratio = hazard_ratio-value +1  ) %>%
      mutate( hazard_ratio_low_ci = hazard_ratio_low_ci-value +1 ) %>%
      mutate( hazard_ratio_high_ci =hazard_ratio_high_ci-value +1 )
    
    
    
    #可视化
    library(ggplot2)
    ggplot(data=pre)+
      geom_ribbon(aes(x=x,ymin=hazard_ratio_low_ci,ymax=hazard_ratio_high_ci),fill='#94837A',alpha=0.2)+
      geom_line(aes(x=x,y=hazard_ratio),color='#94837A', show.legend = F)+
      labs(x=indic_names[which(names(indicators)==namepi)],y=NULL)+
      coord_cartesian(ylim=c(0.5,2.0))+  #yes!!yes!!yes!! 直接截断ylim(0.5,2.0)的图形不完整！！！！！
      
      theme_bw()
    

  }
  
  p <- lapply(names(indicators),fit)
  library(patchwork)
  p[[1]]+p[[2]]+p[[3]]+plot_spacer()+plot_spacer()+
    p[[4]]+p[[5]]+p[[6]]+p[[7]]+plot_spacer()+ 
    p[[8]]+p[[9]]+plot_spacer()+plot_spacer()+plot_spacer() +
    p[[10]]+p[[11]]+p[[12]]+p[[13]]+p[[14]] +
    p[[15]]+p[[16]]+p[[17]]+p[[18]]+plot_spacer() +
    p[[19]]+p[[20]]+p[[21]]+plot_spacer()+plot_spacer()+
    plot_layout(nrow = 6)
  
  
  
}   



#分层分析（性别）

data_female <- mydata %>% filter(sex == 2)
data_male <- mydata %>% filter(sex == 1)
indicators_female <- mydata %>% select(systolic:WC) 
indicators_male <- mydata %>% select(systolic:WC) 

 #表
    etable4 <- mymodel(data = data_female,cov = "age" )
    etable5 <- mymodel(data = data_male,cov = "age")
    for(i in 1:length(names(etable4))) {etable4[[i]] <- as.character(etable4[[i]])}
    for(i in 1:length(names(etable4))) {etable5[[i]] <- as.character(etable5[[i]])}
    write.table(etable4,file = 'Female table.csv',sep = ',',row.names = FALSE)
    write.table(etable5,file = 'Male table.csv',sep = ',',row.names = FALSE)
    
 #图   

  female_plot <- myplot(data=data_female,cov = 'age',prob = c(0,1));female_plot
  male_plot <- myplot(data=data_male,cov = 'age',prob = c(0,1));male_plot
  ggsave(female_plot, file='female plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
  ggsave(male_plot, file='male plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
  
  
     
    

  
#敏感性分析
  
    #-----co_variate 
    
    #表

    etable6 <- mymodel(data = mydata,cov = "age+sleep+activity+smoke+drink")
    etable7 <- mymodel(data = mydata,cov = "age+sleep+activity+smoke+drink+da007_1_+da007_3_+da007_4_")
    
    
    #图
    cov1 <- myplot(data = mydata,cov = "age+sleep+activity+smoke+drink",prob=c(0,1));cov1
    cov2 <- myplot(data = mydata,cov = "age+sleep+activity+smoke+drink+da007_1_+da007_3_+da007_4_",prob=c(0,1));cov2
    ggsave(cov1, file='cov_for_lifestyle_plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
    ggsave(cov2, file='cov_for_diseases_plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
    
    
    
    #-----exclude_extreme values
  
  

    exclude_extreme <- myplot(data = mydata,cov = "age+sex",prob=c(0.05,0.95))
  

    
    #-----exclude_diseases
    
    
    data_exclude_disease <- mydata %>%
      filter(da007_1_==0,da007_2_==0,da007_3_==0)
    exclude_diseases <-myplot(data = data_exclude_disease,cov ="age+sex",prob=c(0,1))
    ggsave(exclude_diseases, file='exclude_diseases_plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
    
    
  
    #-----log_trans
  
  for (i in names(data)[17:37]) {
    data[[i]] <- log(data[[i]])}
  str(data)
  
   
     log <- myplot(data = data,cov = "age+sex",prob = c(0,1));log
     ggsave(log, file='log_transform_plot.pdf', width=10, height=8, path = '~/Desktop/夹心No.3/result')
     
  
  


  
  