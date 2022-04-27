
  

  
    
  #建模及模型评价与优化
  
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
      
  
     table <- bind_cols(AIC_select_optimal_model,LR_nonlinear_result)
     
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
  
  
  setwd('~/Desktop/夹心No.3/result');getwd()
  write.table(table2,file = 'Table 3.csv',sep = ',',row.names = FALSE)
  
  
  #建模及模型可视化
  
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
    n = c(1,4,8,10,15,19)
    library(ggplot2)
    ggplot(data=pre)+
      geom_ribbon(aes(x=x,ymin=hazard_ratio_low_ci,ymax=hazard_ratio_high_ci),fill='#F9D9BA',alpha=0.5)+
      geom_line(aes(x=x,y=hazard_ratio),color='#CD5B32', show.legend = F)+
      labs(x=indic_names[which(names(indicators)==namepi)]
           ,y = if( which(names(indicators)==namepi) %in% n ){
             'HR (95%CI)'
           }else{NULL})+
      coord_cartesian(ylim=c(0.5,2.0))+  #yes!!yes!!yes!! 直接截断ylim(0.5,2.0)的图形不完整！！！！！
  
      
      geom_vline(aes(xintercept = as.numeric(a[which(names(indicators)==namepi),1]))
                 ,color = '#909090',linetype = 'dotted',show.legend = TRUE)+
      geom_vline(aes(xintercept = as.numeric(a[which(names(indicators)==namepi),2]))
                 ,color = '#909090',linetype = 'dotted')+
      geom_vline(aes(xintercept = as.numeric(a[which(names(indicators)==namepi),3]))
                 ,color = '#909090',linetype = 'dotted')+

    theme(panel.background = element_rect(fill = 'transparent',colour = NA)
          ,axis.line = element_line(color = 'black',size = 0.4,lineend = 'butt')
          ,axis.title = element_text(size = 6)
          ,axis.text.y = if( which(names(indicators)==namepi) %in% n ){
            element_text(size = 8)
          }else{
            element_blank() 
          })

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

  

  figure1 <- myplot (data = mydata,cov = 'age+sex', prob=c(0,1) );figure1
  ggsave(figure1, file='Figure 1.pdf', width=8, height=8, path = '~/Desktop/夹心No.3/result')
  




  

  
  

