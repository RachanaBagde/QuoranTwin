#----------------
#This gives the rank
#----------------

 r <- ave(k$X,k$id1,FUN=function(x)rank(x,ties.method="first"))
 k$rank <- r

 ## so now rank is 14th column
 #thus rank for any id as
 
 k[id,14]


#-----------------



dat <- read.csv("top20.csv",stringsAsFactors=FALSE)
 match <- dat
 match[,-1]<-0


con <- paste0(names(match[,-1]),"_imp") 


q_following_imp <-4                            
topic_followed_imp <-4                         
topic_answered_imp <-5                         
nfollowers_imp <-9                              
nfollowing_imp <-9                              
N.ans.answered_imp<-7                          
Nupvotes.on.top.5.ans..Answered.addition_imp <-8
X30.days.views_imp <-3                          
All.time.views_imp <-6                         
N.ans.upv.by.A..April.month_imp <-2             
min_prevotes_imp <-1 


#calculating match score

match$topic_followed <- (21-k[row.names(k),14])*topic_followed_imp

match$q_following <- (21-k[row.names(k),14])*q_following_imp

match$topic_answered <- (21-k[row.names(k),14])*topic_answered_imp

match$nfollowers <- (21-k[row.names(k),14])*nfollowers_imp

match$nfollowing <- (21-k[row.names(k),14])*nfollowing_imp

match$N.ans.answered <- (21-k[row.names(k),14])*N.ans.answered_imp

match$Nupvotes.on.top.5.ans..Answered.addition <- (21-k[row.names(k),14])*Nupvotes.on.top.5.ans..Answered.addition_imp

match$X30.days.views <- (21-k[row.names(k),14])*X30.days.views_imp

match$All.time.views <- (21-k[row.names(k),14])*All.time.views_imp

match$N.ans.upv.by.A..April.month <- (21-k[row.names(k),14])*N.ans.upv.by.A..April.month_imp


match$min_prevotes<- (21-k[row.names(k),14])*min_prevotes_imp








#-----------------------------

m[which(d[,y]==x),y]


matcher <- function(x,y){  return(na.omit(m[which(d[,y]==x),y]))  }


max_matcher <- function(x) { return(sum(matcher(x,3:13)))    }


result <- foreach(1:1000, function(x) {if(max(max_matcher(1:1000))==max_matcher(x))return(x)}) 


### u need to think about doing this group-wise now..

m %>% group_by(id1) %>% do(data.frame(result))
