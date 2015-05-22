

## Getting the commons from the collection of 100 quorans A is following.

rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=length(intersect(c(t(dat[i, 96:120])),
                                       c(t(dat[j, 96:120])))))
  }))
})) -> commons


##------------------------------------------------------------------


ya1 <-group_by(commons,id1)%>% top_n(20,common)

ya2 <- group_by(ya1,id1)%>% top_n(20,id2)
     
---------------------------------------------------
topics
---------------------------------------------------
rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=length(intersect(c(t(dat[i, 131:140])),
                                       c(t(dat[j, 131:140])))))
  }))
})) -> commons

ya1 <-group_by(commons,id1)%>% top_n(20,common)

ya2 <- group_by(ya1,id1)%>% top_n(20,id2)



---------------------------------------------------------
getting top 20 following
-------------------------
g <- datfinal %>%
gather(key="Id2_key",value="Id2",starts_with("follow"))%>%
gather(key="c_key",value="c",starts_with("common"))

m <- g%>%group_by(id1)%>%top_n(20,c)%>%arrange(id1)
k <- m%>% group_by(id1)%>%filter(row_number()<=20)





-----------
Nfollowers
-----------
rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 5]-dat[j, 5])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$N.ans.answered <- nnf$id2


 ##---------


rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 6]-dat[j, 6])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$Nupvotes.on.top.5.ans..Answered.addition <- nnf$id2

##-----------


rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 7]-dat[j, 7])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$X30.days.views <- nnf$id2

##------------

rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 8]-dat[j, 8])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$All.time.views <- nnf$id2

##-------------
rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 9]-dat[j, 9])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$N.ans.upv.by.A..April.month <- nnf$id2

##-------------
rbind_all(lapply(1:nrow(dat), function(i) {
  rbind_all(lapply(setdiff(1:nrow(dat), i), function(j) {
    data.frame(id1=i,
               id2=j,
               common=abs((dat[i, 141]-dat[j, 141])))
  }))
})) -> commons

nf <- commons%>%group_by(id1)%>%top_n(20,-common)%>%filter(row_number()<=20)
nnf <- nf%>% group_by(id1) %>% arrange(common)
nnf <- arrange(nnf,id1)
k$min_prevotes <- nnf$id2

write.csv(k,file="top20.csv")
##------------


---------------------------------
#minimum from 10 pre votes of dat
---------------------------------

 p <- sapply(1:nrow(dat),function(x){min(dat[x,10:19])})


----------------
#This gives the rank
----------------

 r <- ave(k$X,k$id1,FUN=function(x)rank(x,ties.method="first"))
