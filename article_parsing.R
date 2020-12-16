library(dplyr)
library(tidyr)
library(purrr)
library(tidytext)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(igraph)
library(ggraph)
library(reshape2)
library(caret)
library(Lahman)
`%!in%`<-Negate(`%in%`)
articles<-read.csv('C:/Users/peter/Documents/baseball-databases/mlb_trade_rumors/articles.csv')
names(articles)<-c('team','id','date','header','content')
articles$date<-as.Date(articles$date,'%Y-%m-%d')
articles<-articles%>%mutate(inseason=ifelse(month(date)>=4&month(date)<=10,1,0),year=ifelse(inseason==1|(inseason==0&month(date)>10),year(date),year(date)-1),max_date=date('2020-12-09'),days=as.numeric(max_date-date))%>%group_by(team)%>%mutate(count=row_number(),filter=((max(days)<2050|max(days)>2850)&max(count)==count))%>%ungroup()

team_dates<-articles%>%group_by(team)%>%summarise(n=n(),min_date=min(date),days=max(days))
names(team_dates)<-c('Team','Count','Earliest Date','Days')
team_dates$Team<-str_to_title(gsub(team_dates$Team,pattern = '-',replacement = ' '))
write.csv(team_dates%>%arrange(Days),'team_dates.csv',row.names = F)

ggplot(team_dates,aes(reorder(team,-days),days))+geom_bar(stat='identity')+coord_flip()
articles$team<-str_to_title(gsub(articles$team,pattern = '-',replacement = ' '))
ggplot(articles%>%select(days,count,team,filter),aes(days,count,col=team))+geom_line()+theme(legend.position = "none")+geom_label_repel(data=articles%>%select(days,count,team,filter)%>%filter(filter==T),aes(label=team),vjust = .5, size=4)+labs(title='Number of Days to Reach 2000 Posts by Team',x='Days',y='Number of Posts')

###################
in_off_perc<-articles%>%filter(year>=2016&year<=2019)%>%group_by(team,inseason)%>%summarise(n=n())%>%spread(inseason,n)%>%mutate(n=`0`+`1`,off_perc=round(`1`/(`0`+`1`),3)*100,off_class=paste0(off_perc,'%'))%>%arrange(desc(off_perc))%>%select(-off_perc)
names(in_off_perc)<-c('Team','In Season','Off Season','Total','Off Season Percentage')
in_off_perc$Team<-str_to_title(gsub(in_off_perc$Team,pattern = '-',replacement = ' '))
write.csv(in_off_perc%>%arrange(desc(`Off Season Percentage`)),'off_season_perc.csv',row.names = F)

###LIMIT TO 2016-2019 common time frame
lim_articles<-articles%>%filter(year>=2016&year<=2019)
lim_articles_header<-lim_articles%>%select(-content)
multi_teams<-lim_articles_header%>%filter(grepl(header,pattern = 'deal|trade'))%>%group_by(id)%>%mutate(n_teams=length(unique(team)))%>%filter(n_teams<=4,n_teams>1)%>%group_by(id)%>%summarise(team1=sort(unique(team))[1],team2=sort(unique(team))[2],team3=sort(unique(team))[3],team4=sort(unique(team))[4])%>%nest(-id)
multi_teams_func<-function(data){
  if(is.na(data$team3)){
    fin<-data[1:2]
  }else if(is.na(data$team4)){
    fin<-as.data.frame(mapply(c,data[1:2],data[,c(1,3)],data[2:3]))
  }else{
    fin<-as.data.frame(mapply(c,data[1:2],data[,c(1,3)],data[,c(1,4)],data[2:3],data[,c(2,4)],data[3:4]))
  }
  fin
}
f_multi_teams<-multi_teams%>%mutate(un=map(data,~multi_teams_func(.x)))%>%select(-data)%>%unnest(un)
team_connection<-f_multi_teams%>%group_by(team1,team2)%>%summarise(n=n())
team_connection$team1<-str_to_title(gsub(team_connection$team1,pattern = '-',replacement = ' '))
team_connection$team2<-str_to_title(gsub(team_connection$team2,pattern = '-',replacement = ' '))

team_connection %>% filter(n>20)%>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n^3), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = .5) +
  theme_void()

###########
lim_articles_header$header<-iconv(lim_articles_header$header, from = 'UTF-8', to = 'ASCII//TRANSLIT')
lim_articles_header$header<-tolower(gsub(lim_articles_header$header,pattern='\n|[[:punct:]]',replacement = ' '))
articles_header_token<-lim_articles_header%>%select(-c(max_date,days,count,filter))%>%unnest_tokens(output = word,input = header, token = "words")%>%anti_join(stop_words)
articles_header_token<-articles_header_token%>%filter(!grepl(word,pattern = '[[:digit:]]')&!(word%in%c('twitter','email','season','comments','share','team','league','send','retweet','cdt')))
mention_player_team<-articles_header_token%>%filter(word%in%tolower((People%>%filter(year(finalGame)>2015))$nameLast)&word%!in%c('minor','washington','white','adams'))%>%group_by(team,word)%>%summarise(n=n())%>%arrange(desc(n))
mention_player_team$team<-str_to_title(gsub(mention_player_team$team,pattern = '-',replacement = ' '))
mention_player_team$word<-str_to_title(mention_player_team$word)
names(mention_player_team)<-c('Team','Word','Mentions')
write.csv(mention_player_team,'player_team_mentions.csv',row.names = F)

############
header_cats<-lim_articles_header%>%mutate(trade=ifelse(grepl(header,pattern = 'trade|deal'),1,0),add=ifelse(grepl(header,pattern ='select|sign'),1,0),drop=ifelse(grepl(header,pattern ='option|release|outright|non tender'),1,0))
cat_perc_year<-header_cats%>%group_by(team,year,inseason)%>%summarise(trade=sum(trade)/n(),add=sum(add)/n(),drop=sum(drop)/n())%>%mutate(dminus=add-drop)%>%mutate(date=ifelse(inseason==1,paste0(year,'-01-01'),paste0(year,'-12-01')))%>%arrange(desc(dminus))%>%group_by(year,inseason)%>%mutate(rank=row_number())
cat_perc_year$date<-as.Date(cat_perc_year$date)
cat_perc_year$team<-str_to_title(gsub(cat_perc_year$team,pattern = '-',replacement = ' '))
ggplot(cat_perc_year,aes(date,dminus,col=team))+geom_smooth(se=F)+geom_label_repel(data=cat_perc_year%>%filter((year==2019&(rank<4|rank>26)&inseason==0)|(year==2017&rank==30&inseason==1)),aes(label=team),vjust = .5, size=4)+theme(legend.position = "none")+labs(title='Buying Vs. Selling by Team Over Time',x='Date',y='Buying Minus Selling')
cat_perc_year_final<-cat_perc_year%>%ungroup()%>%select(team,date,trade,add,drop,dminus)%>%mutate(trade=paste0(round(trade,3)*100,'%'),add=paste0(round(add,3)*100,'%'),drop=paste0(round(drop,3)*100,'%'),dminus=paste0(round(dminus,3)*100,'%'))%>%arrange(date,team)
names(cat_perc_year_final)<-c('Team','Date','Trade','Add','Drop','Add-Drop')
cat_perc_year_final$Team<-str_to_title(gsub(cat_perc_year_final$Team,pattern = '-',replacement = ' '))
write.csv(cat_perc_year_final,'add_drop_team.csv',row.names = F)

#############
articles_header_token$team<-str_to_title(gsub(articles_header_token$team,pattern = '-',replacement = ' '))
Teams$name[which(Teams$name=='Los Angeles Angels of Anaheim')]<-'Los Angeles Angels'
Teams$name[which(Teams$name=='St. Louis Cardinals')]<-'St Louis Cardinals'
articles_token_record<-articles_header_token%>%left_join(Teams%>%select(yearID,name,W,L),by=c('year'='yearID','team'='name'))

winning<-articles_token_record%>%filter(word%!in%c('cashman','dombrowski','nats','dave','aroldis','gerrit','george','yu','kubatko','phils','victor','bucs','billy','halos','masnsports','dan','south')&word%!in%(articles_token_record%>%select(team)%>%distinct()%>%mutate(team=tolower(team))%>%unnest_tokens(bits,team))$bits&word%!in%tolower(mention_player_team$Word))%>%group_by(word)%>%summarise(n=n(),win=mean(W)/(mean(W)+mean(L)))%>%filter(n>1000&(win>.526))
names(winning)<-c('Word','Mentions','Winning Percentage')
write.csv(winning%>%arrange(desc(`Winning Percentage`))%>%mutate(`Winning Percentage`=paste0(round(`Winning Percentage`,3)*100,'%')),'winning_words.csv',row.names = F)
losing<-articles_token_record%>%filter(word%!in%c('cashman','dombrowski','nats','dave','aroldis','gerrit','george','yu','kubatko','phils','victor','bucs','billy','halos','masnsports','dan','south')&word%!in%(articles_token_record%>%select(team)%>%distinct()%>%mutate(team=tolower(team))%>%unnest_tokens(bits,team))$bits&word%!in%tolower(mention_player_team$Word))%>%group_by(word)%>%summarise(n=n(),win=mean(W)/(mean(W)+mean(L)))%>%filter(n>1000&(win<.491))
names(losing)<-c('Word','Mentions','Winning Percentage')
write.csv(losing%>%arrange(`Winning Percentage`)%>%mutate(`Winning Percentage`=paste0(round(`Winning Percentage`,3)*100,'%')),'losing_words.csv',row.names = F)

#############
team_vocab<-articles_header_token%>%filter(word%!in%c('cashman','dombrowski','nats','dave','aroldis','gerrit','george','yu','manny','dipoto','boone','kubatko','phils','victor','bucs','billy','halos','masnsports','dan','south','van','wagenen','zaidi','luhnow','slusser','la','russa','preller','mookie','matt','maddon','kapler','justin','jeter','jeff','john','grant','giancarlo','epstein','duquette','cherington','callaway','bryce','bloom','andrew','aaron','adams','alderson','baker','zach')&word%!in%(articles_token_record%>%select(team)%>%distinct()%>%mutate(team=tolower(team))%>%unnest_tokens(bits,team))$bits&word%!in%tolower(mention_player_team$Word))%>%group_by(team,year,inseason,word)%>%summarise(n=n())%>%group_by(word)%>%filter(max(n)>120)%>%spread(word,n)
team_vocab[is.na(team_vocab)]<-0
team_vocab_res<-team_vocab%>%left_join(Teams%>%select(yearID,name,W,L),by=c('year'='yearID','team'='name'))%>%mutate(winp=W/(W+L))
summary(lm(winp~.,team_vocab_res%>%select(-c(team,year,W,L))))

articles_team_res<-articles_header_token%>%mutate(year=ifelse(inseason==0,year+1,year))%>%group_by(team,year,inseason)%>%summarise(posts=length(unique(id)))%>%left_join(Teams%>%select(yearID,name,W,L),by=c('year'='yearID','team'='name'))%>%mutate(winp=W/(W+L))%>%na.omit()
articles_team_res_voc<-articles_team_res%>%left_join(team_vocab_res%>%select(team,year,tax,surgery,roster,rotation,inseason,trade,payroll,organization,game,baseball,seasons))
articles_team_res_voc$inseason<-as.factor(articles_team_res_voc$inseason)
articles_team_res_voc_perc<-articles_team_res_voc%>%ungroup()%>%mutate_at(8:17,~.x/posts)

model<-lm(winp~tax+payroll+organization+game+seasons+inseason:payroll+trade+surgery,articles_team_res_voc_perc%>%filter(year<2019))
articles_team_res_voc_perc_pred<-articles_team_res_voc_perc%>%filter(year==2019)%>%cbind.data.frame(pred=predict(model,articles_team_res_voc_perc%>%filter(year==2019)))
team_voc_pred<-articles_team_res_voc_perc_pred%>%group_by(team)%>%summarise(W=mean(W),L=mean(L),winp=mean(winp),xwinp=mean(pred),xW=xwinp*162,xL=162-(xwinp*162))
ggpred<-team_voc_pred%>%melt('team',c('W','xW'))
ggplot(ggpred,aes(reorder(team,value),value,fill=variable))+geom_bar(stat='identity',position='dodge')+coord_flip()+labs(title='Wins vs. Expected Wins 2019 Based on MLB Trade Rumors Posts',x='Team',y='Wins')+scale_fill_brewer(palette = 'Paired',name='Varibale',labels=c("Wins",'Expected\n Wins'))
#bias toward overestimating win total, notable over values Boston, Cubs and under is dodgers and cardinals
#conversation around trade, seasons, organization and payroll while in season is bad and conversation about luxury tax, the game and payroll out of season is good
articles_team_res_voc_perc%>%group_by(inseason)%>%summarise_if(is.numeric,var)
#most all teams complain about payroll in the offseason but the number of teams doing it in season is much lower. The variation in the percentage of posts which mention
#payroll inseason is more than 10x higher between teams

##############
transactions<-read.csv('transaction.csv')
transactions<-transactions%>%mutate(date=word(X0, 1,3, sep=" "))
transactions$date<-as.Date(transactions$date,'%B %d, %Y')
transactions<-transactions%>%na.omit()
transactions$X0<-gsub(transactions$X0,pattern = 'St. Louis Cardinals',replacement = 'St Louis Cardinals')
transactions$X0<-gsub(transactions$X0,pattern = '\\.[[:alpha:]]\\.',replacement = '')
transactions$X0<-gsub(transactions$X0,pattern = 'Jr.',replacement = '')
q <- max(str_count(transactions$X0, "\\."))
trans_spread<-transactions%>%separate(X0, paste0("play_",as.character(c(1:(q+1)))), sep = "\\.")
trans_spread$play_1<-word(trans_spread$play_1,4,-1,sep=' ')
trans_melt<-trans_spread%>%melt(id.vars='date')%>%select(-variable)%>%na.omit()
players<-People%>%unite('name',c(nameFirst,nameLast),sep=' ')%>%filter(year(finalGame)>2013)%>%select(name)%>%distinct()
players$name<-gsub(players$name,pattern = '\\. [[:alpha:]]\\.',replacement = '')
players$name<-gsub(players$name,pattern = 'Jr.',replacement = 'Jr')

trans_melt_var<-trans_melt%>%mutate(team=str_extract(value, paste(unique(articles_team_res_voc_perc$team), collapse="|")),player=str_extract(value, paste(players$name, collapse="|")),trans=str_extract(value, paste(c('released','signed','purchased','traded','granted','retired'), collapse="|")))%>%na.omit()%>%distinct()%>%select(-value)
trans_melt_var_ex<-trans_melt_var%>%mutate(trans=ifelse(trans%in%c('purchased','signed'),'add',ifelse(trans%in%c('released','granted','retired'),'drop',trans)))
trans_melt_var_ex<-trans_melt_var_ex%>%mutate(inseason=ifelse(month(date)>=4&month(date)<=10,1,0),year=ifelse(inseason==1|(inseason==0&month(date)>10),year(date),year(date)-1))

offseason_sign<-trans_melt_var_ex%>%filter(inseason==0&trans=='add')
offseason_sign_sum<-offseason_sign%>%mutate(year=ifelse(month(date)>=10,year(date)+1,year(date)),month=ifelse(month(date)>=10,month(date)-12,month(date)))%>%group_by(month,year)%>%summarise(n=n())%>%ungroup()
offseason_sign_sum$year<-as.factor(offseason_sign_sum$year)
offseason_sign_sum<-offseason_sign_sum%>%mutate(rel=n/mean(n))
offseason_sign_sum$cat<-'signings'

ggplot(offseason_sign_sum,aes(month,n,col=year))+geom_smooth(se=F)+labs(title='MLB Offseason Signings by Year',x='Month',y='Number of Signings')+scale_color_discrete(name='Year')+scale_x_discrete(labels=c('Nov','Dec','Jan','Feb','Mar'))

offseason_rum<-articles%>%filter(inseason==0,year>2014&year<2020)%>%select(date,id)%>%distinct()
offseason_rum_sum<-offseason_rum%>%mutate(year=ifelse(month(date)>=10,year(date)+1,year(date)),month=ifelse(month(date)>=10,month(date)-12,month(date)))%>%group_by(month,year)%>%summarise(n=n())%>%ungroup()
offseason_rum_sum$year<-as.factor(offseason_rum_sum$year)
offseason_rum_sum<-offseason_rum_sum%>%mutate(rel=n/mean(n))
offseason_rum_sum$cat<-'rumors'
ggplot(offseason_rum_sum,aes(month,rel,col=year))+geom_smooth(se=F)+labs(title='MLB Offseason Rumors by Year',x='Month',y='Number of Rumors')+scale_color_discrete(name='Year')+scale_x_discrete(labels=c('Nov','Dec','Jan','Feb','Mar'))

offseason_sum<-offseason_rum_sum%>%rbind(offseason_sign_sum)%>%mutate(fcat=paste0(year,'-',cat))
offseason_sum_spread<-offseason_sum%>%select(-c(fcat,n))%>%spread(cat,rel)
cor(offseason_sum_spread$rumors,offseason_sum_spread$signings)
#correlations between rumors and signings 0.4
##################
articles_player_team<-articles%>%filter(inseason==0,year>=2016)%>%select(date,team,id,header,year)%>%mutate(player=str_extract_all(header, paste(players$name, collapse="|")))
articles_player_team_un<-articles_player_team%>%unnest(player)%>%distinct()

rumors_wsign<-articles_player_team_un%>%left_join(offseason_sign%>%select(-c(trans,inseason)),by=c('year','player'))%>%filter(date.x<date.y)%>%na.omit()
rumors_wsign_sum<-rumors_wsign%>%group_by(year,player,team.y,team.x)%>%summarise(n=n())%>%group_by(year,player,team.y)%>%mutate(perc=n/sum(n),max=ifelse(n==max(n),1,0),sign=ifelse(team.x==team.y,1,0))

cor(rumors_wsign_sum$max,rumors_wsign_sum$sign)
summary(glm(sign~max+n+perc,rumors_wsign_sum,family='binomial'))

rumors_wsign_token<-rumors_wsign%>%unnest_tokens(word,header)
rumors_wsign_token<-rumors_wsign_token%>%filter(!grepl(word,pattern = '[[:digit:]]'))
rumors_wsign_token$word<-iconv(rumors_wsign_token$word,"latin1", "ASCII", sub="")
rumors_wsign_token<-rumors_wsign_token%>%anti_join(stop_words)
names(rumors_wsign_token)[which(names(rumors_wsign_token)=='player')]<-'player1'
rumors_wsign_vocab<-rumors_wsign_token%>%filter(word%!in%c("V1","alex","anthony","anthopoulos","bridich","cards","cdt","chris","daniels","derek","edwin","eric","greg","heyman","jake","jerry","jon","jose","josh","kendrys","kenley","kinzer","lance","lerner","marcell","mark","mike","mlbtr","nightengale","patrick","retweet","rhp","robert","steve","todd","twitter",'cashman','dombrowski','nats','dave','aroldis','gerrit','george','yu','manny','dipoto','boone','kubatko','phils','victor','bucs','billy','halos','masnsports','dan','south','van','wagenen','zaidi','luhnow','slusser','la','russa','preller','mookie','matt','maddon','kapler','justin','jeter','jeff','john','grant','giancarlo','epstein','duquette','cherington','callaway','bryce','bloom','','andrew','aaron','adams','alderson','baker','zach')&word%!in%(articles_token_record%>%select(team)%>%distinct()%>%mutate(team=tolower(team))%>%unnest_tokens(bits,team))$bits&word%!in%tolower(mention_player_team$Word))%>%group_by(team.x,player1,team.y,year,word)%>%summarise(n=n())%>%group_by(word)%>%filter(max(n)>30)%>%ungroup()%>%spread(word,n)
rumors_wsign_vocab[is.na(rumors_wsign_vocab)]<-0
rumors_wsign_vocab_join<-rumors_wsign_vocab%>%left_join(rumors_wsign_sum,by=c('team.x','team.y','year','player1'='player'))

rumors_wsign_vocab_join<-rumors_wsign_vocab_join%>%select(-c(agents,offer,writes,handed,free,winter,n,time,option,market,left,ll,million,link,catcher,closer,roster,draft,qualifying,share,shortstop,team,term,spending,send,seasons,season,players,player,future,top,tax,payroll,late,organization,money,center,boras,baseman,club,bullpen,agent,offseason,signing))

signing_model<-glm(sign.y~.,rumors_wsign_vocab_join%>%filter(year!=2019)%>%select(-c(team.x,team.y,player1,year)),family = 'binomial')
summary(signing_model)
#higher percentage of rumors to a team is a good sign, if it is the most talked about team same goes. Good words: sign, sports, comments. Bad words: owner, pick, teams,pick, dh, agency
rumors_wsign_vocab_pred<-rumors_wsign_vocab_join%>%filter(year==2019)%>%cbind.data.frame(predict=predict.glm(signing_model,rumors_wsign_vocab_join%>%filter(year==2019)%>%select(-c(team.x,team.y,player1,year)),type='response'))
rumors_wsign_vocab_pred<-rumors_wsign_vocab_pred%>%group_by(player1)%>%mutate(pred=ifelse(predict==max(predict),1,0),fpred=ifelse((sum(pred)>1&unique(team.x)[1]==team.x),1,ifelse((sum(pred)>1&unique(team.x)[1]!=team.x),0,'pass')))%>%ungroup()%>%mutate(ffpred=ifelse(fpred=='pass',pred,fpred))%>%filter(max(sign.y)==1)
rumors_wsign_vocab_pred$sign.y<-as.factor(rumors_wsign_vocab_pred$sign.y)
rumors_wsign_vocab_pred$ffpred<-as.factor(rumors_wsign_vocab_pred$ffpred)
confusionMatrix(rumors_wsign_vocab_pred$ffpred,rumors_wsign_vocab_pred$sign.y)
#basic model gives 86% accuracy, 89% specificity, 66% sensitivity. Mean connectedness is 9.17, so naive accuracy is 89% 
#predict this offseason
rumors_wsign<-articles_player_team_un%>%filter(year==2020)%>%left_join(offseason_sign%>%select(-c(trans,inseason)),by=c('year','player'))%>%filter(date.x<date.y|is.na(date.y))
rumors_wsign_sum<-rumors_wsign%>%group_by(year,player,team.y,team.x)%>%summarise(n=n())%>%group_by(year,player,team.y)%>%mutate(perc=n/sum(n),max=ifelse(n==max(n),1,0),sign=ifelse(team.x==team.y,1,0))

cor(rumors_wsign_sum$max,rumors_wsign_sum$sign)
summary(glm(sign~max+n+perc,rumors_wsign_sum,family='binomial'))

rumors_wsign_token<-rumors_wsign%>%unnest_tokens(word,header)
rumors_wsign_token<-rumors_wsign_token%>%filter(!grepl(word,pattern = '[[:digit:]]'))
rumors_wsign_token$word<-iconv(rumors_wsign_token$word,"latin1", "ASCII", sub="")
rumors_wsign_token<-rumors_wsign_token%>%anti_join(stop_words)
names(rumors_wsign_token)[which(names(rumors_wsign_token)=='player')]<-'player1'
rumors_wsign_vocab<-rumors_wsign_token%>%filter(word%!in%c("V1","alex","anthony","anthopoulos","bridich","cards","cdt","chris","daniels","derek","edwin","eric","greg","heyman","jake","jerry","jon","jose","josh","kendrys","kenley","kinzer","lance","lerner","marcell","mark","mike","mlbtr","nightengale","patrick","retweet","rhp","robert","steve","todd","twitter",'cashman','dombrowski','nats','dave','aroldis','gerrit','george','yu','manny','dipoto','boone','kubatko','phils','victor','bucs','billy','halos','masnsports','dan','south','van','wagenen','zaidi','luhnow','slusser','la','russa','preller','mookie','matt','maddon','kapler','justin','jeter','jeff','john','grant','giancarlo','epstein','duquette','cherington','callaway','bryce','bloom','','andrew','aaron','adams','alderson','baker','zach')&word%!in%(articles_token_record%>%select(team)%>%distinct()%>%mutate(team=tolower(team))%>%unnest_tokens(bits,team))$bits&word%!in%tolower(mention_player_team$Word))%>%group_by(team.x,player1,team.y,year,word)%>%summarise(n=n())%>%group_by(word)%>%filter(max(n)>5)%>%ungroup()%>%spread(word,n)
rumors_wsign_vocab<-rumors_wsign_vocab%>%select(-team.y)
rumors_wsign_vocab[is.na(rumors_wsign_vocab)]<-0
rumors_wsign_vocab_join<-rumors_wsign_vocab%>%left_join(rumors_wsign_sum,by=c('team.x','year','player1'='player'))

rumors_wsign_vocab_join<-rumors_wsign_vocab_join%>%select('player1','team.x',names(signing_model$data)[(names(signing_model$data))%!in%c('max','sports')])
rumors_wsign_vocab_join$max<-0
rumors_wsign_vocab_join$sports<-0

rumors_wsign_vocab_pred<-rumors_wsign_vocab_join%>%cbind.data.frame(predict=predict.glm(signing_model,rumors_wsign_vocab_join%>%select(-c(player1,team.x)),type='response'))
rumors_wsign_vocab_pred<-rumors_wsign_vocab_pred%>%group_by(player1)%>%mutate(pred=ifelse(predict==max(predict),1,0),fpred=ifelse((sum(pred)>1&unique(team.x)[1]==team.x),1,ifelse(sum(pred)==1,pred,0)))
