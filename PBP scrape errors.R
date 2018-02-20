#helper file to rapm scrape function.R to handle errors. Most/all errors are from players in OT who didn't record a stat or sub in/out
if(id=="0021700266"){
  nba[nba$Quarter==3& nba$Time>=9.45000000, box$PLAYER_ID[box$PLAYER_NAME=="Julius Randle"]]<-0
}
if(id=="0021700642"){
  nba[nba$Quarter==3& nba$Time>=7.933333, box$PLAYER_ID[box$PLAYER_NAME=="Quincy Acy"]]<-0
}
if(id=="0021700377"){
  nba[nba$Quarter==3& nba$Time>=3.050000, box$PLAYER_ID[box$PLAYER_NAME=="Jordan Clarkson"]]<-0
}
if(id=="0021700584"){
  nba[nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Dante Cunningham"]]<-1
}
if(id=="0021700615"){
  nba[nba$Quarter==3 & nba$Time>=1.133333, box$PLAYER_ID[box$PLAYER_NAME=="Pascal Siakam"]]<-0
}

if(id=="0021700346"){
  nba[nba$Quarter==3& nba$EVENTNUM%in% c(494, 496, 499, 501:506, 508:510), box$PLAYER_ID[box$PLAYER_NAME=="Omri Casspi"]]<-(-1)
}
if(id=="0021700375"){
  nba[nba$Quarter==2& nba$EVENTNUM%in% c(243, 245, 247:259, 251:253, 256), box$PLAYER_ID[box$PLAYER_NAME=="Marcin Gortat"]]<-0
  nba[nba$Quarter==2& nba$EVENTNUM%in% c(257, 259), box$PLAYER_ID[box$PLAYER_NAME=="Marcin Gortat"]]<-(-1)
  nba[nba$Quarter==2& nba$EVENTNUM<=240, box$PLAYER_ID[box$PLAYER_NAME=="Marcin Gortat"]]<-0
  nba[nba$Quarter==4& nba$EVENTNUM<=611, box$PLAYER_ID[box$PLAYER_NAME=="Marcin Gortat"]]<-0

    nba[nba$Quarter==2& nba$EVENTNUM<=240, box$PLAYER_ID[box$PLAYER_NAME=="DeAndre Jordan"]]<-0
}

if(id=="0041500134"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Jae Crowder"]]<-1
}
if(id=="0021500721"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="JR Smith"]]<-(-1)
}
if(id=="0021700346"){
  nba[nba$Quarter==3& nba$EVENTNUM<=513, box$PLAYER_ID[box$PLAYER_NAME=="Omri Casspi"]]<-0
}

if(id=="0021500587"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Hollis Thompson"]]<-(1)
}
if(id=="0021500025"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Kentavious Caldwell-Pope"]]<-(1)
}

if(id=="0021500523"){
  nba[nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Wesley Matthews"]]<-(1)
}

if(id=="0021500624"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Paul Pierce"]]<-(1)
}
if(id=="0021500359"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Marcus Morris"]]<-(1)
}
if(id=="0021500515"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Goran Dragic"]]<-(1)
}
if(id=="0021501197"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Tyler Ennis"]]<-(-1)
}
if(id=="0021500166"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Zach LaVine"]]<-(1)
}
if(id=="0021500674"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Rudy Gay"]]<-(1)
}
if(id=="0021400071"){
  nba[nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Gary Neal"]]<-(1)
}
if(id=="0021400665"){
  nba[nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Gerald Wallace"]]<-0
  nba[nba$Quarter==2& nba$EVENTNUM>=280, box$PLAYER_ID[box$PLAYER_NAME=="Gerald Wallace"]]<-(-1)
  nba[nba$Quarter==2& nba$EVENTNUM>=280, box$PLAYER_ID[box$PLAYER_NAME=="James Young"]]<-0
  
}
if(id=="0021400302"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Bradley Beal"]]<-(1)
}
if(id=="0021400947"){
  nba[nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Dante Cunningham"]]<-(-1)
}
if(id=="0021400257"){
  nba[nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Mike Dunleavy"]]<-(1)
}
if(id=="0021400762"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Nicolas Batum"]]<-(-1)
}
if(id=="0021400436"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Courtney Lee"]]<-(1)
}
if(id=="0021401022"){
  nba[nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Bojan Bogdanovic"]]<-(1)
}
if(id=="0021400004"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Jared Dudley"]]<-(-1)
}
if(id=="0021400916"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="PJ Tucker"]]<-(-1)
}
if(id=="0021400950"){
  nba[nba$Quarter==4& nba$EVENTNUM<=376, box$PLAYER_ID[box$PLAYER_NAME=="Joe Ingles"]]<-(1)
}
if(id=="0021400855"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Brandon Knight"]]<-(1)
}
if(id=="0021400424"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Steve Blake"]]<-(-1)
}
if(id=="0041400131"){
  nba[nba$Quarter==4& nba$EVENTNUM<=500, box$PLAYER_ID[box$PLAYER_NAME=="Terrence Ross"]]<-0
  nba[nba$Quarter==4 & nba$EVENTNUM>=501, box$PLAYER_ID[box$PLAYER_NAME=="Amir Johnson"]]<-0
}
if(id=="0021400904"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Kyle Singler"]]<-(1)
}
if(id=="0021300423"){
  nba[nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Jarrett Jack"]]<-(1)
}
if(id=="0021300138"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Matthew Dellavedova"]]<-(-1)
}
if(id=="0021301052"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Austin Rivers"]]<-(1)
}
if(id=="0021300257"){
  nba[nba$Quarter==7, box$PLAYER_ID[box$PLAYER_NAME=="Kirk Hinrich"]]<-(1)
}
if(id=="0021300613"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Jimmy Butler"]]<-(1)
}
if(id=="0021300566"){
  nba[nba$Quarter==7 & nba$EVENTNUM<=670, box$PLAYER_ID[box$PLAYER_NAME=="E'Twaun Moore"]]<-0
  nba[nba$Quarter==7 & nba$EVENTNUM>670, box$PLAYER_ID[box$PLAYER_NAME=="Ronnie Price"]]<-0
  
}
if(id=="0021301014"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Robbie Hummel"]]<-(-1)
}
if(id=="0041300174"){
  nba[nba$Quarter==5 & nba$EVENTNUM<=611, box$PLAYER_ID[box$PLAYER_NAME=="Dorell Wright"]]<-(1)
}
if(id=="0041300173"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Mo Williams"]]<-(1)
}
if(id=="0021300406"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Ricky Rubio"]]<-(1)
}
if(id=="0021300240"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Darren Collison"]]<-(1)
}
if(id=="0021300375"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Giannis Antetokounmpo"]]<-(1)
}
if(id=="0021301061"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Gerald Henderson"]]<-(1)
}
if(id=="0021300816"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="E'Twaun Moore"]]<-(1)
}
if(id=="0021300864"){
  nba[nba$Quarter==7, box$PLAYER_ID[box$PLAYER_NAME=="John Salmons"]]<-(1)
}
if(id=="0021300371"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Ramon Sessions"]]<-(1)
}
if(id=="0021600480"){
  nba[nba$Quarter==4& nba$EVENTNUM%in%400:454, box$PLAYER_ID[box$PLAYER_NAME=="Doug McDermott"]]<-(1)
}
if(id=="0021600648"){
  nba[nba$Quarter==4 & nba$EVENTNUM<=571 , box$PLAYER_ID[box$PLAYER_NAME=="Salah Mejri"]]<-(0)
  nba[nba$Quarter==4 & nba$EVENTNUM>571 , box$PLAYER_ID[box$PLAYER_NAME=="Seth Curry"]]<-(0)
}
if(id=="0021600726"){
  nba[ nba$EVENTNUM%in%129:146, box$PLAYER_ID[box$PLAYER_NAME=="Corey Brewer"]]<-(1)
}
if(id=="0021600931"){
  nba[ nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Rodney Hood"]]<-(1)
}
if(id=="0021600253"){
  nba[ nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Garrett Temple"]]<-(-1)
}
if(id=="0021600973"){
  nba[ nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Alex Abrines"]]<-(0)
}
if(id=="0021201040"){
  nba[ nba$Quarter==1 & nba$EVENTNUM<112, box$PLAYER_ID[box$PLAYER_NAME=="JJ Redick"]]<-(0)
  nba[ nba$Quarter==1 & nba$EVENTNUM>=112, box$PLAYER_ID[box$PLAYER_NAME=="Monta Ellis"]]<-(0)
  
}
if(id=="0021200161"){
  nba[nba$EVENTNUM%in% 345:364, box$PLAYER_ID[box$PLAYER_NAME=="Jeff Green"]]<-(1)
  nba[nba$EVENTNUM%in% 392, box$PLAYER_ID[box$PLAYER_NAME=="Jeff Green"]]<-(1)
}
if(id=="0021200642"){
  nba[ nba$Quarter==4 & nba$EVENTNUM<523, box$PLAYER_ID[box$PLAYER_NAME=="Alonzo Gee"]]<-(0)
  nba[ nba$Quarter==4 & nba$EVENTNUM>=523, box$PLAYER_ID[box$PLAYER_NAME=="Kyrie Irving"]]<-(0)
}
if(id=="0021200488"){
  nba[ nba$EVENTNUM%in% 436:457, box$PLAYER_ID[box$PLAYER_NAME=="Jodie Meeks"]]<-(-1)
}
if(id=="0041200216"){
  nba[ nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Lance Stephenson"]]<-(1)
}
if(id=="0021200184"){
  nba[ nba$Quarter==5 & nba$EVENTNUM<609, box$PLAYER_ID[box$PLAYER_NAME=="Reggie Williams"]]<-(0)
  nba[ nba$Quarter==5 & nba$EVENTNUM>=609, box$PLAYER_ID[box$PLAYER_NAME=="Jeffery Taylor"]]<-(0)
}
if(id=="0021100265"){
  nba[ nba$EVENTNUM%in% 206:219, box$PLAYER_ID[box$PLAYER_NAME=="Kawhi Leonard"]]<-(1)
}
if(id=="0021100036"){
  nba[ nba$EVENTNUM%in% 396:437, box$PLAYER_ID[box$PLAYER_NAME=="Travis Outlaw"]]<-(1)
}
if(id=="0021100420"){
  nba[nba$Quarter==4& !nba$EVENTNUM%in% c(557, 558, 560), box$PLAYER_ID[box$PLAYER_NAME=="Rasual Butler"]]<-(0)
  nba[nba$Quarter==4& nba$EVENTNUM%in% c(557, 558, 560), box$PLAYER_ID[box$PLAYER_NAME=="Linas Kleiza"]]<-(0)
  
}
if(id=="0021100842"){
  nba[ nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Bismack Biyombo"]]<-(1)
}
if(id=="0041000162"){
  nba[ !nba$EVENTNUM%in% 267:268, box$PLAYER_ID[box$PLAYER_NAME=="Armon Johnson"]]<-(0)
  nba[ nba$EVENTNUM%in% 267:268, box$PLAYER_ID[box$PLAYER_NAME=="Nicolas Batum"]]<-(0)
  
}
if(id=="0021000126"){
  nba[ !nba$EVENTNUM%in% 387:389 & nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Tony Battie"]]<-(0)
  nba[ nba$EVENTNUM%in% 387:389& nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Andre Iguodala"]]<-(0)
  
}
if(id=="0021000975"){
  nba[nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Quentin Richardson"]]<-(1)
  nba[ nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Brandon Bass"]]<-(0)
  
}
if(id=="0021000103"){
  nba[ !nba$EVENTNUM%in% 661:663 & nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Eddie House"]]<-(0)
  nba[ nba$EVENTNUM%in% 661:663 & nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Udonis Haslem"]]<-(0)
  
}
if(id=="0021000525"){
  nba[ !nba$EVENTNUM%in% 558:560 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="James Posey"]]<-(0)
  nba[ nba$EVENTNUM%in% 558:560 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Roy Hibbert"]]<-(0)
  
}
if(id=="0021001078"){
  nba[ !nba$EVENTNUM%in% 441:444 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Steve Novak"]]<-(0)
  nba[ nba$EVENTNUM%in% 441:444 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="George Hill"]]<-(0)
  
}
if(id=="0021000189"){
  nba[ nba$Quarter==2 & nba$EVENTNUM%in% 107:128, box$PLAYER_ID[box$PLAYER_NAME=="Ronnie Price"]]<-(-1)
}
if(id=="0020900869"){
  nba[ nba$Quarter==3 , box$PLAYER_ID[box$PLAYER_NAME=="Metta World Peace"]]<-(1)
}
if(id=="0021000095"){
  nba[ !nba$EVENTNUM%in% 125:127 & nba$Quarter==1, box$PLAYER_ID[box$PLAYER_NAME=="Tiago Splitter"]]<-(0)
  nba[ nba$EVENTNUM%in% 125:127 & nba$Quarter==1, box$PLAYER_ID[box$PLAYER_NAME=="George Hill"]]<-(0)
  
}
if(id=="0020901017"){
  nba[ nba$EVENTNUM%in% 394:395 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="David West"]]<-(1)
  nba[ nba$EVENTNUM%in% 394:395 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Emeka Okafor"]]<-(0)
  nba[ nba$EVENTNUM<394 & nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Emeka Okafor"]]<-(1)
  
}
if(id=="0021000095"){
  nba[ !nba$EVENTNUM%in% 391:392 & nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Ryan Anderson"]]<-(0)
  nba[ nba$EVENTNUM%in% 391:392 & nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Rashard Lewis"]]<-(0)
  
}
if(id=="0020800512"){
  nba[ !nba$EVENTNUM%in% 291:296 & nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Devean George"]]<-(0)
  nba[ nba$EVENTNUM%in% 291:296 & nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Jason Terry"]]<-(0)
  
}
if(id=="0020800766"){
  nba[ !nba$EVENTNUM%in% 358:360 & nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="JR Smith"]]<-(0)
  nba[ nba$EVENTNUM%in% 358:360 & nba$Quarter==3, box$PLAYER_ID[box$PLAYER_NAME=="Carmelo Anthony"]]<-(0)
  
}
if(id=="0020801223"){
  nba[  nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Arron Afflalo"]]<-(-1)
  nba[  nba$Quarter==2, box$PLAYER_ID[box$PLAYER_NAME=="Mario Chalmers"]]<-(0)
  
}
if(id=="0020801075"){
  nba[  nba$Quarter==3 &!nba$EVENTNUM%in% 370:372, box$PLAYER_ID[box$PLAYER_NAME=="Patrick O'Bryant"]]<-(0)
  nba[  nba$Quarter==3&  nba$EVENTNUM%in% 370:372, box$PLAYER_ID[box$PLAYER_NAME=="Chris Bosh"]]<-(0)
  
}
if(id=="0020801075"){
  nba[  nba$Quarter==4 &!nba$EVENTNUM%in% 450:453, box$PLAYER_ID[box$PLAYER_NAME=="Dikembe Mutombo"]]<-(0)
  nba[  nba$Quarter==4&  nba$EVENTNUM%in% 450:453, box$PLAYER_ID[box$PLAYER_NAME=="Carl Landry"]]<-(0)
  
}
if(id=="0020700226"){
  nba[  nba$Quarter==3 &!nba$EVENTNUM%in% c(351, 353, 381) , box$PLAYER_ID[box$PLAYER_NAME=="Royal Ivey"]]<-(0)
  nba[  nba$Quarter==3&  nba$EVENTNUM%in% c(351, 353, 381), box$PLAYER_ID[box$PLAYER_NAME=="Michael Redd"]]<-(0)
  
}
if(id=="0020801223"){
  nba[  nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Jason Kapono"]]<-(1)
  
}
if(id=="0020601040"){
  nba[  nba$Quarter==4, box$PLAYER_ID[box$PLAYER_NAME=="Mark Blount"]]<-(1)
  
}
if(id=="0020600999"){
  nba[  nba$Quarter==4 & nba$EVENTNUM<=383, box$PLAYER_ID[box$PLAYER_NAME=="Stephon Marbury"]]<-(1)
  
}
if(id=="0020600926"){
  nba[  nba$Quarter==3 &!nba$EVENTNUM%in% 368:371, box$PLAYER_ID[box$PLAYER_NAME=="Chris Kaman"]]<-(0)
  nba[  nba$Quarter==3&  nba$EVENTNUM%in% 368:371, box$PLAYER_ID[box$PLAYER_NAME=="Corey Maggette"]]<-(0)
  
}
if(id=="0020600887"){
  nba[  nba$Quarter==5, box$PLAYER_ID[box$PLAYER_NAME=="Rashad McCants"]]<-(-1)
  nba[  nba$Quarter==6, box$PLAYER_ID[box$PLAYER_NAME=="Craig Smith"]]<-(-1)
  
}
if(id=="0020600431"){
  nba[  nba$Quarter%in% 5:6, box$PLAYER_ID[box$PLAYER_NAME=="Derek Anderson"]]<-(1)
}
if(id=="0020600340"){
  nba[  nba$Quarter%in% 5, box$PLAYER_ID[box$PLAYER_NAME=="Luke Walton"]]<-(1)
  nba[  nba$Quarter%in% 6, box$PLAYER_ID[box$PLAYER_NAME=="Shane Battier"]]<-(-1)
  
}
if(id=="0020600244"){
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM<=389, box$PLAYER_ID[box$PLAYER_NAME=="Kevin Ollie"]]<-(1)
  
}
if(id=="0020600140"){
  nba[  nba$Quarter%in%5, box$PLAYER_ID[box$PLAYER_NAME=="Corey Maggette"]]<-(1)
  
}
if(id=="0020501025"){
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM<=314, box$PLAYER_ID[box$PLAYER_NAME=="Nazr Mohammed"]]<-(-1)
  
}
if(id=="0020500579"){
  nba[  nba$Quarter==1 &!nba$EVENTNUM%in% 102:104, box$PLAYER_ID[box$PLAYER_NAME=="Brian Scalabrine"]]<-(0)
  nba[  nba$Quarter==1&  nba$EVENTNUM%in% 102:104, box$PLAYER_ID[box$PLAYER_NAME=="Kendrick Perkins"]]<-(0)
  
}
if(id=="0020500433"){
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM<=346, box$PLAYER_ID[box$PLAYER_NAME=="Chris Kaman"]]<-(1)
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM%in%347:348, box$PLAYER_ID[box$PLAYER_NAME=="Walter McCarty"]]<-(1)
  
}
if(id=="0020500188"){
  nba[  nba$Quarter%in% 5 & nba$EVENTNUM<=500 , box$PLAYER_ID[box$PLAYER_NAME=="Nick Van Exel"]]<-(-1)
  
}
if(id=="0020500240"){
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM<=325 , box$PLAYER_ID[box$PLAYER_NAME=="Tayshaun Prince"]]<-(-1)
}
if(id=="0020500124"){
  nba[  nba$Quarter%in% 4 & nba$EVENTNUM<=445 , box$PLAYER_ID[box$PLAYER_NAME=="Joe Johnson"]]<-(-1)
}
if(id=="0020500038"){
  nba[  nba$Quarter%in% 3  & nba$Time>=1, box$PLAYER_ID[box$PLAYER_NAME=="Jon Barry"]]<-(0)
}
