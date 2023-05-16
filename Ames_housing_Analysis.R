d<-read_excel("ames.xlsx")
d<-d %>% mutate(Bathrooms = round(Bsmt.Full.Bath + Bsmt.Half.Bath * 0.5 + Full.Bath + Half.Bath * 0.5,0)) #normalising the number of bathrooms for ease if calculation
sum(is.na(d$Bathrooms))
