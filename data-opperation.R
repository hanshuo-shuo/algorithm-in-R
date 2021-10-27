# ex3
attach(airquality)
head(airquality)

#replace NA with 0
airquality[is.na(airquality)]=0

# median of each column
apply(airquality,2,median)

#mean of ezch column
apply(airquality,2,mean)

#range and qu
summary(airquality)


####find the largst row of Ozone
Ozone
#replace NA with 0
Ozone[is.na(Ozone)]=0
Ozone
airquality[which(Ozone==Ozone[which.max(Ozone)],arr.ind=T),]      

#####upper qu is 4 so
airquality[which(Ozone>4,arr.ind=T),][,3]


