#analysis continued

boxplot(sightings$Group.Size~sightings$Activity)

aggregate(Group.Size~Activity, data = sightings, FUN = median)


sightings$Activity <- factor(sightings$Activity, levels = c("SPONGEFORAGE", 
                                                            "FORAGE",
                                                            "TRAVEL",
                                                            "REST",
                                                            "SOCIAL"))

boxplot(sightings$Group.Size~sightings$Activity)


##Group size and shark bite prevalence

ind_group_sizes <- aggregate(Group.Size~Dolphin.ID, data = sightings, FUN = median)
ind_group_sizes$sightings <- table(sightings$Dolphin.ID)[match(ind_group_sizes$Dolphin.ID, 
                                                               names(table(sightings$Dolphin.ID)))]

hist(sightings$Age, probability = TRUE, breaks = 20)
hist(shark_bites$Age.at.bite, add= TRUE, col= adjustcolor("green",alpha.f = 0.3), 
     probability = TRUE, breaks = 20)

ind_group_sizes$shark_bites <- c(table(shark_bites$Dolphin.ID)[match(ind_group_sizes$Dolphin.ID, 
                                                                 names(table(shark_bites$Dolphin.ID)))])


ind_group_sizes$shark_bites[is.na(ind_group_sizes$shark_bites)] <- 0

plot(ind_group_sizes$shark_bites,ind_group_sizes$Group.Size, ylim = c(0,15))

lm(shark_bites ~ Group.Size + sightings, 
   data = ind_group_sizes[which(ind_group_sizes$sightings >=15),]) |> summary()

ind_group_sizes$Sex <- dolphins$Sex[match(ind_group_sizes$Dolphin.ID, 
                                          dolphins$Dolphin.ID)]

ind_group_sizes <- ind_group_sizes[which(ind_group_sizes$Sex!="UNKNOWN" & 
                                         ind_group_sizes$sightings >= 15),]

mod <- lm(shark_bites ~ Group.Size + Sex + sightings, 
   data = ind_group_sizes) 

pmod <- glm(shark_bites ~ Group.Size + Sex + sightings, family = poisson,
   data = ind_group_sizes) 

summary(pmod)

plot(ind_group_sizes$Group.Size, ind_group_sizes$shark_bites, 
     col = as.factor(ind_group_sizes$Sex), pch=16)




newdata <- expand.grid(Group.Size = seq(1, 13, 1), 
                      sightings = 43, 
                      Sex = c("MALE", "FEMALE"))

newdata$predicted <- predict(pmod, newdata, type = "response")

newdata <- newdata[order(newdata$Group.Size),]

maledata <- newdata[newdata$Sex == "MALE",]
femaledata <- newdata[newdata$Sex == "FEMALE",]

plot(ind_group_sizes$Group.Size, 
     ind_group_sizes$shark_bites, 
     col = as.factor(ind_group_sizes$Sex), pch=16, 
     ylim = c(0, 2))

lines(maledata$Group.Size, maledata$predicted, col = "darkgreen", lwd = 2)

lines(femaledata$Group.Size, femaledata$predicted, col = "purple", lwd = 2)

##Need to add age into all of this! 
