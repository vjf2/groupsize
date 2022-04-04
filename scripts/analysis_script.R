# Mann Lab - Using R and Github
# Vivienne Foroughirad
# April 4th, 2022

# Read in the data (check working directory first)
# and global settings

dolphins <- read.csv("dolphins.csv")

shark_bites <- read.csv("shark_bites.csv")

sightings <- read.csv("sightings.csv")

# Preview 

head(dolphins)

head(dolphins, 10)
tail(dolphins)

table(dolphins$Sex)

View(shark_bites)

dim(sightings)

sightings[1:5, 1:3]

max(sightings$Age)
range(sightings$Age)
summary(sightings$Age)

# Combine 
head(shark_bites)

shark_bites$Sex <- dolphins$Sex[match(shark_bites$Dolphin.ID, 
                                      dolphins$Dolphin.ID)]
head(shark_bites)

# Filter 

adult_sightings <- sightings[which(sightings$Life.History.Status=="adult"),]

head(sightings)
head(adult_sightings)

# Calculate and Aggregate

shark_bites$Birth.Date <- dolphins$Birth.Date[match(shark_bites$Dolphin.ID, 
                                      dolphins$Dolphin.ID)]

shark_bites$Shark.Bite.Date <- as.Date(shark_bites$Shark.Bite.Date)
shark_bites$Birth.Date <- as.Date(shark_bites$Birth.Date)

shark_bites$Age.at.bite <- shark_bites$Shark.Bite.Date - shark_bites$Birth.Date

shark_bites$Age.at.bite <- shark_bites$Age.at.bite / 365.25 
shark_bites$Age.at.bite <- as.numeric(shark_bites$Age.at.bite)

summary(shark_bites$Age.at.bite)

aggregate(Age.at.bite~Sex, data = shark_bites, FUN = median)

# Plot

hist(shark_bites$Age.at.bite)
hist(sightings$Age)

plot(sightings$Depth, sightings$Group.Size)

boxplot(sightings$Group.Size~sightings$Activity)


# Explore data and write out a research question
# Two figures 

#R graph gallery

