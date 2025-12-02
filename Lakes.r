setwd() #Specify Working Directory
library(effsize) # Effsize library
###
#Geai Data
geai <- read.csv('Geai.csv', header = TRUE)
names(geai) <- c('choronamid.1', 'copepod.1','cladoceran.1', 'cyclopoid.2', 'calanoid.2','holopedium.2', 'choronamid.2','daphnia.2','bosmina.2')
geai$choronamid.1[1] <- 15556
geai.2 <- geai[,c(4:6,8:9)]
###

###
#Croche Data
croche <- (read.csv('Croche.csv', header = TRUE))
names(croche) <- c('copepod.1','cladoceran.1', 'calanoid.2', 'cyclopoids.2', 'daphnia.2', 'bosmina.2')
croche.2 <- croche[,3:6]
###

###
#Day 1 Histogram - Copepods
copepod.vector <- c(croche$copepod.1, geai$copepod.1)
copepod.bins <- nclass.Sturges(copepod.vector)
copepod.range <- range(copepod.vector, na.rm = TRUE)
copepod.breaks <- seq((copepod.range[1]) - 1, (copepod.range[2]) + 1, length.out = copepod.bins + 1)

hist(croche$copepod.1, col = '#5a5a5aa9',
        breaks = copepod.breaks, ylim = c(0,70), border = 'black',
        main = 'Copepod Count Distribution',
        xlab = 'Size(μm)', ylab = 'Count', xlim = c(0,3000)) #Croche Day 1 - Copepods
hist(geai$copepod.1, col = '#fff2009d', add = TRUE, 
        breaks = copepod.breaks, ylim = c(0,70), border = 'black')   #Geai Day 1 - Copepods

legend('topright',
        legend = c('Croche', 'Geai'),
        fill = c('#5a5a5aa9', '#fff2009d'),
        )
###

###
#Day 1 Histogram - Cladocerans
cladoceran.vector <- c(croche$cladoceran.1, geai$cladoceran.1)
cladoceran.bins <- nclass.Sturges(cladoceran.vector)
cladoceran.range <- range(cladoceran.vector, na.rm = TRUE)
cladoceran.breaks <- seq((cladoceran.range[1]) - 1, (cladoceran.range[2]) + 1, length.out = cladoceran.bins + 1)
hist(croche$cladoceran.1, col = '#5a5a5aa9',
        breaks = cladoceran.breaks, ylim = c(0,50),
        main = 'Cladoceran Count Distribution',
        xlab = 'Size (μm)', ylab = 'Count', xlim = c(0,2000)) #Croche Day 1 - Cladocerans
hist(geai$cladoceran.1, col = '#fff2009d', add = TRUE,
        breaks = cladoceran.breaks, ylim = c(0,50))   #Geai Day 1 - Cladocerans
legend('topright',
        legend = c('Croche', 'Geai'),
        fill = c('#5a5a5aa9', '#fff2009d'),
        )

###

###
#Boxplots
croche.2 <- croche.2[c('cyclopoids.2', 'calanoid.2', 'daphnia.2', 'bosmina.2')]
geai.2 <- geai.2[c('cyclopoid.2', 'calanoid.2', 'daphnia.2','bosmina.2', 'holopedium.2')]
bp.geai <- boxplot(geai.2, plot = FALSE)
bp.croche <- boxplot(croche.2, plot = FALSE)


boxplot(geai.2,
        main = 'Zooplankton Size Distribution', xlab = 'Size (μm)', yaxt = 'n',
        col = '#fff2009d', xlim = c(0,13), at = c(0:4 * 3 + .5),
        ylim = range(geai.2, croche.2, na.rm = TRUE), horizontal = TRUE
        )

boxplot(croche.2, at = c(0:3 * 3 + 1.5), yaxt = 'n',
        col = '#5a5a5aa9', add = TRUE, horizontal = TRUE)

axis(2, at = c(0:4 * 3 + 1), labels = c('Cyc.', 'Cal.', 'Dap.', 'Bos.', 'Hol.'), tick = TRUE, las = 1)
legend('topright',
        legend = c('Croche', 'Geai'),
        fill = c('#5a5a5aa9', '#fff2009d'))

x.coord.n.bp <- c(bp.geai$stats[3,], bp.croche$stats[3,])
x.coord.n.bp[5] <- x.coord.n.bp[5] - 100 # Holopedium adjustment
x.coord.n.bp[9] <- x.coord.n.bp[9] + 2000 # Bosmina Croche adjustment
x.coord.n.bp[4] <- x.coord.n.bp[4] + 400 # Bosmina Croche adjustment
x.coord.n.bp[8] <- x.coord.n.bp[8] + 200 # Daphnia Croche adjustment
x.coord.n.bp[3] <- x.coord.n.bp[3] + 200 # Daphnia Geai adjustment
x.coord.n.bp[2] <- x.coord.n.bp[2] - 200 # Calanoid Geai adjustment
x.coord.n.bp[7] <- x.coord.n.bp[7] + 550 # Calanoid Croche adjustment
x.coord.n.bp[6]<- 1000 # Cyclopoid adjustment
x.coord.n.bp[1] <- 1000 # Cyclopoid adjustment
labels.n.bp <- c(bp.geai$n, bp.croche$n)
bp.colours <- c('black', 'black', 'black', 'black', 'black',
                'black', 'black', '#ffffff', 'black')
positions.y.n.bp <- c(c(0:4 * 3 + .5),c(0:3 * 3 + 1.5))
text(x = x.coord.n.bp , y = positions.y.n.bp ,
        labels = c(paste0('n = ',labels.n.bp)), col = bp.colours)

###

###
#Pooled - Geai

geai.pooled <- unlist(geai[c(2:6,8:9)], use.names = FALSE)
geai.pooled <- geai.pooled[!is.na(geai.pooled)]
hist(geai.pooled, main = 'Geai Pooled Data', xlab = 'Size (μm)', ylab = 'Count', ylim = c(0,70), xlim = c(0,3000))
#Pooled - Croche
croche.pooled <- unlist(croche, use.names = FALSE)
croche.pooled <- croche.pooled[!is.na(croche.pooled)]
hist(croche.pooled, main = 'Croche Pooled Data', xlab = 'Size (μm)', ylab = 'Count', ylim = c(0,250), xlim = c(0,2500))

###

###
#Pooled Histograms

pooled.vector <- c(croche.pooled, geai.pooled)
pooled.bins <- nclass.Sturges(pooled.vector)
pooled.range <- range(pooled.vector, na.rm = TRUE)
pooled.breaks <- seq((pooled.range[1]) - 1, (pooled.range[2]) + 1, length.out = pooled.bins + 1)

hist(croche.pooled, col = '#5a5a5aa9', density = NULL, angle = 0,
     breaks = pooled.breaks, ylim = c(0,300), border = 'black',
     main = 'Pooled Data', xlim = c(0,3000), xlab = 'Size (μm)', ylab = 'Count') #Croche Pooled

hist(geai.pooled, col = '#fff2009d', density = NULL,
     breaks = pooled.breaks, ylim = c(0,150), border = 'black', add = TRUE) #Geai Pooled

legend('topright',
       legend = c('Croche', 'Geai'),
       fill = c('#5a5a5aa9', '#fff2009d')
)

###

###
# T-Tests
copepod.t <- t.test(croche$copepod.1, y = geai$copepod.1,
        alternative = "l", paired = FALSE, var.equal = FALSE)

cladoceran.t <- t.test(croche$cladoceran.1, y = geai$cladoceran.1,
        alternative = "l", paired = FALSE, var.equal = FALSE)

pooled.t <- t.test(croche.pooled, y = geai.pooled,
        alternative = "l", paired = FALSE, var.equal = FALSE)

cyc.t <- t.test(croche.2$cyclopoids.2, y = geai.2$cyclopoid.2,
        alternative = "l", paired = FALSE, var.equal = FALSE)

cal.t <- t.test(croche.2$calanoid.2, y = geai.2$calanoid.2,
        alternative = "l", paired = FALSE, var.equal = FALSE)

dap.t <- t.test(croche.2$daphnia.2, y = geai.2$daphnia.2,
        alternative = "l", paired = FALSE, var.equal = FALSE)

bos.t <- t.test(croche.2$bosmina.2, y = geai.2$bosmina.2,
        alternative = "l", paired = FALSE, var.equal = FALSE)

###
# Chaoborus Data - Ignore calls as Chironamids
choronamids <- unlist(geai[c('choronamid.1', 'choronamid.2')], use.names = FALSE)
choronamids <- choronamids[!is.na(choronamids)]
boxplot(choronamids, main = 'Chaoborus Size Distribution', xlab = 'Size (μm)', horizontal = TRUE, col = '#fff2009d')

###
# Effect Sizes
copepod.es <- cohen.d(croche$copepod.1, geai$copepod.1, hedges.correction = FALSE, na.rm = TRUE)
cladoceran.es <- cohen.d(croche$cladoceran.1, geai$cladoceran.1, hedges.correction = FALSE, na.rm = TRUE)
pooled.es <- cohen.d(croche.pooled, geai.pooled, hedges.correction = FALSE, na.rm = TRUE)
