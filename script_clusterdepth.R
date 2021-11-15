# Multiple comparison procedure using permuco
#
# author: Jaromil Frossard
# contact:jaromil.frossard@gmail.com
#

install.packages("permuco")
library(permuco)

design = permuco::attentionshifting_design
signal = permuco::attentionshifting_signal

##summary of the data
# design data frame
summary(design)
dim(design)
# signal matrix
dim(signal)

# plot
ts.plot(t(signal))

## removing signal pre stimuli
signal <- signal[,as.numeric(colnames(signal))>0]
dim(signal)


# clustermass test with permuco
###########################################################
# formula similar to aov
# visibility, emotion, direction are within factors 
# 7 test, 614 times points, 4000 permutation
mod_cm <- clusterlm(signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
                 data = design)

plot(mod_cm)

mod_cm

## extracting all results for plotting
## list of data frame per effect
summary(mod_cm,table_type = "full")


## Cluster depth tests with permuco
###########################################################
mod_cd <- clusterlm(signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
                    multcomp = "clusterdepth", data = design)


plot(mod_cd)

mod_cd


summary(mod_cd,table_type = "full")


## TFCE with permuco
###########################################################
mod_tfce <- clusterlm(signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
                      multcomp = "tfce", data = design)


## troendle with permuco
###########################################################
mod_tr <- clusterlm(signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
                    multcomp = "troendle", data = design)

