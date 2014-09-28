nv.ssize <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/analysis/verification/nv.ssize-pop-sampled-richness.csv")
View(nv.ssize)
nv.ssize$zresid <- (nv.ssize$ssize_25 - nv.ssize$expected_25) / nv.ssize$sd_25
nv.ssize$zresid50 <- (nv.ssize$ssize_50 - nv.ssize$expected_50) / nv.ssize$sd_50
ggplot(nv.ssize, aes(x = zresid50, y = ..density..)) + geom_density()
ggplot(nv.ssize, aes(x = zresid, y = ..density..)) + geom_density()
nv.ssize.single <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/analysis/verification/nv.ssize-pop-sampled-richness-singlelocus.csv")
View(nv.ssize.single)
nv.ssize.single$zresid <- (nv.ssize.single$ssize_25 - nv.ssize.single$expected_25) / nv.ssize.single$sd_25
nv.ssize.single$zresid50 <- (nv.ssize.single$ssize_50 - nv.ssize.single$expected_50) / nv.ssize.single$sd_50
ggplot(nv.ssize.single, aes(x = zresid, y = ..density..)) + geom_density()
ggplot(nv.ssize.single, aes(x = zresid50, y = ..density..)) + geom_density()
