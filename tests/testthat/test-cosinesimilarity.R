context("Cosine similarity")

data("pcaPhoneTestData", package = "flipExampleData")

dat <- pcaPhoneTestData$data.set[,1:3]
ind <- complete.cases(dat)
dat <- dat[ind, ]
wgt <- pcaPhoneTestData$weight[ind]

expect_equal(CosineSimilarities(dat, wgt)[1, 2],
             sum(dat[, 1] * dat[, 2] * wgt) / sqrt(sum(dat[, 1] ^ 2 * wgt) * sum(dat[, 2] ^ 2 * wgt)))
