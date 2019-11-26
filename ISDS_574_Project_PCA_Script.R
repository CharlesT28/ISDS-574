
rm(list=ls()); gc()
setwd('e:/MSIT/ISDS_574/Project')

dat = read.csv('no outliers - listings.csv', head=T, stringsAsFactors=F, na.strings='')
dim(dat) # I usually check it to make sure the data were imported correctly
colnames(dat)

datcol=c('minimum_nights','number_of_reviews','reviews_per_month','calculated_host_listings_count','availability_365')


## 2. PCA with normalization ##
##obj2 = prcomp( na.omit(dat[, 4:ncol(dat)]), scale=T) # PCA on all numeric variables
obj2 = prcomp( na.omit(dat[, datcol]), scale=T) #PCA on continuous variables
# We put na.omit outside dat since there are missing values in it.

dat[, datcol] %>% is.na() %>% sum() # 4 missing cells

sum(is.na(dat[,datcol]))

obj2$rotation # loading matrix
obj2$x        # principal component score matrix

summary(obj2)