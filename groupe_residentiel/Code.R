# This code file, will contain all the code, that is used to retreave or treat data.
# All the models will be presented in separate .R files.
# The descriptions of the work will be stored directly inside .R files or in .md files.

# Loading packages
require(tidyverse)
getwd()

###########################
# Sitadel2 database loading
###########################
# We initialise this file by loading data from the disponible sources.
## Iterating field
i.ix = 1:12
j.ix = 2017:2018
## Auxilary df generation
df = data.frame()
## Dataframes iterative generation for 2017-2018
for (j in j.ix) {
    for (i in i.ix) {
        df.load = read.csv(sprintf("https://www.statistiques.developpement-durable.gouv.fr/sites/default/files/2019-01/logements-%d-%02d.csv", j, i))
        df = merge(df, df.load, all=TRUE)
    }
}
save = df
## Dataframes iterative generation for 2019
df = save
for (i in 1:8) {
    df.load = read.csv(sprintf("https://www.statistiques.developpement-durable.gouv.fr/sites/default/files/2019-%02d/logements-2019-%02d.csv", i+1, i))
    df = merge(df, df.load, all=TRUE)
}
## Verification
dim(df)
## Saving resulting dataframe
write.csv(df, file = "./data/raw_data/logements.csv")


#########################
# Sitadel2 database study
#########################
# First analysis steps
require(tidyverse)
## Loading data
df = read.csv("./data/raw_data/logements.csv")
## Variables listing
names(df)
## Viewing date
head(df$DATEREELLE_DECISION_FAV)
## Date transormation
df.m = df %>% 
    mutate(MONTH = str_sub(DATEREELLE_DECISION_FAV, 6, 7),
        YEAR = str_sub(DATEREELLE_DECISION_FAV, 1, 4))

# Terrain superficie study
## Groupping
df.g = df.m %>% 
    group_by(YEAR) %>%
    summarise(mean = mean(SUPERFICIE_T))
## Plotting
df.g %>% ggplot(aes(YEAR, mean)) + geom_point() # observing outlier for 2013
## Regroupping
df.g.m = df.m %>% 
    group_by(YEAR, MONTH) %>%
    summarise(mean = mean(SUPERFICIE_T))
df.g.m %>% filter(YEAR == 2011) # abberating value at 09
## Centerd view on 2013-09
df.m %>% 
    filter((YEAR == 2013) & (MONTH == "09")) %>% 
    select(SUPERFICIE_T) # 2nd entry is suspicious
df.m %>% 
    filter((YEAR == 2013) & (MONTH == "09")) %>%
    filter(SUPERFICIE_T == 925088) %>% 
    View() # HALCYON RETREAT SAS X = 32635
## Reanalysis
# dim(df.m)
df.m = df.m %>% filter(X != 32635) %>% filter(YEAR > 201)
## Reechantillonage
df.g = df.m %>% 
    group_by(YEAR) %>%
    summarise(mean = mean(SUPERFICIE_T))
df.g.m = df.m %>% 
    group_by(YEAR, MONTH) %>%
    summarise(mean = mean(SUPERFICIE_T))
## Replotting 
df.g %>% ggplot(aes(YEAR, mean)) + geom_point()
df.g.m %>% ggplot(aes(MONTH, mean, group = YEAR, col = YEAR)) + 
    geom_point() + 
    geom_path()
df.g.m %>% ggplot(aes(MONTH, mean, group = YEAR, col = YEAR)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F)

# Autorisation base creation
df = read.csv("./data/raw_data/logements.csv")
## Date separation
df.m = df %>% 
    mutate(MONTH = str_sub(DATEREELLE_DECISION_FAV, 6, 7),
        YEAR = str_sub(DATEREELLE_DECISION_FAV, 1, 4))
# 
df.aut = df.m %>% 
    filter(TYPE_EVT == 2 & # autorisations
        TYPE_OPERATION_CONSTR == c(5:7) # logement
        ) %>% 
    select(YEAR, MONTH, # date decision favorable
        SDP_B1, SDP_D1) # espace crée
## By year
df.gaut = df.aut %>% 
    filter(YEAR > 2012) %>%
    group_by(YEAR) %>%
    summarise(M_B1 = mean(SDP_B1), M_D1 = mean(SDP_D1))
df.gaut %>% ggplot(aes(YEAR, M_B1)) + 
    geom_point() + geom_path(group = 1)
## By month
df.gaut.m = df.aut %>% 
    filter(YEAR > 2015) %>%
    group_by(YEAR, MONTH) %>%
    summarise(M_B1 = mean(SDP_B1))
df.gaut.m %>% ggplot(aes(MONTH, M_B1, group = YEAR, col = YEAR)) + 
    geom_point() + geom_path()
df.gaut.m %>% ggplot(aes(MONTH, M_B1, group = YEAR, col = YEAR)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F) +
    geom_path()

###################################
# Other databases loading and study
###################################
# Loading packages
require(tidyverse)
### Loading files ###
# Read filenames
files = list.files(path = "./data/raw_data/", pattern = ".tsv")

##### 1 #####
# Read file
bsbu = read.table(paste0("./data/raw_data/", files[1]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
bsbut = as.data.frame(t(bsbu[c(1,grep(",FR", bsbu[,1])),]), 
    stringsAsFactors = FALSE)
# Rename
nam = paste(str_split_fixed(bsbut[1,], ",", 3)[,1],
    str_split_fixed(bsbut[1,], ",", 3)[,2], 
    sep = "_")
names(bsbut) = nam
# Create double index time system and recreate data
bsbut = bsbut[-1,] %>% 
    mutate(YEAR = str_split_fixed(bsbut[-1,1], "M", 2)[,1],
        MONTH = str_split_fixed(bsbut[-1,1], "M", 2)[,2]) %>% 
    select(-1)
View(bsbut) # verification
# Write data
write.csv(bsbut, file = "./data/raw_data/bsbu.csv")

##### 2 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
isbr = read.table(paste0("./data/raw_data/", files[2]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
isbrt = as.data.frame(t(isbr[c(1,grep(",FR", isbr[,1])),]), 
    stringsAsFactors = FALSE)
# Rename
nam = paste(str_split_fixed(isbrt[1,], ",", 4)[,1],
    str_split_fixed(isbrt[1,], ",", 4)[,2], 
    str_split_fixed(isbrt[1,], ",", 4)[,3],
    sep = "_")
names(isbrt) = nam
# Create double index time system and recreate data
isbrt = isbrt[-1,] %>% 
    mutate(YEAR = str_split_fixed(isbrt[-1,1], "M", 2)[,1],
        MONTH = str_split_fixed(isbrt[-1,1], "M", 2)[,2]) %>% 
    select(-1)
View(isbrt) # verification
# Write data
write.csv(isbrt, file = "./data/raw_data/isbr.csv")

##### 3 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
isbu = read.table(paste0("./data/raw_data/", files[3]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
isbut = as.data.frame(t(isbu[c(1,grep(",FR", isbu[,1])),]), 
    stringsAsFactors = FALSE)
# Rename
nam = paste(str_split_fixed(isbut[1,], ",", 5)[,1],
    str_split_fixed(isbut[1,], ",", 5)[,2], 
    str_split_fixed(isbut[1,], ",", 5)[,3],
    str_split_fixed(isbut[1,], ",", 5)[,4],
    sep = "_")
names(isbut) = nam
# Create double index time system and recreate data
isbut = isbut[-1,] %>% 
    mutate(YEAR = str_split_fixed(isbut[-1,1], "M", 2)[,1],
        MONTH = str_split_fixed(isbut[-1,1], "M", 2)[,2]) %>% 
    select(-1)
View(isbut) # verification
# Write data
write.csv(isbut, file = "./data/raw_data/isbu.csv")

##### 4 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
is_inv = read.table(paste0("./data/raw_data/", files[4]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
is_invt = as.data.frame(t(is_inv[c(1,grep(",FR", is_inv[,1])),]), 
    stringsAsFactors = FALSE) # no data for France !!!!!
# Rename
# nam = paste(str_split_fixed(is_invt[1,], ",", 2)[,1],
#     str_split_fixed(is_invt[1,], ",", 2)[,2], 
#     sep = "_")
# names(is_invt) = nam
# Create double index time system and recreate data
# is_invt = is_invt[-1,] %>% 
#     mutate(YEAR = is_invt[-1,1]) %>% 
#     select(-1)
# View(is_invt) # verification
# Write data
# write.csv(is_invt, file = "./data/raw_data/is_inv.csv")

##### 5 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
cobp = read.table(paste0("./data/raw_data/", files[5]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
cobpt = as.data.frame(t(cobp[c(1,grep(",FR", cobp[,1])),]), 
    stringsAsFactors = FALSE)
# Rename
nam = paste(str_split_fixed(cobpt[1,], ",", 5)[,1],
    str_split_fixed(cobpt[1,], ",", 5)[,2], 
    str_split_fixed(cobpt[1,], ",", 5)[,3],
    str_split_fixed(cobpt[1,], ",", 5)[,4],
    sep = "_")
names(cobpt) = nam
# Create double index time system and recreate data
cobpt = cobpt[-1,] %>% 
    mutate(YEAR = str_split_fixed(cobpt[-1,1], "M", 2)[,1],
        MONTH = str_split_fixed(cobpt[-1,1], "M", 2)[,2]) %>% 
    select(-1)
View(cobpt) # verification
# Write data
write.csv(cobpt, file = "./data/raw_data/cobp.csv")

##### 6 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
colb = read.table(paste0("./data/raw_data/", files[6]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
colbt = as.data.frame(t(colb[c(1,grep(",FR", colb[,1])),]), 
    stringsAsFactors = FALSE) # no French data
# Rename
# nam = paste(str_split_fixed(colbt[1,], ",", 5)[,1],
#     str_split_fixed(colbt[1,], ",", 5)[,2], 
#     str_split_fixed(colbt[1,], ",", 5)[,3],
#     str_split_fixed(colbt[1,], ",", 5)[,4],
#     sep = "_")
# names(colbt) = nam
# Create double index time system and recreate data
# colbt = colbt[-1,] %>% 
#     mutate(YEAR = str_split_fixed(colbt[-1,1], "M", 2)[,1],
#         MONTH = str_split_fixed(colbt[-1,1], "M", 2)[,2]) %>% 
#     select(-1)
# View(colbt) # verification
# Write data
# write.csv(colbt, file = "./data/raw_data/colb.csv")

##### 7 #####
# Cleaning
list = ls()
list = setdiff(list, c("files"))
rm(list)
# Read file
copi = read.table(paste0("./data/raw_data/", files[7]), 
    sep = "\t", stringsAsFactors = FALSE)
# Transpose, select France
copit = as.data.frame(t(copi[c(1,grep(",FR", copi[,1])),]), 
    stringsAsFactors = FALSE) # no French data !!!
# Rename
# nam = paste(str_split_fixed(copit[1,], ",", 5)[,1],
#     str_split_fixed(copit[1,], ",", 5)[,2], 
#     str_split_fixed(copit[1,], ",", 5)[,3],
#     str_split_fixed(copit[1,], ",", 5)[,4],
#     sep = "_")
# names(copit) = nam
# Create double index time system and recreate data
# copit = copit[-1,] %>% 
#     mutate(YEAR = str_split_fixed(copit[-1,1], "M", 2)[,1],
#         MONTH = str_split_fixed(copit[-1,1], "M", 2)[,2]) %>% 
#     select(-1)
# View(copit) # verification
# Write data
# write.csv(copit, file = "./data/raw_data/copi.csv")

##################
# Regroupping data
##################
list = ls()
rm(list)
# Charging packages
require(tidyverse)
# Loading data
cobp = read.csv("./data/raw_data/cobp.csv")
isbu = read.csv("./data/raw_data/isbu.csv")
isbr = read.csv("./data/raw_data/isbr.csv")
bsbu = read.csv("./data/raw_data/bsbu.csv")
# Concatenating data
data = full_join(
    isbu[,-1], 
    isbr[,-1], 
    by = c("YEAR", "MONTH"))
dim(data)
data = full_join(
    data,
    bsbu[,-1],
    by = c("YEAR", "MONTH"))
dim(data)
data = full_join(
    data,
    cobp[,-1],
    by = c("YEAR", "MONTH"))
corm = cor(data)
# Class check
for (i in 1:ncol(data)) {
    cat(names(data)[i], " : ", class(data[,i]), "\n")
}
# Saving data
write.csv(data, file = "./data/derived_data/full_data_eus.csv")

################
# Modifying data
################
# Loading data
data = read.csv("./data/derived_data/full_data_eus.csv", 
    stringsAsFactors = F)
# Class check
for (i in 1:ncol(data)) {
    cat(names(data)[i], " : ", class(data[,i]), "\n")
}
# Add NA instead of : (done?)
data.n[] = lapply(data, gsub, 
    pattern = ":", 
    replacement = NA)
# Remove all characters
data.c[] = lapply(data.n, gsub, 
    pattern = "[[:alpha:]]|[[:space:]]", 
    replacement = "")
data.f = lapply(data.c, as.numeric) %>%
    data.frame()
# Class check
for (i in 1:ncol(data.f)) {
    cat(names(data.f)[i], " : ", class(data.f[,i]), "\n")
}
dim(data)
dim(data.f)
# Writting data
write.csv(data.f, file = "./data/derived_data/full_data_allnum.csv")

###########################
# Data study and recreation
###########################
# Packages
require(tidyverse)
require(NMF)
# Load data
data = read.csv("./data/derived_data/full_data_allnum.csv")
# Heatmap
data.x = data %>%
    filter(YEAR >= 2004) %>%
    na.omit()
cord = cor(as.matrix(data.x))
aheatmap(cord)
# summary(data.x)
# Important data selection for further analysis 
# NSA
data.ns = data %>%
    select(YEAR, MONTH, contains("NSA"))
dim(data.ns)
write.csv(data.ns, file = "./data/derived_data/full_nsa.csv")
# SCA
data.ca = data %>%
    select(-c(X.1, X, contains("NSA")))
dim(data.ca)
write.csv(data.ca, file = "./data/derived_data/full_sca.csv")
# Names extractions
for (i in 1:ncol(data)) {
    cat(names(data)[i], "\n")
}

######################
# Time series analysis
######################
# Packages
require(tidyverse)
require(ggfortify)
require(tseries)
require(forecast)
# Loading data
data = read.csv("./data/derived_data/full_nsa.csv")
names(data)
# First, we will study all residential sector (F_CC11)
## PSQM_F_CC11_NSA_I15 - index
# # Creating time series
# data %>% 
#     select(YEAR, MONTH, PSQM_F_CC11_NSA_I15) %>%
#     View()
# datas = data$PSQM_F_CC11_NSA_I15 %>%
#     na.omit()
# serie = ts(datas, start = 1994, frequency = 12)
# # Serie study
# plot(serie)
## PSQM_F_CC11_NSA_PCH_SM - persentagechange for prev. month
# Creating time series
data %>% 
    select(YEAR, MONTH, PSQM_F_CC11_NSA_PCH_SM) %>%
    View()
datas = data$PSQM_F_CC11_NSA_PCH_SM[1:237]
serie = ts(rev(datas), 
    start = c(2000,1), 
    frequency = 12)
# Serie study
autoplot(serie) 
summary(serie)
# Create diff serie
sdiff = diff(serie,
    lag = 1,
    differences = 1)
autoplot(sdiff) 
summary(sdiff)
# Ts plot
serie %>% ggseasonplot(polar = T)
serie %>% ggseasonplot() 
sdiff %>% ggseasonplot()
## there is no strong evidence on seasonality presence
# Stationarity test
adf.test(serie)
## we can not accept H0 (at 15%) => serie is stationary  
adf.test(sdiff) # more robust stationnarity
Box.test(serie, lag = 25, type = "Ljung-Box")
Box.test(sdiff, lag = 25, type = "Ljung-Box")
## serie is nonstationary
# PACF and ACF nondiff
par(mfrow = c(1, 2), mar = c(5, 5, 1, 2) + 0.1)		
acf(serie,
    xlab = expression(italic(k)),
    ylab = expression(italic(r[k])),
    las = 1)
pacf(serie,
    xlab = expression(italic(k)),
    ylab = expression(italic(hat(phi)[kk])),
    las = 1)
# PACF and ACF diff
par(mfrow = c(1, 2), mar = c(5, 5, 1, 2) + 0.1)		
acf(sdiff,
    xlab = expression(italic(k)),
    ylab = expression(italic(r[k])),
    las = 1)
pacf(sdiff,
    xlab = expression(italic(k)),
    ylab = expression(italic(hat(phi)[kk])),
    las = 1)
# Detailed analysis and decomposition
stl(serie, 12) %>% autoplot() 
stl(sdiff, 12) %>% autoplot()
# Autoarima
auto.arima(serie)
auto.arima(sdiff)
# Arima
model = arima(serie, 
    order = c(1, 0, 1),
    seasonal = c(1, 1, 1))



########################
# Descriptive statistics
########################
# Packages
require(tidyverse)
require(ggfortify)
require(tseries)
require(forecast)
# Loading data
data = read.csv("./data/derived_data/full_nsa.csv")
# names(data)
# Selecting variables
n = c("YEAR", "MONTH",
    "I2015_NSA_IS.IP_F_CC1",
    "I2015_NSA_IS.PEI_F_CC11",
    "RT12.NSA_F_CC11_IS.PEI",
    "BS.CCI.BAL_NSA", "BS.COB.BAL_NSA",
    "BS.CEME.BAL_NSA", "BS.CTA.BAL_NSA",
    "BS.CPE.BAL_NSA",
    "BS.FLBA1.PC_NSA", "BS.FLBA2.PC_NSA", "BS.FLBA3.PC_NSA",
    "BS.FLBA4.PC_NSA", "BS.FLBA5.PC_NSA", "BS.FLBA6.PC_NSA",
    "BS.FLBA7.PC_NSA",
    "PSQM_F_CC11_NSA_I15")
ndata = data %>% 
    filter(YEAR >= 2000) %>%
    select(n) %>% 
    mutate(TI = YEAR - 1999) %>%
    select(-YEAR) %>%
    na.omit()
dim(ndata)
# Verification
ndata %>%
    group_by(YEAR) %>%
    count()
# Correlation analysis
cor(ndata) %>% aheatmap()
ndata  %>% pairs()
# Summary stats
summary(ndata)
# Simple model
factors = c("TI", 
    # "MONTH",
    # "I2015_NSA_IS.IP_F_CC1",
    # "I2015_NSA_IS.PEI_F_CC11",
    # "RT12.NSA_F_CC11_IS.PEI",
    # "BS.CCI.BAL_NSA", 
    # "BS.COB.BAL_NSA",
    # "BS.CEME.BAL_NSA", 
    # "BS.CTA.BAL_NSA",
    # "BS.CPE.BAL_NSA",
    # "BS.FLBA1.PC_NSA", 
    "BS.FLBA2.PC_NSA", "BS.FLBA3.PC_NSA",
    "BS.FLBA4.PC_NSA", "BS.FLBA5.PC_NSA", "BS.FLBA6.PC_NSA",
    "BS.FLBA7.PC_NSA", 
    "PSQM_F_CC11_NSA_I15")
cor(ndata[,factors]) %>% aheatmap()
model = lm(PSQM_F_CC11_NSA_I15 ~ ., ndata[,factors])
summary(model) # few significant indicators
plot(model) # normal residuals
# Graphiques
faccons = c("BS.FLBA1.PC_NSA", "BS.FLBA2.PC_NSA", "BS.FLBA3.PC_NSA",
    "BS.FLBA4.PC_NSA", "BS.FLBA5.PC_NSA", "BS.FLBA6.PC_NSA",
    "BS.FLBA7.PC_NSA")
time = ts(ndata[,faccons], 
    start = c(2004, 10), 
    frequency = 12)
time %>% autoplot()
time %>% pairs()


##############################
# Time series analysis 2 (SCA)
##############################
# Packages
require(tidyverse)
require(ggfortify)
require(tseries)
require(forecast)
# Loading data
data = read.csv("./data/derived_data/full_sca.csv")
names(data)
# First, we will study all residential sector (F_CC11)
## PSQM_F_CC11_SCA_I15 - index for 2015
data %>% 
    select(YEAR, MONTH, PSQM_F_CC11_SCA_I15) %>%
    View()
datas = data$PSQM_F_CC11_SCA_I15[1:237]
    # na.omit()
serie = ts(rev(datas), 
    start = c(2000,1), 
    frequency = 12)
# Serie study
autoplot(serie) 
summary(serie)
# Create diff serie
sdiff = diff(serie,
    lag = 1,
    differences = 1)
autoplot(sdiff) 
summary(sdiff)
# Ts plot
serie %>% ggseasonplot(polar = T)
serie %>% ggseasonplot() 
sdiff %>% ggseasonplot()
## there is no strong evidence on seasonality presence
# Stationarity test
adf.test(serie)
## we can not accept H0 (at 15%) => serie is stationary  
adf.test(sdiff) # more robust stationnarity
Box.test(serie, lag = 25, type = "Ljung-Box")
Box.test(sdiff, lag = 25, type = "Ljung-Box")
## serie is nonstationary
# PACF and ACF nondiff
par(mfrow = c(1, 2), mar = c(5, 5, 1, 2) + 0.1)		
acf(serie,
    xlab = expression(italic(k)),
    ylab = expression(italic(r[k])),
    las = 1)
pacf(serie,
    xlab = expression(italic(k)),
    ylab = expression(italic(hat(phi)[kk])),
    las = 1)
# PACF and ACF diff
par(mfrow = c(1, 2), mar = c(5, 5, 1, 2) + 0.1)		
acf(sdiff,
    xlab = expression(italic(k)),
    ylab = expression(italic(r[k])),
    las = 1)
pacf(sdiff,
    xlab = expression(italic(k)),
    ylab = expression(italic(hat(phi)[kk])),
    las = 1)
# Detailed analysis and decomposition
stl(serie, 12) %>% autoplot() 
stl(sdiff, 12) %>% autoplot()
# Autoarima
auto.arima(serie)
auto.arima(sdiff)
# Arima
arima(sdiff, 
    order = c(1, 0, 1),
    seasonal = c(1, 0, 1))


#####################################
# Time serie study auxilary functions
#####################################
# Packages - run before !!!
require(tidyverse)
require(DataCombine)
require(ggfortify)
require(tseries)
require(forecast)
require(gridExtra)
require(grid)
# Plots
plot.ts = function(timeserie) {
    # Graphs
    p1 = autoplot(timeserie)
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none")  
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0))
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0))
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2)
}
# Tests
test.ts = function(timeserie, t) {
    # Summary
    summary(timeserie) %>% 
        print()
    # ADF test
    # for (i in 1:t) {
        adf.test(timeserie) %>% 
            print()
    # }
    # Box test
    Box.test(timeserie, lag = 25, type = "Ljung-Box") %>% 
        print()
}
# Time serie creation
create.ts = function(data, varia, year, names = NA) {
    serie = data %>%
        group_by(YEAR, MONTH) %>%
        arrange(YEAR, MONTH) %>%
        filter(YEAR >= year) %>%
        na.omit() %>%
        ungroup()
    serie = serie %>% 
        select(varia) %>% 
        ts(start = c(serie$YEAR[1], serie$MONTH[1]), 
            frequency = 12, names = names)
    serie
}


###########################
# Analysis of time series 2
###########################
# Loading data
data = read.csv("./data/derived_data/full_sca.csv")
names(data)
#####################
# PSQM_F_CC11_SCA_I15
varia = "PSQM_F_CC11_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000)    
plot.ts(ts1)
test.ts(ts1)
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
plot.ts(ts1.1)
test.ts(ts1.1)
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.2)
test.ts(ts1.2)
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.3)
# ts1.3 = create.ts(data, varia, year = 2000) %>%
#     diff(lag = 12, diff = 1) %>%
#     diff(lag = 1, diff = 1)
# plot.ts(ts1.3) # Des résultats identiques
#####################
# PSQM_F_CC111_SCA_I15
varia = "PSQM_F_CC111_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000)    
plot.ts(ts1)
test.ts(ts1, t = 5)
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
plot.ts(ts1.1)
test.ts(ts1.1)
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.2)
test.ts(ts1.2)
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.3)
#####################
# PSQM_F_CC112_SCA_I15
varia = "PSQM_F_CC112_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000)    
plot.ts(ts1)
test.ts(ts1)
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
plot.ts(ts1.1)
test.ts(ts1.1)
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.2)
test.ts(ts1.2)
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>%
    diff(lag = 12, diff = 1)
plot.ts(ts1.3)
#################
# All index study
# Variable choice
names(data)
varia = c("PNUM_F_CC112_SCA_I15", "PNUM_F_CC111_SCA_I15",
    "PNUM_F_CC11_X_CC113_SCA_I15" #,
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    # "PSQM_F_CC111_SCA_I15", "PSQM_F_CC112_SCA_I15","PSQM_F_CC113_SCA_I15",
    # "PSQM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
for (i in 1:(length(varia)-2)) {
    timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
}
colnames(timeser.c) = varia
# names(timeser.c) = c(1:length(varia))
# Plot all
autoplot(timeser.c, facets = F) + ggtitle("Pemis")

###########################
# Analysis of time series 3
###########################
# Analysis of Yearly data
# Packages
require(tidyverse)
require(openxlsx)
require(NMF)
# Data
dat = read.xlsx("./data/raw_data/econ-gen-pib-composante-euro.xlsx")
# Summary
summary(dat)
View(dat)
# Convert character to numeric
dat.c = 
    lapply(dat, gsub, 
        pattern = ",", 
        replacement = ".") %>%
    as.data.frame() 
dat.c = lapply(dat.c, as.character) %>%
    as.data.frame() 
# New summary
summary(dat.c)
# Correlation
cor(dat.c) %>% aheatmap()
pairs(dat.c) # We can observe cycles resulting in different relation 
# In other words we can observe structural economical changes
# Save data
write.csv(dat.c, file = "./data/derived_data/econ-gen-pib-composante-euro.csv")


#########################
# All index study, part 2
#########################
# Number of dwelling houses
data = read.csv("./data/derived_data/full_nsa.csv")
# names(data)
varia = c("PNUM_F_CC111_NSA_I15",
    "PNUM_F_CC112_NSA_I15",
    "PNUM_F_CC11_X_CC113_NSA_I15" #,
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    # "PSQM_F_CC111_SCA_I15", "PSQM_F_CC112_SCA_I15","PSQM_F_CC113_SCA_I15",
    # "PSQM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("One dwelling buildings",
    "Two- and more dwelling",
    "Residential buildings except for community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
for (i in 1:(length(varia)-2)) {
    timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
}
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/ndh_mens_i2015.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : Number of dwelling houses, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()
###########################
# Building permits : m2 of useful floor area
# names(data)
varia = c(# "PNUM_F_CC111_SCA_I15",
    # "PNUM_F_CC112_SCA_I15",
    # "PNUM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    "PSQM_F_CC111_NSA_I15", 
    "PSQM_F_CC112_NSA_I15",
    "PSQM_F_CC113_NSA_I15"#,
    # "PSQM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("One dwelling buildings",
    "Two- and more dwelling",
    "Community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
for (i in 1:(length(varia)-2)) {
    timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
}
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/psqm_mens_i2015.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : m2 of useful floor area, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()
###########################
# Building permits : m2 of useful floor area
# names(data)
varia = c(# "PNUM_F_CC111_SCA_I15",
    # "PNUM_F_CC112_SCA_I15",
    "PNUM_F_CC11_X_CC113_NSA_I15",
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    # "PSQM_F_CC111_SCA_I15", 
    # "PSQM_F_CC112_SCA_I15",
    # "PSQM_F_CC113_SCA_I15"#,
    "PSQM_F_CC11_X_CC113_NSA_I15"#,
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("Number of dwellings, except for community buildings",
    "m2 of usefull floor area, except for community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
# for (i in 1:(length(varia)-2)) {
#     timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
# }
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/C11xC113_i2015.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : m2 of useful floor area, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()


###########################################
# Analysis of time series - Images creation
###########################################
# Loading data
data = read.csv("./data/derived_data/full_nsa.csv")
# names(data)
#####################
# PSQM_F_CC11_SCA_I15
varia = "PSQM_F_CC11_NSA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C11_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C11d1_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
timeserie = ts1.2
png(filename="./images/C11d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, seasonal difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, seasonal difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>% 
    diff(lag = 12, diff = 1)
timeserie = ts1.3
png(filename="./images/C11d1d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, double difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, double difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
#####################
# PSQM_F_CC111_SCA_I15
varia = "PSQM_F_CC111_NSA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C111_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C111d1_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
timeserie = ts1.2
png(filename="./images/C111d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, seasonal difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, seasonal difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>% 
    diff(lag = 12, diff = 1)
timeserie = ts1.3
png(filename="./images/C111d1d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, doubledouble difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, doubledouble difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
#####################
# PSQM_F_CC112_SCA_I15
varia = "PSQM_F_CC112_NSA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C112_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C112d1_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Season diff
ts1.2 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 12, diff = 1)
timeserie = ts1.2
png(filename="./images/C112d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, seasonal difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, seasonal difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Double diff
ts1.3 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1) %>% 
    diff(lag = 12, diff = 1)
timeserie = ts1.3
png(filename="./images/C112d1d12_tsanal.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, double difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, double difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()



########################################
# All index study, part X - SCA analysis
########################################
# Number of dwelling houses
data = read.csv("./data/derived_data/full_sca.csv")
# names(data)
varia = c("PNUM_F_CC111_SCA_I15",
    "PNUM_F_CC112_SCA_I15",
    "PNUM_F_CC11_X_CC113_SCA_I15" #,
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    # "PSQM_F_CC111_SCA_I15", "PSQM_F_CC112_SCA_I15","PSQM_F_CC113_SCA_I15",
    # "PSQM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("One dwelling buildings",
    "Two- and more dwelling",
    "Residential buildings except for community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
for (i in 1:(length(varia)-2)) {
    timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
}
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/ndh_mens_i2015_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : Number of dwelling houses, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()
###########################
# Building permits : m2 of useful floor area
# names(data)
varia = c(# "PNUM_F_CC111_SCA_I15",
    # "PNUM_F_CC112_SCA_I15",
    # "PNUM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    "PSQM_F_CC111_SCA_I15", 
    "PSQM_F_CC112_SCA_I15",
    "PSQM_F_CC113_SCA_I15"#,
    # "PSQM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("One dwelling buildings",
    "Two- and more dwelling",
    "Community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
for (i in 1:(length(varia)-2)) {
    timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
}
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/psqm_mens_i2015_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : m2 of useful floor area, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()
###########################
# Building permits : m2 of useful floor area
# names(data)
varia = c(# "PNUM_F_CC111_SCA_I15",
    # "PNUM_F_CC112_SCA_I15",
    "PNUM_F_CC11_X_CC113_SCA_I15",
    # "PSQM_F_CC1_SCA_I15",
    # "PSQM_F_CC11_SCA_I15",
    # "PSQM_F_CC111_SCA_I15", 
    # "PSQM_F_CC112_SCA_I15",
    # "PSQM_F_CC113_SCA_I15"#,
    "PSQM_F_CC11_X_CC113_SCA_I15"#,
    # "PSQM_F_CC12_SCA_I15",
    # "PSQM_F_CC122_SCA_I15",
    # "PSQM_F_CC12_X_CC122_SCA_I15")
    )
nam = c("Number of dwellings, except for community buildings",
    "m2 of usefull floor area, except for community buildings")
# Series creation
timeser = list()
for (i in 1:length(varia)) {
    timeser[[i]] = create.ts(data, varia[i], 
        year = 2000, names = varia[i])
}
# Concatenate
timeser.c = ts.union(timeser[[1]], timeser[[2]], dframe = F)
# for (i in 1:(length(varia)-2)) {
#     timeser.c = ts.union(timeser.c, timeser[[i+2]], dframe = F)
# }
colnames(timeser.c) = nam
# names(timeser.c) = c(1:length(varia))
# Plot all
png(filename="./images/C11xC113_i2015_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
autoplot(timeser.c, facets = F) + 
    ggtitle("Building permits : m2 of useful floor area, Index 2015") +
    xlab("Year") + ylab("Index 2015") + 
    guides(color = guide_legend(title = "Variable :"))
dev.off()


###########################################
# Analysis of time series - Images creation
###########################################
# Loading data
data = read.csv("./data/derived_data/full_sca.csv")
# names(data)
#####################
# PSQM_F_CC11_SCA_I15
varia = "PSQM_F_CC11_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C11_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C11d1_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
#####################
# PSQM_F_CC111_SCA_I15
varia = "PSQM_F_CC111_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C111_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C111d1_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,one dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
#####################
# PSQM_F_CC112_SCA_I15
varia = "PSQM_F_CC112_SCA_I15"
# No diff
ts1 = create.ts(data, varia, year = 2000) 
timeserie = ts1 
png(filename="./images/C112_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
## Simple diff
ts1.1 = create.ts(data, varia, year = 2000) %>%
    diff(lag = 1, diff = 1)
timeserie = ts1.1
png(filename="./images/C112d1_tsanal_SCA.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area,two- and more dwelling buildings",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()


##################################################
# Time series regression study (ARIMA) on CSA data
##################################################
# Packages
require(stargazer)
require(tidyverse)
require(DataCombine)
require(ggfortify)
require(tseries)
require(forecast)
require(gridExtra)
require(grid)
# require(networkD3)
require(webshot)
require(png)
require(magick)
require(xtable)
# Auxilary data creation function
create.ts = function(data, varia, year, names = NA) {
    serie = data %>%
        group_by(YEAR, MONTH) %>%
        arrange(YEAR, MONTH) %>%
        filter(YEAR >= year) %>%
        na.omit() %>%
        ungroup()
    serie = serie %>% 
        select(varia) %>% 
        ts(start = c(serie$YEAR[1], serie$MONTH[1]), 
            frequency = 12, names = names)
    serie
}
# Tests
test.ts = function(timeserie, t) {
    # Summary
    summary(timeserie) %>% 
        print()
    # ADF test
    # for (i in 1:t) {
        adf.test(timeserie) %>% 
            print()
    # }
}
# Data loading
data = read.csv("./data/derived_data/full_sca.csv") # Adjusted
# names(data)
###################
# Full market study
#####################
# PSQM_F_CC11_SCA_I15
varia = "PSQM_F_CC11_SCA_I15" # For adjusted data
# First difference data
tsm = create.ts(data, varia, year = 2000) # %>% 
    # diff(lag = 1, diff = 1)
# Plot full serie
#plot.ts(tsm) # Here we use generic method
# ARIMA(3,1,1) - first difference, non seasonal
model = Arima(tsm, order = c(3, 1, 1))
# Les resultats
summary(model)
# Automatic algorithm verification
auto.arima(tsm) # Our model produces a better fit
# Serie des resultats
# res = ts(model$res, frequency = 12)
# Plot the two series
res = ts.union(tsm, model$fit)
colnames(res) = c("Original serie", "Fitted values")
# Start device
png(filename="./images/C11_SCA_arima.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)
    p1 = res %>% autoplot() + 
        ggtitle("Fit comparison") + 
        ylab("Index 2015") + 
        xlab("Year, month") + 
        guides(color = guide_legend(title = "Series :"))
    p2 = tsm %>% forecast() %>% autoplot() +
        ggtitle("Predictions") + 
        ylab("Index 2015") + 
        xlab("Year, month")
    grid.arrange(p1, p2, nrow = 1,
        top = textGrob("Building permits, residential buildings : m2 of usefull floor area",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
#####################
# PSQM_F_CC11_SCA_I15
# Data loading
data2 = read.csv("./data/derived_data/full_nsa.csv") # Nonadjusted
# Variable choice
varia2 = "PSQM_F_CC11_NSA_I15" # For nonadjusted data
# Seasonnality correction
# Creating seasonal dummies
data2 = data2 %>% 
    mutate(t1 = as.numeric(MONTH == 1),
        t2 = as.numeric(MONTH == 2),
        t3 = as.numeric(MONTH == 3),
        t4 = as.numeric(MONTH == 4),
        t5 = as.numeric(MONTH == 5),
        t6 = as.numeric(MONTH == 6),
        t7 = as.numeric(MONTH == 7),
        t8 = as.numeric(MONTH == 8),
        t9 = as.numeric(MONTH == 9),
        t10 = as.numeric(MONTH == 10),
        t11 = as.numeric(MONTH == 11),
        t12 = as.numeric(MONTH == 12),
        y = rep_len(273:1, nrow(data2)),
        y2 = y^2) %>%
    filter(X <= 273 & X != 1) %>% # serie starts at 1997,1
    filter(YEAR >= 2000) # start at 2000
# Regressing variable on time and seasonal dummies
ols = lm(PSQM_F_CC11_NSA_I15 ~ 0 + y + #y2 + 
    t1 + t2 + t3 + t4 + t5 + t6 +
    t7 + t8 + t9 + t10 + t11 + t12, data2) 
    # No intercept to avoid singularity
summary(ols) # Fully significant
stargazer(ols, flip = TRUE)
# Serie concatenation
correction = ts(rev(ols$fit), 
    start = c(2000, 1), 
    frequency = 12)
tsm = ts(rev(data2$PSQM_F_CC11_NSA_I15), 
    start = c(2000, 1), 
    frequency = 12)
cortsm = ts.union(correction, tsm)
# Plot
colnames(cortsm) = c("Original serie", "Correction part")
# Difference
tsx = tsm - correction
# Save results in html
print(xtable(summary(ols)), 
    file = "./html/C11_CSA_correction.html", 
    type = "html",
    html.table.attributes = 'align="center" height=100% width=100% valign=center border=10 style="text-align: center; vertical-align: middle"')
## write.table(results, 
##     file = "./html/C11_CSA_correction.html", 
##     quote = FALSE,
##     col.names = FALSE,
##     row.names = FALSE)
webshot(url = "./html/C11_CSA_correction.html", 
    file = "./html/C11_CSA_ols.png")
# Start device
png(filename="./images/C11_CSA_correction.png", width = 450, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)
    p1 = cortsm %>% autoplot() + 
        ggtitle("Correction") + 
        ylab("Index 2015") + 
        xlab("Year, month") + 
        guides(color = guide_legend(title = "Series :")) +
        theme(legend.position = "bottom")
    p2 = (tsm - correction) %>% autoplot() +
        ggtitle("Adjusted serie") + 
        ylab("Index 2015, adjusted") + 
        xlab("Year, month")
    # p3 = plot(density(tsx), 
    #     top = "Density plot for adjusted data")
    # p4 = image_read("./html/C11_CSA_ols.png", 
    #     density = 1200) %>%
    #         image_ggplot()
    grid.arrange(p1, p2, nrow = 2,
        top = textGrob("Building permits",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
# First differences study
tsx2 = tsx %>% diff(lag = 1, diff = 1)
timeserie = tsx2
png(filename="./images/C11_CSA_d1.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
    # Graphs
    p1 = autoplot(timeserie) + 
        ggtitle("Evolution") +
        xlab("Year") +
        ylab("Index 2015, first difference")
    p2 = ggseasonplot(timeserie) + 
        theme(legend.position = "none") + 
        ggtitle("Seasonal comparaison")  +
        xlab("Month") +
        ylab("Index 2015, first difference")
    p3 = ggAcf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("ACF") +
        xlab("Lag")
    p4 = ggPacf(timeserie, 
        lag.max = round(length(timeserie)/4,0)) +
        ggtitle("PACF") +
        xlab("Lag")
    # Arranging
    grid.arrange(p1, p2, p3, p4, nrow = 2,
        top = textGrob("Building permits : m2 of usefull floor area, first differences",
            gp = gpar(fontsize = 20, font = 3)))
dev.off()
# Save this to txt
test.ts(tsx2)
# Subsetting
n = 204
train = 1:n
test = n:length(tsx)
# Model
tr = ts(tsx[train], start = c(2000, 1), frequency = 12)
model = arima(tr, order = c(2, 1, 3))
# Summarize
print(summarise(model), 
    file = "./html/C11_CSA_arima.html")
webshot(url = "./html/C11_CSA_arima.html", 
    file = "./html/C11_CSA_arima.png")
# Fitting
sp = ts(append(fitted(model), forecast(model, h = 32)$mean), start = c(2000, 1), frequency = 12)
# Concatenation
trsp = ts.union(tsx, sp) 
colnames(trsp) = c("Original serie", "Fitted and predicted")
dif = tsx - sp
# Plot1
png(filename="./images/C11_CSA_arimaresults1.png", width = 450, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)
trsp %>% autoplot(facets = F) + 
    geom_vline(xintercept = 2017) + 
    ggtitle("Original vs Fitted") + 
    xlab("Year") + 
    ylab("Index 2015, adjusted") +
    theme(legend.position = "bottom")
dev.off()
# Plot2
png(filename="./images/C11_CSA_arimaresults2.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)	
# p1 = trsp %>% autoplot(facets = F) + 
#     geom_vline(xintercept = 2017) + 
#     ggtitle("Comparaison") + 
#     xlab("Year") + 
#     ylab("Index 2015, adjusted") +
#     theme(legend.position = "bottom")
p2 = autoplot(dif) + 
    ggtitle("Residuals") + 
    xlab("Year") + 
    ylab("Residuals")
p3 = ggAcf(dif, 
    lag.max = round(length(dif)/4,0)) +
    ggtitle("ACF") +
    xlab("Lag")
p4 = ggPacf(dif, 
    lag.max = round(length(dif)/4,0)) +
    ggtitle("PACF") +
    xlab("Lag")
grid.arrange(#p1, 
    p2, p3, p4, nrow = 2,
    top = textGrob("Building permits : m2 of usefull floor area, ARIMA",
        gp = gpar(fontsize = 20, font = 3)))
dev.off()
# Forecast with confidence interval
png(filename="./images/C11_CSA_arimaforecast.png", width = 900, height = 600)
par(mar = c(5, 5, 1, 2) + 0.1)
forecast(model, h = 10) %>% 
    autoplot() + ggtitle("Predictions")
dev.off()
# Model summary
stargazer(model)


###########
# DF3 study 
###########
# Loading
require(tidyverse)
# Data
df = read.csv("./data/derived_data/df3.csv")
names(df)
df3 = df[,-c(1:2)]
# summary(df3[])
print(xtable(summary(df3[, 4:7])), 
    file = "./html/df3.html", 
    type = "html",
    html.table.attributes = 'align="center" height=100% width=100% valign=center border=10 style="text-align: center; vertical-align: middle"')
require(formattable)
df4 = df3[,4:7]
names(df4) = c("Number of authorisations, individual housing",
    "Number of authorisations, groupped housing",
    "Number of authorisations, collective housing",
    "Total number of authorisations")
stargazer(df4)
names(df3)

# Tom's regressions
reg1 = lm(total_nombre_de_logements~PIB+Taux_interet, data = df3)
stargazer(reg1)
reg2 = lm(log(total_nombre_de_logements)~log(PIB)+log(Taux_interet),data=df3)
stargazer(reg2)
ds = df3[,c(12, 15, 20, 23)]
reg3 = lm(log(df3[,6]) ~ log(as.matrix(ds)))
summary(reg3)
require(stargazer)
stargazer(reg3)
ds2 = df3[,c(20, 23)]
reg4 = lm(log(df3[,6]) ~ log(as.matrix(ds2)))
summary(reg4)
plot(reg3)
require(lmtest)
dwtest(reg3)
plot(reg3$res)
acf(reg3$res)
pacf(reg3$res)
res = ts(reg3$res)
autoplot(res)
arima(res, c(1, 0, 0))
p1 = plot(reg3)[1]
ls(p1)

# Data reorganisation 
# Demographical data
# Packages 
require(tidyverse)
# Read data
dat = read.csv("./data/raw_data/Données démographique.csv", sep = ";")
# Subset France
datx = dat[c(1, 3:20),]
# Change names
datx[,1] = c(
    "France",
    "Hm15",
    "Hm20",
    "H20_59",
    "H60p",
    "H75p",
    "H15_49",
    "Fm15",
    "Fm20",
    "F20_59",
    "F60p",
    "F75p",
    "F15_49",
    "Em15",
    "Em20",
    "E20_59",
    "E60p",
    "E75p",
    "E15_49"
)
# Transpose
t_dat = t(datx[,2:ncol(datx)])
# Names
colnames(t_dat) = datx[,1]
rownames(t_dat) = 1:nrow(t_dat)
# Write
write.csv(t_dat, file = "./data/derived_data/demographie.csv")


# Changing Political information data 
# Load packages 
require(openxlsx)
require(tidyverse)
# Load data 
polit = read.xlsx("./data/raw_data/Présidence.xlsx")
colnames(polit) = c("Year", 
    "Leader",
    "Partie",
    "BordPolit")
summary(polit)
polit = polit[1:(nrow(polit)-1),]
write.csv(polit, file = "./data/derived_data/presidence.csv")