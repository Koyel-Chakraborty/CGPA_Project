library(readxl)
df <- read_excel('cgpa.xlsx')#This file contains the whole dataset
cgpa <- df$CGPA
cgpa.h0 <- cgpa - rep(7.5,times = length(cgpa))
df_M <- subset(df, df$Gender == 'M')
df_F <- subset(df, df$Gender == 'F')
cgpa.m <- df_M$CGPA
cgpa.f <- df_F$CGPA

test.container <- c()
test.name <- c()

#Parametric Tests

#t-Test
test.container <- append(test.container,t.test(cgpa.h0,alternative = "greater")[3])
test.name <- append(test.name,'t Test')


#Fisher's t-Test
#Equal Variance Assumption
test.container <- append(test.container,t.test(cgpa.m,cgpa.f,alternative = "less",var.equal = TRUE)[3])
test.name <- append(test.name,"Fisher's t Test _ Equal Variance")
#Unequal Variance Assumption
test.container <- append(test.container,t.test(cgpa.m,cgpa.f,alternative = "less",var.equal = FALSE)[3])
test.name <- append(test.name,"Fisher's t Test _ Unequal Variance")

#Fisher's F Test
test.container <- append(test.container,var.test(cgpa.m,cgpa.f,alternative = "greater")[3])
test.name <- append(test.name,"Fisher's F Test")


#Non-Parametric Tests

#Sign Test
x = length(subset(cgpa, cgpa>7.5))
test.name <- append(test.name,'Sign Test')
test.container <- append(test.container,binom.test(x,28,alternative = 'greater')[3])


cgpa2 <- read.csv("cgpa_without_tie_or_zero.csv") #This file contains no tie in absolute values or value = 7.5
cgpa2.h0 <- cgpa2$CGPA - rep(7.5,times = length(cgpa2$CGPA))


#Wilcoxon Signed Rank Test
test.container <- append(test.container,wilcox.test(cgpa2.h0, alternative = 'greater')[3])
test.name <- append(test.name,'Wilcoxon Signed Rank Test')


cgpa3 <- read.csv('cgpa_without_tie.csv') #This file contains no tie in absolute values
cgpa3.m <- subset(cgpa3, cgpa3$Gender=='M')$CGPA
cgpa3.f <- subset(cgpa3, cgpa3$Gender=='F')$CGPA

#Mann-Whitney U Test
test.container <- append(test.container,wilcox.test(cgpa3.m,cgpa3.f,alternative = "less")[3])
test.name <- append(test.name,"Mann Whitney U Test")

cgpa4 <- read.csv('cgpa_ks.csv') #This file contains no tie in values
cgpa4.m <- subset(cgpa4, cgpa4$Gender=='M')$CGPA
cgpa4.f <- subset(cgpa4, cgpa4$Gender=='F')$CGPA

#Ansari-Bradley Test
test.container <- append(test.container,ansari.test(cgpa4.m,cgpa4.f,alternative = "greater")[2])
test.name <- append(test.name,"Ansari Bradley Test")


names(test.container) <- test.name
test.container

