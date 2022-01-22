df <- read.csv('cgpa_ks.csv')
head(df)

#For Combined Sample
ks.test(df$CGPA, 'pnorm', mean(df$CGPA), sd(df$CGPA),exact = TRUE)

df_M <- subset(df, df$Gender == 'M')
df_F <- subset(df, df$Gender == 'F')
head(df_M)
head(df_F)

#For Male Sample
ks.test(df_M$CGPA, 'pnorm', mean(df_M$CGPA), sd(df_M$CGPA),exact = TRUE)

#For Female Sample
ks.test(df_F$CGPA, 'pnorm', mean(df_F$CGPA), sd(df_F$CGPA),exact = TRUE)
