options(scipen=999)

# Bar Plot Jumlah Penduduk ---------------------------------------------------------------------------------------------------------------
library(data.table)
library(xlsx)
library(ggplot2)
library(dplyr)



url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Jumlah%20Penduduk.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Jumlah Penduduk.xlsx", mode = "wb")

#read Data
df = read.xlsx("Jumlah Penduduk.xlsx", 1)
head(df)

#Cleaning data
colnames(df) = df[1,]
df = df %>% slice(2:39)
df = df[order(df$`2021`, decreasing = TRUE),]
head(df)

#Barplot Jumlah Penduduk 2022
ggplot(df, aes(x = reorder(`Kabupaten/ Kota`, -`2022`), y = `2022`, fill = `2022`)) +
  geom_col()+
  coord_flip() +
  theme(legend.position="none") +
  labs(x ="Kota/ Kabupaten", y ="Jumlah Penduduk",
       title="Bar Plot Jumlah Penduduk Kota/ Kabupaten di Provinsi Jawa Timur 2022")+
  geom_text(aes(label = `2022`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 3200000), expand = c(0, 0))


# Time Series Jumlah Penduduk ---------------------------------------------------------------------------------------------------------------

#read Data
df = read.xlsx("Jumlah Penduduk.xlsx", 1)
head(df)

#Cleaning data
colnames(df) = df[1,]
df = df %>% slice(2:40)
head(df)

#transpose data frame
df_s = cbind(df[1:1], stack(df[2:6]))
colnames(df_s) = c("Kabupaten/ Kota", "Jumlah Penduduk", "Tahun")
head(df_s)

df10kab1 = df_s[df_s$`Kabupaten/ Kota` %in% 
                  c("Kota Surabaya", "Kabupaten Malang", "Kabupaten Jember", "Kabupaten Sidoarjo","Kabupaten Banyuwangi",
                    "Kabupaten Kediri", "Kabupaten Pasuruan", "Kabupaten Lamongan", "Kabupaten Jombang", "Kabupaten Gresik", "Kabupaten Bojonegoro"), ]

df10kab2 = df_s[df_s$`Kabupaten/ Kota` %in% 
                  c("Kabupaten Blitar", "Kabupaten Tuban", "Kabupaten Probolinggo","Kabupaten Sumenep",
                    "Kabupaten Lumajang", "Kabupaten Mojokerto", "Kabupaten Nganjuk", "Kabupaten Tulungagung", "Kabupaten Bangkalan", "Kabupaten Sampang"), ]

df10kab3 = df_s[df_s$`Kabupaten/ Kota` %in% 
                  c("Kabupaten Ponorogo", "Kabupaten Ngawi",  "Kabupaten Pamekasan", "Kota Malang" ,
                    "Kabupaten Bondowoso", "Kabupaten Madiun", "Kabupaten Trenggalek", "Kabupaten Situbondo", "Kabupaten Magetan", "Kabupaten Pacitan"),]

df10kab4 = df_s[df_s$`Kabupaten/ Kota` %in% 
                  c("Kota Kediri", "Kota Probolinggo","Kota Batu",
                    "Kota Pasuruan", "Kota Madiun", "Kota Blitar", "Kota Mojokerto"),]

#Time Series 10 Kabupaten 1
ggplot(df10kab1, aes(x = Tahun, y = `Jumlah Penduduk`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  scale_y_continuous(expand = c(0, 0), limits = c(1000000, 3000000))+
  labs(x="Tahun", y="Jumlah Penduduk",
       title="Time Series Jumlah Penduduk di 11 Kabupaten/ Kota Provinsi Jawa Timur (1)",
       subtitle="Tahun 2018 - 2022") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 2
ggplot(df10kab2, aes(x = Tahun, y = `Jumlah Penduduk`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  scale_y_continuous(expand = c(0, 0), limits = c(950000, 1250000))+
  labs(x="Tahun", y="Jumlah Penduduk",
       title="Time Series Jumlah Penduduk di 10 Kabupaten/ Kota Provinsi Jawa Timur (2)",
       subtitle="Tahun 2018 - 2022") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 3
ggplot(df10kab3, aes(x = Tahun, y = `Jumlah Penduduk`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  scale_y_continuous(expand = c(0, 0), limits = c(550000, 1000000))+
  labs(x="Tahun", y="Jumlah Penduduk",
       title="Time Series Jumlah Penduduk di 10 Kabupaten/ Kota Provinsi Jawa Timur (3)",
       subtitle="Tahun 2018 - 2022") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 4
ggplot(df10kab4, aes(x = Tahun, y = `Jumlah Penduduk`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  scale_y_continuous(expand = c(0, 0), limits = c(100000, 300000))+
  labs(x="Tahun", y="Jumlah Penduduk",
       title="Time Series Jumlah Penduduk di 9 Kabupaten/ Kota Provinsi Jawa Timur (4)",
       subtitle="Tahun 2018 - 2022") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series Jawa Timur
df_j = df_s[df_s$`Kabupaten/ Kota` == "Jawa Timur", ]
head(df_j)

ggplot(df_j, aes(x = Tahun, y = `Jumlah Penduduk`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Jumlah Penduduk",
       title="Time Series Jumlah Penduduk di Provinsi Jawa Timur",
       subtitle="Tahun 2018 - 2022") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()+
  geom_text(data = df_j, aes(y = `Jumlah Penduduk`, label = sprintf("%0.0f", round((`Jumlah Penduduk`), digits = 2)), vjust=1.2, hjust=0.5))+
  theme(legend.position = "none")

# Time Series Angka Kematian ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Angka%20Kematian%20Bayi.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Angka Kematian Bayi.xlsx", mode = "wb")

#read Data
df2 = read.xlsx("Angka Kematian Bayi.xlsx", 1)
head(df2)

colnames(df2) = df2[1,]
df2 = df2 %>% slice(2:40)
head(df2)

df2 <- cbind(df2[1:1], stack(df2[5:9]))
colnames(df2) <- c("Kabupaten/ Kota", "Angka Kematian Bayi", "Tahun")
df2$`Angka Kematian Bayi` <- as.numeric(as.character(df2$`Angka Kematian Bayi`))
df2 = df2[order(df2$`Angka Kematian Bayi`, decreasing = TRUE),]
head(df2)

df2_10kab_1 = df2[df2$`Kabupaten/ Kota` %in% 
                  c("Kabupaten Probolinggo", "Kabupaten Jember", "Kabupaten Bangkalan", "Kabupaten Situbondo",  "Kabupaten Bondowoso",
                    "Kabupaten Pasuruan", "Kabupaten Sumenep", "Kabupaten Sampang", "Kabupaten Pamekasan"), ]

df2_10kab_2 = df2[df2$`Kabupaten/ Kota` %in% 
                    c("Kabupaten Bojonegoro", "Kota Pasuruan", "Kabupaten Lumajang", "Kabupaten Lamongan", "Kabupaten Madiun", 
                      "Kabupaten Tuban", "Kabupaten Nganjuk", "Kabupaten Banyuwangi", "Kabupaten Malang", "Kabupaten Jombang"), ]

df2_10kab_3 = df2[df2$`Kabupaten/ Kota` %in% 
                    c("Kota Batu", "Kabupaten Kediri", "Kabupaten Ponorogo", "Kabupaten Ngawi", "Kabupaten Blitar", "Kabupaten Sidoarjo",
                      "Kabupaten Gresik", "Kota Madiun", "Kabupaten Pacitan", "Kabupaten Magetan"), ]

df2_10kab_4 = df2[df2$`Kabupaten/ Kota` %in% 
                    c("Kota Surabaya",  "Kabupaten Mojokerto", "Kota Mojokerto",  "Kabupaten Tulungagung","Kota Kediri",
                      "Kabupaten Trenggalek",  "Kota Probolinggo", "Kota Malang", "Kota Blitar"), ]

#Time Series 10 Kabupaten 1
ggplot(df2_10kab_1, aes(x = Tahun, y = `Angka Kematian Bayi`, 
                     group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Angka Kematian Bayi",
       title="Time Series Angka Kematian Bayi di 9 Kabupaten/ Kota Provinsi Jawa Timur (1)",
       subtitle="Tahun 2012 - 2016") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 2
ggplot(df2_10kab_2, aes(x = Tahun, y = `Angka Kematian Bayi`, 
                        group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Angka Kematian Bayi",
       title="Time Series Angka Kematian Bayi di 11 Kabupaten/ Kota Provinsi Jawa Timur (2)",
       subtitle="Tahun 2012 - 2016") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 3
ggplot(df2_10kab_3, aes(x = Tahun, y = `Angka Kematian Bayi`, 
                        group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Angka Kematian Bayi",
       title="Time Series Angka Kematian Bayi di 10 Kabupaten/ Kota Provinsi Jawa Timur (3)",
       subtitle="Tahun 2012 - 2016") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

#Time Series 10 Kabupaten 4
ggplot(df2_10kab_4, aes(x = Tahun, y = `Angka Kematian Bayi`, 
                        group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Angka Kematian Bayi",
       title="Time Series Angka Kematian Bayi di 10 Kabupaten/ Kota Provinsi Jawa Timur (4)",
       subtitle="Tahun 2012 - 2016") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()


#Time Series Jawa Timur
df2_j = df2[df2$`Kabupaten/ Kota` == "Jawa Timur", ]
head(df2_j)

ggplot(df2_j, aes(x = Tahun, y = `Angka Kematian Bayi`, 
                 group = `Kabupaten/ Kota`, color = `Kabupaten/ Kota`)) +
  geom_line()+
  labs(x="Tahun", y="Angka Kematian Bayi",
       title="Time Series Angka Kematian Bayi di Provinsi Jawa Timur",
       subtitle="Tahun 2012 - 2016") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()+
  geom_text(data = df2_j, aes(y = `Angka Kematian Bayi`, label = sprintf("%0.0f", round((`Angka Kematian Bayi`), digits = 2)), vjust=1.2, hjust=0.5))+
  theme(legend.position = "none")


# Bar Plot Migrasi per kabupaten kota ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Migrasi%20Penduduk.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Migrasi Penduduk.xlsx", mode = "wb")

#read Data
df3 = read.xlsx("Migrasi Penduduk.xlsx", 1)
head(df3)

colnames(df3) = df3[1,]
df3 = df3 %>% slice(2:39)
head(df3)

#migrasi masuk
df3$`Migrasi Masuk` <- as.numeric(as.character(df3$`Migrasi Masuk`))
dfmasuk = df3[order(df3$`Migrasi Masuk`, decreasing = TRUE),]
head(dfmasuk)

ggplot(dfmasuk, aes(x = reorder(`Kabupaten/ Kota`, -`Migrasi Masuk`), y = `Migrasi Masuk`, 
                fill = `Migrasi Masuk`)) +
  scale_y_continuous(limits = c(0, 124000), expand = c(0, 0))+
  geom_col()+ 
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kota/ Kabupaten", y="Migrasi Masuk",
       title="Bar Plot Migrasi Masuk Kota/ Kabupaten di Provinsi Jawa Timur 2015")+
  geom_text(aes(label = `Migrasi Masuk`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= -45, hjust=0.5, vjust=0, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))

#migrasi keluar
df3$`Migrasi Keluar` <- as.numeric(as.character(df3$`Migrasi Keluar`))
dfkeluar = df3[order(df3$`Migrasi Keluar`, decreasing = TRUE),]
head(dfkeluar)

ggplot(dfkeluar, aes(x = reorder(`Kabupaten/ Kota`, -`Migrasi Keluar`), y = `Migrasi Keluar`, 
                    fill = `Migrasi Keluar`)) +
  geom_col()+
  scale_y_continuous(limits = c(0, 173000), expand = c(0, 0))+
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kota/ Kabupaten", y="Migrasi Keluar",
       title="Bar Plot Migrasi Keluar Kota/ Kabupaten di Provinsi Jawa Timur 2015")+
  geom_text(aes(label = `Migrasi Keluar`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= -45, hjust=0.5, vjust=0, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))


#migrasi netto
df3$`Migrasi Neto` <- as.numeric(as.character(df3$`Migrasi Neto`))
dfnetto = df3[order(df3$`Migrasi Neto`, decreasing = TRUE),]
head(dfmasuk)

ggplot(dfnetto, aes(x = reorder(`Kabupaten/ Kota`, -`Migrasi Neto`), y = `Migrasi Neto`, 
                    fill = `Migrasi Neto`)) +
  geom_col()+
  coord_flip() +
  scale_y_continuous(limits = c(-50000, 85000), expand = c(0, 0))+
  theme(legend.position="none") +
  labs(x="Kota/ Kabupaten", y="Migrasi Netto",
       title="Bar Plot Migrasi Netto Kota/ Kabupaten di Provinsi Jawa Timur 2015") +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= -45, hjust=0.5, vjust=0, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))

# Barplot Migrasi Seumur Hidup  ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Migrasi%20Seumur%20Hidup%20Indonesia.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Migrasi Seumur Hidup Indonesia.xlsx", mode = "wb")

#read Data Migrasi Masuk
dfmigmasuk = read.xlsx("Migrasi Seumur Hidup Indonesia.xlsx", 1)
head(dfmigmasuk)

colnames(dfmigmasuk) = dfmigmasuk[1,]
dfmigmasuk = dfmigmasuk %>% slice(2:36)
head(dfmigmasuk)

dfmigmasuk <- cbind(dfmigmasuk[1:1], stack(dfmigmasuk[2:10]))
colnames(dfmigmasuk) <- c("Provinsi", "Migrasi Masuk", "Tahun")
dfmigmasuk$`Migrasi Masuk` <- as.numeric(as.character(dfmigmasuk$`Migrasi Masuk`))
head(dfmigmasuk)

dfmigmasuk = dfmigmasuk[dfmigmasuk$Provinsi == "JAWA TIMUR", ]
head(dfmigmasuk)
dfmigkeluar = read.xlsx("Migrasi Seumur Hidup Indonesia.xlsx", 2)
head(dfmigkeluar)

#Read Data Migrasi Keluar
colnames(dfmigkeluar) = dfmigkeluar[1,]
dfmigkeluar = dfmigkeluar %>% slice(2:36)
head(dfmigkeluar)

dfmigkeluar <- cbind(dfmigkeluar[1:1], stack(dfmigkeluar[2:10]))
colnames(dfmigkeluar) <- c("Provinsi", "Migrasi Keluar", "Tahun")
dfmigkeluar$`Migrasi Keluar` <- as.numeric(as.character(dfmigkeluar$`Migrasi Keluar`))
head(dfmigkeluar)

dfmigkeluar = dfmigkeluar[dfmigkeluar$Provinsi == "JAWA TIMUR", ]
head(dfmigkeluar)

#Migrasi Total
df2migkeluar = dfmigkeluar$`Migrasi Keluar`
dftahun = dfmigkeluar$Tahun
dfmig = dfmigmasuk
dfmig <- cbind(dfmig[1:2])
head(dfmig)

dfmig$`Migrasi Keluar` = df2migkeluar
dfmig <- cbind(dfmig[1:1], stack(dfmig[2:3]))
dfmig$Tahun = dftahun
colnames(dfmig) = c("Provinsi", "Jumlah Orang", "Jenis Migrasi", "Tahun")
head(dfmig)

#Barplot Migrasi
ggplot(dfmig, aes(x = Tahun , y = `Jumlah Orang`, fill = `Jenis Migrasi`)) +
  geom_col(position = "dodge")+
  coord_flip() +
  theme(legend.position="none") +
  labs(x ="Kota/ Kabupaten", y ="Jumlah Penduduk",
       title="Bar Plot Migrasi Seumur Hidup di Provinsi Jawa Timur",
       subtitle="Tahun 1971 - 2015")+
  geom_text(aes(label = `Jumlah Orang`, hjust = -0.1, vjust = 0.5), size = 3, col = 'brown') + 
  theme(legend.title=element_blank(), legend.position="bottom")+
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 4100000), expand = c(0, 0))  +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F"))

#Time Series Migrasi
ggplot(dfmig, aes(x = Tahun, y = `Jumlah Orang`, 
                  group = `Jenis Migrasi`, fill = `Jenis Migrasi`, color = `Jenis Migrasi`)) +
  geom_line()+
  labs(x="Tahun", y="Jumlah Migrasi",
       title="Time Series Jumlah Migrasi di Provinsi Jawa Timur",
       subtitle="Tahun 1971 - 2015") +
  theme(legend.title=element_blank(), legend.position="bottom")+
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()

# Bar Plot Jumlah Rumah Tangga ---------------------------------------------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Jumlah%20Rumah%20Tangga.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Jumlah Rumah Tangga.xlsx", mode = "wb")

#read Data
df4 = read.xlsx("Jumlah Rumah Tangga.xlsx", 1)
head(df4)

colnames(df4) = df4[1,]
df4 = df4 %>% slice(2:39)
df4$`2015` = as.numeric(as.character(df4$`2015`))
df4 = df4[order(df4$`2015`, decreasing = TRUE),]
head(df4)

#Bar Plot Jumlah Rumah Tangga
ggplot(df4, aes(x = reorder(`Kabupaten/ Kota`, -`2015`), y = `2015`, fill = `2015`)) +
  geom_col()+
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kota/ Kabupaten", y="Jumlah Rumah Tangga",
       title="Bar Plot Jumlah Rumah Tangga Kota/ Kabupaten di Provinsi Jawa Timur Tahun 2015")+
  geom_text(aes(label = `2015`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 820000), expand = c(0, 0))


# Pie Chart Gender  ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Jenis%20Kelamin%20Berdasarkan%20Kelompok%20Umur.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Jenis Kelamin Berdasarkan Kelompok Umur.xlsx", mode = "wb")

#read Data
df5 = read.xlsx("Jenis Kelamin Berdasarkan Kelompok Umur.xlsx", 1)
head(df5)

#Cleaning data
colnames(df5) = df5[1,]
df5 = df5 %>% slice(2:19)
head(df5)

df5 <- cbind(df5[1:1], stack(df5[2:4]))
colnames(df5) <- c("Kelompok Umur", "Jumlah", "Jenis Kelamin")
head(df5)

df5_jatim = df5[df5$`Kelompok Umur` == "Jawa Timur", ]
df5_jatim = df5_jatim %>% slice(1:2)
head(df5_jatim)

df5_jatim$Jumlah = as.numeric(as.character(df5_jatim$Jumlah))
percentage <- round(df5_jatim$Jumlah/sum(df5_jatim$Jumlah)*100,2)

ggplot(df5_jatim, aes(x = " ", y = `Jumlah`, fill = `Jenis Kelamin`))+
  geom_col(color = "black") +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#F4D166", "#1C6AA8")) +
  theme_void()+
  labs(title="Pie Chart Gender di Provinsi Jawa Timur Tahun 2020") +  
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=0))

# Bar Chart Gender Kelompok Usia ---------------------------------------------------------------------------------------------------------------

#read Data
df6 = read.xlsx("Jenis Kelamin Berdasarkan Kelompok Umur.xlsx", 1)
head(df6)

#Cleaning data
colnames(df6) = df6[1,]
df6 = df6 %>% slice(2:17)
head(df6)

df6 <- cbind(df6[1:1], stack(df6[2:4]))
colnames(df6) <- c("Kelompok Umur", "Jumlah", "Jenis Kelamin")
df6$Jumlah <- as.numeric(as.character(df6$Jumlah))

df6_L = filter(df6, `Jenis Kelamin` == "Laki-Laki")
df6_P = filter(df6, `Jenis Kelamin` == "Perempuan")
df6_T = filter(df6, `Jenis Kelamin` == "Total")

head(df6_L)
head(df6_P)
head(df6_T)

#Bar Plot Jenis Kelamin Laki-Laki berdasarkan kelompok umur 
ggplot(df6_L, aes(x = `Kelompok Umur`, y = `Jumlah`, fill = `Jumlah`)) +
  geom_col()+
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kelompok Umur", y="Jumlah Penduduk",
       title="Bar Plot Jumlah Penduduk Laki-Laki berdasarkan kelompok Umur Pada Tahun 2020")+
  geom_text(aes(label = `Jumlah`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 1800000), expand = c(0, 0))

#Bar Plot Jenis Kelamin Perempuan berdasarkan kelompok umur 
ggplot(df6_P, aes(x = `Kelompok Umur`, y = `Jumlah`, fill = `Jumlah`)) +
  geom_col()+
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kelompok Umur", y="Jumlah Penduduk",
       title="Bar Plot Jumlah Penduduk Perempuan berdasarkan kelompok Umur Pada Tahun 2020")+
  geom_text(aes(label = `Jumlah`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 1800000), expand = c(0, 0))

#Bar Plot Jenis Kelamin berdasarkan kelompok umur 
ggplot(df6_T, aes(x = `Kelompok Umur`, y = `Jumlah`, fill = `Jumlah`)) +
  geom_col()+
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Kelompok Umur", y="Jumlah Penduduk",
       title="Bar Plot Jumlah Penduduk Total berdasarkan kelompok Umur Pada Tahun 2020")+
  geom_text(aes(label = `Jumlah`, hjust = -0.1, vjust = 0.3), size = 3, col = 'navy') +  
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle= 0, hjust=0.5, vjust=1, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=0, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1))+
  scale_y_continuous(limits = c(0, 3500000), expand = c(0, 0))


# Time Series Kepadatan Penduduk ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Kepadatan%20Penduduk.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Kepadatan Penduduk.xlsx", mode = "wb")


#read Data
df7 = read.xlsx("Kepadatan Penduduk.xlsx", 1)
head(df7)

#Cleaning data
colnames(df7) = df7[1,]
df7 = df7 %>% slice(2:40)
head(df7)


df7_j = df7[df7$`Kabupaten/ Kota` == "Jawa Timur", ]
head(df7_j)

#transpose data frame
df7_j = cbind(df7_j[1:1], stack(df7_j[2:6]))
colnames(df7_j) = c("Provinsi", "Kepadatan Penduduk", "Tahun")
head(df7_j)

#Time Series Jawa Timur
ggplot(df7_j, aes(x = Tahun, y = `Kepadatan Penduduk`, 
                 group = `Provinsi`, color = `Provinsi`)) +
  geom_line()+
  labs(x="Tahun", y="Kepadatan Penduduk",
       title="Time Series Kepadatan Penduduk di Provinsi Jawa Timur",
       subtitle="Tahun 2014 - 2018") +
  theme(axis.title = element_text(colour = '#8E0152'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#35978F')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#8E0152", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#35978F")) +
  geom_point()+
  geom_text(data = df7_j, aes(y = `Kepadatan Penduduk`, label = sprintf("%0.0f", round((`Kepadatan Penduduk`), digits = 2)), vjust=1.2, hjust=0.5))+
  theme(legend.position = "none")

# Violin Plot Rumah Tangga ---------------------------------------------------------------------------------------------------------------

library(vioplot)

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Jumlah%20Rumah%20Tangga.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Jumlah Rumah Tangga.xlsx", mode = "wb")

#read Data
dfrt = read.xlsx("Jumlah Rumah Tangga.xlsx", 1)
head(dfrt)

colnames(dfrt) = dfrt[1,]
dfrt = dfrt %>% slice(2:39)
dfrt$`2015` = as.numeric(as.character(dfrt$`2015`))
head(dfrt)

vioplot(dfrt$`2015`,
        ylab = 'Jumlah Rumah Tangga',
        col = '#F7C45E',
        col.axis = '#FA9F48',
        col.lab = "#8C2F00",
        frame.plot = F,
        cex.axis = 0.8,
        rectCol = "#C01F00", 
        lineCol = "black",
        colMed = "black", 
        border = "black",
        pchMed = 16,
        plotCentre = "point")
title("Violin Plot Jumlah Rumah Tangga Kabupaten/ Kota di Provinsi Jawa Timur",
      adj  = 0.5, line = 1.5,
      col.main = '#8C2F00')
mtext(" Tahun 2015 ", adj = 0.5, line = 0,
      col = '#FA9F48')
axis(1, col.axis = '#FA9F48', col = '#843400', cex.axis = 0.8, labels =F)
axis(2, col.axis = '#FA9F48', col = '#843400', cex.axis = 0.8, labels = F)

#Statistika Destkriptif
summary(dfrt$`2015`)

# Regresi migrasi keluar dan jumlah penduduk ---------------------------------------------------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Migrasi%20Penduduk.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Migrasi Penduduk.xlsx", mode = "wb")


#migrasi keluar
df3 = read.xlsx("Migrasi Penduduk.xlsx", 1)
head(df3)
colnames(df3) = df3[1,]
df3 = df3 %>% slice(2:39)
df3$`Migrasi Keluar` <- as.numeric(as.character(df3$`Migrasi Keluar`))
head(df3)


#Jumlah Penduduk

url <- "https://raw.githubusercontent.com/farrayaa/Final-Project/main/Exploration%20Data%20Analysis/Dataset/Jumlah%20Penduduk%202010%20-%202015.xlsx"
# Baca data langsung dari GitHub
download.file(url, destfile = "Jumlah Penduduk 2010 - 2015.xlsx", mode = "wb")


df8 = read.xlsx("Jumlah Penduduk 2010 - 2015.xlsx", 1)
head(df8)

colnames(df8) = df8[1,]
df8 = cbind(df8[1:4])
df8 = df8 %>% slice(2:39)
head(df8)

#bind column
df3$`Jumlah Penduduk` = df8$`2015`
dfscatter = df3
head(dfscatter)

par(mfrow=c(1,2))
#regresi robust 
library(MASS)
olsm = lm(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`)
rr.huber <- rlm(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`, method = "M",
                psi = psi.huber, maxit=50)
summary(rr.huber)
plot(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`)
abline(rr.huber)
abline(olsm, col='red')

#remove outlier
dfscatter = dfscatter[order(dfscatter$`Migrasi Keluar`, decreasing = FALSE),]
dfscatter = dfscatter %>% slice(1:37)
head(dfscatter)

#Regresi robust fiks
olsm = lm(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`)
rr.huber <- rlm(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`, method = "M",
                psi = psi.huber, maxit=50)
summary(rr.huber)
plot(dfscatter$`Jumlah Penduduk` ~ dfscatter$`Migrasi Keluar`)
abline(rr.huber)
abline(olsm, col='red')

dev.off()

#Visualisasi
ggplot(dfscatter, aes(`Migrasi Keluar`, `Jumlah Penduduk`, fill =`Jumlah Penduduk`)) +
  geom_jitter() +
  geom_smooth(formula = y ~ x, method="rlm", se=F) + 
  labs(x="Migrasi Keluar", y="Jumlah Penduduk",
       title="Scatter Plot Hubungan Antara Migrasi Keluar dengan Jumlah Penduduk",
       subtitle="Kabupaten/ Kota di Provinsi Jawa Timur 2015") +
  theme(axis.title = element_text(colour = '#449559'))+
  theme(axis.text.x=element_text(angle=0, hjust=0, vjust=1.5, colour = '#BE2A3E')) +
  theme(axis.text.y=element_text(angle=0, hjust=0, vjust=1.5, colour = '#BE2A3E')) +
  theme(plot.title=element_text(size=15, hjust=0.5, face="bold", colour="#22763F", vjust=1)) +
  theme(plot.subtitle=element_text(size=12, hjust=0.5, face="bold", color="#E36148"))+
  theme(legend.position = "none")

#Korelasi 
cor.test(x=dfscatter$`Migrasi Keluar`, y=dfscatter$`Jumlah Penduduk`, method = 'pearson')
