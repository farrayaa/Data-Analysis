# Unduh repository dari GitHub
download.file("https://github.com/farrayaa/Final-Project/archive/main.zip", "Final-Project.zip")

# Ekstrak file ZIP
unzip("Final-Project.zip")

# Set direktori kerja ke dalam folder "STROKE DASHBOARD"
setwd("Final-Project-main/STROKE DASHBOARD")

# Jalankan aplikasi
library(shiny)
runApp()
