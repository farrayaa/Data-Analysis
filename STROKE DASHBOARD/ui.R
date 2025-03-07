ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Dataset Stroke Kesehatan"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pendahuluan", tabName = "introduction", icon = icon("book")),
      menuItem("Informasi Data", tabName = "data_info", icon = icon("info-circle")),
      menuItem("Statistik Deskriptif", tabName = "descriptive", icon = icon("table"),
               menuSubItem("Univariate", tabName = "desc_basic"),
               menuSubItem("Multivariate", tabName = "desc_grouped")),
      menuItem("Visualisasi Numerik", tabName = "numerical_visualization", icon = icon("chart-bar")),
      menuItem("Visualisasi Kategorikal", tabName = "categorical_visualization", icon = icon("chart-pie")),
      menuItem("Visualisasi Multivariat", tabName = "multivariate_visualization", icon = icon("layer-group")),
      menuItem("Analisis Korelasi", tabName = "correlation_analysis", icon = icon("chart-line")),
      menuItem("Kesimpulan", tabName = "conclusion", icon = icon("clipboard")),
      menuItem("Klasifikasi Stroke", tabName = "classification", icon = icon("heartbeat"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(tabName = "introduction",
              fluidRow(
                box(title = "Analisis Data Kesehatan", status = "primary", solidHeader = TRUE, width = 12,
                    tags$img(src = "dataset-cover.jpg", height = "300px", style = "display: block; margin-left: auto; margin-right: auto;"),
                    h4("Deskripsi Data"),
                    p("Dataset ini mengandung informasi penting mengenai pasien dengan risiko stroke, termasuk variabel-variabel seperti jenis kelamin, hipertensi, penyakit jantung, status pernikahan, jenis pekerjaan, status merokok, dan kejadian stroke. Analisis ini bertujuan untuk mengeksplorasi dan memahami berbagai faktor yang mempengaruhi risiko stroke melalui pendekatan statistik deskriptif, visualisasi data yang menarik, dan analisis korelasi. Dengan demikian, analisis ini dapat membantu dalam identifikasi pola dan hubungan antar variabel yang signifikan, yang pada akhirnya mendukung upaya pencegahan dan pengelolaan stroke berdasarkan profil pasien."),
                    h4("Penulis"),
                    p("Farah Syahfira (5003211080)"),
                    p("Annisa Aulia Fauziah (5003211110)"),
                    p("Asti Pratiwi (5003211120)")
                )
              )
      ),
      # Data Information Tab
      tabItem(tabName = "data_info",
              fluidRow(
                box(title = "Dataset", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("data_table")
                )
              ),
              fluidRow(
                box(title = "Informasi Variabel", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("data_types")
                )
              )
      ),
      # Basic Descriptive Statistics Tab
      tabItem(tabName = "desc_basic",
              fluidRow(
                box(title = "Pilih Tipe Variabel", status = "primary", solidHeader = TRUE,
                    radioButtons("var_type", "Pilih tipe variabel:",
                                 choices = c("Numerik" = "numeric", "Kategorikal" = "factor"),
                                 selected = "numeric"),
                    uiOutput("var_select_ui"),
                    actionButton("update", "Update")
                ),
                box(title = "Statistik Ringkasan", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("summaryTable"),
                    reactableOutput("modeTable")
                )
              )
      ),
      # Grouped Descriptive Statistics Tab
      tabItem(tabName = "desc_grouped",
              fluidRow(
                box(title = "Pilih Variabel", status = "primary", solidHeader = TRUE,
                    selectInput("group_var_type", "Pilih tipe variabel:", choices = c("Numerik" = "numeric", "Kategorikal" = "factor")),
                    uiOutput("group_var_ui")
                ),
                box(title = "Pilih Variabel Kategorikal untuk Pengelompokan", status = "primary", solidHeader = TRUE,
                    selectInput("group_cat_var", "Pilih variabel kategorikal:", choices = NULL),
                    actionButton("update_grouped", "Update")
                )
              ),
              fluidRow(
                box(title = "Statistik Ringkasan Berdasarkan Grup", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("groupedSummaryTable")
                )
              )
      ),
      # Numerical Visualization Tab
      tabItem(tabName = "numerical_visualization",
              fluidRow(
                box(title = "Pilih Variabel Numerik", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxGroupInput("num_vars", "Pilih variabel:",
                                       choices = NULL, inline = TRUE),
                    actionButton("update_num", "Update"),
                    checkboxInput("select_all_num", "Pilih Semua", value = FALSE)
                )
              ),
              fluidRow(
                uiOutput("numerical_plots")
              ),
              fluidRow(
                box(title = "Interpretasi", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Umur"),
                    p("Histogram menunjukkan distribusi usia dengan puncak pada usia sekitar 41 tahun. Boxplot mengungkapkan beberapa outlier di usia lebih tua."),
                    h4("Tingkat Glukosa Darah"),
                    p("Histogram menunjukkan distribusi tingkat glukosa dengan puncak sekitar 91.48 mg/dL. Boxplot menunjukkan adanya outlier pada nilai tinggi glukosa darah."),
                    h4("BMI"),
                    p("Histogram menunjukkan distribusi BMI dengan puncak sekitar 27.80 kg/m². Boxplot menunjukkan beberapa outlier pada nilai BMI tinggi.")
                )
              )
      ),
      # Categorical Visualization Tab
      tabItem(tabName = "categorical_visualization",
              fluidRow(
                box(title = "Pilih Variabel Kategorikal", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxGroupInput("cat_vars", "Pilih variabel:",
                                       choices = NULL, inline = TRUE),
                    actionButton("update_cat", "Update"),
                    checkboxInput("select_all_cat", "Pilih Semua", value = FALSE)
                )
              ),
              fluidRow(
                uiOutput("categorical_plots")
              ),
              fluidRow(
                box(title = "Interpretasi", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Gender"),
                    p("Dataset menunjukkan distribusi gender yang lebih banyak perempuan (59%) dibandingkan laki-laki (41%)."),
                    h4("Hipertensi"),
                    p("Sebagian besar pasien tidak memiliki hipertensi (93%), sementara 7% pasien memiliki hipertensi."),
                    h4("Penyakit Jantung"),
                    p("Sebagian besar pasien tidak memiliki penyakit jantung (96%), sementara 4% memiliki penyakit jantung."),
                    h4("Status Pernikahan"),
                    p("Mayoritas pasien sudah menikah (62%), sedangkan 38% belum menikah."),
                    h4("Tipe Pekerjaan"),
                    p("Pasien paling banyak bekerja di sektor swasta (56.47%), diikuti oleh wiraswasta (15.07%), pekerjaan pemerintah (12.57%), anak-anak (15.39%), dan tidak pernah bekerja (0.50%)."),
                    h4("Tempat Tinggal"),
                    p("Distribusi tempat tinggal menunjukkan hampir seimbang antara penduduk perkotaan (49.2%) dan pedesaan (50.8%)."),
                    h4("Status Merokok"),
                    p("Sebagian besar pasien tidak pernah merokok (36.33%), diikuti oleh mantan perokok (16.10%), perokok aktif (15.21%), dan status merokok tidak diketahui (32.26%)."),
                    h4("Stroke"),
                    p("Sebagian besar pasien tidak mengalami stroke (96%), sementara 4% dari mereka mengalami stroke.")
                )
              )
      ),
      # Multivariate Visualization Tab
      tabItem(tabName = "multivariate_visualization",
              fluidRow(
                box(title = "Pilih Tipe Visualisasi", status = "primary", solidHeader = TRUE, width = 12,
                    radioButtons("viz_type", "Pilih tipe visualisasi:",
                                 choices = c("Numerik" = "numerical", "Kategorikal" = "categorical"),
                                 selected = "numerical")
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.viz_type == 'numerical'",
                  box(title = "Pilih Variabel", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("num_variable", "Pilih variabel numerik:", choices = NULL),
                      selectInput("cat_variable", "Pilih variabel kategorikal:", choices = NULL),
                      actionButton("update_num", "Update")
                  ),
                  box(title = "Histogram berdasarkan Grup", status = "primary", solidHeader = TRUE, width = 6,
                      plotOutput("multiHistPlot")
                  ),
                  box(title = "Boxplot berdasarkan Grup", status = "primary", solidHeader = TRUE, width = 6,
                      plotOutput("multiBoxPlot")
                  )
                ),
                conditionalPanel(
                  condition = "input.viz_type == 'categorical'",
                  box(title = "Pilih Variabel", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("cat_variable1", "Pilih variabel kategorikal pertama:", choices = NULL),
                      selectInput("cat_variable2", "Pilih variabel kategorikal kedua:", choices = NULL),
                      actionButton("update_cat", "Update")
                  ),
                  box(title = "Bar Chart", status = "primary", solidHeader = TRUE, width = 6,
                      plotOutput("multiBarPlot")
                  ),
                  box(title = "Pie Chart", status = "primary", solidHeader = TRUE, width = 6,
                      plotOutput("multiPieChart")
                  )
                )
              ),
              fluidRow(
                box(title = "Interpretasi", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Umur dan Status Merokok"),
                    p("Visualisasi histogram dan boxplot menunjukkan bahwa perokok cenderung lebih tua dibandingkan dengan yang tidak pernah merokok."),
                    h4("Umur dan Status Pernikahan"),
                    p("Individu yang sudah menikah cenderung lebih tua dibandingkan dengan yang belum menikah, sesuai dengan harapan bahwa orang menikah pada usia lebih matang."),
                    h4("Tingkat Glukosa Darah dan Status Pernikahan"),
                    p("Tidak ada perbedaan signifikan dalam tingkat glukosa darah antara individu yang sudah menikah dan yang belum menikah."),
                    h4("BMI dan Status Merokok"),
                    p("Perokok aktif cenderung memiliki BMI yang lebih rendah dibandingkan dengan mereka yang tidak pernah merokok, sementara mantan perokok memiliki distribusi BMI yang mirip dengan yang tidak pernah merokok."),
                    h4("BMI dan Status Pernikahan"),
                    p("Individu yang sudah menikah cenderung memiliki BMI yang lebih tinggi dibandingkan dengan yang belum menikah, mungkin karena perubahan gaya hidup setelah menikah."),
                    h4("Tingkat Glukosa Darah dan Hipertensi"),
                    p("Individu dengan hipertensi cenderung memiliki tingkat glukosa darah yang lebih tinggi, menunjukkan risiko diabetes yang lebih besar."),
                    h4("Tingkat Glukosa Darah dan Penyakit Jantung"),
                    p("Pasien dengan penyakit jantung memiliki tingkat glukosa darah yang lebih tinggi, yang menunjukkan hubungan antara penyakit jantung dan diabetes."),
                    h4("BMI dan Hipertensi"),
                    p("Individu dengan hipertensi cenderung memiliki BMI yang lebih tinggi, menunjukkan hubungan antara obesitas dan tekanan darah tinggi."),
                    h4("BMI dan Penyakit Jantung"),
                    p("Pasien dengan penyakit jantung cenderung memiliki BMI yang lebih tinggi, yang mengindikasikan hubungan antara obesitas dan risiko penyakit jantung.")
                )
              )
      ),
      # Correlation Analysis Tab
      tabItem(tabName = "correlation_analysis",
              fluidRow(
                box(title = "Pilih Variabel untuk Korelasi", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("corr_vars", "Pilih variabel:", choices = NULL, multiple = TRUE),
                    actionButton("update_corr", "Update")
                )
              ),
              fluidRow(
                box(title = "Plot Korelasi", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("corrPlot")
                )
              ),
              fluidRow(
                box(title = "Nilai Korelasi", status = "primary", solidHeader = TRUE, width = 12,
                    reactableOutput("corrTable")
                )
              ),
              fluidRow(
                box(title = "Scatter Plots", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("scatterPlotsUI")
                )
              ),
              fluidRow(
                box(title = "Interpretasi", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Umur dan Tingkat Glukosa Darah"),
                    p("Tidak ada hubungan linier yang signifikan antara umur dan tingkat glukosa darah."),
                    h4("Umur dan BMI"),
                    p("Terdapat hubungan positif sedang antara umur dan BMI, menunjukkan bahwa BMI cenderung meningkat seiring bertambahnya umur."),
                    h4("Tingkat Glukosa Darah dan BMI"),
                    p("Tidak ada hubungan linier yang signifikan antara tingkat glukosa darah dan BMI.")
                )
              )
      ),
      # Conclusion Tab
      tabItem(tabName = "conclusion",
              fluidRow(
                box(title = "Kesimpulan", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Kesimpulan"),
                    p("Analisis dataset pasien dengan risiko stroke ini memberikan wawasan penting mengenai profil demografis dan faktor-faktor risiko yang mungkin berkontribusi terhadap kejadian stroke. Dari hasil statistik deskriptif, visualisasi numerik, dan visualisasi kategorikal, beberapa temuan utama dapat disimpulkan sebagai berikut:"),
                    tags$ul(
                      tags$li("Dataset ini didominasi oleh perempuan (59.5%) dan mayoritas pasien sudah menikah (65.7%)."),
                      tags$li("Distribusi usia menunjukkan populasi yang luas, dengan rata-rata usia 41 tahun dan adanya beberapa outlier di kelompok usia yang lebih tua."),
                      tags$li("Sebagian besar pasien tidak memiliki hipertensi (91.6%) dan penyakit jantung (96.0%), namun segmen kecil dengan kondisi ini tetap relevan."),
                      tags$li("Tingkat glukosa darah rata-rata berada pada 91.48 mg/dL, dengan beberapa pasien memiliki kadar yang sangat tinggi, yang mungkin mengindikasikan risiko diabetes atau pradiabetes."),
                      tags$li("BMI rata-rata adalah 27.80 kg/m², menunjukkan adanya prevalensi obesitas dalam populasi pasien."),
                      tags$li("Sebagian besar pasien tidak pernah merokok (58.6%), namun ada segmen signifikan yang merupakan mantan perokok (19.2%) dan perokok aktif (17.6%)."),
                      tags$li("Mayoritas pasien bekerja di sektor swasta (58.2%), dengan distribusi tempat tinggal yang hampir seimbang antara daerah perkotaan dan pedesaan."),
                      tags$li("Sebagian besar pasien dalam dataset ini tidak mengalami stroke (93.8%), namun analisis lebih lanjut diperlukan untuk memahami faktor-faktor yang berkontribusi terhadap kejadian stroke pada 6.2% pasien yang mengalami stroke.")
                    ),
                    p("Temuan ini menunjukkan bahwa faktor-faktor risiko stroke mungkin lebih kompleks dan memerlukan analisis lebih lanjut. Analisis ini memberikan wawasan awal yang penting dalam memahami hubungan antar variabel dalam dataset pasien dengan risiko stroke dan dapat membantu dalam pencegahan dan pengelolaan risiko stroke secara lebih efektif.")
                )
              )
      ),
      # Classification Tab
      tabItem(tabName = "classification",
              fluidRow(
                box(title = "Masukkan Variabel untuk Prediksi", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("input_vars_ui"),
                    actionButton("predict_stroke", "Prediksi Stroke")
                )
              ),
              fluidRow(
                box(title = "Hasil Prediksi", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("prediction_result")
                )
              )
      )
    )
  )
)
