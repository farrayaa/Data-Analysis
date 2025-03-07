server <- function(input, output, session) {
  # Informasi Data
  output$data_table <- renderDataTable({
    datatable(data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_types <- renderDataTable({
    data.frame(
      Variabel = names(data),
      Tipe = data_types
    )
  })
  
  # Menampilkan pemilihan variabel berdasarkan var_type
  observe({
    var_type <- input$var_type
    
    if (!is.null(var_type)) {
      if (var_type == "numeric") {
        choices <- names(data[sapply(data, is.numeric)])
      } else if (var_type == "factor") {
        choices <- names(data[sapply(data, is.factor)])
      }
      
      output$var_select_ui <- renderUI({
        selectInput("variable", "Pilih variabel:", choices = choices, selected = choices[1])
      })
    }
  })
  
  # Perbarui acara tombol untuk tab statistik deskriptif
  observeEvent(input$update, {
    var <- input$variable
    
    output$summaryTable <- renderReactable({
      req(var)
      if (is.numeric(data[[var]])) {
        summary <- as.data.frame(t(summary(data[[var]])))
        reactable(summary, striped = TRUE, highlight = TRUE)
      }
    })
    
    output$modeTable <- renderReactable({
      req(var)
      if (is.factor(data[[var]])) {
        summary <- as.data.frame(table(data[[var]]))
        colnames(summary) <- c("Nilai", "Jumlah")
        summary$Proporsi <- summary$Jumlah / sum(summary$Jumlah)
        total_count <- sum(summary$Jumlah)
        mode_val <- summary$Nilai[which.max(summary$Jumlah)]
        summary$Mode <- ifelse(summary$Nilai == mode_val, "Mode", "")
        summary <- rbind(summary, c("Total", total_count, 1, ""))
        reactable(summary, striped = TRUE, highlight = TRUE, columns = list(
          Proporsi = colDef(format = colFormat(percent = TRUE, digits = 2)),
          Mode = colDef(html = TRUE, align = "center")
        ))
      }
    })
  })
  
  # Perbarui pilihan variabel numerik dan kategorikal untuk grouped descriptive statistics
  observe({
    var_type <- input$group_var_type
    if (!is.null(var_type)) {
      if (var_type == "numeric") {
        choices <- names(data[sapply(data, is.numeric)])
      } else if (var_type == "factor") {
        choices <- names(data[sapply(data, is.factor)])
      }
      
      output$group_var_ui <- renderUI({
        selectInput("group_var", "Pilih variabel:", choices = choices, selected = choices[1])
      })
    }
  })
  
  observe({
    updateSelectInput(session, "group_cat_var", choices = names(data[sapply(data, is.factor)]))
  })
  
  observeEvent(input$update_grouped, {
    group_var <- input$group_var
    group_cat_var <- input$group_cat_var
    
    output$groupedSummaryTable <- renderReactable({
      req(group_var, group_cat_var)
      
      if (is.numeric(data[[group_var]])) {
        grouped_summary <- data %>%
          group_by(.data[[group_cat_var]]) %>%
          summarize(
            Mean = mean(.data[[group_var]], na.rm = TRUE),
            SD = sd(.data[[group_var]], na.rm = TRUE),
            Min = min(.data[[group_var]], na.rm = TRUE),
            Q1 = quantile(.data[[group_var]], 0.25, na.rm = TRUE),
            Median = median(.data[[group_var]], na.rm = TRUE),
            Q3 = quantile(.data[[group_var]], 0.75, na.rm = TRUE),
            Max = max(.data[[group_var]], na.rm = TRUE)
          )
      } else {
        grouped_summary <- data %>%
          group_by(.data[[group_cat_var]], .data[[group_var]]) %>%
          summarize(
            Count = n()
          ) %>%
          ungroup() %>%
          group_by(.data[[group_cat_var]]) %>%
          mutate(Percentage = Count / sum(Count))
      }
      
      reactable(grouped_summary, striped = TRUE, highlight = TRUE)
    })
  })
  
  # Perbarui pilihan variabel numerik dan kategorikal
  observe({
    updateSelectInput(session, "num_variable", choices = names(data[sapply(data, is.numeric)]))
    updateSelectInput(session, "cat_variable", choices = names(data[sapply(data, is.factor)]))
    updateSelectInput(session, "cat_variable1", choices = names(data[sapply(data, is.factor)]))
    updateSelectInput(session, "cat_variable2", choices = names(data[sapply(data, is.factor)]))
  })
  
  # Variabel numerik untuk visualisasi
  observe({
    updateCheckboxGroupInput(session, "num_vars", choices = names(data[sapply(data, is.numeric)]))
  })
  
  observe({
    updateCheckboxGroupInput(session, "cat_vars", choices = names(data[sapply(data, is.factor)]))
  })
  
  # Fungsi Pilih Semua untuk variabel numerik
  observeEvent(input$select_all_num, {
    if (input$select_all_num) {
      updateCheckboxGroupInput(session, "num_vars", selected = names(data[sapply(data, is.numeric)]))
    } else {
      updateCheckboxGroupInput(session, "num_vars", selected = character(0))
    }
  })
  
  # Fungsi Pilih Semua untuk variabel kategorikal
  observeEvent(input$select_all_cat, {
    if (input$select_all_cat) {
      updateCheckboxGroupInput(session, "cat_vars", selected = names(data[sapply(data, is.factor)]))
    } else {
      updateCheckboxGroupInput(session, "cat_vars", selected = character(0))
    }
  })
  
  observeEvent(input$update_num, {
    selected_vars <- input$num_vars
    
    output$numerical_plots <- renderUI({
      req(selected_vars)
      plot_output_list <- lapply(selected_vars, function(var) {
        list(
          box(title = paste("Histogram dari", var), status = "primary", solidHeader = TRUE, width = 6,
              plotOutput(outputId = paste("hist_", var, sep = ""))),
          box(title = paste("Boxplot dari", var), status = "primary", solidHeader = TRUE, width = 6,
              plotOutput(outputId = paste("box_", var, sep = "")))
        )
      })
      do.call(tagList, unlist(plot_output_list, recursive = FALSE))
    })
    
    for (var in selected_vars) {
      local({
        local_var <- var
        output[[paste("hist_", local_var, sep = "")]] <- renderPlot({
          color <- brewer.pal(8, "Set2")[1]
          ggplot(data, aes_string(x = local_var)) +
            geom_histogram(binwidth = 1, fill = color, color = "white", aes(y = ..density..)) +
            geom_density(color = "black") +
            theme_minimal()
        })
        output[[paste("box_", local_var, sep = "")]] <- renderPlot({
          ggplot(data, aes_string(y = local_var)) + geom_boxplot(fill = brewer.pal(8, "Set2")[2]) + theme_minimal()
        })
      })
    }
  })
  
  observeEvent(input$update_cat, {
    selected_vars <- input$cat_vars
    
    output$categorical_plots <- renderUI({
      req(selected_vars)
      plot_output_list <- lapply(selected_vars, function(var) {
        list(
          box(title = paste("Bar Chart dari", var), status = "primary", solidHeader = TRUE, width = 6,
              plotOutput(outputId = paste("bar_", var, sep = ""))),
          box(title = paste("Pie Chart dari", var), status = "primary", solidHeader = TRUE, width = 6,
              plotOutput(outputId = paste("pie_", var, sep = "")))
        )
      })
      do.call(tagList, unlist(plot_output_list, recursive = FALSE))
    })
    
    for (var in selected_vars) {
      local({
        local_var <- var
        output[[paste("bar_", local_var, sep = "")]] <- renderPlot({
          ggplot(data, aes_string(x = local_var, fill = local_var)) + 
            geom_bar() + 
            scale_fill_brewer(palette = "Set2") + 
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            theme_minimal()
        })
        output[[paste("pie_", local_var, sep = "")]] <- renderPlot({
          pie_data <- data.frame(table(data[[local_var]]))
          ggplot(pie_data, aes(x = "", y = Freq, fill = Var1)) + 
            geom_bar(stat = "identity", width = 1) + 
            coord_polar("y") + 
            scale_fill_brewer(palette = "Set2") + 
            theme_minimal() +
            geom_text(aes(label = scales::percent(Freq/sum(Freq))), position = position_stack(vjust = 0.5))
        })
      })
    }
  })
  
  observeEvent(input$update_num, {
    num_var <- input$num_variable
    cat_var <- input$cat_variable
    
    output$multiHistPlot <- renderPlot({
      req(num_var, cat_var)
      color <- brewer.pal(8, "Set2")[1]
      ggplot(data, aes_string(x = num_var, fill = cat_var)) + 
        geom_histogram(binwidth = 1, position = "dodge", aes(y = ..density..)) + 
        geom_density(color = "black", fill = NA) +
        scale_fill_brewer(palette = "Set2") + 
        theme_minimal()
    })
    
    output$multiBoxPlot <- renderPlot({
      req(num_var, cat_var)
      ggplot(data, aes_string(x = cat_var, y = num_var, fill = cat_var)) + 
        geom_boxplot() + 
        scale_fill_brewer(palette = "Set2") + 
        theme_minimal()
    })
  })
  
  observeEvent(input$update_cat, {
    cat_var1 <- input$cat_variable1
    cat_var2 <- input$cat_variable2
    
    output$multiBarPlot <- renderPlot({
      req(cat_var1, cat_var2)
      ggplot(data, aes_string(x = cat_var1, fill = cat_var2)) + 
        geom_bar(position = "dodge") + 
        scale_fill_brewer(palette = "Set2") + 
        geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9)) +
        theme_minimal()
    })
    
    output$multiPieChart <- renderPlot({
      req(cat_var1, cat_var2)
      pie_data <- data %>%
        count(.data[[cat_var1]], .data[[cat_var2]]) %>%
        group_by(.data[[cat_var1]]) %>%
        mutate(Percentage = n / sum(n))
      
      ggplot(pie_data, aes(x = "", y = Percentage, fill = .data[[cat_var2]])) + 
        geom_bar(stat = "identity", width = 1) + 
        coord_polar("y") + 
        scale_fill_brewer(palette = "Set2") + 
        theme_minimal() +
        facet_wrap(~.data[[cat_var1]]) +
        geom_text(aes(label = scales::percent(Percentage)), position = position_stack(vjust = 0.5))
    })
  })
  
  # Classification with XGBoost
  output$input_vars_ui <- renderUI({
    req(data)
    input_list <- lapply(names(X_train), function(var) {
      if (is.factor(data[[var]])) {
        label <- switch(var,
                        "gender" = "Pilih Jenis Kelamin",
                        "hypertension" = "Pilih Hipertensi",
                        "heart_disease" = "Pilih Penyakit Jantung",
                        "ever_married" = "Pilih Status Pernikahan",
                        "work_type" = "Pilih Tipe Pekerjaan",
                        "Residence_type" = "Pilih Tempat Tinggal",
                        "smoking_status" = "Pilih Status Merokok",
                        paste("Pilih", var))
        choices <- switch(var,
                          "gender" = c("Laki-laki", "Perempuan"),
                          "hypertension" = c("Tidak", "Ya"),
                          "heart_disease" = c("Tidak", "Ya"),
                          "ever_married" = c("Belum", "Sudah"),
                          "work_type" = c("Anak-anak", "Pemerintah", "Tidak Pernah Bekerja", "Swasta", "Wiraswasta"),
                          "Residence_type" = c("Perkotaan", "Pedesaan"),
                          "smoking_status" = c("Tidak Merokok", "Merokok", "Mantan Perokok", "Tidak Diketahui"),
                          levels(data[[var]]))
        selectInput(var, label, choices = choices)
      } else {
        label <- switch(var,
                        "age" = "Masukkan Umur (tahun)",
                        "avg_glucose_level" = "Masukkan Tingkat Glukosa Rata-rata (mg/dL)",
                        "bmi" = "Masukkan BMI (kg/m²)",
                        paste("Masukkan", var))
        numericInput(var, label, value = median(data[[var]], na.rm = TRUE))
      }
    })
    do.call(tagList, input_list)
  })
  
  observeEvent(input$predict_stroke, {
    req(data)
    
    new_data <- data.frame(matrix(ncol = ncol(X_train), nrow = 1))
    colnames(new_data) <- colnames(X_train)
    
    for (var in colnames(new_data)) {
      if (is.factor(data[[var]])) {
        new_data[[var]] <- factor(input[[var]], levels = levels(data[[var]]))
      } else {
        new_data[[var]] <- as.numeric(input[[var]])
      }
    }
    
    new_data <- xgb.DMatrix(data.matrix(new_data))
    
    prediction <- predict(model, new_data)
    prediction <- ifelse(prediction > 0.5, "Stroke", "Tidak Stroke")
    
    output$prediction_result <- renderPrint({
      paste("Hasil prediksi: ", prediction)
    })
  })
  
  # Perbarui pilihan variabel korelasi
  observe({
    updateSelectInput(session, "corr_vars", choices = names(data[sapply(data, is.numeric)]))
  })
  
  observeEvent(input$update_corr, {
    corr_vars <- input$corr_vars
    
    output$corrPlot <- renderPlot({
      req(corr_vars)
      corr_data <- data[corr_vars]
      corr_matrix <- cor(corr_data, use = "complete.obs")
      
      # Palet warna yang lebih lembut untuk corrplot
      col <- colorRampPalette(c("#ADD8E6", "#FFE8B8", "#F08080"))(200)
      
      corrplot(corr_matrix, method = "color", col = col, 
               type = "full", order = "hclust", 
               tl.col = "black", tl.srt = 45, 
               addCoef.col = "black", # Tambahkan koefisien korelasi
               cl.pos = "b", # Posisi legenda warna
               number.cex = 0.7) # Ukuran teks untuk koefisien korelasi
    })
    
    output$corrTable <- renderReactable({
      req(corr_vars)
      corr_data <- data[corr_vars]
      corr_matrix <- cor(corr_data, use = "complete.obs")
      corr_df <- as.data.frame(as.table(corr_matrix))
      colnames(corr_df) <- c("Var1", "Var2", "Korelasi")
      reactable(corr_df, striped = TRUE, highlight = TRUE)
    })
    
    output$scatterPlotsUI <- renderUI({
      req(corr_vars)
      plot_output_list <- lapply(1:length(corr_vars), function(i) {
        lapply(1:length(corr_vars), function(j) {
          if (i != j) {
            plotname <- paste("scatterPlot", i, j, sep = "_")
            plotOutput(plotname, height = 280, width = 250)
          }
        })
      })
      do.call(tagList, unlist(plot_output_list, recursive = FALSE))
    })
    
    observe({
      req(corr_vars)
      for (i in 1:length(corr_vars)) {
        for (j in 1:length(corr_vars)) {
          local({
            ii <- i
            jj <- j
            if (ii != jj) {
              plotname <- paste("scatterPlot", ii, jj, sep = "_")
              output[[plotname]] <- renderPlot({
                ggplot(data, aes_string(x = corr_vars[ii], y = corr_vars[jj])) + 
                  geom_point() + 
                  geom_smooth(method = "lm", color = "red", se = FALSE) +
                  theme_minimal()
              })
            }
          })
        }
      }
    })
  })
}
