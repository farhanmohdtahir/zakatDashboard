library(shiny)

shinyServer(
  function(input, output) {
    
    
    mydata <- reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      all_data <-read.csv(inFile$datapath, header = input$header)
      return(all_data)
    })
    
    shahalam_var <- reactive({
      all_data <- mydata()
      
      shahalam <- all_data[all_data$Kampus == 'UiTM Shah Alam', ]  
      
      chosen_shahalam_table <- table(shahalam$Kategori.Asnaf, shahalam$Fakulti)  
      
      shahalam_zero_removed <- chosen_shahalam_table[, colSums(chosen_shahalam_table != 0) > 0]   
      #removing unnecessary faculty data, which are the fac that there is no student get zakat. zero value.
      
      
      shahalam_transpose <- t(shahalam_zero_removed)    
      #in order to make this data applicable to be used in plotly, we should transposing the data, 
      #row change to column and column change to row. This is the command
      
      shahalam_dataframe <- as.data.frame.matrix(shahalam_transpose) #data in form of data frame 
      
      shahalam_final_data <- data.frame(Fakulti = row.names(shahalam_dataframe), shahalam_dataframe)
      # the final data that can be used for plotly, but, there are extra column in this data, which is really unnecessary
      
      rownames(shahalam_final_data) <- c()  #remove rownames/ top left column
      
      shahalam_plot <- plot_ly(shahalam_final_data, x = ~Fakulti, y = ~FAKIR, type = 'bar', name = 'Fakir') %>%
        add_trace(y = ~FISABILILLAH, name = 'Fisabilillah') %>%
        add_trace(y = ~MISKIN, name = 'Miskin') %>%
        layout(yaxis = list(title = 'Student'), barmode = 'stack',
               title='Zakat Distribution for Each Faculty in UiTM Shah Alam',margin = list (b=150))
      
      return(shahalam_plot)
    })
    
    puncakalam_var <- reactive({
      all_data <- mydata()
      
      puncakalam <- all_data[all_data$Kampus == 'UiTM Kampus Puncak Alam', ]  
      
      chosen_puncakalam_table <- table(puncakalam$Kategori.Asnaf, puncakalam$Fakulti)  
      
      puncakalam_zero_removed <- chosen_puncakalam_table[, colSums(chosen_puncakalam_table != 0) > 0]    
      
      puncakalam_transpose <- t(puncakalam_zero_removed)    
      
      puncakalam_dataframe <- as.data.frame.matrix(puncakalam_transpose) 
      
      puncakalam_final_data <- data.frame(Fakulti = row.names(puncakalam_dataframe), puncakalam_dataframe)
      
      rownames(puncakalam_final_data) <- c()  
      
      puncakalam_plot <- plot_ly(puncakalam_final_data, x = ~Fakulti, y = ~FAKIR, type = 'bar', name = 'Fakir') %>%
        add_trace(y = ~FISABILILLAH, name = 'Fisabilillah') %>%
        add_trace(y = ~MISKIN, name = 'Miskin') %>%
        layout(yaxis = list(title = 'Student'), barmode = 'stack',
               title='Zakat Distribution for Each Faculty in UiTM Puncak Alam',margin = list (b=150))
      
      return(puncakalam_plot)
    })
    
    puncakperdana_var <- reactive({
      all_data <- mydata()
      
      puncakperdana <- all_data[all_data$Kampus == 'UiTM Kampus Puncak Perdana', ]  
      
      chosen_puncakperdana_table <- table(puncakperdana$Kategori.Asnaf, puncakperdana$Fakulti)  
      
      puncakperdana_zero_removed <- chosen_puncakperdana_table[, colSums(chosen_puncakperdana_table != 0) > 0]    
      
      puncakperdana_transpose <- t(puncakperdana_zero_removed)    
      
      puncakperdana_dataframe <- as.data.frame.matrix(puncakperdana_transpose) 
      
      puncakperdana_final_data <- data.frame(Fakulti = row.names(puncakperdana_dataframe), puncakperdana_dataframe)
      
      rownames(puncakperdana_final_data) <- c()
      
      puncakperdana_plot <- plot_ly(puncakperdana_final_data, x = ~Fakulti, y = ~FAKIR, type = 'bar', name = 'Fakir') %>%
        add_trace(y = ~FISABILILLAH, name = 'Fisabilillah') %>%
        add_trace(y = ~MISKIN, name = 'Miskin') %>%
        layout(yaxis = list(title = 'Student'), barmode = 'stack',
               title='Zakat Distribution for Each Faculty in UiTM Puncak Perdana')
      
      return(puncakperdana_plot)
    })
    
    sgbuloh_var <- reactive({
      all_data <- mydata()
      
      sgbuloh <- all_data[all_data$Kampus == 'UiTM Kampus Hospital Sg. Buloh', ]  
      
      chosen_sgbuloh_table <- table(sgbuloh$Kategori.Asnaf, sgbuloh$Fakulti)  
      
      sgbuloh_zero_removed <- chosen_sgbuloh_table[, colSums(chosen_sgbuloh_table != 0) > 0]    
      
      sgbuloh_transpose <- t(sgbuloh_zero_removed)    
      
      sgbuloh_dataframe <- as.data.frame.matrix(sgbuloh_transpose) 
      
      sgbuloh_final_data <- data.frame(Fakulti = row.names(sgbuloh_dataframe), sgbuloh_dataframe)
      
      rownames(sgbuloh_final_data) <- c()  
      
      sgbuloh_plot <- plot_ly(sgbuloh_final_data, x = ~Fakulti, y = ~FAKIR, type = 'bar', name = 'Fakir') %>%
        add_trace(y = ~FISABILILLAH, name = 'Fisabilillah') %>%
        add_trace(y = ~MISKIN, name = 'Miskin') %>%
        layout(yaxis = list(title = 'Student'), barmode = 'stack',
               title = 'Zakat Distribution for Each Faculty in UiTM Sungai Buloh')
    
      return(sgbuloh_plot)
      })
    output$contents <-renderTable(mydata())
    
    output$student <- renderValueBox({
      all_data <- mydata()
      Nama<-table(all_data$Nama)
      valueBox(577, "Number of students who received zakat", icon = icon("group"), color = "blue")
    })
    
   
    
    output$plot <- renderPlotly({
      all_data <-mydata()
      chosen_asnaf<-table(all_data$Kategori.Asnaf) #save data as table  
      chosen_asnaf_dataframe<-as.data.frame(chosen_asnaf)    #convert data from table to dataframe 
      library(plotly)
      chosen_asnaf_pie <- plot_ly(chosen_asnaf_dataframe, labels = ~Var1, values = ~Freq, type = 'pie',
                                  textposition = 'inside', textinfo = 'label+percent',
                                  insidetextfont = list(color = '#FFFFFF')) %>%
        layout(title = 'Zakat Distribution based on Category of Asnaf', margin = list (b=30), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      chosen_asnaf_pie
    })
    
    output$plot0 <- renderPlotly({
      all_data <-mydata() 
      
      chosen_kampus<-table(all_data$Kampus)
      chosen_kampus_dataframe<-as.data.frame(chosen_kampus)
      library(plotly)
      chosen_kampus_pie <- plot_ly(chosen_kampus_dataframe, labels = ~Var1, values = ~Freq, type = 'pie',
                                   textposition = 'inside', textinfo = 'label+percent',
                                   insidetextfont = list(color = '#FFFFFF')) %>%
        layout(title = 'Zakat Distribution Based on Campus',margin = list (b=30),xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      chosen_kampus_pie
    })
    
    
    output$plot1 <- renderPlotly({
      all_data <-mydata()   
      
      bycampus_table <- table(all_data$Kampus, all_data$Kategori.Asnaf)     
      
      a <- as.data.frame.matrix(bycampus_table)       #change from table mode kepada dataframe mode  
      
      bycampus_dataframe <- data.frame(Kampus = row.names(a), a)    #make rownames in a dataframe as an value data    
      
      rownames(bycampus_dataframe) <- c()       #remove unnecessary rownames
      
      plot_ly(bycampus_dataframe, x = ~Kampus, y = ~FAKIR, type = 'bar', name = 'Fakir') %>%
        add_trace(y = ~FISABILILLAH, name = 'Fisabilillah') %>%
        add_trace(y = ~MISKIN, name = 'Miskin') %>%
        layout(yaxis = list(title = 'Student'), barmode = 'group',title='Zakat Distribution 
               Based on Each Campus and Category of Asnaf',margin = list (b=100))
      
    })
    
    
    stacked_plot <- reactive({
      shahalam_plot <- shahalam_var()
      puncakalam_plot <- puncakalam_var()
      puncakperdana_plot <- puncakperdana_var()
      sgbuloh_plot <- sgbuloh_var()
      
      switch(input$dist,
             p2 = shahalam_plot,
             p3 = puncakalam_plot,
             p4 = puncakperdana_plot,
             p5 = sgbuloh_plot,
             q)
    })
    
    output$plot2 <- renderPlotly({
      stacked_plot()     #call stacked_plot switch yang kita dah declare atas
    })
    
    
    
    output$report <- downloadHandler(
      
      filename = "report.html",
      content = function(file) {
        
        tempReport  <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(n = input$slider)
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    
    
  }
)