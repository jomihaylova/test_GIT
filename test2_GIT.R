server_v1 = function(input, output, session){
  
  #  if (!interactive()) {
  #    session$onSessionEnded(function() {
  #      stopApp()
  #      q("no")
  #    })
  #  }
  
####################################################################################################
#### text RIMA overview and guidelines for use
  
  output$text_rima1 <- renderText({
    paste("What is RIMA")
  })
  
  output$text_rima2 <- renderText({
    paste("RIMA is a quantitative approach that enables rigorous analysis of how households cope with shocks and stressors.
           Comparisons can be made between different types of households (for example, male-headed versus female-headed or urban versus rural) in a given country or area. 
           Resilience analysis using RIMA provides the necessary evidence to more effectively design, deliver, monitor and evaluate assistance to vulnerable populations, based on what they need most.")
  })
  
  output$text_rima3 <- renderText({
    paste("How to use this app to calculate Resilience Capacity Index?")
  }) 
  
  output$text_rima4 <- renderText({
    paste("1  - Ensure that your dataset is properly cleaned. <br/>", 
          "2  - Click on browse to upload the dataset. In case there are multiple datasets, select the one you want to use. <br/>",
          "3  - Select the dataset format under the file format menu (STATA, SPSS, EXCEL, CSV/TXT). If the selected dataset is in excel format, the app will ask you to specify the sheet's name where data are stored. If the dataset is in CSV/TXT format, you need to specify the field separator. <br/>",
          "4  - Your dataset should have variable names in the first row of the table. If this is the case, select yes for the variable name option. If the dataset doesn't have variables names, select no so that the app will generate variables names.  <br/>",
          "5  - Click now on validate. The first six rows of your dataset should be displayed in the input data panel. <br/>",
          "6  - Now, you can proceed with the selection of variables under each pillar. <br/>",
          "7  - You should select at least 3 variables for each pillar: ABS, AST, SSN, and AC.<br/>",
          "8  - For food security indicators, you should select 2 or 3 variables.<br/>",
          "9 - For profile variables, you can select up to 3 variables. <br/>",
          "10 - You can now navigate on the different panels to see the output.")
  })
  
  
  ####################################################################################################
  # Load data and select
  output$selectfile <- renderUI({
    list(hr(), 
         selectInput("Select", "Select the file you want to analyze", choices=input$inputfiles$name)
    )
  })
  mon_fichier=reactive(input$inputfiles$datapath[input$inputfiles$name==input$Select])
  data = reactive(switch(input$extend,
                         txt=list(element=fluidRow(column(6,textInput('sep', "Separator", placeholder ="tab", value = '')),
                                                   column(4,radioButtons('header', "Variables name",c("Yes"=T,"No"=F)))
                         ),
                         df=eventReactive(input$valid_data,read.delim(mon_fichier(),sep = input$sep, header = input$header==T))
                         ),
                         spss=list(element=fluidRow(#column(), #col_select
                         ),
                         df=eventReactive(input$valid_data, read_sav(mon_fichier()))
                         ),
                         stata=list(element=fluidRow(#column(),#col_select
                         ),
                         df=eventReactive(input$valid_data,read_stata(mon_fichier()))
                         ),
                         excel=list(element=fluidRow(column(6,textInput('sheet', "Sheet name", placeholder = "Feuille1", value = "Sheet1")),
                                                     column(5,radioButtons('col_names', "Variables names",c("Yes"=T,"No"=F)))
                         ),
                         df=eventReactive(input$valid_data,{
                           df0=read_excel(mon_fichier(), sheet = input$sheet, col_names=T)
                           if(input$col_names==F) colnames(df0)=paste0("var_",1:ncol(df0))
                           if(input$col_names==T) colnames(df0)=colnames(read_excel(mon_fichier(), sheet = input$sheet, col_names = T ))
                           return(df0)
                         })
                         )))
  output$agruments_donnees=renderUI(data()$element)
  output$result=renderTable(head(df2()))
  
  ####################################################################################################
  # browser() var
  aa=reactive(names(df2()))
  
  output$sel_var=renderUI(
    wellPanel("Select variables",
              selectInput("absvar", "Select ABS variables",choices = NULL, multiple = T),
              selectInput("astvar", "Select AST variables",choices = NULL, multiple = T),
              selectInput("acvar", "Select AC variables",choices = NULL, multiple = T),
              selectInput("ssnvar", "Select SSN variables",choices = NULL, multiple = T),
              selectInput("fsvar", "Select FS variables",choices = NULL, multiple = T), 
              selectInput("profile", "Select profile variables",choices = aa(), multiple = T), 
              style = "background: teal")
  )
  sel_abs=reactive(setdiff(input$absvar,c(input$astvar,input$acvar,input$ssnvar,input$fsvar)))
  absvar_choices=reactive(setdiff(aa(),c(input$astvar,input$acvar,input$ssnvar,input$fsvar)))
  observeEvent(absvar_choices(), {
    choices=absvar_choices()
    updateSelectInput(session, "absvar", choices = choices, selected = sel_abs())
  })
  
  sel_ast=reactive(setdiff(input$astvar,c(input$absvar,input$acvar,input$ssnvar,input$fsvar)))
  astvar_choices=reactive(setdiff(aa(),c(input$absvar,input$acvar,input$ssnvar,input$fsvar)))
  observeEvent(astvar_choices(), {
    choices = astvar_choices()
    updateSelectInput(session, "astvar", choices = choices, selected = sel_ast())
  })
  
  sel_ac=reactive(setdiff(input$acvar,c(input$absvar,input$astvar,input$ssnvar,input$fsvar)))
  acvar_choices=reactive(setdiff(aa(),c(input$astvar,input$absvar,input$ssnvar,input$fsvar)))
  observeEvent(acvar_choices(), {
    choices = acvar_choices()
    updateSelectInput(session, "acvar", choices = choices, selected = sel_ac())
  })
  
  sel_ssn=reactive(setdiff(input$ssnvar,c(input$absvar,input$astvar,input$acvar,input$fsvar)))
  ssnvar_choices=reactive(setdiff(aa(),c(input$astvar,input$acvar,input$absvar,input$fsvar)))
  observeEvent(ssnvar_choices(), {
    choices = ssnvar_choices()
    updateSelectInput(session, "ssnvar", choices = choices, selected = sel_ssn())
  })
  
  sel_fs=reactive(setdiff(input$fsvar,c(input$absvar,input$astvar,input$acvar,input$ssnvar)))
  fsvar_choices=reactive(setdiff(aa(),c(input$astvar,input$acvar,input$absvar,input$ssnvar)))
  observeEvent(fsvar_choices(), {
    choices = fsvar_choices()
    updateSelectInput(session, "fsvar", choices = choices, selected = sel_fs())
  })
  
  ####################################################################################################
  # Evaluation / Statistic table
  df1=reactive(data()$df()[!apply(data()$df(), 2, function(x) all(is.na(x)) | all(x==""))])
  df2=reactive(df1()[complete.cases(df1()), ])
  
  data_sum=reactive (cbind(df2()[, input$absvar] , df2()[input$astvar] ,df2()[input$acvar], df2()[input$ssnvar]))
  
  sum_data=reactive (t(pastecs:::stat.desc(data_sum())))
  
  vars.out=reactive({
    req(input$absvar)
    stat<- subset(sum_data(), select=-c(range, sum, CI.mean.0.95, var, std.dev, coef.var))
    colnames(stat) <- c("N_obs", "N_null", "N_na", "Min", "Max", "Median","Mean", "SE.mean")
    stat
  })
  
  output$sum_stat=renderTable(vars.out(),rownames = TRUE)
  
  
  ####################################################################################################
  ###### Pillars and variables
  
  # Create subset of variables for each pillar ++++++++++++++++++++++++++++
  absvar = reactive(df2()[, input$absvar])
  astvar = reactive(df2()[, input$astvar])
  acvar  = reactive(df2()[, input$acvar])
  ssnvar = reactive(df2()[, input$ssnvar])
  
  # Correlation between variables under pillar ++++++++++++++++++++++++++++
  plot_cor_under <- function(req,subset,name){
    req(req)
    return(corrplot(cor(subset),
             method=("color"),
             mar=c(0,0,1,0),
             cl.ratio=0.3,
             addCoef.col = "black",
             title = paste("Correlation between variables under", name, "pillar")))
  }
  
  output$sum_abs=renderPlot(plot_cor_under(req = input$absvar, subset = absvar(), name = "ABS"))
  output$sum_ast=renderPlot(plot_cor_under(req = input$astvar, subset = astvar(), name = "AST"))
  output$sum_ac =renderPlot(plot_cor_under(req = input$acvar, subset = acvar(), name = "AC"))
  output$sum_ssn=renderPlot(plot_cor_under(req = input$ssnvar, subset = ssnvar(), name = "SSN"))
  
  output$downloadVC <- downloadHandler(
    filename = function() {
      "Variable_correlation.pdf"
    },
    content = function(file) {
      pdf(file)
      #Cor between var
      arrangeGrob(print(plot_cor_under(req = input$absvar, subset = absvar(), name = "ABS")),
                  print(plot_cor_under(req = input$astvar, subset = astvar(), name = "AST")), 
                  print(plot_cor_under(req = input$acvar, subset = acvar(), name = "AC")),
                  print(plot_cor_under(req = input$ssnvar, subset = ssnvar(), name = "SSN"))) 
      dev.off()
    })

 
  # Define functions for pillar calculation ++++++++++++++++++++++++++++
  score_calcul=function(grp_var){
    M1 = fa(grp_var,  rotate =  "none", nfactors = length(grp_var) )
    p1_= as.matrix(M1$loadings)
    P2_ = as.vector(colSums(p1_^2))
    p3_ = P2_/length(grp_var)
    Pov_ = p3_/sum(p3_)
    cum_pov_ = cumsum(Pov_)
    numf= min(which(cum_pov_ > 0.95))
    
    M_<-fa(grp_var, rotate="none", nfactors = numf, scores="Bartlett", fm="pa",SMC = FALSE)
    # ## calculate weight
    M1_= as.matrix(M_$loadings)
    M2_ = as.vector(colSums(M1_^2))
    M3_ = M2_/length(grp_var)
    s_Pov_ = M3_/sum(M3_)
    s_cum_pov_ = cumsum(s_Pov_)
    
    weight_ =s_Pov_
    weight_ = head(weight_, numf) 
    weight_score = sweep(M_$scores, MARGIN=2, weight_, `*`)
    score.sum = apply(weight_score,1,sum)
    # ## generate index pillar
    return(score.sum/sum(weight_))
  }
  
  # creating pillars ++++++++++++++++++++++++++++
  abs=reactive({
    validate(need(length(input$absvar) >=3, "select at least 3 variables"))
    score_calcul(absvar())
  })

 
  ast=reactive({
    validate(need(length(input$astvar) >=3, "select at least 3 variables"))
    score_calcul(astvar())
  })
  
  ac=reactive({
    validate(need(length(input$acvar) >=3, "select at least 3 variables"))
    score_calcul(acvar())
  })
  
  ssn=reactive({
    validate(need(length(input$ssnvar) >=3, "select at least 3 variables"))
    score_calcul(ssnvar())
  })
  
  # RCI calculation and plot path ++++++++++++++++++++++++++++
  
  Rima_pillars=reactive(cbind(abs=abs(),ast=ast(),ac=ac(),ssn=ssn()))
  fsvar = reactive(df2()[, input$fsvar])
  fsvar2 <- reactive({
    req(input$fsvar)
    fs2 <- data.frame(lapply(fsvar(), as.numeric, trimws))
    fs3 <- scale(fs2, scale = TRUE)
    if (length(input$fsvar)==2) colnames(fs3) <- c("FS1","FS2")
    if (length(input$fsvar)==3) colnames(fs3) <- c("FS1","FS2","FS3")
    fs3
  })
  
  rcivar = reactive(cbind(abs=abs(),ast=ast(),ac=ac(),ssn=ssn(), fsvar2()))
  
  df2_rci=reactive({
    model.2FS = 'RES =~ FS1 + FS2
                RES ~ abs + ast + ac + ssn'
    model.3FS = 'RES =~ FS1 + FS2 + FS3
                RES ~ abs + ast + ac + ssn'
    validate(need(length(input$fsvar) ==2 | length(input$fsvar) ==3, "the number of FS indicators should be 2 or 3"))
    
    if (length(input$fsvar)==2) fit = lavaan::cfa(model=model.2FS, data = rcivar(), estimator = "ML", se = "robust.sem")
    if (length(input$fsvar)==3) fit = lavaan::cfa(model=model.3FS, data = rcivar(), estimator = "ML", se = "robust.sem")
    
    #plot path
    output$rcivar_plot = renderPlot(
      semPaths(fit,
               what = "std",
               whatLabels = "est",
               style = "lisrel",
               layout = "tree2",
               rotation = 3,
               sizeMan = 6,
               sizeLat = 10,
               mar=c(4,5,4,5)))
    
    RCI = lavaan::lavPredict(fit)
  })
  
  RCI_res2=reactive({
    rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
    RCI_res1= rescale(df2_rci())
    RCI_res1=data.frame(RCI_res1)
    RCI_res3=setNames(RCI_res1, replace(names(RCI_res1), names(RCI_res1) == 'RES', 'RCI_res'))
    cbind(Rima_pillars(), df2_rci(), RCI_res3)
  })
  
  
  # resilience structure matrix
  res_pillar=reactive(cbind(Rima_pillars(), df2_rci()))
  cor_rci=reactive({
    req(res_pillar)
    rci.cor=cor(res_pillar())
    rci.cor=rci.cor[1:4,5]
    rci.cor=base::abs(rci.cor)
    rci.cor=(rci.cor/sum(rci.cor))*100
    rci.cor
  })
  # plot RSM
  output$fig_rci=renderPlot(barplot(cor_rci(), main="Resilience Structure Matrix", font.axis=1.3))
  
  
  # Download graph for RCI results tab ++++++++++++++++++++++++++++
  output$downloadRS <- downloadHandler(
    filename = function() {
      "RCI_results.pdf"
    },
    content = function(file) {
      pdf(file)
      arrangeGrob(print(barplot(cor_rci(), main="Resilience Structure Matrix", font.axis=1.3)),
                  print({
                    model.2FS = 'RES =~ FS1 + FS2
                    RES ~ abs + ast + ac + ssn'
                    model.3FS = 'RES =~ FS1 + FS2 + FS3
                    RES ~ abs + ast + ac + ssn'
                    validate(need(length(input$fsvar) ==2 | length(input$fsvar) ==3, "the number of FS indicators should be 2 or 3"))
                    if (length(input$fsvar)==2) fit = lavaan::cfa(model=model.2FS, data = rcivar(), estimator = "ML", se = "robust.sem")
                    if (length(input$fsvar)==3) fit = lavaan::cfa(model=model.3FS, data = rcivar(), estimator = "ML", se = "robust.sem")
                    semPaths(fit, what = "std", whatLabels = "est", style = "lisrel", layout = "tree2",
                             rotation = 3, sizeMan = 6, sizeLat = 10, mar=c(4,5,4,5))})) 
      dev.off()
    })
  
  ####################################################################################################
  # Output data table ++++++++++++++++++++++++++++
  output_data = reactive(cbind(df2(), RCI_res2()))
  output$outvar_rslt = renderTable(head(output_data()))
 
   output$downloadData=downloadHandler(
    filename = function() {
      paste("output_data", "csv", sep=".")
    },
    content = function(file) {
      write.csv(output_data(), file)
    }
  )
  
   
 ####################################################################################################
 # RCI and RSM profiling ++++++++++++++++++++++++++++
  # RCI  
   rci_profile <- function(i){
       req(length(input$profile)>=i)
       bd=output_data()
       RC_index=output_data()$RCI_res
       x_var=as.factor(output_data()[, input$profile[[i]]])
       ggplot(bd, aes_string(x=x_var, y=RC_index)) +
       stat_summary(geom = "bar", fun = "mean", color = "orange", fill="#69b3a2") +
       labs(title =paste("Average RCI by", input$profile[[i]]), x=paste(input$profile[[i]]), y="RCI")+
       theme_grey(base_size = 8)+ theme(axis.text.x = element_text(size  = 6, angle = 45, hjust = 1, vjust = 1))
       }
 
   output$RCI_profil1 <- renderPlot(rci_profile(i=1),res = 110)
   output$RCI_profil2 <- renderPlot(rci_profile(i=2),res = 110) 
   output$RCI_profil3 <- renderPlot(rci_profile(i=3),res = 110)
  
   # RSM
   rsm_data <- function(i){ 
     rsm <-output_data() %>% group_by_at(input$profile[[i]]) %>% 
     dplyr::summarise(abs = cor(abs, RES),
                      ast= cor(ast, RES),
                      ssn= cor(ssn, RES),
                      ac= cor(ac, RES))
      pill<- c("abs", "ast", "ssn", "ac")
      rsm_1 = rsm[, pill]
      rsm_2= base::abs(rsm_1)
      rsm_2_x = rsm[, input$profile[[i]]]
      rsm_3 <- cbind(rsm_2_x, rsm_2)
      rsm_4 <-  reshape2::melt(rsm_3, id.vars = c(input$profile[[i]]))
      return(rsm_4)
   }
   
   rsm_profile <- function(i, data){
        req(length(input$profile)>=i)
        x_var=as.factor(data[,input$profile[[i]]])
        ggplot(data, aes(fill=variable, y=value, x=x_var)) + 
        geom_bar(position="fill", stat="identity") +
        labs(title =paste("RSM by", input$profile[[i]]), x=paste(input$profile[[i]]), y="pillar contribution to RCI") +
        theme_grey(base_size = 8)+ theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1))
   }
   
   output$RSM_profil_1 = renderPlot(rsm_profile(i=1, data = rsm_data(i=1)), res = 110)
   output$RSM_profil_2 = renderPlot(rsm_profile(i=2, data = rsm_data(i=2)), res = 110)
   output$RSM_profil_3 = renderPlot(rsm_profile(i=3, data = rsm_data(i=3)), res = 110)
   
   # Download profile
   output$downloadProfile <- downloadHandler(
     filename = function() {
       "By_profile.pdf"
     },
     content = function(file) {
       pdf(file)
       if (length(input$profile)==1){arrangeGrob(print(rci_profile(i=1)),
                                                 print(rsm_profile(i=1, data = rsm_data(i=1))))}
       else if (length(input$profile)==2){arrangeGrob(print(rci_profile(i=1)),
                                                      print(rsm_profile(i=1, data = rsm_data(i=1))),
                                                      print(rci_profile(i=2)),
                                                      print(rsm_profile(i=2, data = rsm_data(i=2))))}
       else {arrangeGrob(print(rci_profile(i=1)),
                         print(rsm_profile(i=1, data = rsm_data(i=1))),
                         print(rci_profile(i=2)),
                         print(rsm_profile(i=2, data = rsm_data(i=2))),
                         print(rci_profile(i=3)),
                         print(rsm_profile(i=3, data = rsm_data(i=3))))}
       dev.off()
     })
   
   
 ####################################################################################################
  # Radar pillar ++++++++++++++++++++++++++++ 
   radar_data <- function(req,var, pil, pilname){
     req(req)
     t1 <- cor(pil, var)
     rownames(t1) <- pilname
     t2 <- base::abs(t1)
     max=1
     min=0
     ret = data.frame(rbind(max, min, t2))
     return(ret)
   }
   
   colors_fill <- c(scales::alpha("tomato", 0.3))
   colors_line <- c(scales::alpha("royalblue", 0.7))
   colors_cglcol <- c(scales::alpha("black", 0.7))
   
   
   plotradar <- function(data,title){
     radarchart(data,
                seg = 4,  # Number of axis segments
                title = paste("Variable correlation with", title, "pillar"),
                pcol = colors_line,
                pfcol = colors_fill,
                cglcol = colors_cglcol,
                plty = 1,
                centerzero=TRUE,
                axistype=4,
                palcex = 0.75,
                calcex = 0.75,
                plwd = 2)
   }
   
   output$fig_abs =renderPlot(plotradar(data = radar_data(req=input$absvar, var = absvar(), pil =abs(), pilname = "abs"), title = "ABS"), width = 500, height = 500, res = 100)
   output$fig_ast =renderPlot(plotradar(data = radar_data(req=input$astvar, var = astvar(), pil =ast(), pilname = "ast"), title = "AST"), width = 500, height = 500, res = 100)
   output$fig_ac  =renderPlot(plotradar(data = radar_data(req=input$acvar , var = acvar() , pil =ac(), pilname = "ac"), title = "AC"), width = 500, height = 500, res = 100)
   output$fig_ssn =renderPlot(plotradar(data = radar_data(req=input$ssnvar, var = ssnvar(), pil =ssn(), pilname = "ssn"), title = "SSN"), width = 500, height = 500, res = 100)
   
   #Download radars:
   output$downloadRadar <- downloadHandler(
     filename = function() {
       "Pillar_radar.pdf"
     },
     content = function(file) {
       pdf(file)
       arrangeGrob(print(plotradar(data = radar_data(req=input$absvar, var = absvar(), pil =abs(), pilname = "abs"), title = "ABS")),
                   print(plotradar(data = radar_data(req=input$astvar, var = astvar(), pil =ast(), pilname = "ast"), title = "AST")), 
                   print(plotradar(data = radar_data(req=input$acvar , var = acvar() , pil =ac(), pilname = "ac"), title = "AC")),
                   print(plotradar(data = radar_data(req=input$ssnvar, var = ssnvar(), pil =ssn(), pilname = "ssn"), title = "SSN")))
       dev.off()
     })
   

####################################################################################################  
  # Scenario changing
  output$scenariovar <- renderUI({
    if(is.null(input$inputfiles)) {return(selectInput("scevar", 'Select variable for scenario analysis', choices = NULL))}
    selectInput("scevar", 'Select variable for scenario analysis', choices = aa())})
    
  
  output$scenario <- renderText({ "Developing" })
   
####################################################################################################  
  # Pseudo panel 
  output$pseudo <- renderText({ "To add: pseudo analysis codes" })
  # Load base and follow up dataset
  output$selectbase <- renderUI({list(hr(), selectInput("Select", "Select the baseline dataset", choices=input$inputfiles$name))})
  output$selectfu   <- renderUI({list(hr(), selectInput("Select", "Select the folowup dataset", choices=input$inputfiles$name))})

  
  
}
