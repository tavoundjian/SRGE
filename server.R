shinyServer(function(input, output, session) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  fileload <- reactive({
    lfile <- input$loadfile
    if(is.null(lfile)) {
      return(NULL)
    } 
    read.csv(lfile$datapath, stringsAsFactors = F) -> load
    
    factor(load$result, levels = unique(load$result)) -> load$result
    factor(load$age, levels = unique(load$age)) -> load$age
    factor(load$gender, levels = unique(load$gender)) -> load$gender
    factor(load$prev, levels = unique(load$prev)) -> load$prev
    factor(load$type, levels = unique(load$type)) -> load$type
    factor(load$case_status, levels = unique(load$case_status)) -> load$case_status
    
    load
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata() 
  })
  
  #The following set of functions populate the column selectors
  output$req.text <- renderUI({
    df <- filedata()
    
    if (is.null(df)) return(NULL)
    
    HTML("<h5><b>Required Variables</b></h5>")
    
  })
  
  output$opt.text <- renderUI({
    df <- filedata()
    
    if (is.null(df)) return(NULL)
    
    HTML("<h5><b>Optional Variables</b></h5>")
    
  })
  
  output$case <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.case", "Case Status:", c("Select Case Variable:",items))
  })
  
  case.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    case=input$col.case
    
    assign('case',case, envir=.GlobalEnv)
  })
  
  output$case.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.case == "Select Case Variable:") return(HTML("<h3><b>Please Select Case Variable</b></h3>"))
    
    HTML("<h3><b>Specify Case Value</h3></b>")
  })
  
  output$case.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.case == "Select Case Variable:") return(NULL)
    
    case <<- case.panel.f()
    cases <<- df[,which(colnames(df) == case)]
    
    selectInput("case.order", NA, unique(cases), selectize=T)
  })
  
  output$agegrp <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.age", "Age Group:",c("Select Age Variable:",items))
  })
  
  age.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    age=input$col.age
    
    assign('age',age, envir = .GlobalEnv)
  })
  
  make.row <- function(x,label) {
    fluidRow(
      textInput(paste0(label,x,sep=""),paste0("Value = ",x))
    )
  }
  
  output$age.factor.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.age == "Select Age Variable:") return(HTML("<h3><b>Please Select Age Variable</b></h3>"))
    
    age <- age.panel.f()
    ages <- df[,which(colnames(df) == age)]
    
    if (is.factor(ages) == F & is.character(ages) == F) {
      HTML("<h3><b> 1. Specify Value Labels </b></h3>")
    }
    
  })
  
  output$age.factor.inputs <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.age == "Select Age Variable:") return(NULL)
    
    age <- age.panel.f()
    
    ages <- df[,which(colnames(df) == age)]
    
    if (is.factor(ages) == F & is.character(ages) == F) {
      
      age.l <- unique(ages)[!is.na(unique(ages))]
      
      mytabs <<- lapply(age.l, make.row,"age")
      do.call(fluidPage,mytabs)
    }
  })
  
  output$age.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.age == "Select Age Variable:") return(NULL)
    
    age <- age.panel.f()
    ages <- df[,which(colnames(df) == age)]
    
    if (is.factor(ages) == F & is.character(ages) == F) {
      HTML("<h3><b> 2. Select Age Group Order </h3></b>")
    } else {
      HTML("<h3><b> Select Age Group Order</h3></b>")
    }
  })
  
  output$age.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.age == "Select Age Variable:") return(NULL)
    
    age <<- age.panel.f()
    ages <<- df[,which(colnames(df) == age)]
    
    if (is.factor(ages) == F & is.character(ages) == F) {
      age.l <<- unique(ages)[!is.na(unique(ages))]
      age.txt <<- paste0("c(",paste0(paste0("input$","age",age.l,collapse=","),")"))
      
      age.f <<- suppressWarnings(factor(ages, levels = age.l, labels = eval(parse(text=age.txt))))
      
      selectInput("age.order", label="", choices=levels(age.f), selectize=T, multiple=T)
    } else {
      age.l <<- levels(df[,which(colnames(df) == age)])
      selectInput("age.order", NA, age.l, selectize=T, multiple=T)
    }
  })
  
  # output$test <- renderUI({
  #   age.levels <<- paste(input$age.order, collapse=", ")
  # })
  
  output$result <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.result", "Test Result:",c("Select Test Result Variable:",items))
    
  })
  
  result.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    result=input$col.result
    
    assign('result',result, envir = .GlobalEnv)
  })

  output$result.factor.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.result == "Select Test Result Variable:") return(HTML("<h3><b>Please Select Test Result Variable</h3></b>"))
    
    result <- result.panel.f()
    results <- df[,which(colnames(df) == result)]
    
    if (is.factor(results) == F & is.character(results) == F) {
      HTML("<h3><b> 1. Specify Value Labels </b></h3>")
    }
    
  })
  
  output$result.factor.inputs <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.result == "Select Test Result Variable:") return(NULL)
    
    result <- result.panel.f()
    
    results <- df[,which(colnames(df) == result)]
    
    if (is.factor(results) == F & is.character(results) == F) {
      
      result.l <- unique(results)[!is.na(unique(results))]
      
      mytabs <<- lapply(result.l, make.row,"result")
      do.call(fluidPage,mytabs)
    }
  })
  
  output$result.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.result == "Select Test Result Variable:") return(NULL)
    
    result <- result.panel.f()
    results <- df[,which(colnames(df) == result)]
    
    if (is.factor(results) == F & is.character(results) == F) {
      HTML("<h3><b> 2. Select Test Result Order </h3></b>")
    } else {
      HTML("<h3><b> Select Test Result Order</h3></b>")
    }
  })
  
  output$result.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$col.result == "Select Test Result Variable:") return(NULL)
    
    result <<- result.panel.f()
    results <<- df[,which(colnames(df) == result)]
    
    if (is.factor(results) == F & is.character(results) == F) {
      result.l <<- unique(results)[!is.na(unique(results))]
      result.txt <<- paste0("c(",paste0(paste0("input$","result",result.l,collapse=","),")"))
      
      result.f <<- suppressWarnings(factor(results, levels = result.l, labels = eval(parse(text=result.txt))))
      
      selectInput("result.order", label="", choices=levels(result.f), selectize=T, multiple=T)
    } else {
      result.l <<- levels(df[,which(colnames(df) == result)])
      selectInput("result.order", NA, result.l, selectize=T, multiple=T)
    }
  })
  
  # output$test2 <- renderUI({
  #   result.levels <<- paste(input$results.order, ", ")
  # })
  
  output$gender <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.gender", "Gender:",c("Select Gender Variable:",items))
    
  })
  
  output$gender.display <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    checkboxInput("gender.chk", "Include?", T)
  })
  
  gender.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    gender=input$col.gender
    
    assign('gender',gender, envir = .GlobalEnv)
  })
  
  output$gender.factor.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$gender.chk == F) return(HTML("<h3><b>Variable not included, please continue to next tab</b></h3>"))
    if (input$col.gender == "Select Gender Variable:") return(HTML("<h3><b>Please Select Gender Variable</h3></b>"))
    
    gender <- gender.panel.f()
    genders <- df[,which(colnames(df) == gender)]
    
    if (is.factor(genders) == F & is.character(genders) == F) {
      HTML("<h3><b> 1. Specify Value Labels </b></h3>")
    }
    
  })
  
  output$gender.factor.inputs <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$gender.chk == F) return(NULL)
    if (input$col.gender == "Select Gender Variable:") return(NULL)
    
    gender <<- gender.panel.f()
    
    genders <<- df[,which(colnames(df) == gender)]
    
    if (is.factor(genders) == F & is.character(genders) == F) {
      
      gender.l <<- unique(genders)[!is.na(unique(genders))]
      
      mytabs <<- lapply(gender.l, make.row,"gender")
      do.call(fluidPage,mytabs)
    }
  })
  
  output$gender.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$gender.chk == F) return(NULL)
    if (input$col.gender == "Select Gender Variable:") return(NULL)
    
    gender <- gender.panel.f()
    genders <- df[,which(colnames(df) == gender)]
    
    if (is.factor(genders) == F & is.character(genders) == F) {
      HTML("<h3><b> 2. Select Gender Order </h3></b>")
    } else {
      HTML("<h3><b> Select Gender Order</h3></b>")
    }
  })
  
  output$gender.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$gender.chk == F) return(NULL)
    if (input$col.gender == "Select Gender Variable:") return(NULL)
    
    gender <- gender.panel.f()
    genders <- df[,which(colnames(df) == gender)]
    
    if (is.factor(genders) == F & is.character(genders) == F) {
      gender.l <<- unique(genders)[!is.na(unique(genders))]
      gender.txt <<- paste0("c(",paste0(paste0("input$","gender",gender.l,collapse=","),")"))
      
      gender.f <<- suppressWarnings(factor(genders, levels = gender.l, labels = eval(parse(text=gender.txt))))
      
      selectInput("gender.order", label="", choices=levels(gender.f), selectize=T, multiple=T)
    } else {
      gender.l <<- levels(df[,which(colnames(df) == gender)])
      selectInput("gender.order", NA, gender.l, selectize=T, multiple=T)
    }
  })
  
  output$type <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.type", "Test Type:",c("Select Test Type Variable:",items))
    
  })
  
  output$type.display <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    checkboxInput("type.chk", "Include?", T)
  })
  
  type.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    type <- input$col.type

    assign('type',type, envir = .GlobalEnv)
  })
  
  output$type.factor.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$type.chk == F) return(HTML("<h3><b>Variable not included, please continue to next tab</b></h3>"))
    if (input$col.type == "Select Test Type Variable:") return(HTML("<h3><b>Please Select Test Type Variable</h3></b>"))
    
    type <- type.panel.f()
    types <- df[,which(colnames(df) == type)]
    
    if (is.factor(types) == F & is.character(types) == F) {
      HTML("<h3><b> 1. Specify Value Labels </b></h3>")
    }
    
  })
  
  output$type.factor.inputs <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$type.chk == F) return(NULL)
    if (input$col.type == "Select Test Type Variable:") return(NULL)
    
    type <<- type.panel.f()
    
    types <<- df[,which(colnames(df) == type)]
    
    if (is.factor(types) == F & is.character(types) == F) {
      
      type.l <- unique(types)[!is.na(unique(types))]
      
      mytabs <<- lapply(type.l, make.row,"type")
      do.call(fluidPage,mytabs)
    }
  })
  
  output$type.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$type.chk == F) return(NULL)
    if (input$col.type == "Select Test Type Variable:") return(NULL)
    
    type <- type.panel.f()
    types <- df[,which(colnames(df) == type)]
    
    if (is.factor(types) == F & is.character(types) == F) {
      HTML("<h3><b> 2. Select Test Type Order </h3></b>")
    } else {
      HTML("<h3><b> Select Test Type Order</h3></b>")
    }
  })
  
  output$type.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$type.chk == F) return(NULL)
    if (input$col.type == "Select Test Type Variable:") return(NULL)
    
    type <- type.panel.f()
    types <- df[,which(colnames(df) == type)]
    
    if (is.factor(types) == F & is.character(types) == F) {
      type.l <<- unique(types)[!is.na(unique(types))]
      type.txt <<- paste0("c(",paste0(paste0("input$","type",type.l,collapse=","),")"))
      
      type.f <<- suppressWarnings(factor(types, levels = type.l, labels = eval(parse(text=type.txt))))
      
      selectInput("type.order", label="", choices=levels(type.f), selectize=T, multiple=T)
    } else {
      type.l <<- levels(df[,which(colnames(df) == type)])
      selectInput("type.order", NA, type.l, selectize=T, multiple=T)
    }
  })
  
  output$prev <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("col.prev", "Previous Result:",c("Select Previous Result Variable:",items))
    
  })
  
  output$prev.display <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    checkboxInput("prev.chk", "Include?", T)
  })
  
  prev.panel.f <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    prev=input$col.prev
    
    assign('prev',prev, envir = .GlobalEnv)
  })
  
  output$prev.factor.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$prev.chk == F) return(HTML("<h3><b>Variable not included, please continue to next tab</b></h3>"))
    if (input$col.prev == "Select Previous Result Variable:") return(HTML("<h3><b>Please Select Previous Result Variable</h3></b>"))
    
    prev <- prev.panel.f()
    prevs <- df[,which(colnames(df) == prev)]
    
    if (is.factor(prevs) == F & is.character(prevs) == F) {
      HTML("<h3><b> 1. Specify Value Labels </b></h3>")
    }
    
  })
  
  output$prev.factor.inputs <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$prev.chk == F) return(NULL)
    if (input$col.prev == "Select Previous Result Variable:") return(NULL)
    
    prev <- prev.panel.f()
    
    prevs <- df[,which(colnames(df) == prev)]
    
    if (is.factor(prevs) == F & is.character(prevs) == F) {
      
      prev.l <- unique(prevs)[!is.na(unique(prevs))]
      
      mytabs <<- lapply(prev.l, make.row,"prev")
      do.call(fluidPage,mytabs)
    }
  })
  
  output$prev.panel.title <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$prev.chk == F) return(NULL)
    if (input$col.prev == "Select Previous Result Variable:") return(NULL)
    
    prev <- prev.panel.f()
    prevs <- df[,which(colnames(df) == prev)]
    
    if (is.factor(prevs) == F & is.character(prevs) == F) {
      HTML("<h3><b> 2. Select Previous Result Order </h3></b>")
    } else {
      HTML("<h3><b> Select Previous Result Order</h3></b>")
    }
  })
  
  output$prev.panel <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$prev.chk == F) return(NULL)
    if (input$col.prev == "Select Previous Result Variable:") return(NULL)
    
    prev <<- prev.panel.f()
    prevs <<- df[,which(colnames(df) == prev)]
    
    if (is.factor(prevs) == F & is.character(prevs) == F) {
      prev.l <<- unique(prevs)[!is.na(unique(prevs))]
      prev.txt <<- paste0("c(",paste0(paste0("input$","prev",prev.l,collapse=","),")"))
      
      prev.f <<- suppressWarnings(factor(prevs, levels = prev.l, labels = eval(parse(text=prev.txt))))
      
      selectInput("prev.order", label="", choices=levels(prev.f), selectize=T, multiple=T)
    } else {
      prev.l <<- levels(df[,which(colnames(df) == prev)])
      selectInput("prev.order", NA, prev.l, selectize=T, multiple=T)
    }
  })
  
  output$agg.var <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$aggregate.chk == F) return(NULL)
    
    selectInput("col.agg", "Weight:", choices = c("Select Weight Variable", names(df)))
  })
  
  observe({
    if(input$createGrid > 0){
      #print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })
  # observe({
  #   if(input$action1 > 0){
  #     #print('2')
  #     session$sendCustomMessage("myCallbackHandler", "2")
  #   }
  # })
  
  filedata2 <- reactive({
    load <- fileload()
    
    if (is.null(load)) {
      df <- filedata()
      
      f.age <<- input$age.order
      f.result <<- input$result.order
      f.gender <<- input$gender.order
      f.type <<- input$type.order
      f.prev <<- input$prev.order
      f.case <<- input$case.order
      
      if (is.factor(ages) == F &
          is.character(ages) == F) {
        v.age <<- factor(age.f,
                         levels = c(f.age),
                         labels = c(f.age))
      } else {
        v.age <<- factor(df[,which(colnames(df) == age)],
                         levels = c(f.age),
                         labels = c(f.age))
      }
      
      if (is.factor(results) == F &
          is.character(results) == F) {
        v.result <<- factor(result.f,
                            levels = c(f.result),
                            labels = c(f.result))
      } else {
        v.result <<- factor(df[,which(colnames(df) == result)],
                            levels = c(f.result),
                            labels = c(f.result))
      }
      
      if (is.factor(genders) == F &
          is.character(genders) == F) {
        v.gender <<- factor(gender.f,
                            levels = c(f.gender),
                            labels = c(f.gender))
      } else {
        v.gender <<- factor(df[,which(colnames(df) == gender)], 
                            levels = c(f.gender), 
                            labels = c(f.gender))
      }
      
      if (is.factor(types) == F &
          is.character(types) == F) {
        v.type <<- factor(type.f,
                          levels = c(f.type),
                          labels = c(f.type))
      } else {
        v.type <<- factor(df[,which(colnames(df) == type)],
                          levels = c(f.type),
                          labels = c(f.type))
      }
      
      if (is.factor(prevs) == F &
          is.character(prevs) == F) {
        v.prev <<- factor(prev.f,
                          levels = c(f.prev),
                          labels = c(f.prev))
      } else {
        v.prev <<- factor(df[,which(colnames(df) == prev)],
                          levels = c(f.prev),
                          labels = c(f.prev))
      }
      
      pre.case <<- df[,which(colnames(df) == case)]
      
      pre.case2 <<- ifelse(pre.case == f.case, 1, 0)
      
      v.case <<- factor(df[,which(colnames(df) == case)],
                        levels = c(0,1),
                        labels = c("Non-Case","Case"))
      
      if (input$aggregate.chk == T) {
        v.freq <<- df[,which(colnames(df) == input$col.agg)]
        
        rg <<- data.frame(
          age = v.age,
          result = v.result,
          gender = v.gender,
          type = v.type,
          prev = v.prev,
          case_status = v.case,
          freq = v.freq
        )
      } else {
        rg <<- data.frame(
          age = v.age,
          result = v.result,
          gender = v.gender,
          type = v.type,
          prev = v.prev,
          case_status = v.case,
          freq = 1
        )
    }

    group_by(rg, type, result, age, gender, prev, case_status) %>%
      summarise(.,freq = sum(freq)) -> df2

    df.case <- df2[df2$case_status == "Case",]
    names(df.case)[7] <- "case.freq"
    full_join(df2, df.case[,c(-6)], by=c("type","result","age","gender","prev")) ->> df3

    with(df3,
         ifelse(is.na(case.freq) == T, 0, case.freq)
    ) -> df3$case.freq

    #write.table(df3, "fulltab")
    
    stacked <- data.frame()
    for (i in 1:nrow(df2)) {
      df2[rep(seq_len(nrow(df2))[i], df2[i,'freq']),] -> s

      rbind.data.frame(stacked, s) -> stacked
      
      #print(i)
    }
    
    brk.start <<- length(unique(rg$result))+1
    brk.end <<- (length(unique(rg$result))*2)-1
    
    brk.int <<- brk.start:brk.end
    
    assign('stacked', stacked, envir = .GlobalEnv)
    
    } else {
      brk.start <<- length(unique(load$result))+1
      brk.end <<- (length(unique(load$result))*2)-1
      
      brk.int <<- brk.start:brk.end
      
      assign('load',load,envir = .GlobalEnv)
    }
    
    #write.table(stacked, "stacked")

    # as.data.frame(with(stacked,
    #                    table(agegrp5, testresult))) -> data
    # 
    # as.data.frame(with(stacked[stacked$case_status == 1,],
    #                    table(agegrp5, testresult))) -> cases
    # 
    # names(cases)[3] <- "cases"
    # 
    # full_join(data, cases) -> data
    # unite(data, Cases.Labs, cases, Freq, sep = "/") -> data
    # 
    # spread(data, testresult, Cases.Labs) -> data
  })
  
  # output$filetable2 <- renderTable({
  #   rg2 <<- filedata2()
  #   
  #   rg2
  # }
  # )
  
  savegrid <- reactive({
    gData <- filedata2()
    if (is.null(gData)) return(NULL)

    write.csv(gData, "stacked.csv")
  })
  
  # output$savegrid <- downloadHandler(
  #   filename = function() { 
  #     paste(input$dataset, '.csv', sep='') 
  #   },
  #   content = function(file) {
  #     write.csv(datasetInput(), file)
  #   }
  # )
  
  # observeEvent(input$SaveGrid, {
  #   
  #   savegrid()
  #   
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'Data File Saved')
  # })
  
  observeEvent(input$LoadGrid, {
    loadcheck <<- fileload()
    
    session$sendCustomMessage("myCallbackHandler", "1")
  })
  
  output$gen.filt <- renderUI({
    load <- fileload()
    
    if (is.null(load)) {
      df <- filedata2()
      if (is.null(df)) return(NULL)
      
      if (is.factor(genders) == F &
          is.character(genders) == F) {
        selectInput("gen.filter", "Gender", c("All",levels(gender.f)))
      } else {
        selectInput("gen.filter", "Gender", c("All", levels(f.gender)))
      }
    } else {
      selectInput("gen.filter", "Gender", c("All", levels(loadcheck$gender)))
    }
    
  })
  
  output$prev.filt <- renderUI({
    load <- fileload()
    
    if (is.null(load)) {
      df <- filedata2()
      if (is.null(df)) return(NULL)
      
      if (is.factor(prevs) == F &
          is.character(prevs) == F) {
        selectInput("prev.filter", "Previous Result", c("All",levels(prev.f)))
      } else {
        selectInput("prev.filter", "Previous Result", c("All",f.prev))
      }
    } else {
      selectInput("prev.filter", "Previous Result", c("All",levels(loadcheck$prev)))
    }
    
  })
  
  output$test.filt <- renderUI({
    load <- fileload()
    
    if (is.null(load)) {
      df <- filedata2()
      if (is.null(df)) return(NULL)
      
      if (is.factor(types) == F &
          is.character(types) == F) {
        selectInput("test.filter", "Test Type", c("All", levels(type.f)))
      } else {
        selectInput("test.filter","Test Type", c("All",f.type))
      }
    } else {
      selectInput("test.filter", "Test Type", c("All",levels(loadcheck$type)))
    }
    
  })
  
  output$table <- renderDataTable(datatable({
    rg2 <<- filedata2()
    if (is.null(rg2)) return(NULL)
    
    if (input$gen.filter != "All") {
      rg2 <- rg2[rg2$gender == input$gen.filter,]
    }
    if (input$prev.filter != "All") {
      rg2<- rg2[rg2$prev == input$prev.filter,]
    }
    if (input$test.filter != "All") {
      rg2 <- rg2[rg2$type == input$test.filter,]
    }
    
    display <- as.data.frame(with(rg2, table(age, result)))
    
    cases <- as.data.frame(with(rg2[rg2$case_status == "Case",],
                                table(age, result)))
    
    names(cases)[3] <- "cases"
    
    display <- suppressMessages(full_join(display, cases))
    
    assign('dis.long',display,envir=.GlobalEnv)
    
    display2 <- display
    display2$p.case <- display2$cases/display2$Freq
    ifelse(is.nan(display2$p.case) == T, 0,display2$p.case) -> display2$p.case
    display2$Freq <- NULL
    display2$cases <- NULL
    display2 <- spread(display2, result, p.case)
    
    display <- unite(display, Cases.Labs, cases, Freq, sep = "/")
    
    display <- spread(display, result, Cases.Labs)
    
    names(display2)[-1] <- paste0('p.',names(display2)[-1])
    full_join(display, display2, "age") ->> display3
    
    # brk.start <<- length(unique(rg2$result))+1
    # brk.end <<- length(display3)
    # 
    # brk.int <<- brk.start:brk.end
    
    brks <- quantile(display3[,brk.int], 
                     probs = seq(.40, .95, .05), na.rm = TRUE)
    
    assign('display', display, envir = .GlobalEnv)
    
    display3
  },extensions = 'Scroller', options=list(scrollY = 445, scroller = TRUE, 2, pageLength = 12, dom='ti',
                                          columnDefs = list(list(targets = 9:15, visible = FALSE))),
  selection = list(target = 'cell')) %>% formatStyle(names(display3)[2:(brk.start-1)],
                                                     names(display3[,brk.int]),
                                                     backgroundColor = styleInterval(brks, my_palette))
  )
  
  #### Clear Selection Button ####
  proxy = dataTableProxy('table')
  
  con=reactiveValues(cc=NULL)
  
  observeEvent(input$clear, {
    if(is.null(input$table_cells_selected) == T) {
      con$cc <- "No cells selected"
    }
    else {
      con$cc <- selectCells(proxy, NULL)
    }
  })
  
  output$legend <- renderPlot({
    # get an empty box
    plot(0:15,type="n",axes=F, xlab=NA,ylab=NA)
    # run across the three primaries
    gradient.rect(0.5,0,14.9,12,reds=c(1,0),
                  col=my_palette,gradient="x")
    text(1.5,14,"Low % cases")
    text(14,14,"High % cases")
  },width=650, height = 185)
  
  ####Dynamic Calculations ####
  
  #### Totals ####
  output$totals <- renderUI({
    df <- filedata2()
    if (is.null(df)) return(NULL)
    
    if (input$gen.filter != "All") {
      df <- df[df$gender == input$gen.filter,]
    }
    if (input$prev.filter != "All") {
      df<- df[df$prev == input$prev.filter,]
    }
    if (input$test.filter != "All") {
      df <- df[df$type == input$test.filter,]
    }
    
    nrow(df) ->> total.labs
    nrow(df[df$case_status == 'Case',]) ->> total.cases
    
    HTML(paste(
      paste0('<b>Total labs: </b>',total.labs),
      paste0('<b>Total cases: </b>',total.cases),
      sep = '<br/>'))
  })
  
  #### Cumulative ####
  output$cumulative <- renderUI({
    selected <- NULL
    selected <- input$table_cells_selected
    assign('selected', selected, envir = .GlobalEnv)
    
    df.load <- filedata2()
    if (is.null(df.load)) return(NULL)
    
    if (input$gen.filter != "All") {
      df.load <- df.load[df.load$gender == input$gen.filter,]
    }
    if (input$prev.filter != "All") {
      df.load<- df.load[df.load$prev == input$prev.filter,]
    }
    if (input$test.filter != "All") {
      df.load <- df.load[df.load$type == input$test.filter,]
    }

    nrow(df.load) -> total.labs
    nrow(df.load[df.load$case_status == "Case",]) -> total.cases
    
    df2 <<- as.data.frame(with(df.load, table(age, result)))
    
    cases <<- as.data.frame(with(df.load[df.load$case_status == "Case",],
                                table(age, result)))
    
    names(cases)[3] <- "cases"
    
    df <- suppressMessages(full_join(df2, cases))
    
    #assign('chk',df, envir=.GlobalEnv)
    
    if(length(selected) > 0) {
      
      df.selected <- data.frame()
      for(i in 1:nrow(selected)) {
        row <- df[df$result == names(display)[[selected[i,2]]] & df$age == display[selected[i,1],1],]
        
        rbind(df.selected, row) -> df.selected
      }
      
      #assign('chk2', df.selected, envir=.GlobalEnv)
      
      selected.labs <- sum(df.selected$Freq)
      selected.cases <- sum(df.selected$cases)
      
      #Percent of cases lost
      selected.cases/total.cases -> pct.cases.lost
      
      #efficiency gained
      selected.labs/total.labs -> efficiency.gained
      
      #lab reports per case found
      selected.labs/selected.cases -> lab.reports.per.case
      
      # % of cases identified vs. % cases worked
      ((total.cases-selected.cases)/total.cases)/((total.labs-selected.labs)/total.labs) -> eff.index
      
      #### PARSING ALGORITHM DEFINITION ####
      data.frame(a = c(as.character(display[selected[1:nrow(selected),1],1])),
                 a.1 = c(as.numeric(display[selected[1:nrow(selected),1],1])),
                 b = factor(c(names(display)[selected[1:nrow(selected),2]]),
                            levels=levels(df.load$result))) -> chk
      chk <- arrange(chk,b,a.1)
      
      chk2 <- unique(chk$b)
      
      for (i in 1:length(chk2)) {
        chk[chk$b == chk2[i],'c'] <- c(1,diff(as.numeric(chk[chk$b == chk2[i],'a.1'])))
      }
      
      chk$d <- lag(chk$b)
      
      ep <- 1
      
      for (i in 1:nrow(chk)) {
        
        if (is.na(chk[i,'d'])) {
          chk$e <- ep
        } else if(chk[i,'b'] == chk[i,'d']) {
          if (chk[i,'c'] == 1) {
            chk[i,'e'] <- ep
          } else if (chk[i,'c'] >= 1) {
            ep <- ep+1
            
            chk[i,'e'] <- ep
          }
        } else if(chk[i,'b'] != chk[i,'d']) {
          ep <- 1
          
          chk[i,'e'] <- ep
        }
      }
      
      chk %>%
        group_by(b,e) %>%
        summarize(.,first=min(as.numeric(a.1)),last=max(as.numeric(a.1))) -> chk3
      
      left_join(chk3, unique(chk[,c('a.1','a')]), by=c('first'='a.1')) %>%
        left_join(., unique(chk[,c('a.1','a')]), by=c('last'='a.1'))-> chk3
      
      chk3$d <- ifelse(chk3$first == chk3$last, as.character(chk3$a.x), paste0(chk3$a.x,' to ', chk3$a.y))
      
      dcast(chk3[,c("b","d")], d~b, value.var="d") -> chk4
      
      e <- vector()
      for (i in 2:length(chk4)) {
        paste(names(chk4)[i],paste(chk4[is.na(chk4[,i])==F,i],collapse = ", "), sep = ": ") -> f
        
        paste(e, f, sep="<br/>") -> e
      }
      substr(e, 6,nchar(e)) -> e
      
      #Save data frame for display later
      data.frame(
        Gender = input$gen.filter,
        prev = input$prev.filter,
        Test = input$test.filter,
        Labs = selected.labs,
        Cases = selected.cases,
        Loss = signif(pct.cases.lost,3),
        Gain = signif(efficiency.gained,3),
        NNT = signif(lab.reports.per.case,3),
        Eff.Ratio = signif(eff.index,3),
        selected = e,
        selection = paste(selected, collapse=",")
      ) ->> save.df
      assign('save.df',save.df, envir = .GlobalEnv)
      
      # full.selected <- data.frame()
      # for(i in 1:nrow(selected)) {
      #   row <- df.full3[df.full3$testresult == names(display)[[selected[i,2]]] & df.full3$agegrp5 == display[selected[i,1],1],]
      #   
      #   rbind(full.selected, row) -> full.selected
      # }
      # 
      # full.selected %>%
      #   ungroup() %>%
      #   summarise(.,N_IndexTx = sum(N_IndexTx,na.rm=T),
      #             N_NewSyphDx = sum(N_NewSyphTx,na.rm=T),
      #             N_NewHIVDx = sum(N_NewHIVDx,na.rm=T),
      #             Labs = sum(Freq,na.rm=T),
      #             Cases = sum(cases,na.rm=T)) -> full.selected2
      # 
      # 
      # names(full.selected2) <- c("Index Cases Treated", "Partners: New Cases of Syphilis",
      #                            "Partners: HIV Case Finding","Labs","Cases")
      # 
      # full.selected2$selected <- save.df$selected
      # assign('full.selected2',full.selected2, envir=.GlobalEnv)
      
      
      paste0(
        '<b>Selected Labs: </b>', selected.labs
      ) -> str.sl
      paste0(
        '<b>Selected Cases: </b>', selected.cases
      ) -> str.sc
      paste0(
        '<b>Case Finding Loss: </b>', signif(pct.cases.lost*100,3), '%'
      ) -> str1
      paste0(
        '<b>Efficiency Gained: </b>', signif(efficiency.gained*100,3), '%'
      ) -> str3
      paste0(
        '<b>Lab Reports per Verified Case: </b>', signif(lab.reports.per.case,3)
      ) -> str4
      paste0(
        '<b>Efficiency Index: </b>', signif(eff.index,3)
      ) -> str5
      
      HTML(paste('<br><b>All cells selected:</b>',str.sl, str.sc, str1, str3, str4, str5, sep='<br/>'))
      
    } else{
      h3('Select cells to run evaluation')
    }
  })
  
  #### Last Selected ####
  output$last_selected <- renderUI({
    l.selected <- NULL
    l.selected <- input$table_cell_clicked
    
    all <- input$table_cells_selected
    
    df.load <- filedata2()
    if (is.null(df.load)) return(NULL)
    
    if (input$gen.filter != "All") {
      df.load <- df.load[df.load$gender == input$gen.filter,]
    }
    if (input$prev.filter != "All") {
      df.load<- df.load[df.load$prev == input$prev.filter,]
    }
    if (input$test.filter != "All") {
      df.load <- df.load[df.load$type == input$test.filter,]
    }
    
    nrow(df.load) -> total.labs
    nrow(df.load[df.load$case_status == 'Case',]) -> total.cases
    
    df2 <- as.data.frame(with(df.load, table(age, result)))
    
    cases <- as.data.frame(with(df.load[df.load$case_status == 'Case',],
                                table(age, result)))
    
    names(cases)[3] <- "cases"
    
    df <- suppressMessages(full_join(df2, cases))
    
    if(length(all) > 0) {
      
      df.selected <- df[df$result == names(display)[[as.numeric(l.selected$col)]] & df$age == display[as.numeric(l.selected$row),1],]
      
      selected.labs <- df.selected$Freq
      selected.cases <- df.selected$cases
      
      #Percent of cases lost
      selected.cases/total.cases -> pct.cases.lost
      
      #efficiency gained
      selected.labs/total.labs -> efficiency.gained
      
      #lab reports per case found
      selected.labs/selected.cases -> lab.reports.per.case
      
      assign('s.labs',selected.labs,envir=.GlobalEnv)
      assign('s.cases',selected.cases,envir=.GlobalEnv)
      
      paste0(
        '<b>Selected Labs: </b>', selected.labs
      ) -> str.sl
      paste0(
        '<b>Selected Cases: </b>', selected.cases
      ) -> str.sc
      paste0(
        '<b>Selected cases/total cases: </b>', signif(pct.cases.lost*100,3), '%'
      ) -> str1
      paste0(
        '<b>Selected labs/total labs: </b>', signif(efficiency.gained*100,3), '%'
      ) -> str3
      paste0(
        '<b>Lab Reports per case found: </b>', signif(lab.reports.per.case,3)
      ) -> str4
      
      HTML(paste("<br><b>Last Selected Cell:</b>",str.sl,str.sc,str1, str3, str4, sep='<br/>'))
    }
    else{
      HTML('')
    }
    
  })
  
  #### Save Algorithm Button ####
  observeEvent(input$save.btn, {
    selected <- NULL
    selected <- input$table_cells_selected
    
    if (length(selected) > 0) {
      
      #### RG Metrics ####
      save.df <- save.df
      
      if (!exists('display.save.df')) {
        display.save.df <- save.df
      }      else {
        rbind(display.save.df, save.df) -> display.save.df
      }
      assign('display.save.df',display.save.df, envir = .GlobalEnv)
      
      # #### Treatment and Partners Metrics ####
      # df.selected2 <- full.selected2
      # 
      # if (length(df.addl) == 0) {
      #   df.addl <- df.selected2
      # }      else if (length(df.addl) > 0) {
      #   rbind(df.addl, df.selected2) -> df.addl
      # }
      # assign('df.addl',df.addl, envir = .GlobalEnv)
    }
  })
  
  #### Delete Button ####
  observeEvent(input$delete.btn, {
    selected2 <- NULL
    selected2 <- input$save_rows_selected
    assign('selected2',selected2,envir = .GlobalEnv)
    
    if (length(selected2) > 0) {
      assign('display.save.df',display.save.df[!(1:nrow(display.save.df) %in% selected2),], envir = .GlobalEnv)
    }
  })
  
  #### Evaluate: Main Output Table ####
  output$save <- DT::renderDataTable(datatable({
    selected <- NULL
    selected <- selected
    
    if (length(selected) > 0) {
      
      save.df <- save.df
      
      if (length(display.save.df) == 0) {
        display.save.df <- save.df
      }      else if (length(display.save.df) > 0) {
        rbind(display.save.df, save.df) -> display.save.df
      }
      assign('display.save.df',display.save.df, envir = .GlobalEnv)
    }
    
    if(input$save.btn|
       input$delete.btn|
       input$combine.btn) {
      unique(display.save.df[,c(-11)]) 
    }
  },selection = list(target = 'row'), escape = c(9),
  extensions = 'Scroller', options = list(dom = 't',
                                          scrollY = 600,
                                          scroller = TRUE, 2)
  ))
  
  #### Plot Window ####
  output$plot <- renderPlot({
    
    a <- display.save.df
    
    if(input$save.btn) {
      i <- ggplot(a, aes(Loss, Gain))
      
      ggplot(a, aes(Loss, Gain)) +
        geom_point(aes(colour = as.factor(row.names(a)))) +
        #geom_abline(intercept = 0, slope = 1) +
        coord_cartesian(xlim=c(0,max(a$Loss)+.1),ylim=c(0,max(a$Gain)+.1)) + 
        facet_grid(Test ~ Gender)
    }
  })
  
  # output$prev.panel <- renderUI({
  #   df <- filedata()
  #   if (is.null(df)) return(NULL)
  #   
  #   prevs <- prev.panel.f()
  #   
  #   selectInput("prev.order", "Select Previous Result Order", prevs, selectize = T, multiple = T)
  #   
  # })
  
  # output$test5 <- renderUI({
  #   prev.levels <<- input$prev.order
  # })
  # 
  
  # output$tabs <- renderUI({
  #   df <-filedata()
  #   if (is.null(df)) return(NULL)
  #   
  #   tabs <- c("age","result","gender","type","prev")
  #   labels <- c("Age","Test Result","Gender", "Test Type", "Prev.")
  #   
  #   tabsetPanel(
  #     tabPanel(title = "Data",
  #       tableOutput("filetable")
  #       )
  #     )
  # })
})

## ALTERNATIVE (MORE COMPLICATED) WAY TO MAKE SORTING PANEL:
# make.row2 <- function(x,y,z) {
#   fluidRow(
#     column(2,
#            selectInput(paste("result",gsub(":","",x),sep=""),x,choices=c(1:y),selected=z
#            ),
#            column(2,
#                   checkboxInput(paste("result",gsub(":","",x),sep=""),"Exclude?")
#            )
#     )
#   )
# }
#
# output$result.panel <- renderUI({
#   df <- filedata()
#   if (is.null(df)) return(NULL)
# 
#   results <- result.panel.f()
#   nResults <- length(results)
# 
#   mytabs<<-mapply(FUN=make.row2, results, z=1:nResults, MoreArgs = list(y=nResults))
#   do.call(fluidPage,mytabs[c(seq(3,nResults*3,3))])
#   #do.call(fluidPage,mytabs)
# })
# 
# output$test2 <- renderUI({
#   result.txt <<- paste0("c(",paste0(paste0("input$","`","result"),c(gsub(":","",results)),"`",collapse=","),")")
# 
#   result.order <<- eval(parse(text=result.txt))
# 
#   o <- order(as.numeric(result.order))
# 
#   result.levels <<- results[o]
# 
#   paste(result.levels, collapse = ",")
# 
# })

# output$result.display <- renderUI({
#   df <-filedata()
#   if (is.null(df)) return(NULL)
#   
#   checkboxInput("result.chk", "Include?", T)
# })