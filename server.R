#Random comment


# Define a server for the Shiny app
shinyServer(function(input, output, session) {
  
  #### SECTION: Define ####
  
  #### Selectors ####
#   df2 <- reactive({
#     if (input$gender != "All") {
#       df <<- df[df$gen_comb == input$gender,]
#     }
#     if (input$fourfold != "All") {
#       df <<- df[df$fourfold == input$fourfold,]
#     }
#     if (input$test != "All") {
#       df <<- df[df$test == input$test,]
#     }
#   })
  
  #### Render Grid ####
  output$table <- renderDataTable(datatable({
    df <- df
    
    if (input$gender != "All") {
      df <- df[df$gen_comb == input$gender,]
    }
    if (input$fourfold != "All") {
      df<- df[df$fourfold == input$fourfold,]
    }
    if (input$test != "All") {
      df <- df[df$test == input$test,]
    }
    
    display <- as.data.frame(with(df, table(agegrp5, testresult)))
    
    factor(display$testresult, 
           levels=c('','Reactive','Non-R','1:1','1:2','1:4','1:8','1:16','1:32+','POS','QNS','Unknown','WR')
    ) -> display$testresult
    
    cases <- as.data.frame(with(df[df$case_status == 1,],
                                table(agegrp5, testresult)))
    factor(cases$testresult, 
           levels=c('','Reactive','Non-R','1:1','1:2','1:4','1:8','1:16','1:32+','POS','QNS','Unknown','WR')
    ) -> cases$testresult
    
    names(cases)[3] <- "cases"
    
    display <- suppressMessages(full_join(display, cases))
    
    display2 <- display
    display2$p.case <- display2$cases/display2$Freq
    ifelse(is.nan(display2$p.case) == T, 0,display2$p.case) -> display2$p.case
    display2$Freq <- NULL
    display2$cases <- NULL
    display2 <- spread(display2, testresult, p.case)
    display2 <- display2[,c(-2)]
    
    display <- unite(display, Cases.Labs, cases, Freq, sep = "/")
    
    display <- spread(display, testresult, Cases.Labs)
    
    display <- display[,c(-2)]
    
    names(display2)[2:13] <- paste0('p.',names(display2)[2:13])
    full_join(display, display2, "agegrp5") -> display3
    
    brks <- quantile(display3[,c(14:25)], probs = seq(.40, .95, .05), na.rm = TRUE)
    rev(heat_hcl(length(brks)+1)) -> my_palette
    assign('my_palette',my_palette, envir=.GlobalEnv)
    
    assign('display', display, envir = .GlobalEnv)
    
    display3
  }, options=list(pageLength = 13, columnDefs = list(list(targets = 14:25, visible = FALSE))),
  selection = list(target = 'cell')) %>% formatStyle(names(display3)[2:13], names(display3)[14:25],
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
  
  #### CREATE SELECTED CELLS OBJECT ####
  #   selector <- reactive({
  #     selected <<- input$table_cells_selected
  #   })
  
  #assign('selected', selected, envir = .GlobalEnv)
  
  #### Render Legend ####
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
    df <- df
    
    if (input$gender != "All") {
      df <- df[df$gen_comb == input$gender,]
    }
    if (input$fourfold != "All") {
      df<- df[df$fourfold == input$fourfold,]
    }
    if (input$test != "All") {
      df <- df[df$test == input$test,]
    }
    
    nrow(df) ->> total.labs
    nrow(df[df$case_status == 1,]) ->> total.cases
    
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
    
    df <- df
    
    if (input$gender != "All") {
      df <- df[df$gen_comb == input$gender,]
    }
    if (input$fourfold != "All") {
      df<- df[df$fourfold == input$fourfold,]
    }
    if (input$test != "All") {
      df <- df[df$test == input$test,]
    }
    #     if (input$testresult != "All") {
    #       df <- df[df$testresult %in% input$testresult,]
    #     }
    
    nrow(df) -> total.labs
    nrow(df[df$case_status == 1,]) -> total.cases
    
    df2 <- as.data.frame(with(df, table(agegrp5, testresult)))
    
    cases <- as.data.frame(with(df[df$case_status == 1,],
                                table(agegrp5, testresult)))
    
    names(cases)[3] <- "cases"
    
    df <- suppressMessages(full_join(df2, cases))
    
    if(length(selected) > 0) {
      
      df.selected <- data.frame()
      for(i in 1:nrow(selected)) {
        row <- df[df$testresult == names(display)[[selected[i,2]]] & df$agegrp5 == display[selected[i,1],1],]
        
        rbind(df.selected, row) -> df.selected
      }
      
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
                            levels=c('Reactive','Non-R','1:1','1:2','1:4','1:8','1:16','1:32+','POS','QNS','Unknown','WR'))) -> chk
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
        Gender = input$gender,
        Fourfold = input$fourfold,
        Test = input$test,
        Labs = selected.labs,
        Cases = selected.cases,
        Loss = paste0(signif(pct.cases.lost*100,3),'%'),
        Gain = paste0(signif(efficiency.gained*100,3),'%'),
        NNT = signif(lab.reports.per.case,3),
        Eff.Ratio = signif(eff.index,3),
        selected = e,
        selection = paste(selected, collapse=",")
      ) ->> save.df
      
      assign('save.df',save.df, envir = .GlobalEnv)

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
    
    df <- df
    
    if (input$gender != "All") {
      df <- df[df$gen_comb == input$gender,]
    }
    if (input$fourfold != "All") {
      df<- df[df$fourfold == input$fourfold,]
    }
    if (input$test != "All") {
      df <- df[df$test == input$test,]
    }
    
    nrow(df) -> total.labs
    nrow(df[df$case_status == 1,]) -> total.cases
    
    df2 <- as.data.frame(with(df, table(agegrp5, testresult)))
    
    cases <- as.data.frame(with(df[df$case_status == 1,],
                                table(agegrp5, testresult)))
    
    names(cases)[3] <- "cases"
    
    df <- suppressMessages(full_join(df2, cases))
      
      if(length(all) > 0) {
        
        df.selected <- df[df$testresult == names(display)[[as.numeric(l.selected$col)]] & df$agegrp5 == display[as.numeric(l.selected$row),1],]
        
        selected.labs <- df.selected$Freq
        selected.cases <- df.selected$cases
        
        #Percent of cases lost
        selected.cases/total.cases -> pct.cases.lost
        
        #efficiency gained
        selected.labs/total.labs -> efficiency.gained
        
        #lab reports per case found
        selected.labs/selected.cases -> lab.reports.per.case
        
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
  
  #### Delete Button ####
  observeEvent(input$delete.btn, {
    selected2 <- NULL
    selected2 <<- input$save_rows_selected
    assign('selected2',selected2,envir = .GlobalEnv)
    
    if (length(selected2) > 0) {
      
      display.save.df <<- display.save.df[!(1:nrow(display.save.df) %in% selected2),]
      
      assign('check',display.save.df, envir = .GlobalEnv)
    }
  })
  
#   #### Saved Algorithm Table ####
#   output$save <- DT::renderDataTable(datatable({
#     d <- display.save.df
#     if (length(d) == 0) {
#       d <- save.df
#     } else if (length(d) > 0) {
#       rbind(d, save.df) -> d
#     }
#     
#     if(input$delete.btn) {
#       d <- d[!(1:nrow(d) %in% selected2),]
#     }
#     
#     #assign('display.save.df',display.save.df, envir = .GlobalEnv)
#   
#     if(input$save.btn|
#        input$delete.btn) {
#       d 
#     }
#   },selection = list(target = 'row'), escape = c(9), options = list(searchable = F))
#   )
  
  #### Save Algorithm Button ####
  observeEvent(input$save.btn, {
    selected <- NULL
    selected <- input$table_cells_selected
    
    if (length(selected) > 0) {
      
      save.df <- save.df
      
      if (length(display.save.df) == 0) {
        display.save.df <- save.df
      }      else if (length(display.save.df) > 0) {
        rbind(display.save.df, save.df) -> display.save.df
      }
      assign('display.save.df',display.save.df, envir = .GlobalEnv)
    }
  })
  
  observeEvent(input$delete.btn, {
    selected2 <- NULL
    selected2 <- input$save_rows_selected
    assign('selected2',selected2,envir = .GlobalEnv)
    
    if (length(selected2) > 0) {
      assign('display.save.df',display.save.df[!(1:nrow(display.save.df) %in% selected2),], envir = .GlobalEnv)
    }
  })
  
  #### Combine Button ####
  observeEvent(input$combine.btn, {
    selected2 <- NULL
    selected2 <- input$save_rows_selected
    
    if(length(selected2) > 0) {
      d <- display.save.df[1:nrow(display.save.df) %in% selected2,]
      d$Gender <- as.character(d$Gender)
      d$Fourfold <- as.character(d$Fourfold)
      d$Test <- as.character(d$Test)
      
      d$Loss %>%
        as.character(.) %>%
        substr(1,(nchar(.)-1)) %>%
        as.numeric(.) -> d$Loss
      
      d$Gain %>%
        as.character(.) %>%
        substr(1,(nchar(.)-1)) %>%
        as.numeric(.) -> d$Gain
      
      ### All, All, All? ####
      if('All' %in% d[,1] == T & 'All' %in% d[,2] == T & 'All' %in% d[,3] == T) {
        ### Yes ###
        all <- d[d[,1] == 'All' & d[,2] == 'All' & d[,3] == 'All',]
        
          ### More than 1? ###
          if (length(all) > 1) {
            ### Yes ###
            
          } else {
            ### No ###
          }
          
          
          
      }
        
        d <- d[!(as.character(d[,1]) == 'All' & 
                   as.character(d[,2]) == 'All' & 
                   as.character(d[,3]) == 'All'),]
        
        comb.err <<- paste("Ignored rows for Overall algorithms (where Gender, Fourfold and Test = All)")
      
        
      
        ### No ###
      
          ### Any same category? ###
          
            ### Yes ###
      
            ### No ###
      
              ### Can the groups be combined? ###
              
                ### Yes ###
        
                ### No ###
    }
  })
  
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
  },selection = list(target = 'row'), escape = c(9), options = list(searchable = F))
  )
  
  #### Plot Window ####
  output$plot <- renderPlot({
    
    a <- display.save.df
    
    a$Loss %>%
      as.character(.) %>%
      substr(1,(nchar(.)-1)) %>%
      as.numeric(.) -> a$Loss
    
    a$Gain %>%
      as.character(.) %>%
      substr(1,(nchar(.)-1)) %>%
      as.numeric(.) -> a$Gain
    
    if(input$save.btn) {
      i <- ggplot(a, aes(Loss, Gain))
      
      ggplot(a, aes(Loss, Gain)) +
        geom_point(aes(colour = as.factor(rownames(a))), size=4) +
        #geom_abline(intercept = 0, slope = 1) +
        coord_cartesian(xlim=c(0,max(a$Gain)+1),ylim=c(0,max(a$Gain)+1)) + 
        facet_grid(Test ~ Gender)
    }
  })
})

