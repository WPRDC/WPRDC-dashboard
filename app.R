# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above (from the RStudio interface).

# Define the production variable by running authentication.R.
source("authentication.R")

force_refresh <- FALSE
if(!production) {
  setwd("/Users/drw/WPRDC/Dashboards/Shiny/WPRDC-dashboard")
} else {
  source("setwd_to_file_location.R")
}

source("prelude.R", local=TRUE)

ui <- shinyUI(fluidPage(
  tags$head(
    tags$head(includeScript("google-analytics.js")),
    tags$style(
    HTML('
         #sidebar {
         background-color: #000;
         color: white;
         }
         
         body, label, input, button, select { 
         font-family: Optima,"Lucida Grande",Tahoma,"Gill Sans",Helvetica;
         }
         .jqstooltip {
             -webkit-box-sizing: content-box;
             -moz-box-sizing: content-box;
             box-sizing: content-box;
         }   
         ')
  )),
  # Application title
  titlePanel("",windowTitle = "Data Center Metrics (Beta)"),
  
  fluidRow(
    HTML("<div style='background-color:white;float:left;color:black;
         width:100%;margin:0;padding:0;left:0;
         font-size:200%'>Data Center Metrics (Beta)"),
    HTML("<span style='float:right'>"),
    img(src="images/small-WPRDC-logo-inverted.png"),
    HTML("</span>"),
    # Shiny R wants all images and JavaScript and stuff to be nested in a
    # folder called "www" which is in the same directory as app.R.
    # So images would be in dashboard_folder/www/images/, but when 
    # you call them, you drop the "www", e.g., img(src="images/z.png")
    HTML("</div>"),
    
    #            p(HTML("<a href=\"javascript:history.go(0)\">Reset this page</a>"))
    
    tabsetPanel(id = "WPRDC-dashboard",
      tabPanel("Web stats",
               fluidRow(column(8, align="right",plotOutput("plot1")),
                        column(width = 2, wellPanel(
                          radioButtons("plot_type", "Plot",
                                       c("Users", "Sessions", "Pageviews")
                          )))
                        ),
               dataTableOutput('analytics_table')), 
      # Show a plot of the generated distribution
      #         h2("Cumulative statistics"),
      #h2('Download stats'),
      tabPanel("Other web stats",h3("Publishers"),dataTableOutput('publishers'),
               h3("Automated Data Imports"),dataTableOutput('etl'),
               h3("Discussion Posts & Data Requests"),dataTableOutput('misc')
      ),
      tabPanel("Resource stats",DT::dataTableOutput('downloads_table'),
               HTML("<div style='font-size:80%'>* A month-by-month list of downloads can be found <a href='https://data.wprdc.org/dataset/wprdc-statistics/resource/e8889e36-e4b1-4343-bb51-fb687eb9a2ff'>here</a>.</div>"),
               HTML("<div style='font-size:80%'>** API calls are dominated by internal operations and are not a good measure of public usage.</div>"),
               HTML("<div style='font-size:80%'>(Note that downloads have only been tracked since March 2016, while pageviews have been tracked since October 2015.)</div>"),
               downloadButton('downloadResourceData', 'Download'),
               shiny::actionButton(inputId='ab1', label="Download monthly-downloads data", 
                                   icon = icon("th"), 
                                   onclick ="window.open('https://data.wprdc.org/datastore/dump/d72725b1-f163-4378-9771-14ce9dad3002', '_blank')")
      ),
      tabPanel("Dataset stats",DT::dataTableOutput('package_table'),
               HTML("<div style='font-size:80%'>* This data can be obtained by summing by dataset package the month-by-month list of downloads (which can be found <a href='https://data.wprdc.org/dataset/wprdc-statistics/resource/e8889e36-e4b1-4343-bb51-fb687eb9a2ff'>here</a>).</div>"),
               HTML("<div style='font-size:80%'>(Note that downloads have only been tracked since March 2016, while pageviews have been tracked since October 2015.)</div>"),
               downloadButton('downloadDatasetData', 'Download')
      ),
      tabPanel("Classroom uses",
               fluidRow(
                   splitLayout(cellWidths = c("10%", "80%", "10%"), HTML("<P>"), plotOutput("uses_plot"), HTML("<P>"))
                 ),
               dataTableOutput('uses_table'),
               HTML(sprintf("Total classroom uses: %d (Pitt: %d, CMU: %d)", 
                            classroom_uses, pitt_uses, cmu_uses))),
      tabPanel("Outreach",
               HTML("<center style='font-size:140%'>Breakdown of outreach/events</center>"),
               #<span style='font-size:75%'>(Last 90 days in orange)</span>
               fluidRow(
                 splitLayout(cellWidths = c("10%", "80%", "10%"), HTML("<P>"), plotOutput("event_types_plot"), HTML("<P>"))
               ),
               HTML("<hr>"),
               fluidRow(HTML("<div class='row' style='margin:1em'>
                             <div class='col-sm-8' style='font-size:170%'>Total outreach instances & events: <b>",
                             outreach_and_events,
                             "</b></div>",
                             "<div class='col-sm-4' style='font-size:170%'>Media mentions:<b>",
                             media_mentions,
                             "</b></div></div>")),
               hr(),
               HTML("<center style='font-size:140%'>Twitter-follower counts</center>"),
               dataTableOutput('twitter_followers')
      ),
      tabPanel("Top Tens",
               fluidRow(
                 column(12,
                   HTML(paste("<br><div style='font-size:150%;margin-left:3em;'><b>Top 10 Datasets by Pageviews for the Last 30 Days</b><br>", convert_to_html_list(top_10_by_pageviews), "</div>", sep=" ")),
                   HTML(paste("<br><div style='font-size:150%;margin-left:3em;'><b>Top 10 Resources by Average Monthly Pageviews since First Download</b><br>", convert_to_html_list_4(top_10_by_average_pageviews), "</div>", sep=" "))
                 )
               )
      ),
    tabPanel("About",
               fluidRow(
                 column(12,
                    includeHTML("about.html")
                  )
                 )
              )
    )
    )
  )
  )

# Define server logic required to render the output
server <- shinyServer(function(input, output) {
    output$plot1 <- renderPlot({
      hues <- c("#0066cc","#0090cc")
      if (input$plot_type == "Users") {
#      barplot(site_stats$users,names.arg=month_list,ylab="Users by Month",xlab="",
#              col=c("#0066cc"),cex.names=1.5,cex.lab=1.5)
      adjust_axis_and_plot(sbm_users,"Users by Month",month_list,hues)
    } else if (input$plot_type == "Sessions") {
      adjust_axis_and_plot(sbm_sessions,"Sessions by Month",month_list,hues)
    } else if (input$plot_type == "Pageviews") {
      adjust_axis_and_plot(sbm_pageviews,"Pageviews by Month",month_list,hues)
    }
    legend("bottomleft",inset=c(0.05,0.05), fill=rev(hues), legend=c("This month","Previous months"))
  })
  output$analytics_table = DT::renderDataTable({
    site_stats_reversed[,!(names(site_stats_reversed) %in% c("year","month"))] #df_analytics
  },options = list(lengthMenu = c(12, 24, 48), pageLength = 12),rownames=FALSE)
  output$uses_plot = renderPlot({
    ## lots of extra space in the margin for sides 2 and 4 (left and right)
    op <- par(mar = c(4,5,4,4) + 0.1)
    counts <- xtabs(count ~ reorder(institution,index) + reorder(term,index), df_uses)
    barplot(counts, 
            xlab="Term", ylab="Classes", col=c("#1295ba","#dd731c","#2bba12"),
            legend = rownames(counts), args.legend = list(x="topleft",inset=c(0.05,0.05)), beside=TRUE, cex.names=1.5, cex.lab=1.5)
    #chrt <- barchart(count~reorder(term,index),data=df_uses,groups=reorder(institution,index),
    #         col=c("#1295ba","#c12e38","#2bba12"),
    #         ylab=list(label="Count",cex=1.7),
    #         scales=list(cex=c(1.7,1.7))) #Pitt blue and gold: #1c2957, #cdb87d
    # CMU red: #cc0023
    #update(chrt, par.settings = list(fontsize = list(text = 8, points = 4))) This doesn't
    #work since you need to adjust the grid parameters to affect lattice plots:
    
    par(op) ## reset
  })
  output$uses_table = DT::renderDataTable({
    df_uses
  },options = list(lengthMenu = c(20, 30, 50), pageLength = 20),rownames=FALSE)
  output$downloads_table = DT::renderDataTable({
    the_downloads_table
  })
  output$downloadResourceData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = "resource_downloads_and_pageviews.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ","
      # Write to a file specified by the 'file' argument
      write.table(resource_download_df, file, sep = sep, row.names = FALSE)
    }
  )
  output$package_table = DT::renderDataTable({
    package_downloads_table
  })
  output$downloadDatasetData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = "dataset_downloads_and_pageviews.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ","
      # Write to a file specified by the 'file' argument
      write.table(package_download_df, file, sep = sep, row.names = FALSE)
    }
  )
  output$publishers = DT::renderDataTable({
    publishers
  },rownames=FALSE)
  output$etl = DT::renderDataTable({
    etl_processes
  },rownames=FALSE)
  output$misc = DT::renderDataTable({
    misc_other_stats
  },rownames=FALSE)
  output$twitter_followers = DT::renderDataTable({
    twitter_followers
  },rownames=FALSE)
  output$event_types_plot = renderPlot({
    ## lots of extra space in the margin for sides 2 and 4 (left and right)
    op <- par(mar = c(4,12,1,4) + 0.1)
    barplot(sort(table(outreach_events_table$Type),decreasing=FALSE),
             las=1,xlab="Count",col=c("#0066cc"),horiz=TRUE)
    par(op) ## reset
  })
  #output$recent_event_types_plot = renderPlot({
  #  dotchart(sort(table(recent_events$Type),decreasing=FALSE),
  #           las=1,xlab="Count",col=c("#ff9900"))
  #})
})

# Run the application 
shinyApp(ui = ui, server = server)

