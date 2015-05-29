library(shiny)
library(shinythemes)
library(shinydashboard)
library(fpc)
require(googleVis)

# Define UI for the course cues web portal
shinyUI(fluidPage(
  style = "background-color: #E6F2FF;height:1400px",
  
  #Can also use a css file 
  #includeCSS("mycss.css"),
  
   # Application title
   titlePanel (    
     title =""
     #h1( "Course Cues - Learning Analytics Initiative")
   ),
  img(src="la2.jpg",width=2000,height = 200),
  
  # Sidebar - customized for each tab 
    sidebarPanel(       
      style = "background-color:#D5E9FF; ",     
      conditionalPanel(condition="input.conditionedPanels == 'Home'",                     
                       h3("About "),
                       tags$br(),
                       div( style ="font-size: 16px;","Course Cues is the one-stop shop for all your course, resource and student engagement information access
                        and reporting needs."), br(), div( style ="font-size: 16px;","Please click one of the tabs to get started. "),
                       tags$br()
                      
      ), 
      
      conditionalPanel(condition="input.conditionedPanels == 'Alerts'",                       
                       h2("Alerts",style ="color:red;" )
      ),  
      
      conditionalPanel(condition="input.conditionedPanels == 'Course'",                      
                       h2("Course" ),
                       br(),
                       selectInput('cid', 'Search by course', "IT xyz")
      ),  
      
      conditionalPanel(condition="input.conditionedPanels == 'Students'", 
                       h2("Students" ),
                       br(),
                       selectInput('sid', 'Search by student', studentlist$stu_id)
      ),
      
      
      conditionalPanel(condition="input.conditionedPanels == 'Assignments'", 
                       
                       h2("Assignments" ),
                       br(),
                       selectInput('aid', 'Search by Assignment', assignments$assign_desc),
                       sliderInput('alimits', 'Choose max score', 0, 100,100,  step = NULL,                                   
                                   ticks = TRUE, animate = FALSE)
      ),
      
      
      conditionalPanel(condition="input.conditionedPanels == 'Resources'",                        
                       h2("Resources" ),
                       br()
      ),
      
      conditionalPanel(condition="input.conditionedPanels == 'Student Engagement'",                        
                       h2("Student Engagement" ),
                       br(),
                       selectInput('rid', 'Search by Resource', resourceList$resource_desc)
      ),
      
      conditionalPanel(condition="input.conditionedPanels == 'Academic History'",                        
                       h2("Academic History" ),
                       br()
      ),
         
      conditionalPanel(condition="input.conditionedPanels == 'Scatter Chart'", 
                       
                       h2("Linear Prediction" ),
                       br(),
                       selectInput('xcol', 'Choose X Variable', c('Final grade', 'Average assignment grade','Resource use')),
                       selectInput('ycol', 'Choose Y Variable', names(queryresult),                       
                        selected=names(queryresult)[[2]]),
                       br(),                       
                       #predict scores                       
                       h4("Predict final score based on current average assignment score "),
                       numericInput('score', 'Enter score: ',0), 
                       br(),
                       h4("Predicted Final Score based on this model: "),verbatimTextOutput('predictedscore')                     
     ),
      
    
      conditionalPanel(condition="input.conditionedPanels == 'K-means 1'", 
                       
                       h3("K-Means Clustering" ),
                       h3(" Model #1" ),
                       br(),
                       numericInput('clusters', 'Choose # of clusters', 3,
                                    min = 1, max = 9),
                       br(),                  
                       #results in text form
                       h4("Results from R "),
                       
                      h5("Cluster size"),    
                      tableOutput('clusterresult3'),                                 
                      h5("Cluster means"),    
                      tableOutput('clusterresult1'),
                      br(),
                      
                       # Button to import data
                       fileInput('file1', 'Test the model on new data. Choose CSV/TXT File',
                                 accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                       br(),
                       #h5("Contents of the uploaded file."),
                     # tableOutput('contents'),
                      br(),
                    
                   # actionButton("predict","Show predicted clusters"),
                      br(),
                      h5("Results of prediction")
                     , tableOutput('predictedcluster')
                  
      ),
     
      conditionalPanel(condition="input.conditionedPanels == 'K-Means 2'", 
                       h3("K-means Clustering " ),
                       h3(" Model #2" ),
                       br(),
                       h4('Cluster Means'),
                       tableOutput('GPAclusterresult'),
                       br(),
                       h4('Cluster Distribution'),
                       tableOutput('gpakmeansTable')
      )
            
    ),
  
  
    # mainpanel contains the tabset panel which in turn contains several tabpanels
    mainPanel(     
      tabsetPanel(well="true",            
        tabPanel(style ="font-size: 16px;",    'Home', 
                 h2( "Welcome to Course Cues!"),
                 br(),
                 div( style ="font-size: 16px;","With Course cues,you can access our  real-time 
                           reporting  system that makes analysis a breeze. Leverage the data to make data driven decisions. 
                          If you need further assistance, contact Course Cues staff at 424-6667. 
                    We would love your feedback as the initiative progresses. 
                  
                     ", tags$a(href="https://leapforward.illinoisstate.edu/", "Click here!"))
                
                 ),
        
        tabPanel('Alerts',
                
                 br(),              
                 br(),
                 h4("Low resource access", style ="color:red;"),
                 tableOutput('followup'),                 
                 br(),                
                 h4("Low assignment scores", style ="color:red;"),
                 tableOutput('followup2'),
                 br(),                 
                 h4("Low Participation", style ="color:red;")              
        ),
        
        
        tabPanel('Course',
                 h3("IT XYZ "),
                 dataTableOutput('courseDataTable')),
        
        tabPanel('Assignments',
                 htmlOutput('table4')
                ),
                  
        tabPanel('Resources',
                 htmlOutput('resources')
                 ),
        
        
        tabPanel('Student Engagement',
                 htmlOutput('resourceDataByStudent')
                 ),
        
        tabPanel('Students', 
                 
                 h3("Current status for  : "),
                 textOutput("text1"),
                 flowLayout(
                   h4("Resource access count"), h4("Average assignment score")                   
                 ),
                 
                 flowLayout(  
                   #flowlayout ensures that the gauges are horizontally aligned.
                   htmlOutput("gauge3"),
                   htmlOutput("gauge2")
                   #currently not displaying final score
                   #,htmlOutput("gauge")
                 )
                 
        ),  
        
        tabPanel('Academic History',
                 br(),
                
                 h4("GPA, Average Assignment Grade and Resource use"),
                 tableOutput('academicHistory')  ),
        
        
        tabPanel('Scatter Chart',
                 plotOutput('scatterPlot')  ),
 
        tabPanel('K-means 1',
                 plotOutput('plot1'),
                 plotOutput('clusplot')
                
                 ),
        
        
        tabPanel('K-Means 2',
                 #htmlOutput("bubble"),
                 br(),
                 br(),
                 h4("Clustering based on Average Assignment grade, Resource use and GPA "),
               
                 tableOutput('displayGPA'),
                 plotOutput('plot2'),
                 br()
                 
                 )
        
        ,id = "conditionedPanels"          
     
      ) 
    )
))