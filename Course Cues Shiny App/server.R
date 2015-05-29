library(shiny)
library(clue)
library(fpc)
library('curl')
library(jsonlite)
require(googleVis)


palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output) {
  
  
  #For assignments
  assignments <- reactive ({
    df <- dbGetQuery(con,"select assign_desc from assignment")
    df
                            
  })
  
  output$assignmentList <-renderText({
    
    list <- data.frame(assignments())
    colNames(list) <- ("AssignmentDescription")
    list$AssignmentDescription
  })
  
  
  #for students
  studentlist <- dbGetQuery(con,"select distinct stu_id from assignment_fact_table")
  
 ##for resources chart
  resc <- dbGetQuery(con,"select resource_desc, sum(num_accessed) as resourceaccesscount 
  from resource_fact_table rf join resource r on r.resource_id = rf.resource_id group by resource_desc")
  
 #for resource slider input
  resourceList <- dbGetQuery(con,"select resource_desc from resource")
  
  
 

  # For Alerts tab
  #Low resource count
  output$followup <-renderTable({
    followup<- dbGetQuery(con, "select student_id as 'Student ID', round(sum(num_accessed)) as Resource_Access_Count from resource_fact_table 
                          group by student_id having Resource_Access_Count < 30")
    
    followup  
  })
  
  
  #Low assign scores
  output$followup2 <-renderTable({
    followup<- dbGetQuery(con, "select  stu_id as 'Student ID', round(avg(assign_score)) as Assignment_Grade 
                                             from  assignment_fact_table    group by stu_id  having  Assignment_Grade  < 50")  
    followup
    
  })
 
  ## datatable for Course tab 
  output$courseDataTable <- renderDataTable({
    queryresult2 <-dbGetQuery(con, "select stu_id as 'Student ID', round(avg(assign_score)) as 'Assignment Score', resourceaccesscount as 
                                            'Resource Access Count', avg(a.final_score) as 'Final Score' from  assignment_fact_table a join
                                            (select student_id, sum(num_accessed) as resourceaccesscount from resource_fact_table 
                                            group by student_id) as r on  a.stu_id = r.student_id group by stu_id")
    #return the dataframe
   queryresult2
  })
  
 
  ##Simple Scatterplot
  output$scatterPlot <- renderPlot({
    y <- queryresult[, input$xcol] 
    x <- queryresult[,input$ycol] 
    #Plot the graph
    par(bg = "#E6F2FF")
  plot(x, y, main="Scatterplot", 
       xlab=input$ycol, ylab=input$xcol, pch=19)

  # Add fit lines
  abline(lm(y ~ x), col="red") # regression line (y~x) 
  #lines(lowess(x,y), col="blue") # lowess line (x,y)
  })
 
 
 #lm function 
 #predicted score
 output$predictedscore <- reactive({
   #Pass the variables and the dataset to lm function
   finalscore.lm = lm(finalscore ~ assignscore , queryresult2)
   #Get the coefficients
   coeffs = coefficients(finalscore.lm )
   #Predict grade
   predicted=coeff[1]+coeff[2]*input$score
   round(max(predicted))
   
 })
  
  
  
  ## K-means - Combine the selected variables from ui into a new data frame
 #get the data for the column names selected by user
  selectedData <- reactive({
   data <- data.frame(queryresult[, c(input$xcol, input$ycol)])
   # data <- data.frame(merge(queryresult, GPA(), by = "Student ID"))
   
   data
  })
  
 #compute kmeans for the above dataframe
  clusters <- reactive({
    scale(selectedData())
     kmeans(selectedData(), input$clusters)
    
  })
  
  
  #plot clusters
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    #use par function to set the background color of the plot even before you call the function.
    par(bg = "#E6F2FF")
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  ##clusplot
 #to see if there is an  overlap between clusters
  output$clusplot <-renderPlot({   
    par(bg = "#E6F2FF")
    clusplot(queryresult,clusters()$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0 )
    
  })
 
 
 ##display cluster info in the sidebar
 
 #cluster size
 output$clusterresult <-  renderText({     
   clusters()$size
 })
 
 #cluster means
 output$clusterresult1<-  renderTable({     
   clustercenters <-data.frame(clusters()$centers)
   clustercenters
 })
 
 
 #cluster table 
 output$clusterresult3<-  renderTable({     
   clustercenters3 <-table(clusters()$cluster)
   clustercenters3
 })
 
 #cluster spread?
 output$clusterresult2 <-  renderText({    
   clusters()$cluster
 }) 
  
 

###googleVis 
output$gvis <- renderGvis({
  bar = dbGetQuery(con,"Select   avg(final_score) as 'Final Score', round(avg(assign_score)) as 'Average Assignment Grade' from assignment_fact_table group by stu_id")
  #par(bg = "#E6F2FF") - does not work for gvis, need to pass it as an argument
  gvisBarChart(bar,options=list(backgroundColor="#E6F2FF", page='enable',height=1000,width=800, col ="grey"))
  
})


###Assignments tab
## Assignments bar chart
output$table4 <-  renderGvis({ 
 
  #Paste0 removes the space that is produced when the strings are contatenated. 
  c <- paste0("select   stu_id, assign_score as 'Assignment Grade' ,  assign_desc as 'Assignment Description' from assignment_fact_table af join
              assignment a on af.assign_id = a.assignment_id where a.assign_desc = '",input$aid,"' and assign_score <=",input$alimits,collapse = ", ")
  
  df1<-dbGetQuery(con,c)
  par(bg = "#E6F2FF")
  
  gvisBarChart(df1,options=list(backgroundColor="#E6F2FF", page='enable',height=600,width=900))
})

###for resource access
#For all resources
output$resources <- renderGvis({
  resc <- dbGetQuery(con,"select resource_desc, sum(num_accessed) as 'Resource Access Count' from 
resource_fact_table rf join resource r on r.resource_id = rf.resource_id group by resource_desc")

 par(bg = "#E6F2FF")
                     
                     gvisBarChart(resc,options=list(backgroundColor="#E6F2FF", page='enable',height=700,width=900))
})

#for resource input slider
#Student Engagement
output$resourceDataByStudent <-  renderGvis({ 

resourceQuery <- paste0("select   student_id,num_accessed as 'Resource Access Count', round(avg(assign_score))  as 'Average Assignment Grade' 
from assignment_fact_table af join resource_fact_table rf 
on af.stu_id = rf.student_id 
join
              resource r on rf.resource_id = r.resource_id 
            where resource_desc = '",input$rid,"' group by student_id",collapse = ", ")

resource.df <- dbGetQuery(con,resourceQuery)

gvisBarChart(resource.df,options=list(backgroundColor="#E6F2FF", page='enable',height=600,width=700))

})



##bubble chart of student, assignscore and resource_count

output$bubble <- renderGvis ({
  bubble1 = dbGetQuery(con,"Select  stu_id, avg(assign_score) as 'Average Assignment Score', avg(final_score)  
                       as 'Final Score' from assignment_fact_table  group by stu_id")
  par(bg = "#E6F2FF")
  gvisBubbleChart(bubble1,idvar="", xvar ="Final Score",yvar="Average Assignment Score",   
                  options=list(backgroundColor="#E6F2FF",     vAxis="{title:'Assignment'}",hAxis="{title:'Final Score'}", 
                               sizeAxis = '{minValue: 0,  maxSize: 10}'))
})


##Student dashboard images
#display text
output$text1 <-  renderText({ 
  #c <- paste("For student: ",input$sid)
  input$sid
})

#gauge for final score currently not displayed
output$gauge <- renderGvis ({
  g <- paste0("Select  stu_id,  avg(final_score)  as f from assignment_fact_table  where stu_id = '",input$sid,"'",collapse = ", ")
 gauge1 = dbGetQuery(con,g)
Gauge <-  gvisGauge(gauge1, 
                    options=list(min=60, max=100, greenFrom=80,
                                 greenTo=100, yellowFrom=70, yellowTo=80,
                                 redFrom=60, redTo=70, width=150, height=150))
})

#gauge for assign scores
output$gauge2 <- renderGvis ({
  g <- paste0("Select  stu_id,  avg(assign_score)  as f from assignment_fact_table  where stu_id = '",input$sid,"'",collapse = ", ")
  gauge2 = dbGetQuery(con,g)
  Gauge2 <-  gvisGauge(gauge2, 
                      options=list(min=40, max=70, greenFrom=60,
                                   greenTo=70, yellowFrom=50, yellowTo=60,
                                   redFrom=40, redTo=50, width=150, height=150))
})


#gauge for resource access count
output$gauge3 <- renderGvis ({
  g <- paste0("Select  student_id,  sum(num_accessed)  as a from resource_fact_table  where student_id = '",input$sid,"'",collapse = ", ")
  gauge3 = dbGetQuery(con,g)
  Gauge2 <-  gvisGauge(gauge3, 
                       options=list(min=10, max=250, greenFrom=100,
                                    greenTo=250, yellowFrom=50, yellowTo=100,
                                    redFrom=10, redTo=50, width=150, height=150))
})


# file upload for kmeans prediction
#output$contents <- reactive({
uploadedscores <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)  
  newfile <- data.frame( read.csv(inFile$datapath, header=TRUE, sep=',',
           quote=''))
  newfile
  
    
  })


#put the  contents of uploaded file into an output object
output$contents <- renderTable({  
  uploadedscores() 
  })


# predict the clusters using cl_predict and display the predicted results
#clusters() returns kmeans cluster and 
#uploadedscores returns the newly uploaded scores ina dataframe

output$predictedcluster <-renderTable({
 
  pcluster <- table(cl_predict(clusters(), uploadedscores())) 
})


#Use REST API to get student GPAs
GPA <- reactive({
  response <-"https://plot.ly/~Kalpi/254.json"
  #get the data from JSON
  document <- as.data.frame(fromJSON(response))
  gpa <-data.frame (document$data.y)
  studentID <-data.frame(document$data.x) 
  studentsgpa <- data.frame(studentID, gpa)
  colnames(studentsgpa)<- c("Student ID", "gpa")
  studentsgpa
})


#get average assignment grades and final grades

grades <-reactive ({
#df <-dbGetQuery(con, "select   stu_id as 'Student ID', avg(a.assign_score)

  #as 'Current assignment Grade' from  assignment_fact_table a
   #                 group by stu_id")
  
df <-dbGetQuery(con, "select  stu_id as 'Student ID', avg(assign_score) as assignscore, 
                                            resourceaccesscount  from  assignment_fact_table a join
                                             (select student_id, sum(num_accessed) as resourceaccesscount from resource_fact_table 
                                             group by student_id) as r on  a.stu_id = r.student_id group by stu_id")

df
})



#merge REST api call response (student ID and GPA) with average assignment grades dataset
#Returns GPA, resource use and assignment grades without student ID
gpaMergeFunction <-reactive({
  GPAgradeMerge<- merge( GPA(),grades(),by="Student ID")
  GPAgradeMerge[-1]
  
})


#Returns data along with  student ID
gpaMergewithstuid <-reactive({
  GPAgradeMerge<- merge( GPA(),grades(),by="Student ID")
  GPAgradeMerge
  
})


#display student ID, course grade and Gpa as a table 
output$academicHistory<-renderTable({  
  #academicHistory <- data.frame(cbind(stuID,GPA()))
  #add column names to dataframe
  #colnames(academicHistory) <- c("Student ID","GPA")
  #academicHistory
  gpaMergewithstuid ()
}) 


#kmeans with assignment grades and GPA
gpakmeansClusters<-reactive({
 # GPAgradeMerge<- merge( GPA(),grades,by="Student ID") 
  kmeansgpa <- kmeans(gpaMergeFunction(),3)
  kmeansgpa  
  
})


#GPA cluster means as a dataframe
gpaMeansClustersDataFrame <-reactive ({
  clustercentersgpa <-data.frame( gpakmeansClusters()$centers)
  clustercentersgpa
  
})


#return table with cluster means
output$gpakmeansTable <-renderTable({
  gpaMeansClustersDataFrame()
  
})

#gpa and assignment cluster size
output$GPAclusterresult <-  renderTable({     
  gpaClustersTable <- table(gpakmeansClusters()$cluster)
  gpaClustersTable
})

#plot clusters with gpa and assign grades
output$plot2 <- renderPlot({
  par(mar = c(5.1, 4.1, 0, 1))
  #use par function to set the background color of the plot even before you call the function.
  par(bg = "#E6F2FF")
  plot(gpaMergeFunction(),
       col = gpakmeansClusters()$cluster,
       pch = 20, cex = 3)
  #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
})



})