
# Shiny Code

library(shiny)

# Load the data

dff = read.csv("LOS_MIMIC-data.csv")

# Fit our model 

fit = lm(LOS ~ DIAG_COUNT + CPT_COUNT + ADMISSION_TYPE + AGE, data = dff)

preds = function(fit, DIAG_COUNT, CPT_COUNT, ADMISSION_TYPE, AGE){
  # get the predicted LOS from new data
  LOS = predict(object=fit, 
                 newdata = data.frame(
                   ADMISSION_TYPE = factor(ADMISSION_TYPE, levels=c('Emergency', 'Urgent', 'Elective')), 
                   DIAG_COUNT = DIAG_COUNT, 
                   CPT_COUNT = CPT_COUNT,
                   AGE = AGE
                   ))
  
  # return as character string that can be easily rendered
  if (LOS < 0) {
    print("Patient Discharged")
  } else
  return(as.character(round(LOS, 2)))
}


# Shiny Code

app = shinyApp(ui = fluidPage(title = 'Predicting Length of Stay in the Hospital :',
                               
                               # Create inputs for each variable in the model 
                               sliderInput('DIAG_COUNT', label = 'Number of diagnoses', 
                                           min = floor(min(dff$DIAG_COUNT)), 
                                           max = ceiling(max(dff$DIAG_COUNT)),
                                           value = floor(mean(dff$DIAG_COUNT))),
                               
                               sliderInput('CPT_COUNT', label = 'Number of procedures performed', 
                                           min = floor(min(dff$CPT_COUNT)), 
                                           max = ceiling(max(dff$CPT_COUNT)),
                                           value = floor(mean(dff$CPT_COUNT))),
                               
                               sliderInput('AGE', label = 'Age', 
                                           min = floor(min(dff$AGE)), 
                                           max = ceiling(max(dff$AGE)),
                                           value = floor(mean(dff$AGE))),
                               
                               radioButtons('ADMISSION_TYPE', label='Admission type',
                                            choices = levels(dff$ADMISSION_TYPE),
                                            inline=TRUE),
                               
                               
                               # return our estimate
                               h3("Predicted length of stay in the hospital in days : ", textOutput('prediction'))),
                
   server = function(input, output){
                  # pass our inputs to our prediction function defined earlier
                  # and pass that result to the output
                  output$prediction = renderText({
                    preds(fit = fit, 
                          CPT_COUNT = input$CPT_COUNT,
                          DIAG_COUNT = input$DIAG_COUNT,
                          AGE = input$AGE,
                          ADMISSION_TYPE = input$ADMISSION_TYPE
                    )
                  })
                })

# Finally, run it !!
runApp(app)
   
   