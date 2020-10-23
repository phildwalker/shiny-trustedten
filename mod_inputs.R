# module for input information

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c(
  "consent",
  # responses about themselves		
  "self_school", "self_class", "self_year", "self_gender", "self_orientation", "self_age", "self_SES", "self_ED", "self_Race", "self_marital", "self_able", "amt",
  # responses about their trusted circle          
  "influ1_relation", "influ1_race", "influ1_orientation","influ1_marital", "influ1_able", "influ1_gender","influ1_age", "influ1_SES","influ1_ED",
  "influ2_relation", "influ2_race", "influ2_orientation","influ2_marital", "influ2_able", "influ2_gender","influ2_age", "influ2_SES","influ2_ED",
  "influ3_relation", "influ3_race", "influ3_orientation","influ3_marital", "influ3_able", "influ3_gender","influ3_age", "influ3_SES","influ3_ED",
  "influ4_relation", "influ4_race", "influ4_orientation","influ4_marital", "influ4_able", "influ4_gender","influ4_age", "influ4_SES","influ4_ED",
  "influ5_relation", "influ5_race", "influ5_orientation","influ5_marital", "influ5_able", "influ5_gender","influ5_age", "influ5_SES","influ5_ED",
  "influ6_relation", "influ6_race", "influ6_orientation","influ6_marital", "influ6_able", "influ6_gender","influ6_age", "influ6_SES","influ6_ED",
  "influ7_relation", "influ7_race", "influ7_orientation","influ7_marital", "influ7_able", "influ7_gender","influ7_age", "influ7_SES","influ7_ED",
  "influ8_relation", "influ8_race", "influ8_orientation","influ8_marital", "influ8_able", "influ8_gender","influ8_age", "influ8_SES","influ8_ED",
  "influ9_relation", "influ9_race", "influ9_orientation","influ9_marital", "influ9_able", "influ9_gender","influ9_age", "influ9_SES","influ9_ED",
  "influ10_relation", "influ10_race", "influ10_orientation","influ10_marital", "influ10_able", "influ10_gender","influ10_age", "influ10_SES","influ10_ED",
  # feedback on experience
  "circle-bad", "define-bias", "produce-bias", "diversify", "like-activity", "learn-activity", "change-activity", "overall-activity"  
)


Placeholder_list <- list(
  placeholder = 'Please select an option below',
  onInitialize = I('function() { this.setValue(""); }')
)

SchoolList <- list(
  'School' = list("Elon", "ISU", "MSU",  "Other")
)

ClassList <- list(
  'Class' = list("ESS341", "KIN345", "KIN445",  "Other")
)

yearScList <- list(
  'Year' = list("1st", "2nd", "3rd", "4th", "5th")
)

AgeList <- list(
  # 'Adolescent' = list("[3-12]", "[13-19]"),
  'Young Adults' = list("[16-19]","[20-29]", "[30-39]"), 
  'Middle Age' = list("[40-49]", "[50-59]"),
  'Elder' = list("[60-69]", "[70-79]", "[80+]")
)

SexList <-  list(
  'Gender' = list("Male", "Female", "Other")
)

SESList <-  list(
  'Income Bracket' = list("Upper Class", "Middle Class", "Lower Middle Class", "Lower Class", "Unknown")
)

EduList <-  list(
  'Highest Education' = list("Less than High School", "High School", "Some College", "College Graduate", "Graduate Studies", "Unknown")
)

RaceList <-  list(
  'Race' = list("White", "Black or African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Hispanic", "Mixed Race", "Other", "Unknown")
)

OrientationList <-  list(
  'Sexual Orientation' = list("Straight", "Gay", "Tran-sexual", "Bi-sexual", "Queer", "Unknown")
)

MaritalList <-  list(
  'Marital Status' = list("Married", "Single", "Unknown")
)

AbleList <-  list(
  'Disability Status' = list("Disability Present", "No Disabliity Present", "Unknown")
)

RelationshipList <-  list(
  'Relationship' = list("Family Member", "Teacher", "Friend", "Boss", "Coach", "Other")
)


select_gender <- function(id="q1_gender", wording="their"){
  selectizeInput(inputId = id, 
                 label= HTML("<strong>Question:</strong>", paste0(" What is ",wording," gender?")), # <br>
                 choices=SexList,
                 options = Placeholder_list
  )
}
  

select_age <- function(id = "q3_age", wording="their"){
  selectizeInput(inputId = id, 
                             label=HTML("<strong>Question:</strong>", paste0(" What is ",wording," age?")),
                             choices=AgeList,
                             options = Placeholder_list
                             )
}
 
select_SES <- function(id = "q2_SES", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," SES?")),
                 choices=SESList,
                 options = Placeholder_list
  )
} 

select_ED <- function(id = "q4_ED", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong>", paste0(" What is ",wording," education level?")),
                 choices=EduList,
                 options = Placeholder_list
  )
} 

select_School <- function(id = "q6_School", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong>", paste0(" What is ",wording," school?")),
                 choices=SchoolList,
                 options = Placeholder_list
  )
}

select_Class <- function(id = "q7_class", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," class?")),
                 choices=ClassList,
                 options = Placeholder_list
  )
}

select_Race <- function(id = "q8_race", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong>", paste0(" What is ",wording," Race/Ethnicity?")),
                 choices=RaceList,
                 options = Placeholder_list
  )
}

select_Orientation <- function(id = "q9_orient", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," sexual orientation?")),
                 choices=OrientationList,
                 options = Placeholder_list
  )
}

select_marital <- function(id = "q10_marital", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," marital status?")),
                 choices=MaritalList,
                 options = Placeholder_list
  )
}

select_able <- function(id = "q11_able", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," disability status?")),
                 choices=AbleList,
                 options = Placeholder_list
  )
}

select_year <- function(id = "q12_year", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," year in school?")),
                 choices=yearScList,
                 options = Placeholder_list
  )
}

select_relationship <- function(id = "q13_relationship", wording="their"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question:</strong> ", paste0(" What is ",wording," relationship to you?")),
                 choices=RelationshipList,
                 options = Placeholder_list
  )
}


text_sample <- textInput(inputId = "q4_text", 
                         label= "Question 4", placeholder = "Sample hint text")

text_influ <- function(id = "influ", personNum){
  textInput(paste0(id,personNum), paste0("Name ",personNum), placeholder = "Type the name of the person") #, value = " "
}


fullBox <- function(linkNM){
  
  boxTitle <- (paste0(linkNM,"_nm"))
  
  div(id = paste0("outer-",linkNM),
    box(id = paste0("box",linkNM),
        title = textOutput(boxTitle), status="primary", solidHeader = TRUE, collapsible = TRUE, collapsed=TRUE,
        select_relationship(id=paste0(linkNM,"_relation")),
        select_Race(id=paste0(linkNM,"_race")),
        select_Orientation(id=paste0(linkNM,"_orientation")),
        select_marital(id=paste0(linkNM,"_marital")),
        select_able(id=paste0(linkNM,"_able")),
        
        select_gender(id=paste0(linkNM,"_gender")),
        select_age(id=paste0(linkNM,"_age")),
        select_SES(id = paste0(linkNM,"_SES")),
        select_ED(id = paste0(linkNM,"_ED"))
    )
  )
}
  
 
  
  

help_demo <- helpText("You can write help text in your form this way")








