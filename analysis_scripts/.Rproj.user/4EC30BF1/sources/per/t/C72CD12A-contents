rm(list = ls())

source("scripts/load_paths.R")


all_survey_data <- read.csv("data/all_survey_data.csv")[-1,] %>% 
  mutate(names = ifelse(Name.of.your.organization.=="SMEP M&E" |
                                               Name.of.your.organization.=="State Ministry of Health (SMEP)" , 
                                             "State Malaria Elimination Program (SMOH)",
                                             ifelse(Name.of.your.organization.=="Management Sciences for Health"|
                                                      Name.of.your.organization.=="PATH/MACEPA"|
                                                      Name.of.your.organization.=="PMI Evolve Project", "Implementing Partners",
                                                    Name.of.your.organization.)))


all_survey_data$Name.of.your.organization. <- str_wrap(all_survey_data$Name.of.your.organization., width = 10)



ggplot(all_survey_data, aes(x = names, fill = Name.of.your.organization.)) +
  geom_bar() +
  labs(title = "Role Distribution", x = "Role", y = "Count") +
  theme_manuscript()





# 3. Word Cloud for Job Descriptions
job_desc_text <- paste(all_survey_data$What.is.your.job.description., collapse = " ")
job_desc_text00 <- paste(all_survey_data$What.are.the.daily.tasks.associated.with.your.job., collapse = " ")

wordcloud(c(job_desc_text00,job_desc_text) , max.words = 100, colors = brewer.pal(8, "Dark2"))



# Ensure the data frame structure
all_survey_data <- as.data.frame(all_survey_data)



role_mapping <- data.frame(
  Role = c("Data Analyst/Scientist", "Monitoring and Evaluation officer",
           "Team Lead Surveillance, M&E", "Programme Manager",
           "Statistician", "M&E", "Monitoring evaluation Officer",
           "Programme Officer", "Monitoring and Evaluation officer ",
           "Program Specialist MEL", "Monitoring and Evaluation Specialist",
           "SMEOR M&E"),
  role_level = c(4, 3, 4, 5, 4, 3, 3, 4, 3, 3, 3, 3)  
)


microstratification_variables_mapping <- data.frame(
  identifier = unique(all_survey_data$During.your.most.recent.participation.in.an.urban.microstratification.exercise..what.variables.did.you.consider.to.determine.prioritization.or.de.prioritization.of.areas.),
  variables_expertise = c(1,3,0,5,5,3,5,2,5,5,3)  
)

microstratification_mapping <- data.frame(
  identifier = unique(all_survey_data$If.you.have.been.previously.involved.in.urban..microstratification.or.de.prioritization..what.methods.where.implemented.to.inform.the.distribution.of.interventions.),
  technical_expertise = c(4,2,0,3,0,0,4,4,2)  
)


microstratification_data_mapping <- data.frame(
  identifier = unique(all_survey_data$Where.do.you.typically.obtain.the.data.you.use.for.urban.microstratification.or.de.prioritization.of.geographic.areas.),
  data_expertise = c(3,2,0,4,4,1,4,2,3,3)  
)


# Convert columns to numeric

all_survey_data$App_Usage_competence_Num <- as.numeric(factor(
  all_survey_data$How.good.are.you.at.using..navigating..and.handling.web.applications..., 
  levels = c("Beginner", "Intermediate", "Expert", "Advanced")
))

all_survey_data$App_Usage_Frequency_Num <- as.numeric(factor(
  all_survey_data$How.frequently.do.you.use.data.visualization.tools.or.web.applications.to.analyze.data., 
  levels = c("Rarely", "Monthly", "weekly", "Daily")
))



all_survey_data$Task_Participation_Num <- as.numeric(factor(
  all_survey_data$Have.you.ever.been.involved.in.urban.microstratification..de.prioritization.or.do.you.currently.work.on.urban.microstratification.or.de.prioritization., 
  levels = c("No", "Yes")
))


all_survey_data <- inner_join(all_survey_data, role_mapping, by = c("What.is.your.role.within.your.organization" = "Role"))
all_survey_data <- inner_join(all_survey_data, microstratification_mapping, by = c("If.you.have.been.previously.involved.in.urban..microstratification.or.de.prioritization..what.methods.where.implemented.to.inform.the.distribution.of.interventions." = "identifier"))
all_survey_data <- inner_join(all_survey_data, microstratification_variables_mapping, by = c("During.your.most.recent.participation.in.an.urban.microstratification.exercise..what.variables.did.you.consider.to.determine.prioritization.or.de.prioritization.of.areas." = "identifier"))
all_survey_data <- inner_join(all_survey_data, microstratification_data_mapping, by = c("Where.do.you.typically.obtain.the.data.you.use.for.urban.microstratification.or.de.prioritization.of.geographic.areas." = "identifier"))


ggparcoord(all_survey_data, 
           columns = 55:61,
           groupColumn = 2, 
           alphaLines = 0.5) +
  labs(title = "Parallel Coordinates Plot", x = "Variables", y = "Value") +
  theme_minimal()



all_data <- all_survey_data %>% 
  dplyr::select(names, role_level, technical_expertise, 
                variables_expertise, data_expertise, App_Usage_competence_Num, 
                App_Usage_Frequency_Num, Task_Participation_Num) %>% 
  mutate(role_level = role_level, 
         technical_expertise = min_max(technical_expertise), 
         variables_expertise = min_max(variables_expertise),
         data_expertise = min_max(data_expertise), 
         App_Usage_competence_Num = min_max(App_Usage_competence_Num), 
         App_Usage_Frequency_Num = min_max(App_Usage_Frequency_Num), 
         Task_Participation_Num = min_max(Task_Participation_Num)) %>% 
  group_by(names) %>% 
  summarise(role = median(role_level), 
            technical_expertise = mean(technical_expertise), 
            variables_expertise = mean(variables_expertise),
            data_expertise = mean(data_expertise), 
            App_competence = mean(App_Usage_competence_Num), 
            App_Frequency = mean(App_Usage_Frequency_Num), 
            Task_Participation = mean(Task_Participation_Num))


data00 <- rbind(rep(5, 7), rep(0, 7), all_data[-1])

# Plot radar chart for multiple organizations
radarchart(data00, axistype = 1,
           pcol = c( "#FFCC00", "#990000", "#FF9933"),
           plwd = 5,  # Line widths
           cglcol = "black", cglty = 3, axislabcol = "black", cglwd = 1, vlcex = 1)


legend(x = "topright", legend = all_data$names, col = c("#FFCC00", "#990000", "#FF9933"), lty = 1, lwd = 2)


new_all_data <- reshape2::melt(all_data, id.vars = "names") %>% 
  mutate(level = value/5)
  




# Option to radar plot

# Create the dot plot
data_segments <- new_all_data %>%
  group_by(variable) %>%
  summarize(min_level = min(level), max_level = max(level), .groups = "drop")

# Create the dot plot
ggplot(new_all_data, aes(x = level, y = variable, color = names)) +
  geom_point(size = 5) +  
  geom_segment(
    data = data_segments,  
    aes(x = min_level, xend = max_level, y = variable, yend = variable),
    color = "gray", linetype = "dashed", size = 0.8
  ) +
   labs(x = "Value", y = "Country") +
   scale_color_manual(values = c(`Implementing Partners` = "#FFCC00",
                                 `National Malaria Elimination Programme (NMEP)` = "#990000", 
                                 `State Malaria Elimination Program (SMOH)` = "#FF9933")) +
 theme_minimal()


# Option_two



new_all_data_v2  <- new_all_data %>%
  mutate(code = ifelse(variable  == "role", 1,
                       ifelse(variable  == "Task_Participation", 2,
                              ifelse(variable  == "App_competence", 3,
                                     ifelse(variable == "App_Frequency", 4, 
                                            ifelse(variable == "variables_expertise", 5,
                                                   ifelse(variable == "data_expertise", 6, 7)))))) )%>% 
  group_by(level, variable) %>%
  mutate(rowz = n(), 
         row_num = ifelse(rowz < 2, 0, 1:n()),
         dodge_offset02 = ifelse(row_num == 1, code - 0.025, 
                                 ifelse(row_num == 2, code + 0.025 , code))) %>%
  
  ungroup()



ggplot(new_all_data_v2, aes(x = level, y = dodge_offset02, color = names)) +
  # Points with manual dodge
  geom_point(size = 5) +
  # Dashed lines spanning min and max
  geom_segment(
    data = new_all_data_v2 %>%
      group_by(code) %>%
      summarize(min_level = min(level), max_level = max(level), .groups = "drop"),
    aes(x = min_level, xend = max_level, y = code, yend = code),
    color = "gray", linetype = "dashed", size = 0.8, inherit.aes = FALSE
  )+
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0.9,7))+
  labs(x = "Levels", y = "Catergoies") +
  scale_color_manual(values = c(`Implementing Partners` = "#FFCC00",
                                `National Malaria Elimination Programme (NMEP)` = "#990000", 
                                `State Malaria Elimination Program (SMOH)` = "#FF9933")) +
  theme_manuscript()

