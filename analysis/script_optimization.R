
# This script can be used to optimize the assigning of students to groups following their preferences

# load libraries
library(ggplot2)
library(purrr)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# read file with students preferences
topics_selected <- read.csv("topics_selected.csv")

# number of students
n <- nrow(topics_selected)

# number of topics
m <- 16

# topics by letters and numbers 
topics_letters <- c("A1","A2","A3","A4","B1","B2","B3","B4","C1","C2","C3","C4","D1","D2","D3","D4")
topics_numbers <- seq(1:16)

# create variables with preferences by numbers (needed for the model latter)
topics_selected <- topics_selected %>% mutate(preference1_num = topics_numbers[match(topics_selected$preference1, topics_letters)],
                                                preference2_num = topics_numbers[match(topics_selected$preference2, topics_letters)],
                                                preference3_num = topics_numbers[match(topics_selected$preference3, topics_letters)],
                                                preferences_num = paste(preference1_num,preference2_num,preference3_num))

preference_data <- strsplit(topics_selected$preferences_num,"[[:space:]]")

# select the 12 most voted topics from the 16 offered 
# determine the capacity of each topic depending on the # of students enrolled each year
# e.g. if there are n = 35 students and 12 groups, this means 11 groups with 3 students and 1 group with 2 students
# other option is to reduce the number of groups/topics and distribute the students accordingly
cost <- as.data.frame(table(unlist(preference_data))) %>% arrange(Freq) %>% 
  mutate(cap = c(rep(0,4),rep(2,1),rep(3,11))) # the cap variable manually assigns 0 students to the 4 less preferable topics, 2 to the next and 3 to the rest  
topics_out <- cost %>% filter(cap==0) %>% select(Var1) %>% pull()
topics_in <- cost %>% filter(cap>0) %>% select(Var1) %>% pull()
capacity <- cost %>% mutate(Var1=as.numeric(as.character(Var1))) %>% arrange(Var1) %>% select(cap) %>% pull()

# calculate the weight for each topic and student
preferences <- function(student) preference_data[[student]]
preferences(32) # e.g. gives the preferences of student 32
weight <- function(student, topic) {
  p <- which(as.numeric(topic) == preferences(as.numeric(student)))
  as.integer(if (length(p) == 0) {
    999999
  } else {
    p
  })
}
weight(32,"3") # e.g. 999999 because topic 3 is not a preference of student 32
weight(32,"15") # e.g. 1 because topic 15 is the first preference of student 32

# Plot the original topic selection from students 
plot_data <- expand.grid(
  topic = topics_numbers,
  weight = 1:3
) %>% rowwise() %>% 
  mutate(count = sum(map_int(seq_len(n), ~weight(.x, topic) == weight))) %>% 
  mutate(topic = factor(topic), weight = factor(weight))
ggplot(plot_data, aes(x = topic, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE)

# run the model to minimize the weight of the preferences following this conditions:
# 1. each course has a capacity
# 2. every student needs to be assigned to exactly one course
# 3. all students have stated individual preferences on a scale from 1 to 3, where 1 is the most preferable course
model <- MIPModel() %>%
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  # minimize the weight of the preferences (because preferences go from 1 to 3)
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:n, j = 1:m),"min") %>%
  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:n) <= capacity[j], j = 1:m) %>%
  # each student needs to be assigned to one course
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)
model

# get result from model using the dlpk solver
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > 0) %>%  
  dplyr::select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         preferences = paste0(preferences(as.numeric(i)), collapse = ",")) %>% ungroup
matching

# summary of the assigned students by preferences
matching %>% 
  group_by(weight) %>% 
  summarise(count = n())

# plot the optimized assigned students to each topic
plot_data <- matching %>% 
  mutate(course = factor(j), weight = factor(weight, levels = c(1, 2, 3))) %>% 
  group_by(course, weight) %>% 
  summarise(count = n()) %>% 
  tidyr::complete(weight, fill = list(count = 0))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) 

# add topic assigned to each student and update the file
matching <- matching %>% rename(student_number = i,topic_assign_num = j)
topics_assigned <- topics_selected %>% right_join(matching) 
topics_assigned <- topics_assigned %>% 
  mutate(topic_assign = topics_letters[match(topics_assigned$topic_assign_num, topics_numbers)]) %>%
  relocate(topic_assign, .after = topic_assign_num)

write.csv(topics_assigned, "topics_assigned.csv")
