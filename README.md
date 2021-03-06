# Data Visualization with Dimple js
#### *Tong Li, June 25, 2017*
            
             
                                 
## Summary
In this project I visualized the relationship between passengers' survival (whether they were survived or perished) on Titanic and passengers' gender, class aboard, and number of siblings/spouses traveled with them.     
                  
               
## Design 
### Exploratory Analysis
I used logistic regression to analyse the dataset (titanic-data.csv) in Rstudio. I recode the original numeric variable age into a categorical variable age group with 0-15, 15-30, 30-45, 45-60 and 60+, for the convenience of doing analysis. I found that passengers' class, sex, number of siblings/spouses aboard, and age (indicated by age group) are related to their survival.  Overall, female passengers tend to have better chance to survive than male passengers, and passengers in class 1 and 2 are more likely to survive compared to passengers in class 3. In different classes, the chance to survive of males and females were not all the same, which represented by a significant interaction between Sex and Pclass in the logistic regression model. In addition, passengers with spouse or more siblings traveled with them had lower chance to survive. The survival rate also varied in different age groups, with the general trend that children and adolescents had better chance to survive.

Bar plots were used to exhibit the relationship between these four variables and survival.                 
                               
### Initial Decisions in Visualization
I used dimple to visualize the data. I considered other types of plots (e.g., bubble plot, line plot), but after I tried them, I went back to my original thought that bar plots were sufficient to present the relationship between these four variables and survival. 

The initial plots can be generated by running titanic_initial.html. The first plot is to show the interaction between Sex and Pclass and how the two variables are related to survival, in which the x axis corresponds to the four combination of Sex and Pclass: female-Pclass 1, male-Pclaass1, female-Pclass 2, male-Pclass 2, female-Pclass 3, and male-Pclass 3, and the y axis correspinds to the number of passengers in each of the four categories on x axis. The different colors on each bar correspond to whether the passengers were survived (pink) or perished (blue). The second plot is to show the relationship between number of siblings/spouse aboard and survival, in which the y axis corresponds to the number of siblings/spouse and the x axis corresponds to the number of passengers for each level of siblings/spouse number. Again, pink and blue on each bar indicate whether the passengers were survived or perished. The third plot is to show the relationship between different age groups and survival. The x axis corresponds to age group, with five categories: NA, 0-20, 20-40, 40-60, and 60+, and the y axis corresponds to the number of passengers for each age group. Pink and blue represent passengers who survived and passengers who perished, as in plot 1 and 2.      
                          
                      
## Feedback and Final Decisions
### Feedback from Three Reviewers:
**Reviewer 1:** I think it is better to be consistent with the y axis in the three plots. Why did you change in the second plot where you used the x axis to show how many people were survived or perished? I don't think there is any good reasons for that. And all the plots need titles.     
      
**Reviewer 2:** For the number of siblings and spouse, I don't think it is necessary to list all the situations. Since most passengers had no siblings or spouse traveled with them, is it possible to recode this variable into two categories: travelling with siblings/spouse and without siblings/spouse?   
                   
**Reviewer 3:** After the SibSp was recoded, have you examined whether there's new interaction which is significant in the model?            
                     
                            
### Improved Versions Based on Feedback
**titanic_2.html:** Took the suggestions from Reviewer 1, titles were added for each plot. The format of plot 2 was also changed to be consistent with the other two plots, with the number of siblings/spouse on the x axis and the number of passengers for each level of siblings/spouse number on the y axis.     
             
**titanic_3.html:** Took the suggestions from Reviewer 2. However, after recode this variable to two categories (with/without siblings or spouse travelling with the passenger), the new variable was not significant anymore in the model. So I recoded this variable into 3 categories: with no siblings/spouse (0), with one sibling/spouse (1), and with more than one sibling/spouse (more). This transformation is reasonable based on the plot, for the number of passengers with more than one sibling/spouse aboard was very low for each level, but the number of passengers with one sibling/spouse aboard was much larger. After this transformation, the new variable with three categories was still significant in the logistic model, for the survival of passengers with no siblings/spouse aboard had better chance to survive compared to passengers with more than 1 sibling/spouse aboard.  
  
                   
**titanic_final:** With the SibSp_category in the logistic model instead of the original SibSp, the interaction between SibSp_category and Sex became significant. However, the only different pattern of survival occurred when the passengers were males with one sibling/spouse aboard, so there was not a obvious trend of the interaction to visualize. Therefore, plot 2 which shows the survival by number of siblings and spouse remains the same in the final plot. The major change in this version is in plot 1, where animation is used to clarify the interaction between Pclass and Sex of passengers. Now only one variable which is the Pclass is on the x axis, and the y axis is the survival rate of passengers for each class. The animation is on Sex, so the readers could see the survival rate for passengers of each gender in all the three classes at the same time, and they could also control whether to view female or male passengers' survival rate in the three classes. Another change is in plot 3, where the age groups are NA, 0-15, 15-30, 30-45, 45-60, and 60+ instead of grouping by 20 years as in the previous versions. This is because in the new way, the youngest group (0-15) only include children and adolescents. It is reasonable to separate them from adults, for adults probably had lower chance of survival since they were very likely to protect children. And the revised plot 3 confirms the reasoning. Finally, a brief explanation of the visualization was added below each plot, for the readers' reference.                           
                        
                                           
## Resources
[dimple.js Documentation][1]                                
[Data Visualization and D3.js (Udacity)][2]


[1]:http://dimplejs.org/
[2]:https://classroom.udacity.com/courses/ud507-nd



