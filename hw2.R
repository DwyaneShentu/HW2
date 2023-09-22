#1
>enrollment_data <- read.csv("C:/Users/73113/Downloads/school_ap_enrollment
_by_race.csv")
>library(dplyr)
>big_states <- enrollment_data %>% arrange(desc(total)) %>% head(5) %>%
  >pull(state)
> big_states
[1] "OHIO"         "GEORGIA"      "ILLINOIS"     "OHIO"        
[5] "PENNSYLVANIA"
#2
>filtered_data <- enrollment_data %>% filter(state %in% c("OHIO", "GEORGIA",
                                                          "ILLINOIS", "OHIO", "PENNSYLVANIA"))
>filtered_data <- filtered_data %>% group_by(state) %>% filter(total >= 200)
>filtered_data <- filtered_data %>% group_by(state) %>% mutate( MajorityMino
                                                                rity = (sum(total-White) / total) > 0.5)
>saber_maj_min <- filtered_data %>% group_by(state, MajorityMinority) %>%
  summarize( ProportionSchoolsOfferAP = mean(ap == "Yes"), ProportionStudents
             InAPSchools = sum(ap == "Yes") / sum(total),TotalEnrollment = sum(total))
#3
>saber_maj_min <- saber_maj_min %>% mutate(AverageProportions = (Proporti
                                                                 onSchoolsOfferAP + ProportionStudentsInAPSchools) / 2)
>sorted_saber_maj_min <- saber_maj_min %>% arrange(desc(AverageProportions),
                                                   MajorityMinority)
>sorted_saber_maj_min <- select(sorted_saber_maj_min, -AverageProportions)
>sorted_saber_maj_min %>% gt()
# the student in Georgia contain the highest the proportion of schools that 
#offer AP courses, the students in Pennsylvania attend schools with AP is the
#highest, although the total enrollment in Illinois is the highest, the students
# attend schools with AP is the lowest.
#4
>filtered_data <- enrollment_data %>% filter(total >= 300)
>bins <- filtered_data %>% mutate(BlackProportionBin = cut(Black / total, breaks
                                                           = seq(0, 1, by = 0.1), labels = FALSE))
>summary_data <- bins %>% group_by(BlackProportionBin) %>% summarize(
  ProportionSchoolsOfferAP = mean(ap == "Yes", na.rm = TRUE),TotalSchools
  = n(),TotalEnrollment = sum(total))
A tibble: 11 x 4
BlackProportionBin ProportionSchoolsOfferAP TotalSchools TotalEnrollment
<int>                    <dbl>        <int>           <int>
  1                  1                    0.863        10682         9978688
2                  2                    0.882         2166         2552278
3                  3                    0.867         1195         1346377
4                  4                    0.839          793          846181
5                  5                    0.856          458          485165
6                  6                    0.812          330          311858
7                  7                    0.803          269          258982
8                  8                    0.787          225          175162
9                  9                    0.764          246          196169
10                 10                    0.713          439          311126
11                 NA                    0.740          496          250094
#Asian
>bins <- filtered_data %>% mutate(AsianProportionBin = cut(Asian / total, breaks
                                                           = seq(0, 1, by = 0.1), labels = FALSE))
>summary_data <- bins %>% group_by(AsianProportionBin) %>% summarize(
  ProportionSchoolsOfferAP = mean(ap == "Yes", na.rm = TRUE),
  TotalSchools = n(),TotalEnrollment = sum(total))
# A tibble: 11 x 4
AsianProportionBin ProportionSchoolsOfferAP TotalSchools TotalEnrollment
<int>                    <dbl>        <int>           <int>
  1                  1                    0.859        13423        13212472
2                  2                    0.952         1430         1597685
3                  3                    0.966          502          551905
4                  4                    0.944          233          255756
5                  5                    0.966          118          134162
6                  6                    0.966           88          101946
7                  7                    1               48           70807
8                  8                    1               31           35710
9                  9                    0.952           21           18461
10                 10                    0.75             4            4852
11                 NA                    0.614         1401          728324
# Schools with a higher proportion of Asian students appear to have better 
#access to AP courses and higher total enrollments compared to schools with 
#a higher proportion of Black students