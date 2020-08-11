library(readr)
library(rgl)

# grab and read cases csv
cases_loc = 'C:\\Users\\erick\\OneDrive\\Documents\\atom_projects\\python\\covid19_project\\new_cases.csv'
cases_data = read.csv(cases_loc)
cases_data = cases_data[, c('fips', 'new_cases')]

# grab and read pop density csv
pop_loc = 'C:\\Users\\erick\\OneDrive\\Documents\\Classes\\Linear\ stats\ modeling\ and\ regression\\covid19\\pop_data.csv'
pop_data = read.csv(pop_loc)
pop_data = pop_data[, c('fips', 'pop', 'pop_density')]

# fips is the agreed upon numerical representation
# of each us county

# combine into one dataframe
final_data = merge(pop_data, cases_data, by='fips')

# define variables
pop_density = final_data$pop_density
new_cases = final_data$new_cases
pop = final_data$pop

# outliers
pop_outs = boxplot(final_data$pop, plot=FALSE)$out
dens_outs = boxplot(final_data$pop_density, plot=FALSE)$out
case_outs = boxplot(final_data$new_cases, plot=FALSE)$out

# remove outliers
final_x = final_data
final_x = final_x[-which(final_x$pop %in% pop_outs),]
final_x = final_x[-which(final_x$pop_density %in% dens_outs),]
final_x = final_x[-which(final_x$new_cases %in% case_outs),]

################################################
#  final_x is the data without the outliers
#  final_data is the data with the outliers
#  In most of the procedures below, I tested on both
#  datasets to compare the results
################################################

# boxplot
boxplot(final_data)$out
boxplot(final_x$pop)$out
boxplot(final_x$pop_density)$out
boxplot(final_x$new_cases)$out

# scatterplot of pop_density vs new_cases
plot(pop_density, new_cases)
plot(pop, new_cases)

# scatterplot matrix
pairs(~ pop_density + pop + new_cases,)
pairs(~ pop_density + pop + new_cases, data = final_x)

# correlation matrix
cor(final_data)
cor(final_x)

# get models with final_data
m0 = lm(new_cases ~ 1)
m1 = lm(new_cases ~ pop_density)
m2 = lm(new_cases ~ pop)
m3 = lm(new_cases ~ pop_density + pop)
m4 = lm(new_cases ~ pop_density + pop + pop_density:pop)

# get models with final_x (outliers removed)
m0 = lm(new_cases ~ 1, data=final_x)
m1 = lm(new_cases ~ pop_density, data=final_x)
m2 = lm(new_cases ~ pop, data=final_x)
m3 = lm(new_cases ~ pop_density + pop, data=final_x)
m4 = lm(new_cases ~ pop_density + pop + pop_density:pop, data=final_x)

# get model summaries
summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

# anova tests
anova(m1)
anova(m2)
anova(m3)
anova(m4)
