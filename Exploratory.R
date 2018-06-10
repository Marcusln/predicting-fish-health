#
## Date
###

# Year 2015 has 53 weeks
View(unique(lice[lice$week == 53,c('year', 'week')]))

# as.Date() does not format these correctly, returns NA
lice[!lice$week == 53 & !lice$year == 2015 & is.na(salmon$year.week2),c('year.week2', 'year', 'week')]

# count observations from each farm

salmon %>% 
  group_by(location.id) %>% 
  count() %>% 
  View()

# some farms do not have much data, remove these?

salmon %>% 
  group_by(brakklagt) %>% 
  count()
