#This script will work through the blog post 
#https://suzan.rbind.io/2018/01/dplyr-tutorial-1/
#

#Select for columns
#filter for rows

library(tidyverse)

?msleep
#built-in R dataset 
glimpse(msleep)

#Basics

#simple select statement
msleep %>%
    select(name, genus, sleep_total, awake) %>%
    glimpse()

#select a chunk of variables/columns

msleep %>%
    select((name:vore), (sleep_total:sleep_cycle)) %>%
    glimpse()

#deselect by using the - character. works with chunks too!
msleep %>% 
    select(-conservation, -(sleep_total:awake)) %>%
    glimpse()

#we can deselect a variable in a chunk, and then add it back again in the same
#select call

msleep %>% 
    select(-(name:order), genus) %>% 
    glimpse()

#We can select using partial column names 

#using starts_with(), ends_with() or contains()

msleep %>%
    select(name, starts_with("sleep")) %>%
    glimpse()

msleep %>%
    select(name, ends_with("wt")) %>%
    glimpse()

msleep %>%
    select(name, contains("_")) %>%
    glimpse()

#We can select using a set of predetermined columns. This is useful if for
#example, we only want factors or only want dbl, or if we only wants columns in
#our model.

classification <- c("name", "genus", "vore", "order", "conservation")

#Notice that !! and one_of() work the same way

msleep %>%
    select(!!classification) %>% 
    glimpse()

msleep %>% 
    select(one_of(classification)) %>% 
    glimpse()


# a simpler way to do the above is to select by data type

#The select_if function allows you to pass functions which return logical
#statements. is.character,is.numeric, is.integer, is.double, is.logical,
#is.factor., and is.POSIXt or is.Date.

#select numeric
msleep %>%
    select_if(is.numeric) %>%
    glimpse

#select strings/chr
msleep %>%
    select_if(is.character) %>%
    glimpse

#select dbl
msleep %>%
    select_if(is.double) %>%
    glimpse

#select factors
msleep %>%
    select_if(is.factor) %>%
    glimpse


# select_if() also works with non data type stuff. We can use other things

msleep %>%
    select_if(is.numeric) %>%
    #we must pass a function, so we put a tilde in front. 
    #we could also put the funs() call wrapper too. 
    select_if(~mean(., na.rm=TRUE) > 10)


#Reordering columns

#the order in which we select columns determines its order. 
#it generally goes left to right

msleep %>%
    select(conservation, sleep_total, name) %>%
    glimpse

# sometimes we want to put our response variable at the beginning. WE can do
# that quickly by using the everything() call
msleep %>%
    select(conservation, sleep_total, everything()) %>%
    glimpse

#We can change names in multiple ways. First we can do it in the select statement.

msleep %>% 
    select(s_total = sleep_total, s_cycle = sleep_cycle) %>%
    glimpse()
# the second way to change names is to use the rename() call

msleep %>% 
    rename(s_total = sleep_total, s_cycle = sleep_cycle) %>%
    glimpse()

#when given a bunch of crappy column names, maybe from excel, we can change them
#rather easily suing the select_all()

#make all column names upper case
msleep %>%
    select_all(toupper) %>% 
    glimpse()

#make all column names lower case
msleep %>%
    select_all(tolower) %>% 
    glimpse()

# We can fix spaces in the column names

#making an unclean database:

msleep2 <- msleep %>% 
    select( name,
            "sleep total" = sleep_total,
            "brain weight" = brainwt) %>% 
    glimpse()

msleep2 %>%
    select_all(~str_replace(., " ", "_")) %>% 
    glimpse()

#Now we are onto Mutating columns with dplyr

#basics

msleep %>%
    select(name, sleep_total) %>%
    mutate(sleep_total_min = sleep_total * 60)

#New columns can be made with aggregate functions such as average, median, max,
#min, sd,

msleep %>%
    select(name, sleep_total) %>%
    mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
           sleep_total_vs_MIN = sleep_total - min(sleep_total))

#ifelse() is great. it first takes a condition, then what to do if TRUE, and
#then what to do if FALSE. It is a great tool for if we want to do different
#thigns across categories.

#For example we can quickly get rid of potential errors in birth weight. THe
#below code goes through and finds any observations where the brainwt is above
#4, if that happens then it is given a NA, otherwise we leave it alone
msleep %>%
    select(name, brainwt) %>%
    mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
    arrange(desc(brainwt))

#we can also implement common stringr functions inside mutate as well. Take
#str_extract too
msleep %>%
    select(name) %>%
    mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$")))

#mutate_all() is very useful for cleaning data. For example, if we wanted all
#data to be lowercase, we could do this:
msleep %>%
    mutate_all(tolower) %>%
    glimpse()

#Make some horrible data. For example, when we scrape data online it might
#contain formatting like the horrible data made below

msleep_ohno <- msleep %>%
    mutate_all(~paste(., "  /n  "))

msleep_ohno[,1:4]

#Now we go through and get rid of any /n and then next mutate all trims the strings

msleep_corr <- msleep_ohno %>%
    mutate_all(~str_replace_all(., "/n", "")) %>%
    mutate_all(str_trim)

msleep_corr[,1:4]

#mutate_if() is great when we want to apply something that can work with only one data type. 
#For example if we wanted to round all the numerics, we could do this 

msleep %>%
    select(name, sleep_total:bodywt) %>%
    mutate_if(is.numeric, round)

# whereas if we used mutate_all() we would have gotten the following error since there are some chr
msleep %>%
    mutate_all(round)

#We can reencode or refactor variables using mutate as well. 
#the .default and .missing are important too since we can add them in for unclean data

msleep %>%
    mutate(conservation2 = recode(conservation,
                                  "en" = "Endangered",
                                  "lc" = "Least_Concern",
                                  "domesticated" = "Least_Concern",
                                  .default = "other")) %>%
    count(conservation2)

#or if you want to specifiy the NA you can do:

msleep %>%
    mutate(conservation2 = recode_factor(conservation,
                                         "en" = "Endangered",
                                         "lc" = "Least_Concern",
                                         "domesticated" = "Least_Concern",
                                         .default = "other",
                                         .missing = "no data",
                                         .ordered = TRUE)) %>%
    count(conservation2)


#We often use the ifelse to make two level factors like below:
msleep %>%
    select(name, sleep_total) %>%
    mutate(sleep_time = ifelse(sleep_total > 10, "long", "short")) 


msleep %>%
    select(name, sleep_total) %>%
    mutate(sleep_total_discr = case_when(
        sleep_total > 13 ~ "very long",
        sleep_total > 10 ~ "long",
        sleep_total > 7 ~ "limited",
        TRUE ~ "short"))%>% 
    # note at this point, it is not a factor yet. We must pass it through one
    # more time to factorize it.
    mutate(sleep_total_discr = factor(sleep_total_discr,
                                      levels = c("short", "limited",
                                                 "long", "very long")))


# we can do case_when across different variables too.

msleep %>%
    mutate(silly_groups = case_when(
        brainwt < 0.001 ~ "light_headed",
        sleep_total > 10 ~ "lazy_sleeper",
        is.na(sleep_rem) ~ "absent_rem",
        TRUE ~ "other")) %>%
    count(silly_groups)




#splitting and merging columns

conservation_expl <- read_csv("conservation_explanation.csv")
conservation_expl 
# we can seperate any column using tidyr seperate()

conservation_table <- conservation_expl %>%
        separate(`conservation abbreviation`, 
                 into = c("abbreviation", "description"), sep = " = ")


# the opposite of seperate() is unite() which works as follows:

conservation_table %>%
    unite(united_col, abbreviation, description, sep=": ")



#unite(new_column_name, column1, column2, seperator = "put your seperator here")

#JOINS

#  Lets say we want to combine two tables using a pk. we can do that in sql very easy, but can also do it in dplyr
#  we will use the conservation_table and msleep data to help out. 
#  Pretend someone, like myself, does not know all the different abbreviations for conservation status. 

msleep %>% glimpse()

# we can see that we have some abbreviations like lc, nt, etc. We can use this
# in combination with our conservation_table to find the full description of the
# abbreviations.

msleep %>%
    select(name, conservation) %>%
    #Since the pk in the conservation_table is uppercase, we must convert to
    #uppercase
    mutate(conservation = toupper(conservation)) %>% 
    #Now we implement a left join. This will select the elements from the
    #consevation table, which select the msleep, since it is the first element
    #being passed into the loop. it is a little hard to see this since it is at
    #the very beginnning of the loop. We connect the pk using the by() call.
    left_join(conservation_table, by = c("conservation" = "abbreviation"))



msleep %>%
    select(name, conservation) %>%
    #Since the pk in the conservation_table is uppercase, we must convert to
    #uppercase
    mutate(conservation = toupper(conservation)) %>% 
    #Now we implement a left join. This will select the elements from the
    #consevation table, which select the msleep, since it is the first element
    #being passed into the loop. it is a little hard to see this since it is at
    #the very beginnning of the loop. We connect the pk using the by() call.
    left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
    #finally, now that we have a joined table, we can creat a description
    #column. If the description is empty, then we just make the description the
    #same the left table input. This could be an NA or it could be something
    #like "domesticated"
    #
    mutate(description = ifelse(is.na(description), conservation, description))



#Gather and spread

#the gather() function will gather up many columns into one The gather function
#needs you to give a name (“key”) for the new descriptive column, and a another
#name (“value”) for the value column. The columns that you don’t want to gather
#need to be deselected at the end

msleep %>%
    select(name, contains("sleep")) %>%
    gather(key = "sleep_measure", value = "time", -name)

#Note that the key column is a chr, if we want an ordered factor, we can do the following:
(msleep_g <- msleep %>%
    select(name, contains("sleep")) %>%
    gather(key = "sleep_measure", value = "time", -name, factor_key = TRUE))

#spreading does the opposite

msleep_g %>%
    spread(sleep_measure, time)


#Filter

#We can filter based upon numeric values
#The most used operators for this are >, >=, <, <=, == and !=

#For example, we can find that animals that sleep greater than 18 hours a day.
msleep %>% 
    select(name, sleep_total) %>% 
    filter(sleep_total > 18)

#we can find a range by using the between() call
msleep %>% 
    select(name, sleep_total) %>% 
    filter(between(sleep_total, 16, 18))

#Another function  similar to between is near(), which will select
#all code that is nearly a given value. You have to specify a tolerance tol to
#indicate how far the values can be

#The following gets everything within a standard deviation of 17
msleep %>% 
    select(name, sleep_total) %>% 
    filter(near(sleep_total, 17, tol = sd(sleep_total)))

#For exact character matches

msleep %>% 
    select(order, name, sleep_total) %>% 
    filter(order == "Didelphimorphia")

#To check for multiple, use %in%
msleep %>% 
    select(order, name, sleep_total) %>% 
    filter(order %in% c("Didelphimorphia", "Diprotodontia"))

#We can deselect columns too using filter and %in%
remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>% 
    select(order, name, sleep_total) %>% 
    filter(!order %in% remove)


msleep %>% 
    select(name, sleep_total) %>% 
    filter(str_detect(tolower(name), pattern = "mouse"))


#Counting
#If you want 
msleep %>%
    count(order, sort = TRUE)


#Summaries and Groups

msleep %>%
    group_by(vore) %>%
    summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
    group_by(order) %>%
    summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

#The summarise() call works with nearly any aggregate function, and allows for additional arithmetics:
#n() - gives the number of observations
# n_distinct(var) - gives the numbers of unique values of var
# sum(var), max(var), min(var), …
# mean(var), median(var), sd(var), IQR(var), …

msleep %>%
    group_by(vore) %>%
    summarise_if(is.numeric, mean, na.rm=TRUE) 

msleep %>%
    group_by(vore) %>%
    summarise_if(is.numeric, mean, na.rm=TRUE) %>%
    rename_if(is.numeric, ~paste0("avg_", .))

msleep %>%
    group_by(vore) %>%
    summarise_if(is.numeric, mean, na.rm=TRUE) %>%
    rename_if(is.numeric, ~paste0( .,"_avg"))



