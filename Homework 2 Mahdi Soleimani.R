list.of.packages <- c("tidyverse", "haven", "knitr","broom","stargazer", "ggplot2", "jtools","psych", "lattice", "kableExtra", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

options(tinytex.verbose = TRUE)


#### (1) Child Mortality in the Ivory Coast

cv <- read_dta("civ88_mort.dta")
cv$pw_indv <- cv$pweight*cv$n_wom


#Q2


##Method 1
#boys
mr_b1 <- lm(cv$dboys_l5[cv$boys_l5>0]/cv$boys_l5[cv$boys_l5>0]~1,weights = cv$pw_indv[cv$boys_l5>0] )
#girls
mr_g1 <- lm(cv$dgirls_l5[cv$girls_l5>0]/cv$girls_l5[cv$girls_l5>0]~1,weights =  cv$pw_indv[cv$girls_l5>0])
percent (0.129595)

##Method 2

#born boys
cv$boys_2 <- cv$boys - cv$boys_l5
#dead boys
cv$dboys_2 <- cv$dboys_u5 - cv$dboys_l5
#born girls
cv$girls_2 <- cv$girls - cv$girls_l5
#dead girls
cv$dgirls_2 <- cv$dgirls_u5 - cv$dgirls_l5

#boys
mr_b2 <- lm(cv$dboys_2[cv$boys_2>0]/cv$boys_2[cv$boys_2>0]~1, weights=cv$pw_indv[cv$boys_2>0])
#girls
mr_g2 <- lm(cv$dgirls_2[cv$girls_2>0]/cv$girls_2[cv$girls_2>0]~1, weights=cv$pw_indv[cv$girls_2>0])
#######################################################################
#### (2) Health status, nutritional status and wealth in Ivory Coast

cvn <- read_dta("civ88-08_nutri.dta")
histogram (~weightkg[cvn$weightkg<300]|year, data=cvn,
           xlab= "Graph 1: Distribution of weight (Kg) in 2008,1993 and 1988 (downwards)",
           breaks=seq(from=0,to=300,by=1),
           layout=c(1,3),
           page.number= 1)



#Q2
histogram (x=cvn$height,
           xlab= "Graph 2: Distribution of height (cm) ",
           breaks=seq(from=0,to=1000,by=5))


cvn$height_tr <- ifelse(cvn$height>150, NA, cvn$height)
cvn$height_tr <- ifelse(cvn$height_tr<25, NA, cvn$height_tr)
summary(cvn$height_tr)
describe(cvn$height_tr)

##calculate the z-scores

cvn$z_score <- (cvn$height_tr-cvn$medheight)/(cvn$stdheight)


#Q3
###a

#COUNTRY:
##1988
reg_c88 <- coefficients(lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1988]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1988]))
##1993
reg_c93 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1993]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1993]))
##2008
reg_c08 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==2008]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==2008]))

##NORTH
##1988
reg_n88 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$north==1]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$north==1]))
##1993
reg_n93 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$north==1]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$north==1]))
##2008
reg_n08 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$north==1]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$north==1]))

##SOUTH
reg_s88 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$north==0]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$north==0]))
##1993
reg_s93 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$north==0]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$north==0]))
##2008
reg_s08 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$north==0]~1,
    weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$north==0]))

##b

##BOYS
##1988
reg_b88 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$sexe==1]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$sexe==1]))
##1993
reg_b93 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$sexe==1]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$sexe==1]))
##2008
reg_b08 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$sexe==1]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$sexe==1]))

##GIRLS
reg_g88 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$sexe==0]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1988 & cvn$sexe==0]))
##1993
reg_g93 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$sexe==0]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==1993 & cvn$sexe==0]))
##2008
reg_g08 <- coefficients (lm (cvn$z_score[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$sexe==0]~1,
                             weights = cvn$pweight[cvn$age>=24 & cvn$age<=48 & cvn$year==2008 & cvn$sexe==0]))



#Q4

#we regress the dummy variable of being a stunted child or not on the consumption per capita.
cvn$dummy_st <- ifelse(cvn$z_score<=-2, 1, 0)

#1988
reg_88 <- lm(cvn$dummy_st[cvn$year==1988 & cvn$age>=24 & cvn$age<=48]~ cvn$conspc88[cvn$year==1988 & cvn$age>=24 & cvn$age<=48] , weights = cvn$pweight[cvn$year==1988 & cvn$age>=24 & cvn$age<=48])
#1993
reg_93 <- lm(cvn$dummy_st[cvn$year==1993 & cvn$age>=24 & cvn$age<=48]~ cvn$conspc88[cvn$year==1993 & cvn$age>=24 & cvn$age<=48] , weights = cvn$pweight[cvn$year==1993 & cvn$age>=24 & cvn$age<=48])
#2008
reg_08 <- lm(cvn$dummy_st[cvn$year==2008 & cvn$age>=24 & cvn$age<=48]~ cvn$conspc88[cvn$year==2008 & cvn$age>=24 & cvn$age<=48] , weights = cvn$pweight[cvn$year==2008 & cvn$age>=24 & cvn$age<=48])

stargazer(reg_88,reg_93,reg_08)


#######################################################################
#### (3) Table

asylum <- read.csv("Homework 2 Mahdi Soleimani.csv")
asylum <- asylum %>%
    select(Country, Year, Value) 



asylum_2014 <- asylum %>%
          filter(Year==2014) %>% 
          select(Value)

asylum_2015 <- asylum %>%
    filter(Year==2015) %>% 
    select(Value)

asylum_country <- asylum %>%
                  filter(Year==2015) %>%
                 select(Country) 

Year_2014 <- c(asylum_2014)
Year_2015 <- c(asylum_2015)
Country <- c(asylum_country)

asylum_clean <- data.frame(Country,Year_2014,Year_2015)  %>%
                rename(Year2014 = Value ,
                       Year2015 = Value.1) %>%
                mutate (pct_change = ((Year2015/Year2014 - 1) * 100)) %>%
                mutate(pct_change = round(pct_change, digits = 1))

kable (asylum_clean, "latex", booktabs = T, caption = "Inflows of Asylum Seekers",
       col.names = c("Country","2014","2015","Growth (%)") ) %>%
       kable_styling(latex_options = "striped")%>%
    footnote(general = " This table shows the inflows of of asylum seekers OECD members of the European Union, taken from https://stats.oecd.org/Index.aspx?DataSetCode=MIG. All data are based on annual submissions.", threeparttable = T) 
    










