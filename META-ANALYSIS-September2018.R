

rm(list=ls())
setwd('/Users/Womps/Documents/U/SkyDrive/Australia Life/PhD year 2/Meta-analysis cognition (remake)')

### Installation of packages ### 

### META ANALYSIS PACKAGES
## install.packages(c("robumeta", "metafor", "dplyr"))

### DATA HANDLING PACKAGES
## install.packages('Runuran')
## install.packages("openxlsx", dependencies = TRUE)
## install.packages('psych')

### Activate packages for use
library(openxlsx)
# library("robumeta")
library(Runuran)
library(metafor)
library(psych)
# # library("dplyr")
library(ggplot2)
# # library(psych)
# # devtools::install_github("mvuorre/brmstools")
#
#
# # Learning materials:
# # CUSTOMIZATION OF FOREST PLOTS
# # http://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups



################################ META-ANALYSIS (EVERYTHING STEP BY STEP) ####################################

#### migrating data from excel to csv (anc cheking the data structure)
dat=read.xlsx ('2018-10-05-New.ID.numbers.short.names.split.xlsx', sheet = 1, skipEmptyRows = FALSE)
str(dat)
write.csv(dat,'2018-10-05-New.ID.numbers.csv', row.names = F)
rm(dat)
x=read.csv ('2018-10-05-New.ID.numbers.csv', h=T)
str(x)
length(unique(x$id))

########################### Auto-complete study information columns ########################################
### saving columns in a vector called "info"

info=c('author','year','intervention', 'title', 'country')# columns that need populating
# how many unique study id's in the dataframe?
id=unique(x$id)
x[which(x$id==id[1]),'author']=x[which(x$id==id[1]),'author'][1]# assigning the value from the first row to the empty rows

###same process as above but done five times (or however many elements were saved in the 'info' vector from above)

for (a in seq_along(info)) {
  for (i in seq_along(id)) {
    x[which(x$id==id[i]),info[a]]=x[which(x$id==id[i]),info[a]][1]
  }
}



########################################### Removing papers that won't be used ##############################

###Liu et al., 2015 from China - excluded because another better report on the same cohort was found
x=x[!is.na(x$id),]
###papers that report change of mean and sd at FU (data reported cannot be used because a correlation coefficient is missing)
###see recommended approach from Cochrane handbook of systematic reviews
###https://handbook-5-1.cochrane.org/chapter_16/16_1_3_2_imputing_standard_deviations_for_changes_from_baseline.htm
###dropping studies as the data reported is not sufficient to calculate SD at follow up
x[which(with(x,correct.sd=='yes' & correction.detail=='change of mean and sd at FU')),'id']=NA
x=x[!is.na(x$id),]



############################# Standardization of cuolumns into percentages ###################################

#### CREATING AN INDEX COLUMN

# Looking at males
str(x$male)
table(x$male < 1 )### some observations are already reported as percentage
x$m.f=x$male+x$female #### adding males and females --> THIS WILL BE THE INDEX COLUMN FOR PERCENTAGE CALCULATIONS (accuracy of these data needs to be confirmed by looking at the original papers)

### rounding decimal points to a maximum of three digits (just for the rows reporting percentages)
x[x$m.f==1 & !is.na(x$m.f),c("male",'female')]=round(x[x$m.f==1&!is.na(x$m.f),c("male",'female')],digits = 3)

#### which papers have reported percentage of males and females instead of numbers???
unique(x[x$m.f==1,c('id','author',"year","sample.stroke.baseline","male",'female',"m.f","n1i")])

######################### Checcking index columns for papers reporting percentages ###########################

### Delgado 2010 - percentages were calculated with the number of stroke patients at baseline (checked in paper)
### Serrano 2007 - same
### Te Winkel-Witlox - same

### getting frequencies for these papers based on their study ids (as authors can be the same)
### formula used --> percentage gender * total stroke patients at baseline (rounded - no decimals)

###calculating number of males
x[  x$id=='51'| # Delgado
      x$id=='169'| # Serrano
      x$id=='179', # Te Winkel
    c('male')]=round(x[x$id=='51'|
                         x$id=='169'|
                         x$id=='179',
                       c('male')]*x[x$id=='51'|
                                      x$id=='169'|
                                      x$id=='179',
                                    c("sample.stroke.baseline")])

###same for females
x[x$id=='51'|
    x$id=='169'|
    x$id=='179',
  c('female')]=round(x[x$id=='51'|
                         x$id=='169'|
                         x$id=='179',
                       c('female')]*x[x$id=='51'|
                                        x$id=='169'|
                                        x$id=='179',
                                      c("sample.stroke.baseline")])

#### does the number of males and females add up to the total sample at baseline??
x[x$id=='51'|
    x$id=='169'|
    x$id=='179',
  c('female')] + x[x$id=='51'|
                     x$id=='169'|
                     x$id=='179',
                   c('male')] == x[x$id=='51'|
                                     x$id=='169'|
                                     x$id=='179',
                                   c('sample.stroke.baseline')] ### should all be true

### Rasquin 2005 - index column for percentage is the number of patients evaluated at day 1 (n1i) (checked paper)
### Da Costa - 2010
### Del Ser 2005 - same
### Peñaloza 2014 - same
### Perrier 2010 - same


### getting frequencies for males
### formula used -->  round(percentage gender*total stroke patients at day one (n1i ---> sample at baseline evaluation)

x[x$id=='46'| # Da Costa
    x$id=='157'| # Rasquin
    x$id=='138'| # Peñaloza
    x$id=='140'| # Perrier
    x$id=='49', # Del Ser
  c('male')]=round(x[x$id=='46'|
                       x$id=='157'|
                       x$id=='138'|
                       x$id=='140'|
                       x$id=='49',
                     c('male')]*x[x$id=='46'|
                                    x$id=='157'|
                                    x$id=='138'|
                                    x$id=='140'|
                                    x$id=='49',
                                  c("n1i")])

###same for females
x[x$id=='46'|
    x$id=='157'|
    x$id=='138'|
    x$id=='140'|
    x$id=='49',
  c('female')]=round(x[x$id=='46'|
                         x$id=='157'|
                         x$id=='138'|
                         x$id=='140'|
                         x$id=='49',
                       c('female')]*x[x$id=='46'|
                                        x$id=='157'|
                                        x$id=='138'|
                                        x$id=='140'|
                                        x$id=='49',
                                      c("n1i")])

#### does the number of males and females add up to the total sample at day 1 (n1i)??
x[x$id=='46'|
    x$id=='157'|
    x$id=='138'|
    x$id=='140'|
    x$id=='49',
  c('female')] + x[x$id=='46'|
                     x$id=='157'|
                     x$id=='138'|
                     x$id=='140'|
                     x$id=='49',
                   c('male')] == x[x$id=='46'|
                                     x$id=='157'|
                                     x$id=='138'|
                                     x$id=='140'|
                                     x$id=='49',
                                   c('n1i')]


### rewriting m.f column to take into account for changes

x$m.f=x$male+x$female
unique(x[x$m.f==1,c('id','author',"year","sample.stroke.baseline","male",'female',"m.f","n1i")])### no more papers with gender in percentages

## making sure all papers have an index column (m.f)
unique(x[is.na(x$m.f),c('id','author','male',"female",'m.f', 'group.id')])
### checking whether these studies report any males or females in any of the subgroups and generating a list of these studies
studies=unique(x[is.na(x$m.f),c('id')])

ids=list()
for (i in seq_along(studies)) {
  print((unique(x[x$id==studies[i],c('id','author','male',"female",'m.f','n1i','group.id')])))
  ids[[i]]=unique(x[x$id==studies[i] & !is.na(x[,'m.f']),c('id')])
}
ids=unlist(ids)

### using study specific information to populate these columns
for (i in seq_along(ids)) {
  total=sum(unique(x[!is.na(x$m.f) & x$id==studies[i], c('m.f')]))
  male=sum(unique(x[!is.na(x$m.f) & x$id==studies[i], c('male')]))
  female=sum(unique(x[!is.na(x$m.f) & x$id==studies[i], c('female')]))
  n.base=x[is.na(x$m.f) & x$id==studies[i], c('n1i')]
  male.in=round(male/total*n.base)
  fem.in=round(female/total*n.base)
  x[is.na(x$m.f) & x$id==studies[i], c('male')]=male.in
  x[is.na(x$m.f) & x$id==studies[i], c('female')]=fem.in
  x[is.na(x$m.f) & x$id==studies[i], c('m.f')]=male.in+fem.in
}

#### checking results (three studies still don't have males, females, or an index column)
for (i in seq_along(studies)) {
  print((unique(x[x$id==studies[i],c('id','author','male',"female",'m.f','n1i','group.id')])))
  
}


### imputing data in these columns with the mean percentage of males and females
male.in=round(mean(x$male/x$m.f, na.rm = T), digits = 3)
fem.in=round(mean(x$female/x$m.f, na.rm = T), digits = 3)
x[is.na(x$m.f),c('male')]=round(male.in*x[is.na(x$m.f),c('n1i')])
x[is.na(x$m.f),c('female')]=round(fem.in*x[is.na(x$m.f),c('n1i')])
x$m.f=x$male+x$female


#### checking results (all papers now have an index column with males, females, and a total sample)
for (i in seq_along(studies)) {
  print((unique(x[x$id==studies[i],c('id','author','male',"female",'m.f','n1i','group.id')])))
  
}


### special considerations
### examining the study groups that have a different sample size at baseline and day one (first cognitive evaluation)
unique(x[which(x$m.f!=x$n1i),c('id','author',"year","sample.stroke.baseline","m.f","n1i","group.id")])

### Creating final percentages
x$male=round(ifelse(x$male > 1, x$male/x$m.f, x$male),digits = 3)
describe(x$male)

x[which(x$male==1),c('author',"year","sample.stroke.baseline","n1i","m.f",'male','female','group.id')]## Poulin 2017 only has males in the coomputer intervention group (should not be a big problem - otherwise will have to use male to female ratio for the whole study)


#### percentage of FEMALES within the study subgroups
x$female=round(ifelse(x$female > 1, x$female/x$m.f, x$female),digits = 3)
describe(x$female)

###special cases (study subgroups with 1 female or male)
x[which(x$female==1),c('author',"year","sample.stroke.baseline","n1i","m.f",'male','female','group.id')]
###subsetting those cases and making them into percentages
x[which(x$female==1),c('female')]=round(x[which(x$female==1),c('female')]/x[which(x$female==1),c('m.f')],digits = 3)
###special cases (study subgroups with 1 male)
x[which(x$male==1),c('author',"year","sample.stroke.baseline","n1i","m.f",'male','female','group.id')]## Poulin 2017 --> already checked

###making sure that reported gender have a number for males and females
length(x$male>=0)==length(x$female>=0) # TRUE
length(is.na(x$male))==length(is.na(x$female)) # TRUE



########################### Correcting dispersio for papers without SD #######################################

length(x[which(x$correct.sd=='yes'),c("correct.sd")]) ##how many rows need to be corrected?
unique(x[which(x$correct.sd=='yes'),c("correct.sd", "correction.detail")]) ##what types of correction are needed?

#     correct.sd                 correction.detail
# 59         yes             corrected for pooling --> already done
# 61         yes              num variables are SE --> do
# 62         yes                         only sd2i --> do
# 476        yes                           all SDs --> do
# 767        yes         all SDs and FIM SD are SE --> do
# 981        yes 95% Cis (done with Cochrane tool) --> already done


###all variables
###(only 1 paper - Cassidy 2004 - does not mention what statistic uses. Values too low to be SD)
unique(x$age.sd)  ### one paper with iqr (ID #17)
x[grep('iqr',x$age.sd),c('id','age','age.sd')] ## --> mean age falls almost exactly in between IQR
x$age.sd=as.numeric(as.character(x$age.sd)) ## WARNING!! forcing column into numeric (on purpose)
x[x$id=='17',c('age.sd')]=round((84-54)/1.35,digits = 2) ## --> entering sd for this paper
x[x$id=='17',c('age.sd')]
unique(x[which(x$age.sd < 5),c('id','author','year','country',"sample.stroke.baseline", "age", "age.sd" )])## there are only 6 studies with age.sd lower than 5 - Cassidy has SD of 1.2, when looking at the paper, study authors do not say whether the reported values were SEM or SD. Therefore age.sd in this paper will be intepreted as SEM

se=as.numeric(as.character(x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),c('age.sd')]))
n=x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),"sample.stroke.baseline"]
sd=round(se*sqrt(n),digits = 3)
x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),'age.sd']=sd

se=x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),c('sd1i','sd2i')]
n=x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),c('n1i','n2i')]
sd=round(se*sqrt(n),digits = 3)
x[which(with(x,correct.sd=='yes' & correction.detail=='num variables are SE')),c('sd1i','sd2i')]=sd


###only sd2i (SD at follow up)
se=x[which(with(x,correct.sd=='yes' & correction.detail=='only sd2i')),'sd2i']
n=x[which(with(x,correct.sd=='yes' & correction.detail=='only sd2i')),'n2i']
sd=round(se*sqrt(n),digits = 3)
x[which(with(x,correct.sd=='yes' & correction.detail=='only sd2i')),'sd2i']=sd


### all SD's
se=x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs')),c('sd1i','sd2i')]
n=x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs')),c('n1i','n2i')]
sd=round(se*sqrt(n),digits = 3)
x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs')),c('sd1i','sd2i')]=sd


### all SDs and FIM SD
se=x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs and FIM SD are SE')),c("disab.fim.sd",'sd1i','sd2i')]
n=x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs and FIM SD are SE')),c('n1i','n2i')]
n=data.frame(x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs and FIM SD are SE')),c('n1i')],n)
names(n)=c('n','n1i','n2i')
sd=round(se*sqrt(n),digits = 3)
x[which(with(x,correct.sd=='yes' & correction.detail=='all SDs and FIM SD are SE')),c("disab.fim.sd",'sd1i','sd2i')]=sd



#### clean instrument names
length(levels(x$instrument))
length(unique(paste(x$instrument,x$instrument.subtest,sep = '-')))
length(unique(x$id))


x$instrument.detail<-paste(x$instrument,x$instrument.subtest,sep = '-')
gsub('-total|\\"','',x$instrument.detail, ignore.case = T)
x$instrument.detail<-gsub('-total|\\"','',x$instrument.detail, ignore.case = T)
data.frame(unique(x$instrument.detail))


####################################################################################################################################### Creating cognitive domains based on cognitive instrument used ####################################################################################################################################

### Saving instrument column in a vector called 'n'
n=x$instrument.detail

### Cognitive domains according to ICF (method adopted from scoping review)
{
  n[grep('mental',n)]='psych-mot sp'
  n[grep('ment',n)]='mental functions'
  n[grep('recall',n)]='memory'
  n[grep('4 letter',n)]='language'
  n[grep('gestalt',n)]='perceptual'
  n[grep('compos',n)]='mental functions'
  n[grep('inform',n)]='language'
  n[grep('digit sp',n)]='memory'
  n[grep('arith',n)]='attention'
  n[grep('simil',n)]='language'
  n[grep('pic compl',n)]='hlcf'
  n[grep('bdt',n)]='hlcf'
  n[grep('sym',n)]='hlcf'
  n[grep('viq',n)]='intellectual'
  n[grep('piq',n)]='intellectual'
  n[grep('bsi',n)]='intellectual'
  n[grep('fsi',n)]='intellectual'
  n[grep('ravl',n)]='memory'
  n[grep('rocf-copy',n)]='perceptual'
  n[grep('rocf',n)]='memory'
  n[grep('rbmt',n)]='memory'
  n[grep('orient',n)]='orientation'
  n[grep('mem',n)]='memory'
  n[grep('lang',n)]='language'
  n[grep('cowat',n)]='language'
  n[grep('clock',n)]='hlcf'
  n[grep('att',n)]='attention'
  n[grep('prax',n)]='praxis'
  n[grep('mmse',n)]='mental functions'
  n[grep('expr',n)]='language'
  n[grep('fim',n)]='mental functions'
  n[grep('tmt-a',n)]='psych-mot sp'
  n[grep('tmt-b',n)]='hlcf'
  n[grep('iqcode',n)]='mental functions'
  n[grep('npi',n)]='consciousness'
  n[grep('-dr',n)]='memory'
  n[grep('namin',n)]='language'
  n[grep('verbal dig',n)]='memory'
  n[grep('verb',n)]='language'
  n[grep('alter',n)]='hlcf'
  n[grep('cube',n)]='perceptual'
  n[grep('-ef',n)]='hlcf'
  n[grep('-sent rep',n)]='language'
  n[grep('-abs',n)]='hlcf'
  n[grep('moca',n)]='mental functions'
  n[grep('neuro',n)]='mental functions'
  n[grep('-inhib',n)]='hlcf'
  n[grep('-interf',n)]='hlcf'
  n[grep('.psy',n)]='psych-mot sp'
  n[grep('stroop-1',n)]='language'
  n[grep('stroop-2',n)]='language'
  n[grep('stroop-3',n)]='hlcf'
  n[grep('stro',n)]='hlcf'
  n[grep('-ir',n)]='memory'
  n[grep('ric',n)]='language'
  n[grep('fluen',n)]='hlcf'
  n[grep('tics',n)]='mental functions'
  n[grep('apha',n)]='language'
  n[grep('nam',n)]='language'
  n[grep('calc',n)]='calculation'
  n[grep('word',n)]='memory'
  n[grep('wll',n)]='memory'
  n[grep('proc',n)]='hlcf'
  n[grep('list',n)]='memory'
  n[grep('camc',n)]='mental functions'
  n[grep('-ip',n)]='hlcf'
  n[grep('const',n)]='hlcf'
  n[grep('concep',n)]='hlcf'
  n[grep('mdrs',n)]='mental functions'
  n[grep('vs-',n)]='attention'
  n[grep('avlt',n)]='memory'
  n[grep('cpt',n)]='attention'
  n[grep('fab',n)]='hlcf'
  n[grep('bdae',n)]='language'
  n[grep('bnis',n)]='mental functions'
  n[grep('vrt',n)]='memory'
  n[grep('amt',n)]='mental functions'
  n[grep('adas',n)]='mental functions'
  n[grep('cogn',n)]='mental functions'
  n[grep('spmsq',n)]='mental functions'
  n[grep('cog.flex',n)]='hlcf'
  n[grep('soc.ac',n)]='global psychosocial'
  n[grep('-nci',n)]='mental functions'
  n[grep('cnst',n)]='language'
  n[grep('ist',n)]='hlcf'
  n[grep('bnt',n)]='language'
  n[grep('fig',n)]='hlcf'
  n[grep('gds',n)]='mental functions'
  n[grep('vlt',n)]='memory'
  n[grep('wwt',n)]='attention'
  n[grep('visuosp',n)]='perceptual'
  n[grep('luria',n)]='psych-mot sp'
  n[grep('schulte',n)]='attention'
  n[grep('ufov',n)]='visual field'
  n[grep('sdsa',n)]='hlcf'
  n[grep('cns-vst',n)]='mental functions'
  n[grep('cns',n)]='consciousness'
  n[grep('ace-r',n)]='mental functions'
  n[grep('knafelc',n)]='mental functions'
  n[grep('bpp',n)]='memory'
  n[grep('count',n)]='attention'
  n[grep('casi',n)]='mental functions'
  n[grep('commands',n)]='language'
  n[grep('dft',n)]='memory'
  n[grep('letter sort',n)]='hlcf'
  n[grep('ef',n)]='hlcf'
  n[grep('pdt',n)]='language'
  n[grep('token',n)]='language'
  n[grep('mis',n)]='memory'
  n[grep('balloons',n)]='attention'
  n[grep('srt',n)]='hlcf'
  n[grep('sub',n)]='hlcf'
  n[grep('vigilance task',n)]='attention'
  n[grep('drs',n)]='attention'
  n[grep('sr-',n)]='memory'
  n[grep('msot',n)]='hlcf'
  n[grep('msq',n)]='hlcf'
  n[grep('repeat',n)]='language'
  n[grep('copying',n)]='perceptual'
  n[grep('reading',n)]='language'
  n[which(n=='lcf')]='mental functions'

x$domain<-n
table(x$domain)
}

######################################### Type of stroke #####################################################

levels(x$stroke.type) ## 12 levels
levels(x$stroke.type)=c('mixed', 'first-ever (mixed)','first-ever (isch or hem)',
                        'first-ever (mixed)','first-ever (mixed)','first-ever (isch)',
                        'ischemic', 'ischemic or hem','ischemic',
                        'first-ever (mixed)','first-ever (mixed)','mixed')
levels(x$stroke.type) ##now 6 levels :)
table(x$stroke.type)
unique(x[x$stroke.type=='first-ever (mixed)','author'])

####fill the "isch" column with the total of patients in the index column 'm.f' (for papers that only included ischemic stroke patients)

x$stroke.type=as.character(x$stroke.type)
table(is.na(x$stroke.type)) #### should all be false
x[is.na(x$stroke.type),'stroke.type']='nothing'#making sure index column does not have NAs

x[x$stroke.type=='first-ever (isch)' | x$stroke.type=='ischemic' ,'isch']=1 ##one represents 100%

######filling hemorrhagic stroke column with zeroes for the papers that did not include hemorrhagic stroke
x[x$stroke.type=='first-ever (isch)' | x$stroke.type=='ischemic' ,'hem']=0


#### rounding numbers for numeric columns
### MEAN EDUCATION
# x$educ=round(x$educ,digits = 3)## ERROR - one paper reported median ()
x[grep('med',x$educ),'educ']
levels(x$educ)[x[which(x$educ=='14 med'),'educ']]='14'
x$educ=as.numeric(as.character(x$educ))
x$educ=round(x$educ,digits = 3)

####SD EDUCATION
# levels(x$educ.sd)[x[which(x$educ.sd=='12-16 (iqr)'),'educ.sd']]='2'
# levels(x$educ.sd)[x[which(x$educ.sd=='12-16 (range)'),'educ.sd']]='2'
# x$educ.sd


### creating percentages for stroke types (based on index column)

### ISCHEMIC
str(x$isch) ## column needs to be at least numeric
x$isch=round(ifelse(x$isch > 1, x$isch/x$m.f, x$isch),digits = 3)
describe(x$isch) ## all values should range from zero to 1

#### looking at papers that have less than 70% ischemic strokes within subgroups
unique(x[x$isch<.7,c('id','author',"year","sample.stroke.baseline","m.f",'isch','group.id')])
### Higher proportion of ICH than other studies (checked checked in paper)
### Penaloza 2014 is the only paper that has a calculated ICH rate that was taken from the results paragraph



################################################################################################################################################################################################################################## Creating percentages for demographic variables, risk factors, and comorbidities (46 columns)  ####################################################################################################################################################################################################################################

names(x)
which(names(x)=='no.educ')
names=c(names(x)[c(which(names(x)=='no.educ'):which(names(x)=='recur.stroke'),
                   which(names(x)=='tia'):which(names(x)=='lesion.bilat'),
                   which(names(x)=='nihss.zero.pts'):which(names(x)=='ht'),
                   which(names(x)=='dyslip'):which(names(x)=='depre.hist'),
                   which(names(x)=='aphasia'),which(names(x)=='disab.fim.75.pt.or.less'),
                   which(names(x)=='disab.rds.5.pts')
)])
names ## columns to loop through
length(names) ### 46 columns

x[which(x$educ.prim.or.more == x$m.f),c('id',"author","year","m.f","educ.prim.or.more","group.id")] # Cherney 2007 has all patients with educ primary or more, which is not unlekily

#### the loop below generates percentages for columns reporting frequencies. This loop also takes into account that some studies already reported percentages, leaving those out - a summary from each column is printed at the end of each iteration 

for (i in seq_along(names)) {
  x[,names[i]]=round(ifelse(x[,names[i]] >= 1, x[,names[i]]/x$m.f, x[,names[i]]),digits = 3)
  print(names[i])
  print(describe(x[,names[i]]))
}

#####Checking if there are any columns that go over 100% (or 1) - this loop should generate all empty results because up until this point everythign should be transformed into percentages
for (i in seq_along(names)) {
  print(unique(x[ x[,names[i]] > 1 & !is.na(x[,names[i]] ), c(names[i] ,'id','author',"m.f")]))
  
}



###################### Processing columns with multiple datapoints #######################

## selecting columns
names(x)
names=c(names(x)[c(which(names(x)=='bp.systolic'):which(names(x)=='bmi'),
                   which(names(x)=='depre.gds'):which(names(x)=='depre.npi'),
                   which(names(x)=='smoke'):which(names(x)=='disab.bi.modif'),
                   which(names(x)=='disab.rds'),
                   which(names(x)=='disab.mrs'):which(names(x)=='iadl.scale'),
                   which(names(x)=='iqcode')
)])
names ###columns to loop thorugh
length(names)  ##31 columns

#### All the data will be put in a matrix called 'mat' - this matrix will be populated itiratively with a loop
mat=matrix(nrow = nrow(x))

#### populating matrix (with line-by-line explanation)
for (i in seq_along(names)) {
  
  a<-as.character(x[,names[i]]) ### transform colum to character
  a<-strsplit(a, ", ") ### split the colum if it has a comma 
  n.obs <- sapply(a, length)  ### count the number of observations in each row
  seq.max <- seq(max(n.obs))  ### create a sequence with the max number of observations
  a <- t(sapply(a, "[", i = seq.max)) #### using this sequence, create a matrix and transpose rows and columns
  # print(length(a))
  mat=data.frame(cbind(mat,a)) ### turn matrix into a data frame
  b <- grep('X',names(mat))  ### in the data frame, columns are assigned 'X' as the default name, grab those columns and put them in a vector "b"
  names(mat)[b]=names[i]  #### replace 'X' with the name of the column we are iterating thorugh
  
}

names(mat)

##### adding index columns
mat=data.frame(x$id,x$m.f,mat)
names(mat)[1:2]=c('id','m.f')
names=names(mat)
m=mat ####shortening matrix name to just "m'"

### organizing alcohol drinking columns into categories
names(m)[grep('alc',names(m))] #which columns have "alc" in their name?
### LOOKING AT THESE COLUMNS SEPARATELY
unique(m[!is.na(m$alcohol),c('id','m.f','alcohol')])
unique(m[!is.na(m$alcohol.1),c('id','m.f','alcohol.1')])
unique(m[!is.na(m$alcohol.2),c('id','m.f','alcohol.2')])
unique(m[!is.na(m$alcohol.3),c('id','m.f','alcohol.3')])


################################## Alcohol drinking levels  #########################################
######VERY LONG STEP DUE TO COMBINED COLUMNS

#### HEAVY DRINKING (INADEQUATE, HEAVY, ABUSIVE, MORE THAN 2-3 TIMES PER WEEK, OR MORE THAN 30 GRAMS A DAY)
alc.heavy=list(unique(m[grep(paste('abuse',
                                   'inad', sep = '|'),
                             m$alcohol),'alcohol']),
               unique(m[grep('heav',m$alcohol.1),'alcohol.1']),
               unique(m[grep(paste('abuse',
                                   'inad', sep = '|'),
                             m$alcohol.2),'alcohol.2']),
               unique(m[grep(paste('more than 2-3 times/week',
                                   'heavy',
                                   '> 30g/d',sep = '|'),
                             m$alcohol.3),'alcohol.3']))

alc.heavy=as.character(unlist(alc.heavy))######making a single list with "unlist"; then turning levels to character 
alc.heavy=alc.heavy[-grep('ex',alc.heavy)]## taking out category that is not needed
alc.heavy

##### ADEQUATE DRINKING (MODERATE DRINKERS, LESS THAN 30GR PER DAY OR MORE THAN ONE PER WEEK)
alc.adeq=as.character(unlist(list(unique(m[grep(paste('mod',
                                                      '> once per week',
                                                      sep = '|'),m$alcohol),'alcohol']),
                                  
                                  unique(m[grep('adeq',m$alcohol.1),'alcohol.1']),
                                  
                                  unique(m[grep(paste('2-3 times/week',
                                                      '< 30g/d',sep = '|'),
                                                m$alcohol.2),'alcohol.2']))))


###### LIGHT DRINKERS 1 TIME A WEEK OR LESS (ASSUMING NO BINGE DRINKING)

alc.light=as.character(unlist(list(unique(m[grep('< 1 per month',
                                                 m$alcohol),'alcohol']),
                                   
                                   unique(m[grep(paste('2-3 times/month',
                                                       '2-4 times/month',sep = '|'),m$alcohol.1),'alcohol.1']),
                                   
                                   unique(m[grep('light',
                                                 m$alcohol.2),'alcohol.2']))))


###### NO DRINKING OR VERY OCCASIONALLY (LESS THAN ONCE PER MONTH)

alc.none=as.character(unlist(list(unique(m[grep(paste('never',
                                                      'none or < 1 per month', sep = '|'),
                                                m$alcohol),'alcohol']))))


#### creating iterators
columns=names(m)[grep('alc',names(m))]
alcohol=list('alc.adeq'=alc.adeq, 'alc.heavy'=alc.heavy, 'alc.light'=alc.light, 'alc.none'=alc.none)
names(alcohol[1])
alcohol[[1]]


##### Creating a columns with alcohol use taken from the other columns
m$alc.adeq=NA
m$alc.heavy=NA
m$alc.light=NA
m$alc.none=NA

###### populating the four columns created above (triple loop)
for (w in seq_along(names(alcohol))) {
  
  for (z in seq_along(columns)) {
    for (i in seq_along(alcohol[[w]])) {
      column=as.character(m[,columns[z]])
      positions=which(column==alcohol[[w]][i])
      observations=as.character(m[which(column==alcohol[[w]][i]),columns[z]])
      m[positions,names(alcohol[w])]=observations
      
    }
  }
}

#### Making sure new columns were populated correctly
unique(m[!is.na(m$alcohol),c('id','m.f',"alc.heavy","alc.adeq","alc.light","alc.none")])


##### cleaning columns and leaving just the numbers or percentages
oh=names(m)[grep('alc.',names(m),fixed = T)]
oh

for (i in seq_along(oh)) {
  a<-strsplit(m[,oh[i]],'\\ ')
  n.obs <- sapply(a, length)  
  seq.max <- seq(max(n.obs))  
  a <- t(sapply(a, "[", i = seq.max)) 
  m[,oh[i]]=a[,1]
  
  
}

m=data.frame(m,x[,'alcohol'])

###### CURRENT DRINKERS
m$alc.current=as.numeric(as.character(x$alcohol)) #### WARNING!! text was deleted from the columns --> done on purpose
unique(m[!is.na(m$alcohol),c('id',"alc.heavy","alc.adeq","alc.light","alc.none",'alc.current', "x....alcohol..")]) ###looks good - two categories dropped ('ex heavy' and 'ever') because they do not fit in the other groups

#####study IDs 143 has the percentage sign (%) --> making it into the 0-1 scale
for (i in seq_along(oh)) {
  
  m[m$id=='143',oh[i]]=gsub('%','',m[m$id=='143',oh[i]])
  m[,oh[i]]=as.numeric(as.character(m[,oh[i]]))
  
}
str(m[,oh]) ## all should be numeric
m[m$id=='143',oh]=round(m[m$id=='143',oh]/100,digits = 3)
unique(m[!is.na(m$alcohol),c('id',"m.f","alc.heavy","alc.adeq","alc.light","alc.none",'alc.current', "x....alcohol..")])


###removing elements in the environment that I am not using
rm(list=setdiff(ls(), c("m",'x')))



####################################################### SIMILAR RECODING FOR SMOKING ###########################################################################

names(m)[grep('smok',names(m))] #which columns have "alc" in their name
### LOOKING AT THESE COLUMNS SEPARATELY
unique(m[!is.na(m$smoke),c('id','m.f','smoke')])
unique(m[!is.na(m$smoke.1),c('id','m.f','smoke.1')])
unique(m[!is.na(m$smoke.2),c('id','m.f','smoke.2')])


################################# SMOKING LEVELS (VERY LONG STEP DUE TO COMBINED COLUMNS) ##################################

####SMOKING CATEGORIES
###CURRENT
smk.current=as.character(unlist(list(unique(m[grep('current',m$smoke),'smoke']),
                                     unique(m[grep('active',m$smoke.1),'smoke.1']),
                                     unique(m[grep('current',m$smoke.2),'smoke.2']))))
###HEAVY
smk.heavy=as.character(unlist(list(unique(m[grep('abuse',m$smoke),'smoke']))))




####PREVIOUS
smk.ex=as.character(unlist(list(unique(m[grep(paste('ex',
                                                    'prev', sep='|'),
                                              m$smoke.1),'smoke.1']))))


###### NEVER SMOKERS

smk.never=as.character(unlist(list(unique(m[grep('never',
                                                 m$smoke),'smoke']))))



#### creating iterators to re-write columns
columns=names(m)[grep('smok',names(m))]
smk=list('smk.current'=smk.current, 'smk.ex'=smk.ex, 'smk.heavy'=smk.heavy, 'smk.never'=smk.never)


##### Creatingcolumns with info taken from the other columns
m$smk.current=NA
m$smk.ex=NA
m$smk.heavy=NA
m$smk.never=NA

###### populating the four columns created above (triple loop)
for (w in seq_along(names(smk))) {
  
  for (z in seq_along(columns)) {
    for (i in seq_along(smk[[w]])) {
      column=as.character(m[,columns[z]])
      positions=which(column==smk[[w]][i])
      observations=as.character(m[which(column==smk[[w]][i]),columns[z]])
      m[positions,names(smk[w])]=observations
      
    }
  }
}

##### checking results from the loop
smk=names(m)[grep('smk.',names(m),fixed = T)]
unique(m[,c('id',smk)])

#####CLEANING THE TEXT
smk
for (i in seq_along(smk)) {
  a<-strsplit(m[,smk[i]],'\\ ')
  n.obs <- sapply(a, length)  
  seq.max <- seq(max(n.obs))  
  a <- t(sapply(a, "[", i = seq.max)) 
  m[,smk[i]]=a[,1]
}

unique(m[,c('id',smk,'smoke')]) ##### study ID 123 reported percentage
m[m$id==123,c('smk.current')]='0.169'  #### manually changed to percentage 0-1
m$smk.current=as.numeric(as.character(m$smk.current))
smoke=as.numeric(as.character(m$smoke)) #### WARNING!! dropping the other categories in the column as they were already moved to their respectives ones
m$smk.current=ifelse(smoke >= 0 & is.na(m$smk.current), smoke , ifelse(m$smk.current >= 0 & is.na(smoke), m$smk.current, m$smk.current)) ### WARNING - all good
unique(m[,c("smk.current","smoke","smoke.1","smoke.2")])

m=data.frame(m,x[,"smoke"])

unique(m[!is.na(m$alcohol),c('id',"smk.current","smk.heavy","smk.ex","smk.never","x....smoke..")]) ### all good with smoking



###removing elements in the environment that I am not using
rm(list=setdiff(ls(), c("m",'x')))



################################# MRS (levels of disability) ##################################


mrs.cols=names(m)[grep('mrs',names(m))]

unique(m[!is.na(m$disab.mrs),mrs.cols])

#################################  LEVELS OF MRS  ##################################

#### ZERO TO TWO POINTS
mrs.0.to.2=as.character(unlist(list(unique(m[grep(paste('0-2 points',
                                                        '< 3 points',sep='|'),m$disab.mrs),'disab.mrs']))))

#### ONE TO 4 POINTS
mrs.1.to.4=as.character(unlist(list(unique(m[grep('1-4 points',m$disab.mrs),'disab.mrs']))))


#### THREE OR MORE
mrs.3.or.more=as.character(unlist(list(unique(m[grep('> 2 points',m$disab.mrs),'disab.mrs']),
                                       unique(m[grep(paste('3-5 points',
                                                           '>= 3 points', sep= "|"),m$disab.mrs.1),'disab.mrs.1']))))


####iterators
columns=mrs.cols
mrs.groups = list('mrs.0.to.2'=mrs.0.to.2,'mrs.1.to.4'=mrs.1.to.4, 'mrs.3.or.more' = mrs.3.or.more)


##### Creating columns with totals from other columns
m$mrs.0.to.2=NA
m$mrs.1.to.4=NA
m$mrs.3.or.more=NA

###### populating the columns created above (triple loop)
for (w in seq_along(names(mrs.groups))) {
  
  for (z in seq_along(columns)) {
    for (i in seq_along(mrs.groups[[w]])) {
      column=as.character(m[,columns[z]])
      positions=which(column==mrs.groups[[w]][i])
      observations=as.character(m[which(column==mrs.groups[[w]][i]),columns[z]])
      m[positions,names(mrs.groups[w])]=observations
      
    }
  }
}

##### comparing results of loop with original data
m=data.frame(x[,"disab.mrs"],m)
unique(m[!is.na(m$x....disab.mrs..),c('id',"mrs.0.to.2","mrs.1.to.4","mrs.3.or.more", "x....disab.mrs..")])

#####CLEANING THE TEXT
mrs.cols=names(m)[grep('mrs.',names(m),fixed = T)]
mrs.cols=mrs.cols[grep('disab',mrs.cols,invert =  T)] ### taking out the ones that don't need to be cleaned

for (i in seq_along(mrs.cols)) {
  a<-strsplit(m[,mrs.cols[i]],'\\ ')
  n.obs <- sapply(a, length)  
  seq.max <- seq(max(n.obs))  
  a <- t(sapply(a, "[", i = seq.max)) 
  m[,mrs.cols[i]]=a[,1]
}

#### making sure re-arramgement looks good
unique(m[!is.na(m$x....disab.mrs..),c('id',"mrs.0.to.2","mrs.1.to.4","mrs.3.or.more",'disab.mrs','disab.mrs.1', "x....disab.mrs..")])

#### WARNING - Keeping just numerical values in column (median values and IQRs will be lost when it is time to dichomomize the distribution)
m$mrs=round(as.numeric(as.character(m$disab.mrs)),digits = 3) 
m$mrs.sd=round(as.numeric(as.character(m$disab.mrs.1)),digits = 3) 

unique(m[!is.na(m$x....disab.mrs..),c('id',"mrs.0.to.2","mrs.1.to.4","mrs.3.or.more",'mrs','mrs.sd', "x....disab.mrs..")])


###################################################### REST OF THE VARIABLES #####################################################

sort(names(m)) ### organizing the columns alphabetically
adl=names(m)[grep('adl.',names(m))] ### ADLS
alc=names(m)[grep('alc.',names(m),fixed = T)]  #### alcohol drinking
smk=names(m)[grep('smk',names(m))] ### smoking
bmi=names(m)[grep('bmi',names(m))]  #### body mass index
bp=names(m)[grep('bp',names(m))] #### blood pressure
bmi=names(m)[grep('bmi',names(m))] #### body mass index
disab=names(m)[c(grep('disab.bi',names(m)),grep('disab.rds',names(m)))] #### disabiliy (except for MRS - looking at it separately)
mrs=names(m)[grep('mrs',names(m))]  #### modified rankin scale
mrs=mrs[c(-1,-2)] ### do not want the original data anymore as it has already been re-organized
dep=names(m)[grep('depre',names(m))]  #### depression
iqcode=names(m)[grep('iq',names(m))]  #### iqcode

############# ACTIVITIES OF DAILY LIVING
unique(m[,adl]) 
####### creating final colunmn names for activities of daily living
new=gsub('1','sd',names(m[,adl])[grep('1',names(m[,adl]))])
positions=grep('1',names(m[,adl]))
adl=m[,adl]
names(adl)[positions]=new
names(adl)

#### alcohol drinking (all good)
unique(m[,alc])
alc=m[,alc]
### smoking (all good)
unique(m[,smk])
smk=m[,smk]

#### blood pressure (some values are median and IQR - won't be able to use for dichotomization)
unique(m[,bp]) 

for (i in seq_along(bp)) {
  
  m[,bp[i]]=as.numeric(as.character(m[,bp[i]])) ### values coerced into numeric (on purpose)
  
}
unique(m[,bp])
####### creating final colunmn names for blood pressure
bp=bp[-1]###dropping the first column as it is empty (this is always the case because bp was the the index column for creating the matrix "m")
bp=m[,bp]
names(bp)=c('bp.syst','bp.syst.sd','bp.diast','bp.diast.sd')
names(bp)

###### bmi
unique(m[,bmi])
m$bmi=round(as.numeric(as.character(m$bmi)), digits = 3)
unique(m[,bmi])
####### creating final colunmn names
new=gsub('1','sd',names(m[,bmi])[grep('1',names(m[,bmi]))])
positions=grep('1',names(m[,bmi]))
bmi=m[,bmi]
names(bmi)[positions]=new
names(bmi)
unique(bmi)
bmi$bmi.sd=as.numeric(as.character(bmi$bmi.sd)) ### dropping IQRs

###### depression
unique(m[,dep]) #### some values are SE
for (i in seq_along(dep)) {
  m[,dep[i]]=as.character(m[,dep[i]]) ### turn column into character
  positions=grep('SE', m[,dep[i]])#### grab row positions of columns reporting SE
  m[positions,dep[i]]=round(as.numeric(gsub('SE','',m[positions,dep[i]]))*sqrt(m[positions,'m.f']),digits = 3) ###take out SE and turn into numeric value, then multiply that by the sqrt of the sample (simple SE to SD formula)
  m[,dep[i]]=as.numeric(as.character(m[,dep[i]])) ### make all the depression columns numeric (1 value will be dropped)
  
}

####### creating final colunmn names for depression

new=gsub('.1','.sd',fixed = T,names(m[,dep])[grep('.1',names(m[,dep]), fixed = T)])
positions=grep('.1',names(m[,dep]), fixed = T)
dep=m[,dep]
names(dep)[positions]=new
### depre.mini will go first in this data frame
dep=dep[,c(names(dep)[grep('mini', names(dep))],names(dep)[grep('mini', names(dep), invert = T)])]
names(dep)

###### disability
unique(m[,disab]) ### one column has standard error still
m$disab.bi.modif.1=as.character(m$disab.bi.modif.1)   ### turning specific column into character for better handling
m[!is.na(m$disab.bi.modif.1) & m$disab.bi.modif.1=='0.7 SE',c('id','m.f','disab.bi.modif.1')] 
0.7*sqrt(50) ### SD equals SE * squared root of the sample size
m[!is.na(m$disab.bi.modif.1) & m$disab.bi.modif.1=='0.7 SE',"disab.bi.modif.1"]=as.character(round(0.7*sqrt(50), digits = 3))
unique(m[,disab]) ### value has been corrected
m$disab.bi.modif.1=as.numeric(as.character(m$disab.bi.modif.1))###dropping the data from studies reporting IQR and range(unfortunately)
new=gsub('1','sd',names(m[,disab])[grep('1',names(m[,disab]))])
positions=grep('1',names(m[,disab]))
disab=m[,disab]
names(disab)[positions]=new
names(disab)





###### modified rankin scale
unique(m[,mrs]) ### all good
mrs=m[,mrs]
##### iqcode
names(m[,iqcode])
new=gsub('1','sd',names(m[,iqcode])[grep('1',names(m[,iqcode]))])
positions=grep('1',names(m[,iqcode]))
iqcode=m[,iqcode]
names(iqcode)[positions]=new
names(iqcode)


#######reorganizing columns
names(m)
m=data.frame(smk,alc,mrs[,c("mrs.0.to.2","mrs.1.to.4","mrs.3.or.more")],bp,bmi,dep,disab,mrs[,c("mrs","mrs.sd")],adl,iqcode)
names(x)
length(names(x)) #### should be 115 columns

###### Saving everything done so far in a brand-new dataset ####
all=data.frame(x[,c(which(names(x)=='id'):which(names(x)=='female'),
                    which(names(x)=='m.f'),
                    which(names(x)=='educ'):which(names(x)=='ht'),
                    which(names(x)=='dyslip'):which(names(x)=='depre.hist'),
                    which(names(x)=='aphasia'),which(names(x)=='dementia'))],
               m,
               x[,c(which(names(x)=='instrument.subtest'),
                    which(names(x)=='instrument.detail'),
                    which(names(x)=='domain'),
                    which(names(x)=='day1i'):which(names(x)=='sd2i'))])

names(all)

# meta.1<-data.frame(unique(meta[,c("author","year","country")]))
# meta<-data.frame(unique(all[,c("author","year","country")]))
# meta.1$author=as.character(meta.1$author)
# meta.1[which(meta.1$author=='Kyoung et al.'),'author']=as.character(meta[which(meta$author=='Lee et al.' & meta$year==2015 ),'author'])

####### DICHOTOMIZING CONTINUOUS OUTCOMES #####

a=all[,c(which(names(all)=='bp.syst'):which(names(all)=='iqcode.sd'))]
a=a[,c(grep('mini', names(a), invert = T))] ##### mini was taken out because it contains no continous level data
names(a)

###### counting the number of iterators we need to create (the dataset has one column for mean and another for SD. Therefore the number of iterators is basically half the columns in the dataset)
length(names(a))/2

### grabbing the actual column names from the data set (should be same length as above)
length(names(a)[c(grep('.sd',names(a),invert=T),
                  which(names(a)=='depre.cesd'),
                  which(names(a)=='depre.sds'))]) 
### saving column names (only the ones containing mean scores. Columns with SD will have to be renamed by pasting 'sd' at the end)
mean=names(a)[c(grep('.sd',names(a),invert=T),
                which(names(a)=='depre.cesd'),
                which(names(a)=='depre.sds'))]
### column names for standard deviations
sd=paste0(names(a)[c(grep('.sd',names(a),invert=T),
                     which(names(a)=='depre.cesd'),
                     which(names(a)=='depre.sds'))], '.sd')

### bringing in sample size and id numbers from master data base (all)
a=data.frame(a,all[,c("id","m.f")])

##### creating an index number for every row
a$seq.id=as.numeric(seq(1:length(a$id)))

#### having a look at the first set of columns that we will loop through (all together and then separately)
unique(a[!is.na(a[,"bp.syst"]),c("id","seq.id",'m.f','bp.syst',"bp.syst.sd")])
unique(a[!is.na(a[,"bp.syst"]),c('seq.id')])
unique(a[!is.na(a[,"bp.syst"]),c('m.f')])
unique(a[!is.na(a[,"bp.syst"]),c('bp.syst')])
unique(a[!is.na(a[,"bp.syst"]),c('bp.syst.sd')])

##### In order to automate the simulation of data, all the column names can be saved in a vector, the position of a particular column name within that vector will become the iterator. There are technically a total of 3 iterators (sample, mean, sd), but they have the same length so we can reduce them to one. The columns sample 'm.f' and 'seq.id' are our index values, so they have to remain with the same name thorughout the iterations

##### Additionally, in order to create valid results, each value generated has to be within a range or margin. For instance, values generated for the modified ranking scale (MRS) can only go from 0 to 6 points - 0 is 'no symptoms' and 6 is 'patient has died'. Everyhing beyond that range is a fake, or invalid score

### setting lower and upper bound parameters for each variable
### lower bounds
names(a)
lb=c(90, # blood pressure (systolic)
     60, # blood pressure (diastolic)
     16, # BMI (lower limit)
     0, # depre GDS --> https://www.ncbi.nlm.nih.gov/pubmed/7183759
     0, # depre GDS (short form) --> https://www.ncbi.nlm.nih.gov/pubmed/7701365
     0, # depre BDI --> 
     0, # depre HDRS (17 item) --> https://www.ncbi.nlm.nih.gov/pubmed/3395203
     0, # depre HDRS (24 item) --> http://www.medafile.com/cln/HDRS.html
     0, # depre ida (only depression items) --> https://www.ncbi.nlm.nih.gov/pubmed/623950
     0, # depre HADS --> https://www.ncbi.nlm.nih.gov/pubmed/6880820 AND https://www.ncbi.nlm.nih.gov/pubmed/10626925 
     0, # HADS (part A - anxiety) 
     0, # HADS (part D - depression)  
     0, # POMS-D --> only 7 items for depression https://www.ncbi.nlm.nih.gov/pubmed/14241709 AND https://www.researchgate.net/publication/299823508_Abbreviated_POMS_Questionnaire_items_and_scoring_key
     0, # modified POMS --> total score
     0, # depre Zung scale --> https://www.ncbi.nlm.nih.gov/pubmed/14221692
     0, # state depression scale --> 4-point per item (10 items) --> https://www.ncbi.nlm.nih.gov/pubmed/22589461
     0, # NPI --> (depression item only) https://www.ncbi.nlm.nih.gov/pubmed/7991117
     0, # Barthel index --> https://www.ncbi.nlm.nih.gov/pubmed/14258950
     0, # modified Barthel Index --> https://www.ncbi.nlm.nih.gov/pubmed/3403500
     1, # rankin scale https://www.ncbi.nlm.nih.gov/pubmed/13432825
     0, # modified rankin
     0, # ADL scale --> https://www.ncbi.nlm.nih.gov/pubmed/7706627
     0, # IADL Lawton (8 points)
     0, # IADL Lawton Scale (24 points) --> https://instruct.uwo.ca/kinesiology/9641/Assessments/Social/IADL.html
     1, # IQ-CODE https://www.ncbi.nlm.nih.gov/pubmed/2594878
     0, # depre CES-D --> journals.sagepub.com/doi/10.1177/014662167700100306
     0, # SDS --> same as Zung self-rating dep scale
     0) # 4 item IADL https://www.ncbi.nlm.nih.gov/pubmed/8310892



#### upper bounds
ub=c(190, # blood pressure (systolic)
     120, # blood pressure (diastolic)
     45, # BMI (upper limit)
     30, # depre GDS
     15, # depre GDS short form
     63, # depre BDI
     53, # depre HDRS (17-item)
     53, # depre HDRS (24-items; only 17 are scored) https://pdfs.semanticscholar.org/6226/0e610f72f3a327ef8702fde5603d2e52e81f.pdf
     15, # depre IDA - only depression subscale
     21, # depre HADS (full scale - papers using this scale cited the full scale but only used the depression portion)
     21, # depre HADS (part A - anxiety)
     21, # depre HADS (part D - depression)
     28, # depre POMS-D 
     160, # mood modified POMS --> total score
     80, # depre Zung scale
     40, # state depression scale
     3, # NPI (depression item only)
     100, # barthel index
     20, # modified barthel index
     6, # rankin scale
     6, # modified rankin
     42, # ADL scale (14 items evalauted from 0-3)
     8, # IADL Lawton (8 points) 
     24, # IADL Lawton (24 points)
     5, # IQ code
     60, # CESD
     80, # SDS
     4) # 4-item IADL

# # cutoff=c(130, # syst
# #        80, # diastolic
# #        30, # BMI
# #        5, # GDS short
# #        9, # GDS
# #        18, # BDI
# #        7, # HDRS
# #        7, # HDRS
# #        4.5, # IDA
# #        7, # HADS (full scale)
# #        7, # HADS-A
# #        7, # HADS-D
# #        6.99, # cutoff reported for HIV patients https://www.researchgate.net/publication/254078410_The_Profile_of_Mood_States_as_a_Screening_Test_for_Major_Depression_in_HIV_Patients
# #
#        )

length(ub)
length(lb)
length(mean)

#### let's test the loop 'outside the loop' before generating the data. All data will be stored in a list called 'results', which will be updated after each iteration

vars=mean
results=list()
ids=unique(a[!is.na(a[,vars[1]]),c('seq.id')])
sim <- round(urnorm(n=a[!is.na(sd[1]) & a$seq.id==ids[85],c('m.f')], 
                   mean=a[!is.na(sd[1]) & a$seq.id==ids[85],c(mean[1])], 
                   sd=a[!is.na(sd[1]) & a$seq.id==ids[85],c(sd[1])],
                   lb = lb[1],
                   ub = ub[1]), digits = 2)
range(sim)
# sr[ids[85],vars[1]]=round(table(sim>lims[1])[2]/sum(table(sim>lims[1])),digits = 2)
results[[1]]<-sim

names(results)[1]=paste0(vars[1],'.index',ids[1])
names(results)[1]


###### getting unique values within each column and the ids (seq.id position) where they are located in the data frame
vars=sd
values=list()
ids=list()

#### looping through 48 columns to obtain unique numerical values (means and SDs) reported in papers, and the index rows in which they are located (index ids)
for (v in seq_along(vars)) {
  a[,vars[v]]=round(as.numeric(as.character(a[,vars[v]])), digits = 2) ##### dropping text values and IQRs
  values[[v]]=unique(a[!is.na(a[,vars[v]]),vars[v]])
  ids[[v]]=a[!is.na(a[,vars[v]]),'seq.id']
  # print(vars[v]) ###these two lines generate massive output, activate as needed
  # print(unique(a[!is.na(a[,vars[v]]),vars[v]]))
  
}
values
sim.num=unlist(ids)

names(a)
# length(vars) ### checking that the variables have the same length before looping
# length(mean)
# length(sd)

##### generating the datapoints ####
results=list()
for (v in seq_along(vars)) {
  for (i in seq_along(ids[[v]])) {
    set.seed(v)
    sim <- round(urnorm(n=a[!is.na(a[,sd[v]]) & a$seq.id==ids[[v]][i],'m.f'], 
                 mean=as.numeric(as.character(a[!is.na(a[,sd[v]]) & a$seq.id==ids[[v]][i],mean[v]])), 
                 sd=a[!is.na(a[,sd[v]]) & a$seq.id==ids[[v]][i],sd[v]],
                 lb = lb[v],
                 ub = ub[v]), digits = 2)
    sim=list(sim)
    names(sim)[1]=paste0(mean[v],'.index',ids[[v]][i])
    print(names(sim[1]))
    results=c(results,sim)
    
  }
}




length(unlist(results)) ##### 217 thousand data points generated
length(names(results)) #### 1074 simulations (not  unique) 

##### how many UNIQUE data simulations were generated?
index=names(results)
index.length=list()
for (i in seq_along(index)) {
  index.length[[i]]=length(results[[i]])
  
}

length(unique(unlist(index.length))) ###### 65 unique samples 
sum(unique(unlist(index.length))) ###### 16 thousand patients

#### what is the range of values obtained for each variable??
d=matrix(ncol=3)
for (i in seq_along(names(results))) {
  d= rbind(d,cbind(names(results[i]),rbind(range(results[i]))))
}
d=d[-1,]
d=data.frame(d)
names(d)=c('index','min','max')
d$min=round(as.numeric(as.character(d$min)), digits = 2)
d$max=round(as.numeric(as.character(d$max)), digits = 2)
d$index=as.character(d$index)

##### obtaining the unique variable names without the index numbers
for (i in 1:length(names(results))) {
  d$index[i]=strsplit(d$index,'.index')[[i]][1]
  }

unique(d[,c('index',"min","max")]) #### yessss!!! data simulation worked!!! no values were out of the possible range!!!

#### creating a database for the next analysis stage
dat=all

##### removing elements that I am not using anymore
rm(list=setdiff(ls(), c("dat",'results')))



####CREATING REHAB STAGES AND LONGEST FOLLOW-UP COLUMNS####
str(dat$day2i)
dat$diff.days<-dat$day2i-dat$day1i

####rehab stages####
dat$stage<-ifelse(dat$day2i <= 60, "1-60 days", 
                  ifelse(dat$day2i > 60 & dat$day2i <= 180, "61-180 days", 
                         ifelse(dat$day2i > 180 & dat$day2i <= 719,"181-719 days", ">719 days")))


#### age groups ####
summary(dat$age)

dat$age.group<-ifelse(dat$age >= 44 & dat$age <65, "44-65", ifelse(dat$age >= 65 & dat$age <70, ">65-70",
                                                               ">70-81"))



#### Key note - in most cases a higher score in an instrument means the person did better; however, some instruments have time as the main unit of measure, meaning that the longer the person takes to complete the task, the worse the performance. For these evaluations the effect size is the opposite (the less time, the better) than most of the assessments (the higher the score, the better performance)

###INVERTING THE EFFECT SIZE FOR TMT AND STROOP
# Studies in which a lower score in the TMT or Stroop are better
# Barret 2011
# Glymour 2008
# Ihle-Hansen 2014
# Lin 2011
# Mehrabian 2015
# Park 2014
# Pihlaja 2014
# Poulin 2017 
# Rand 2010
# Rasquin 2005
# Liu-Ambrose 2015
# Rozenthal-Iluz 2016
# Sarkamo 2010 


#### special cases
# Kim 2014 - scores reported as correct answers (do not have to invert)
# Bath 2017 - has combined scores (some but not all need to be flipped)


### INVERTING (effect) WHEN LOWER SCORE IS BETTER
unique(dat[dat$instrument=='tmt' | dat$instrument=='stroop',c('m1i','m2i',"instrument.detail",'author','year','id')])
studies=unique(dat[dat$instrument=='tmt' | dat$instrument=='stroop',c('id')])
### Taking out Kim 2014 and Bath 2017 (study IDs 95 and 18)
studies=studies[which(studies!='18')]
studies=studies[which(studies!='95')]

for (i in seq_along(studies)) {
  
  dat[dat$id==studies[i] & dat$instrument=='tmt' | 
        dat$id==studies[i] & dat$instrument=='stroop',c('m1i')]=
    dat[dat$id==studies[i] & dat$instrument=='tmt' | 
          dat$id==studies[i] & dat$instrument=='stroop',c('m1i')]*-1
  
  dat[dat$id==studies[i] & dat$instrument=='tmt' | 
        dat$id==studies[i] & dat$instrument=='stroop',c('m2i')]=
    dat[dat$id==studies[i] & dat$instrument=='tmt' | 
          dat$id==studies[i] & dat$instrument=='stroop',c('m2i')]*-1
}

##### Looking at what needs to be inverted in Bath 2017 (ID 18) 
##
dat[dat$id=='18' & dat$instrument=='tmt' |
      dat$id=='18' & dat$instrument=='stroop',
    c("instrument.detail")]
#### positions where the tests measuerdmeasured in time (sec) are
barret=dat[dat$id=='18' & dat$instrument=='tmt' |
             dat$id=='18' & dat$instrument=='stroop',]
positions=grep('sec',barret$instrument.detail)
barret[positions,"m1i"]=barret[positions,"m1i"]*-1
barret[positions,"m2i"]=barret[positions,"m2i"]*-1
##### replacing the values
dat[dat$id=='18' & dat$instrument=='tmt' |
      dat$id=='18' & dat$instrument=='stroop',]=barret

#### checking that the inversion worked
unique(dat[dat$instrument=='tmt' | dat$instrument=='stroop',c('m1i','m2i',"instrument.detail",'author','year','id')])

unique(dat[dat$m1i>dat$m2i,c('m1i',"m2i","instrument.detail")])

##### Other tests in which a lower score is best
## GDS (Global deterioration scale - the more points the more deteriorated)
## WWT (Walk while talking test - measured in time - the more time the worst)
## UFOV (measured in miliseconds - the more time the person takes to process objects the worst) 
### https://www-sciencedirect-com.ezp.lib.unimelb.edu.au/science/article/pii/S0887617706000382
## SDSA (measured in time and number of errors)
## Knafelc - combination of mmse and iqcode (wighted sum)
### SPMSQ - counts number of errors - higher score is worse
## IQ code - more points means worse
### CPT - omission and comission errors - same
### EFPT - more points means more help and less performance
### Balloons test [min]

### Key words from these tests
keys=sort(unique(dat[grep(paste('sec',
                                'efpt',
                                'gds',
                                'wwt',
                                'ufov',
                                'sdsa',
                                'err',
                                'iqcode',
                                'omis',
                                'commis',
                                'min',
                                'spmsq',
                                'time',sep= '|'),dat$instrument.detail),c("instrument.detail")]))

keys=keys[grep(paste('stro','sm','tmt','naming','iqcode','sdsa-rsrt', sep = '|'), keys, invert = T)]

#### inverting scores from these tests and printing the results
for (i in seq_along(keys)) {
  
  dat[dat$instrument.detail==keys[i],c("m1i")]=dat[dat$instrument.detail==keys[i],c("m1i")]*-1
  dat[dat$instrument.detail==keys[i],c("m2i")]=dat[dat$instrument.detail==keys[i],c("m2i")]*-1
  print(dat[dat$instrument.detail==keys[i],c('instrument.detail',"m1i","m2i")])
  
}


###### Calculating effect sizes #####

dat=dat[!is.na(dat$sd2i),] ### making sure no study has a null sd value 

dat <- escalc(measure="SMD", vtype = "UB", data=dat,
              m1i = m1i,
              n1i = n1i,
              sd1i = sd1i,
              m2i = m2i,
              n2i = n2i,
              sd2i = sd2i)
res <- rma(yi, vi, data=dat[dat$intervention=='yes',], slab = id)

# png('2.BaujatPlot-ids.png', width = 3000, height = 3000, res = 300)
baujat(res, symbol = 'slab', progbar = T)
# dev.off()

forest(res)

png('3.FunnelPlot-tau2.png', width = 3000, height = 3000, res = 300)
funnel(res,addtau2 = T,main=paste0("All studies (n=",length(unique(dat[,'id'])),') \n [tau squared]'))
dev.off()

png('3.FunnelPlot.png', width = 3000, height = 3000, res = 300)
funnel(res,main=paste0("All studies (n=",length(unique(dat[,'id'])),')'))
dev.off()


#### mega forest plot 1 ####

{
png('2.ForestPlot-all.png', width = 4000, height = 33000, res = 600)
# par(mar=c(4,4,1,2)) #### 'par' is the parameters to be seet; 'mar' is the number of lines that I want as a margin (bottom, left, top, right)
  par(mar=c(10,4,1,2))
  par(font=1, cex=.38)
###cleaning names for authors
### intervention and observation
obs=length(dat[dat$intervention=='no','intervention'])
int=length(dat[,'intervention'])
int-obs


authors=trimws(gsub('et al.| and| Halper| Kulkantrakorn|-Witlox|-Iluz|ares','',dat$author))
forest(res, order = order(dat$intervention, dat$day2i), ### ordering columns - first by intervention (yes/no), and then by days after stroke 
       slab = paste(autors,dat$year, sep = ', '),
       # slab = dat$id,
       xlim = c(-11,5),
       cex.lab=.8,
       cex.axis = 1,
       cex=1,
       
       rows=c(((obs+5):6),
              (int+5+9):
                (obs+5+10)),# observational first and then intervention (from bottom to top)
       ylim=c(35, ### in a very large forest plot this value needs to start up high so that the bottom reference line can be seen (but not extremely high, otherwise it will cut the bottom line)
              (int+5+9)+8),
       ilab = cbind(dat$n2i, round(dat$day2i), dat$domain, as.character(dat$instrument),as.character(dat$group.id)),
       ilab.xpos = c(-9.2,-8.5,-7.4,-6.1,-4.8),
       # mlab="",
       addfit = T,
       # addcred = T,
       # col=c("indianred1", "green"),
       efac = 0.08,
       showweights = F
)

par(font=2, cex=.4)
text(-10.8,  (int+5+9)+9, "Study",  pos=4)
text(4.9, (int+5+9)+9, "Standardized Mean Difference [95% CI]", pos=2)
text(-9.6, (int+5+9)+9, "Sample", pos=4)
text(-9, (int+5+9)+9, "  Days After \n    Stroke", pos=4)
text(-7.85, (int+5+9)+9, "    ICF \nDomain", pos=4)
text(-6.65, (int+5+9)+9, "Cogn. Test", pos=4)
text(-5.4, (int+5+9)+9, "Subgroup", pos=4)
par(font=1, cex=2)
text(-6.7,  (int+5+9)+30, "Forest Plot of Cognitive\n Recovery After Stroke",  pos=4)
par(font=2, cex=.8)
text(-3.8,  (int+5+9)+3, paste0("Intervention studies (n=",length(unique(dat[dat$intervention=='yes','id'])),')'),  pos=4)
text(-3.8,  ((obs+5))+5, paste0("Observational studies (n=",length(unique(dat[dat$intervention=='no','id'])),')'),  pos=4)


dev.off()
}

#### EFFECT OF INTERVENTION VS OBSERVATIONAL STUDIES

# hidden lines to extract the data for final forest plot
{res$beta
groups=as.character(names(res$beta[,1]))
groups='Overall'
effect=round(as.numeric(res$beta[,1]),digits = 3)
low.ci=round(res$ci.lb, digits = 3)
up.ci=round(res$ci.ub, digits = 3)
overall=cbind(groups,effect,low.ci,up.ci)
}

res.int.mod <- rma(yi, vi, mods = ~ factor(intervention)-1, 
               data=dat, 
               slab=id)  

res.int <- rma(yi, vi,  
                   data=dat[dat$intervention=='yes',], 
                   slab=id) 
{
groups=as.character(names(res.int$beta[,1]))
groups='Intervention'
effect=round(as.numeric(res.int$beta[,1]),digits = 3)
low.ci=round(res.int$ci.lb, digits = 3)
up.ci=round(res.int$ci.ub, digits = 3)
row=cbind(groups,effect,low.ci,up.ci)
empty=rbind(rep(NA,4))
int.all=rbind(rbind(row,empty))
  }
  
res.obs <- rma(yi, vi,  
               data=dat[dat$intervention=='no',], 
               slab=id) 

{
groups=as.character(names(res.obs$beta[,1]))
groups='Observational'
effect=round(as.numeric(res.obs$beta[,1]),digits = 3)
low.ci=round(res.obs$ci.lb, digits = 3)
up.ci=round(res.obs$ci.ub, digits = 3)
row=cbind(groups,effect,low.ci,up.ci)
empty=rbind(rep(NA,4))
obs.all=rbind(rbind(row,empty))

}

png('3.FunnelPlot-obs-int.png', width = 3000, height = 3000, res = 300)
funnel(res.int, main=paste0("All studies (n=",length(unique(dat[,'id'])),') \n [intervention as a moderator]'))
dev.off()

res.no.int <- rma(yi, vi, 
                  data=dat[dat$intervention=='no',], 
                  slab=id)  

png('3.FunnelPlot-obs-alone.png', width = 3000, height = 3000, res = 300)
funnel(res.no.int, main=paste0("Observational studies (n=",length(unique(dat[dat$intervention=='no','id'])),')'))
dev.off()

res.no.obs <- rma(yi, vi, 
                  data=dat[dat$intervention=='yes',], 
                  slab=id)  

png('3.FunnelPlot-int-alone.png', width = 3000, height = 3000, res = 300)
funnel(res.no.obs, main=paste0("Intervention studies (n=",length(unique(dat[dat$intervention=='yes','id'])),')'))
dev.off()


######### ANALYSIS OF MODERATORS #########
### tables within the brackets will be used for the final forest plot

{

#### TYPE OF STROKE (all)
res.stroke <- rma(yi, vi, mods = ~ factor(stroke.type)-1, 
                  data=dat, 
                  slab=id)  
{
groups=gsub('factor(stroke.type)','',as.character(names(res.stroke$beta[,1])), fixed = T)
effect=round(as.numeric(res.stroke$beta[,1]),digits = 3)
low.ci=round(res.stroke$ci.lb, digits = 3)
up.ci=round(res.stroke$ci.ub, digits = 3)
#### Putting everything in a matrix
stroke.all=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stroke.all=rbind(rbind(row,empty),stroke.all)
  
}

}

res.stroke.int=rma(yi, vi, mods = ~ factor(stroke.type)-1, 
                   data=dat[dat$intervention=='yes',], 
                   slab=id)  
{
groups=gsub('factor(stroke.type)','',as.character(names(res.stroke.int$beta[,1])), fixed = T)
effect=round(as.numeric(res.stroke.int$beta[,1]),digits = 3)
low.ci=round(res.stroke.int$ci.lb, digits = 3)
up.ci=round(res.stroke.int$ci.ub, digits = 3)
#### Putting everything in a matrix
stroke.int=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stroke.int=rbind(rbind(row,empty),stroke.int)
  
}
}
res.stroke.obs=rma(yi, vi, mods = ~ factor(stroke.type)-1, 
                   data=dat[dat$intervention=='no',], 
                   slab=id)  
{
groups=gsub('factor(stroke.type)','',as.character(names(res.stroke.obs$beta[,1])), fixed = T)
effect=round(as.numeric(res.stroke.obs$beta[,1]),digits = 3)
low.ci=round(res.stroke.obs$ci.lb, digits = 3)
up.ci=round(res.stroke.obs$ci.ub, digits = 3)
#### Putting everything in a matrix
stroke.obs=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stroke.obs=rbind(rbind(row,empty),stroke.obs)
  
}

}
#### REHAB STAGE (all)
res.stage <- rma(data=dat,yi, vi,slab=id,
                 mods = ~ factor(stage) - 1)
{
groups=gsub('factor(stage)','',as.character(names(res.stage$beta[,1])), fixed = T)
effect=round(as.numeric(res.stage$beta[,1]),digits = 3)
low.ci=round(res.stage$ci.lb, digits = 3)
up.ci=round(res.stage$ci.ub, digits = 3)
#### Putting everything in a matrix
stage.all=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stage.all=rbind(rbind(row,empty),stage.all)
  
}
row1=stage.all[1,]
row4=stage.all[4,]
row7=stage.all[7,]
stage.all[1,]=row7
stage.all[4,]=row1
stage.all[7,]=row4
stage.all

}

res.stage.int <- rma(data=dat[dat$intervention=='yes',],yi, vi,slab=id,
                     mods = ~ factor(stage) - 1)
  
{
groups=gsub('factor(stage)','',as.character(names(res.stage.int$beta[,1])), fixed = T)
effect=round(as.numeric(res.stage.int$beta[,1]),digits = 3)
low.ci=round(res.stage.int$ci.lb, digits = 3)
up.ci=round(res.stage.int$ci.ub, digits = 3)
#### Putting everything in a matrix
stage.int=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stage.int=rbind(rbind(row,empty),stage.int)
  
} 
row1=stage.int[1,]
row4=stage.int[4,]
row7=stage.int[7,]
stage.int[1,]=row7
stage.int[4,]=row1
stage.int[7,]=row4
stage.int

}


res.stage.obs <- rma(data=dat[dat$intervention=='no',],yi, vi,slab=id,
                     mods = ~ factor(stage) - 1)
  
{ 
groups=gsub('factor(stage)','',as.character(names(res.stage.obs$beta[,1])), fixed = T)
effect=round(as.numeric(res.stage.obs$beta[,1]),digits = 3)
low.ci=round(res.stage.obs$ci.lb, digits = 3)
up.ci=round(res.stage.obs$ci.ub, digits = 3)
#### Putting everything in a matrix
stage.obs=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  stage.obs=rbind(rbind(row,empty),stage.obs)
  
} 

row1=stage.obs[1,]
row4=stage.obs[4,]
row7=stage.obs[7,]
stage.obs[1,]=row7
stage.obs[4,]=row1
stage.obs[7,]=row4
stage.obs

}

#### COGNITIVE DOMAIN (all)
res.dom <- rma(data=dat,yi, vi,slab=id,
               mods = ~ factor(domain) - 1)

{
  groups=gsub('factor(domain)','',as.character(names(res.dom$beta[,1])), fixed = T)
effect=round(as.numeric(res.dom$beta[,1]),digits = 3)
low.ci=round(res.dom$ci.lb, digits = 3)
up.ci=round(res.dom$ci.ub, digits = 3)
#### Putting everything in a matrix
dom.all=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  dom.all=rbind(rbind(row,empty),dom.all)
  
}
}

res.dom.int <- rma(data=dat[dat$intervention=='yes',],yi, vi,slab=id,
               mods = ~ factor(domain) - 1)

{
groups=gsub('factor(domain)','',as.character(names(res.dom.int$beta[,1])), fixed = T)
effect=round(as.numeric(res.dom.int$beta[,1]),digits = 3)
low.ci=round(res.dom.int$ci.lb, digits = 3)
up.ci=round(res.dom.int$ci.ub, digits = 3)
#### Putting everything in a matrix
dom.int=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  dom.int=rbind(rbind(row,empty),dom.int)

}
}
res.dom.obs <- rma(data=dat[dat$intervention=='no',],yi, vi,slab=id,
                   mods = ~ factor(domain) - 1)
{
groups=gsub('factor(domain)','',as.character(names(res.dom.obs$beta[,1])), fixed = T)
effect=round(as.numeric(res.dom.obs$beta[,1]),digits = 3)
low.ci=round(res.dom.obs$ci.lb, digits = 3)
up.ci=round(res.dom.obs$ci.ub, digits = 3)
#### Putting everything in a matrix
dom.obs=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  dom.obs=rbind(rbind(row,empty),dom.obs)
}

}

#### AGE GROUPS (all)
res.age <- rma(data=dat,yi, vi,slab=id,
               mods = ~ factor(age.group) - 1)


{
  groups=gsub('factor(age.group)','',as.character(names(res.age$beta[,1])), fixed = T)
  effect=round(as.numeric(res.age$beta[,1]),digits = 3)
  low.ci=round(res.age$ci.lb, digits = 3)
  up.ci=round(res.age$ci.ub, digits = 3)
  #### Putting everything in a matrix
  age.all=matrix(ncol = 4)
  for (i in seq_along(groups)) {
    
    row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
    empty=rbind(rep(NA,4),rep(NA,4))
    age.all=rbind(rbind(row,empty),age.all)
  }
  
  row7=age.all[7,]
  row4=age.all[4,]
  age.all[7,]=row4
  age.all[4,]=row7
  age.all
}


res.age.int <- rma(data=dat[dat$intervention=='yes',],yi, vi,slab=id,
                   mods = ~ factor(age.group) - 1)


{
  groups=gsub('factor(age.group)','',as.character(names(res.age.int$beta[,1])), fixed = T)
effect=round(as.numeric(res.age.int$beta[,1]),digits = 3)
low.ci=round(res.age.int$ci.lb, digits = 3)
up.ci=round(res.age.int$ci.ub, digits = 3)
#### Putting everything in a matrix
age.int=matrix(ncol = 4)
for (i in seq_along(groups)) {
  
  row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
  empty=rbind(rep(NA,4),rep(NA,4))
  age.int=rbind(rbind(row,empty),age.int)
}

row7=age.int[7,]
row4=age.int[4,]
age.int[7,]=row4
age.int[4,]=row7
age.int
}


res.age.obs <- rma(data=dat[dat$intervention=='no',],yi, vi,slab=id,
                   mods = ~ factor(age.group) - 1)

{
  groups=gsub('factor(age.group)','',as.character(names(res.age.obs$beta[,1])), fixed = T)
  effect=round(as.numeric(res.age.obs$beta[,1]),digits = 3)
  low.ci=round(res.age.obs$ci.lb, digits = 3)
  up.ci=round(res.age.obs$ci.ub, digits = 3)
  #### Putting everything in a matrix
  age.obs=matrix(ncol = 4)
  for (i in seq_along(groups)) {
    
    row=cbind(groups[i],effect[i],low.ci[i],up.ci[i])
    empty=rbind(rep(NA,4),rep(NA,4))
    age.obs=rbind(rbind(row,empty),age.obs)
  }
  
  row7=age.obs[7,]
  row4=age.obs[4,]
  age.obs[7,]=row4
  age.obs[4,]=row7
  age.obs
}
}

# View(fp)
#####putting eveything together
na=int.all[,2:4]
na[1,]=NA
na10=rbind(rep(NA,10),rep(NA,10))
fp=cbind(int.all,int.all[,2:4],na)
fp=rbind(fp,cbind(obs.all,na,obs.all[,2:4]),na10)
fp=rbind(fp,cbind(stage.all,stage.int[,2:4],stage.obs[,2:4]),rbind(na10[1,]))

#### not all domains are present in the intervention / observational papers
#### replacing absent domains with NAs to make the dataframes the same length
all.doms=dom.all[!is.na(dom.all[,1]),1]
int.doms=dom.int[!is.na(dom.int[,1]),1]
obs.doms=dom.obs[!is.na(dom.obs[,1]),1]
### what domains are absent in the intervention group? (or what values don't match?)
all.doms[-match(int.doms,all.doms)]
### where should NAs be introduced?
int.doms[match(all.doms,int.doms)]
struct=int.doms[match(all.doms,int.doms)]

### replacing NAs with something meaningful that will allow to do a logicla loop (with TRUE and FALSE)
struct[is.na(struct)]='no domain'

###creting empty cels (3 rows)
nas=rbind(rep(NA,4),rep(NA,4),rep(NA,4))
df=matrix(ncol = 4)
for (i in seq_along(struct)) {
  if (struct[i]==all.doms[i]) {
  info=dom.int[which(dom.int==struct[i]):
    c(which(dom.int==struct[i])+2),]
  df=rbind(df,info)
    
  }else {
    df=rbind(df,nas)
  } 
    
}
 
#### final matrix of cognitive domains for intervention studies (and removing first row, which was the empty data generated outside the loop)
dom.int=df[-1,]


### what domains are absent in the observational study group? (or what values don't match?)
all.doms[-match(obs.doms,all.doms)]  ### only one domain (visual field)

### where should NAs be introduced?
obs.doms[match(all.doms,obs.doms)] 
struct=obs.doms[match(all.doms,obs.doms)]

### replacing NAs with something meaningful that will allow to do a logicla loop (with TRUE and FALSE)
struct[is.na(struct)]='no domain'

###creting empty cels (3 rows)
df=matrix(ncol = 4)
for (i in seq_along(struct)) {
  if (struct[i]==all.doms[i]) {
    info=dom.obs[which(dom.obs==struct[i]):
                   c(which(dom.obs==struct[i])+2),]
    df=rbind(df,info)
    
  }else {
    df=rbind(df,nas)
  } 
  
}


#### final matrix for observational studies (and removing first row, which was the empty data generated outside the loop)
dom.obs=df[-1,]
dom.obs

### dom.all has an extra last empty row that we will have to remove to make it the same dimensions as the domain/intervention and domain/observational dataframes
dom.all=dom.all[-length(dom.all[,1]),]

#### binding effect sizes for all cognitive domains forest plot matrix
fp=rbind(rbind(fp,cbind(dom.all,dom.int[,2:4],dom.obs[,2:4])),na10)

#### adding stroke type to the database
fp=rbind(rbind(fp,cbind(stroke.all,stroke.int[,2:4],stroke.obs[,2:4])),rbind(na10[1,]))

#### adding age groups
fp=rbind(fp,cbind(age.all,age.int[,2:4],age.obs[,2:4]))

#### adding an indent at the beggining of subgroup variables
fp[!is.na(fp[,1]),1]=paste0('       ',fp[!is.na(fp[,1]),1])

##### creating more space between rows to add column titles
fp=rbind(na10,fp)

##### adding main group names
add.names=c('Study type (2)***','Rehab. stage (4)***', 'Cog. domain (14)***', 'Stroke type (6)***', 'Age group (3)***')
positions=c(1,
            length(unique(dat$intervention))*3+1,
            length(unique(dat$intervention))*3+1+length(unique(dat$stage))*3+2,
            length(unique(dat$intervention))*3+1+length(unique(dat$stage))*3+2+length(unique(dat$domain))*3+2,
            length(unique(dat$intervention))*3+1+length(unique(dat$stage))*3+2+length(unique(dat$domain))*3+2+length(unique(dat$stroke.type))*3+2)
fp[positions,1]=add.names

#### adding summary line in the last row
fp[length(fp[,1]),]=c(overall,overall[,2:4],overall[NA,2:4])
fp
##### adding more space at top (3 rows) and bottom (2 more rows) for better visualization
fp=rbind(t(matrix(na10[2,])),
      na10,
      fp,na10)

####
sample.pooled=c(sum(unique(dat[dat$intervention=='yes',c('id','n1i')])[,2]), ## sample intervention
                sum(unique(dat[dat$intervention=='no',c('id','n1i')])[,2]), ## sample no intervention
                sum(unique(dat[dat$stage=='1-60 days' ,c('id','n1i')])[,2]),  ### sample acute 
                sum(unique(dat[dat$stage=='61-180 days',c('id','n1i')])[,2]),  ### subacute
                sum(unique(dat[dat$stage=='181-719 days',c('id','n1i')])[,2]), ### long term
                sum(unique(dat[dat$stage=='>719 days',c('id','n1i')])[,2]),  ### very long term
                sum(unique(dat[dat$domain=='visual field',c('id','n1i')])[,2]),  
                sum(unique(dat[dat$domain=='psych-mot sp',c('id','n1i')])[,2]), 
                sum(unique(dat[dat$domain=='praxis',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='perceptual',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='orientation',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='mental functions',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='memory',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='language',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='intellectual',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='hlcf',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='global psychosocial',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='consciousness',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='calculation',c('id','n1i')])[,2]),
                sum(unique(dat[dat$domain=='attention',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='mixed',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='ischemic or hem',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='ischemic',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='first-ever (mixed)',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='first-ever (isch)',c('id','n1i')])[,2]),
                sum(unique(dat[dat$stroke.type=='first-ever (isch or hem)',c('id','n1i')])[,2]),
                sum(unique(dat[dat$age.group=='44-65',c('id','n1i')])[,2]),
                sum(unique(dat[dat$age.group=='>65-70',c('id','n1i')])[,2]),
                sum(unique(dat[dat$age.group=='>70-81',c('id','n1i')])[,2]),
                sum(unique(dat[,c('id','n1i')])[,2])
                )

positions=grep('-',as.character(fp[,2]))
length(positions)==length(sample.pooled) ### just making sure the elements to add are the same as the samples pooled

#### creating empty sample column and populating it
fp=cbind(fp,'sample'=NA)
fp[positions,'sample']=sample.pooled

#### number of group comparisons
group.comp=c(length(dat[dat$intervention=='yes',c('id','n1i')][,1]), 
                           length(dat[dat$intervention=='no',c('id','n1i')][,2]),
                           length(dat[dat$stage=='1-60 days' ,c('id','n1i')][,2]),
                           length(dat[dat$stage=='61-180 days',c('id','n1i')][,2]),
                           length(dat[dat$stage=='181-719 days',c('id','n1i')][,2]),
                           length(dat[dat$stage=='>719 days',c('id','n1i')][,2]),
                           length(dat[dat$domain=='visual field',c('id','n1i')][,2]),  
                           length(dat[dat$domain=='psych-mot sp',c('id','n1i')][,2]), 
                           length(dat[dat$domain=='praxis',c('id','n1i')][,2]),
                           length(dat[dat$domain=='perceptual',c('id','n1i')][,2]),
                           length(dat[dat$domain=='orientation',c('id','n1i')][,2]),
                           length(dat[dat$domain=='mental functions',c('id','n1i')][,2]),
                           length(dat[dat$domain=='memory',c('id','n1i')][,2]),
                           length(dat[dat$domain=='language',c('id','n1i')][,2]),
                           length(dat[dat$domain=='intellectual',c('id','n1i')][,2]),
                           length(dat[dat$domain=='hlcf',c('id','n1i')][,2]),
                           length(dat[dat$domain=='global psychosocial',c('id','n1i')][,2]),
                           length(dat[dat$domain=='consciousness',c('id','n1i')][,2]),
                           length(dat[dat$domain=='calculation',c('id','n1i')][,2]),
                           length(dat[dat$domain=='attention',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='mixed',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='ischemic or hem',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='ischemic',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='first-ever (mixed)',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='first-ever (isch)',c('id','n1i')][,2]),
                           length(dat[dat$stroke.type=='first-ever (isch or hem)',c('id','n1i')][,2]),
                           length(dat[dat$age.group=='44-65',c('id','n1i')][,2]),
                           length(dat[dat$age.group=='>65-70',c('id','n1i')][,2]),
                           length(dat[dat$age.group=='>70-81',c('id','n1i')][,2]),
                           length(dat[,c('id','n1i')][,2]))


length(positions)==length(group.comp)

#### adding an empty column to populate
fp=cbind(fp,'group.comp'=NA)
fp[positions,'group.comp']=group.comp


#### adding the confidence intervals ino the same column
fp=cbind(fp,'ci.join'=NA)
fp[positions,'ci.join']=paste0(round(as.numeric(fp[!is.na(fp[,"low.ci"]),"low.ci"]), digits=3),' , ',
                               round(as.numeric(fp[!is.na(fp[,"up.ci"]),"up.ci"]), digits = 3))

#### creating names for each column and storing them in a separate matrix
z.3=cbind(c(' Moderators used \n            (df)',fp[,'groups']),
          c("Nº of group\ncomparisons", fp[,'group.comp']),
          c("Sample\npooled", fp[,"sample"]),
          c("Effect Size\n (Cohen's d)", fp[,"effect"]),
         c("95% Conf. Int.\n[lower - upper]",fp[,"ci.join"]))

z.3=z.3[-4,] ### reducing the space between column name and data by deleting one line of empty data

### effect size and confidence intervals have to in a different data.frame for plotting
z=data.frame(fp)

mean=cbind(
  c(as.numeric(as.character(z$effect.1))),
  c(as.numeric(as.character(z$effect.2))))
lower=cbind(
    c(as.numeric(as.character(z$low.ci.1))),
    c(as.numeric(as.character(z$low.ci.2))))
upper=cbind(
  c(as.numeric(as.character(z$up.ci.1))),
  c(as.numeric(as.character(z$up.ci.2))))

# BLACK AND WHITE
{
png('4.ForestPlot-all.png', width = 4000, height = 5000, res = 600)
forestplot(z.3,
           mean=mean,
           lower=lower,
           upper=upper,
           is.summary=c(rep(T,10),
                        rep(F, 13),
                        T,#24
                        rep(F,43),
                        T,#68
                        rep(F,19),
                        T, ##88
                        rep(F,10),
                        T),#99
           fn.ci_norm = c(fpDrawNormalCI, fpDrawDiamondCI),
           legend=c(
             # "Average",
             "Intervention (any)", 
             "Usual care"),
           legend_args = fpLegend(pos = list("bottom", 
                                             inset=.96,
                                             align="horizontal"),
                                  title="Group",
                                  r = unit(.7, "snpc"),
                                  gp = gpar(col="#CCCCCC", 
                                            lwd=1.5)),#surrounding line width
           cex=.8, lineheight = "auto", boxsize=1, colgap=unit(1,"mm"),# separation of columns (make 5 when saving)
           clip=c(log(.35),log(1.35)),# clipping effect size scale into logarithmic
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4,
           graphwidth = unit(2,"inches"), #make this line 2 when saving
           line.margin = .6,# this line needs to be added to avoid crowding of the effect sizes
           graph.pos = 4, #position of the graph in the table (column number)
           xlab="                   <----------- Less impairment    ---     More impairment ----------->                    
           Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05",
           title="Figure 2: Forest Plot of Cognition After Stroke\n [mixed-effects model]",
           txt_gp=fpTxtGp(label=gpar(cex=.6),#change this line to "1" before saving
                          ticks=gpar(cex=.5), 
                          xlab=gpar(cex = .5),
                          title=gpar(cex = 1)),
           grid = TRUE,
           xticks = c(-1.4, -0.5, 0, .5),
           hrzl_lines=list(
             "7" = gpar(lwd=24, lineend="butt", columns=c(1:6), col="#99999922"),
             "46" = gpar(lwd=280, lineend="butt", columns=c(1:6), col="#99999922"),
             "93" = gpar(lwd=175/3, lineend="butt", columns=c(1:6), col="#99999922"),
             "100" = gpar(lwd=15, lineend="butt", columns=c(1:6), col="#99999922")))

dev.off()

}

### adding colours

{
  png('5.ForestPlot-all.png', width = 4000, height = 5000, res = 600)
  forestplot(z.3,
             mean=mean,
             lower=lower,
             upper=upper,
             is.summary=c(rep(T,10),
                          rep(F, 13),
                          T,#24
                          rep(F,43),
                          T,#68
                          rep(F,19),
                          T, ##88
                          rep(F,10),
                          T),#99
             # fn.ci_norm = c(fpDrawNormalCI, fpDrawDiamondCI),
             col=fpColors(box=c(
               "royalblue",
               # "gray")
               "gold"),
               line=c(
                 "darkblue",
                 # "gray",
                 "orange"),
               summary=c(
                 "darkblue",
                 "orange")),
             legend=c(
               # "Average",
               "Intervention (any)", 
               "Usual care"),
             legend_args = fpLegend(pos = list("bottom", 
                                               inset=.96,
                                               align="horizontal"),
                                    title="Group",
                                    r = unit(.7, "snpc"),
                                    gp = gpar(col="#CCCCCC", 
                                              lwd=1.5)),#surrounding line width
             cex=.8, lineheight = "auto", boxsize=1, colgap=unit(1,"mm"),# separation of columns (make 5 when saving)
             clip=c(log(.35),log(1.35)),# clipping effect size scale into logarithmic
             lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4,
             graphwidth = unit(2,"inches"), #make this line 2 when saving
             line.margin = .6,# this line needs to be added to avoid crowding of the effect sizes
             graph.pos = 4, #position of the graph in the table (column number)
             xlab="                   <----------- Less impairment    ---     More impairment ----------->                    
             Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05",
             title="Figure 2: Forest Plot of Cognition After Stroke\n [mixed-effects model]",
             txt_gp=fpTxtGp(label=gpar(cex=.6),#change this line to "1" before saving
                            ticks=gpar(cex=.5), 
                            xlab=gpar(cex = .5),
                            title=gpar(cex = 1)),
             grid = TRUE,
             xticks = c(-1.4, -0.5, 0, .5),
             hrzl_lines=list(
               "7" = gpar(lwd=24, lineend="butt", columns=c(1:6), col="#99999922"),
               "46" = gpar(lwd=280, lineend="butt", columns=c(1:6), col="#99999922"),
               "93" = gpar(lwd=175/3, lineend="butt", columns=c(1:6), col="#99999922"),
               "100" = gpar(lwd=15, lineend="butt", columns=c(1:6), col="#99999922")))
  
  dev.off()
  
}




#### important results
#### sample pooled (total)
sum(unique(dat[,c("id",'n1i')])[,2])
length(unique((unique(dat[,c("id",'n1i')])[,1])))



#### comorbitities

describe(dat$ht) ### 40% percent of studies presence/absence of hypertension
sort(unique(dat$ht)) ### values range from 17 to 100% of the sample
describe(dat$bp.syst) ### another 17% of studies have reported blood pressure (which can be dichomized)

describe(dat$dm) ### 40% percent of studies reported diabetes
sort(unique(dat$dm)) ### values range from 2 to 75%

describe(dat$hd)  ### 7.5% of studies reported presence/absence of heart disease

describe(dat$hyperlip) ### 5% of studies reported presence/absence of hyperlipidemia
sort(unique(dat$hyperlip)) ### values range from 10 to 70%


#### Mega forest plot 2 ####
{
png('2A.ForestPlot-all.png', width = 4000, height = 33000, res = 600)
# par(mar=c(4,4,1,2)) #### 'par' is the parameters to be seet; 'mar' is the number of lines that I want as a margin (bottom, left, top, right)
par(mar=c(10,4,1,2))
par(font=1, cex=.38)
###cleaning names for authors
### intervention and observation
obs=length(dat[dat$intervention=='no','intervention'])
int=length(dat[,'intervention'])
int-obs

vlt.obs=length(dat[dat$intervention=='no' & dat$stage=='>719 days','id'])+9
lt.obs=length(dat[dat$intervention=='no' & dat$stage=='181-719 days','id'])+6+vlt.obs
sa.obs=length(dat[dat$intervention=='no' & dat$stage=='61-180 days','id'])+6+lt.obs
ac.obs=length(dat[dat$intervention=='no' & dat$stage=='1-60 days','id'])+6+sa.obs

vlt.int=length(dat[dat$intervention=='yes' & dat$stage=='>719 days','id'])+15+ac.obs
lt.int=length(dat[dat$intervention=='yes' & dat$stage=='181-719 days','id'])+6+vlt.int
sa.int=length(dat[dat$intervention=='yes' & dat$stage=='61-180 days','id'])+6+lt.int
ac.int=length(dat[dat$intervention=='yes' & dat$stage=='1-60 days','id'])+6+sa.int


authors=trimws(gsub('et al.| and| Halper| Kulkantrakorn|-Witlox|-Iluz|ares','',dat$author))
forest(res, order = order(dat$intervention, dat$day2i), ### ordering columns - first by intervention (yes/no), and then by days after stroke 
       slab = paste(autors,dat$year, sep = ', '),
       # slab = dat$id,
       xlim = c(-11,5),
       cex.lab=.8,
       cex.axis = 1,
       cex=1,
       
       # Rows below are organized by rehab stages. Seven empty rows will separate the different phases. These empty rows will then be populated with results of the data subsetting. Observational will go first and then intervention (from bottom to top)
       rows=c((ac.obs:(sa.obs+7)),
              (sa.obs:(lt.obs+7)),
              (lt.obs:(vlt.obs+7)),
              (vlt.obs:10),
              (ac.int:(sa.int+7)),
              (sa.int:(lt.int+7)),
               (lt.int:(vlt.int+7)),
              (vlt.int:(ac.obs+16))),
                
       ylim=c(35, ### in a very large forest plot this value needs to start up high so that the bottom reference line can be seen (but not extremely high, otherwise it will cut the bottom line)
              (ac.int)+8),
       ilab = cbind(dat$n2i, round(dat$day2i), dat$domain, as.character(dat$instrument),as.character(dat$group.id)),
       ilab.xpos = c(-9.2,-8.5,-7.4,-6.1,-4.8),
       # mlab="",
       addfit = T,
       # addcred = T,
       # col=c("indianred1", "green"),
       efac = 0.08,
       showweights = F
)

par(font=2, cex=.4)
text(-10.8,  (ac.int)+9, "Study",  pos=4)
text(4.9, (ac.int)+9, "Standardized Mean Difference [95% CI]", pos=2)
text(-9.6, (ac.int)+9, "Sample", pos=4)
text(-9, (ac.int)+9, "  Days After \n    Stroke", pos=4)
text(-7.85, (ac.int)+9, "    ICF \nDomain", pos=4)
text(-6.65, (ac.int)+9, "Cogn. Test", pos=4)
text(-5.4, (ac.int)+9, "Subgroup", pos=4)
par(font=1, cex=2)
text(-6.7,  (ac.int)+30, "Forest Plot of Cognitive\n Recovery After Stroke",  pos=4)
par(font=2, cex=.8)
text(-3.8,  (ac.int)+3, paste0("Intervention studies (n=",length(unique(dat[dat$intervention=='yes','id'])),')'),  pos=4)
text(-3.8,  ((ac.obs))+5, paste0("Observational studies (n=",length(unique(dat[dat$intervention=='no','id'])),')'),  pos=4)

par(font=4, cex=.55)
text(-9.3, c(vlt.obs+1.5,lt.obs+1.5,sa.obs+1.5,ac.obs+1.5,
           vlt.int+1.5,lt.int+1.5,sa.int+1.5,ac.int+1.5), pos=4, c("Very long-term (>719 days)",
                                                   "Long-term (181-719 days)",
                                                   "Subacute (61-180 days)",
                                                   "Early stage (1-60 days)",
                                                   "Very long-term (>719 days)",
                                                   "Long-term (181-719 days)",
                                                   "Subacute (61-180 days)",
                                                   "Early stage (1-60 days)"))
par(font=4, cex=.55)
text(-11, c(4.5,ac.obs+10.9), pos=4, c("Overall effect observational studies",
                                "Overall effect intervention studies"))
par(font = 2, cex= .6)
# addpoly(res, row=-2, mlab="", efac = 1, col = "maroon")
text(-7.7, -5, pos=3, bquote(paste("All Studies (Q = ",
                                   .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                   ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                   .(formatC(res$I2, digits=1, format="f")), "%)")))



res.r<-rma(yi, vi, data=dat,
           subset=(intervention=="no"),
           slab=id)
res.s<-rma(yi, vi, data=dat,
           subset=(intervention=="yes"),
           slab=id)

dat.ob=dat %>% filter (intervention=="no")
res.e.ob<-rma(yi, vi, data=dat.ob,
           subset= (stage=="1-60 days"),
           slab=id)
res.m.ob<-rma(yi, vi, data=dat.ob,
           subset=(stage=="61-180 days"),
           slab=id)
res.lt.ob<-rma(yi, vi, data=dat.ob,
           subset=(stage=="181-719 days"),
           slab=id)
res.vlt.ob<-rma(yi, vi, data=dat.ob,
               subset=(stage==">719 days"),
               slab=id)

dat.in=dat %>% filter (intervention=="yes")
res.e.in<-rma(yi, vi, data=dat.in,
              subset= (stage=="1-60 days"),
              slab=id)
res.m.in<-rma(yi, vi, data=dat.in,
              subset=(stage=="61-180 days"),
              slab=id)
res.lt.in<-rma(yi, vi, data=dat.in,
              subset=(stage=="181-719 days"),
              slab=id)
res.vlt.in<-rma(yi, vi, data=dat.in,
              subset=(stage==">719 days"),
              slab=id)


# c(10,41,78,125,151,165,217,268)

addpoly(res.r, row=3, cex=.7, mlab="", col = "white", efac = 0.3)
addpoly(res.vlt.ob, row= 7, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.lt.ob, row= vlt.obs+3.5, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.m.ob, row= lt.obs+3.5, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.e.ob, row= sa.obs+3.5, cex=.5, mlab="", col = "white", efac = 0.3)

addpoly(res.s, row= ac.obs+9.5, cex=.7, mlab="", col = "white", efac = 0.3)
addpoly(res.vlt.in, row=ac.obs+12.5, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.lt.in, row=vlt.int+3.5, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.m.in, row=lt.int+3.5, cex=.5, mlab="", col = "white", efac = 0.3)
addpoly(res.e.in, row=sa.int+3.5, cex=.5, mlab="", col = "white", efac = 0.3)



### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
### add text for the subgroups


# c(10,41,78,125,151,165,217,268)
par(font=4)
text(-10.9, 2, pos=4, cex=.7, bquote(paste("RE Model for Subgroup (Q = ",
                                         .(formatC(res.r$QE, digits=2, format="f")), ", df = ", .(res.r$k - res.r$p),
                                         ", p = ", .(formatC(res.r$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                         .(formatC(res.r$I2, digits=1, format="f")), "%)")))

text(-10.9, 8, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                         .(formatC(res.vlt.ob$QE, digits=2, format="f")), ", df = ", .(res.vlt.ob$k - res.vlt.ob$p),
                                         ", p = ", .(formatC(res.vlt.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                         .(formatC(res.vlt.ob$I2, digits=1, format="f")), "%)")))
text(-10.9, vlt.obs+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                          .(formatC(res.lt.ob$QE, digits=2, format="f")), ", df = ", .(res.lt.ob$k - res.lt.ob$p),
                                          ", p = ", .(formatC(res.lt.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                          .(formatC(res.lt.ob$I2, digits=1, format="f")), "%)")))
text(-10.9, lt.obs+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                          .(formatC(res.m.ob$QE, digits=2, format="f")), ", df = ", .(res.m.ob$k - res.m.ob$p),
                                          ", p = ", .(formatC(res.m.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                          .(formatC(res.m.ob$I2, digits=1, format="f")), "%)")))
text(-10.9, sa.obs+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.e.ob$QE, digits=2, format="f")), ", df = ", .(res.e.ob$k - res.e.ob$p),
                                           ", p = ", .(formatC(res.e.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.e.ob$I2, digits=1, format="f")), "%)")))



text(-10.9, ac.obs+8.5, pos=4, cex=.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.s$QE, digits=2, format="f")), ", df = ", .(res.s$k - res.s$p),
                                           ", p = ", .(formatC(res.s$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.s$I2, digits=1, format="f")), "%)")))
text(-10.9, ac.obs+14, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.vlt.ob$QE, digits=2, format="f")), ", df = ", .(res.vlt.ob$k - res.vlt.ob$p),
                                           ", p = ", .(formatC(res.vlt.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.vlt.ob$I2, digits=1, format="f")), "%)")))
text(-10.9, vlt.int+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.lt.ob$QE, digits=2, format="f")), ", df = ", .(res.lt.ob$k - res.lt.ob$p),
                                           ", p = ", .(formatC(res.lt.ob$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.lt.ob$I2, digits=1, format="f")), "%)")))
text(-10.9, lt.int+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.m.in$QE, digits=2, format="f")), ", df = ", .(res.m.in$k - res.m.in$p),
                                           ", p = ", .(formatC(res.m.in$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.m.in$I2, digits=1, format="f")), "%)")))

text(-10.9, sa.int+5, pos=4, cex=0.7, bquote(paste("RE Model for Subgroup (Q = ",
                                           .(formatC(res.e.in$QE, digits=2, format="f")), ", df = ", .(res.e.in$k - res.e.in$p),
                                           ", p = ", .(formatC(res.e.in$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res.e.in$I2, digits=1, format="f")), "%)")))



dev.off()
}



