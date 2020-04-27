## Effectiveness Experiment Russia and Venezuela :  Do File

# Load libraries

library¥foreign¤
library¥data©table¤
library¥stargazer¤
library¥ggplot2¤
library¥xtable¤
library¥arm¤
library¥mlogit¤
library¥nnet¤
library¥car¤
library¥erer¤
library¥lfe¤
library¥rms¤
library¥prediction¤
library¥glm©predict¤
library¥multcomp¤
library¥mfx¤


#Data
rm¥list=ls¥¤¤
specify_decimal <¬ function¥x« k¤ format¥as©numeric¥round¥x« k¤« nsmall=k¤¤
mod_stargazer <¬ function¥est¤ {
  capture©output¥est¤
}

#### Load Each of the Survey Datasets based on your Working Directory
setwd¥dirname¥rstudioapi::getActiveDocumentContext¥¤$path¤¤
load¥"RussiaSurveyData©Rda"¤
load¥"VenezuelaSurveyData©Rda"¤

rus_emp<¬subset¥rus« employed==1¤
ven_emp<¬subset¥ven« employed==1¤



#########################################################
#############     MAIN TEXT           ###################
#########################################################


###########################################
########     FIGURE 1      ################
###########################################

####### Panel A

rus_d<¬rus[rus$employed==1][«list¥expoutcome_m=mean¥expoutcome¤¤«by=c¥"strategy"«"broker"¤]
rus_d$broker<¬as©character¥rus_d$broker¤
rus_d$broker[rus_d$broker=="Employer"]<¬" Employer"
rus_d$broker[rus_d$broker=="Party"]<¬" Party Activist"
rus_d$broker[rus_d$broker=="Official"]<¬"Government Official"

fig1_panela<¬ggplot¥data=rus_d« aes¥x=broker« y=expoutcome_m« fill=strategy¤¤ + geom_bar¥colour="black"« stat="identity"«position=position_dodge¥¤«size=©3¤+ scale_fill_grey¥ name=""«breaks=c¥"0"« "1"« "2"«"3"¤«labels=c¥"Simple Ask     "« "Organizational Threat     "« "Turnout¬Buying     "«"Individual Threat"¤¤+    xlab¥""¤ + ylab¥"Likelihood of Voting"¤ + theme_bw¥¤+ theme¥legend©position="bottom"«axis©text=element_text¥size=14¤«axis©text©x=element_text¥face = "bold"¤«axis©title©y=element_text¥size=14¤«legend©text=element_text¥size=12¤«plot©title = element_text¥hjust = 0©5«size=16¤¤+ coord_cartesian¥ylim=c¥2©25«3©25¤¤+ geom_text¥aes¥label=specify_decimal¥expoutcome_m«2¤¤« position=position_dodge¥width=0©9¤« vjust=¬0©4¤+ggtitle¥"Panel A: Russia Survey"¤

####### Panel B

ven_d<¬ven[ven$employed==1][«list¥vote_intent_m=mean¥vote_intent«na©rm=TRUE¤¤«by=c¥"strategy_number"«"broker"¤]
ven_d$broker<¬as©character¥ven_d$broker¤
ven_d$broker[ven_d$broker=="Employer"]<¬" Employer"
ven_d$broker[ven_d$broker=="Party"]<¬" Party Activist"
ven_d$broker[ven_d$broker=="Official"]<¬"Neighborhood Leader"

fig1_panelb<¬ggplot¥data=ven_d« aes¥x=broker« y=vote_intent_m« fill=strategy_number¤¤ + geom_bar¥colour="black"« stat="identity"«position=position_dodge¥¤«size=©3¤+ scale_fill_grey¥ name=""«breaks=c¥"0"« "1"« "2"«"3"¤«labels=c¥"Simple Ask     "« "Organizational Threat     "« "Turnout¬Buying     "«"Individual Threat"¤¤+    xlab¥""¤ + ylab¥"Likelihood of Voting"¤ + theme_bw¥¤+ theme¥legend©position="bottom"«axis©text=element_text¥size=14¤«axis©text©x=element_text¥face = "bold"¤«axis©title©y=element_text¥size=14¤«legend©text=element_text¥size=12¤«plot©title = element_text¥hjust = 0©5«size=16¤¤+ coord_cartesian¥ylim=c¥2«4©5¤¤+ geom_text¥aes¥label=specify_decimal¥vote_intent_m«2¤¤« position=position_dodge¥width=0©9¤« vjust=¬0©4¤+ggtitle¥"\n\nPanel B: Venezuela Survey"¤


###########################################
########     FIGURE 2      ################
###########################################

interval <¬ ¬qnorm¥¥1¬0©95¤/2¤  # 95% multiplier

####### Panel A

### Brokers
emp_diff<¬lm¥expoutcome~factor¥employer4¤« data=subset¥rus«employed==1¤¤
party_diff<¬lm¥expoutcome~factor¥activist4¤« data=subset¥rus«employed==1¤¤
official_diff<¬lm¥expoutcome~factor¥official4¤« data=subset¥rus«employed==1¤¤

### Strategies
gift_diff<¬lm¥expoutcome~factor¥gift4¤« data=subset¥rus«employed==1¤¤
threat_diff<¬lm¥expoutcome~factor¥threat4¤« data=subset¥rus«employed==1¤¤
benign_diff<¬lm¥expoutcome~factor¥benign4¤« data=subset¥rus«employed==1¤¤
org_diff<¬lm¥expoutcome~factor¥org4¤« data=subset¥rus«employed==1¤¤

coefficients<¬data©frame¥var=as©character¥¤«fe = as©numeric¥¤«se= as©numeric¥¤« subset=as©character¥¤¤

coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥benign_diff¤$coefficients[«1][2]«se= summary¥benign_diff¤$coefficients[«2][2]« subset="Simple Ask     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥org_diff¤$coefficients[«1][2]«se= summary¥org_diff¤$coefficients[«2][2]« subset="Organizational Threat     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥gift_diff¤$coefficients[«1][2]«se= summary¥gift_diff¤$coefficients[«2][2]« subset="Turnout¬Buying     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥threat_diff¤$coefficients[«1][2]«se= summary¥threat_diff¤$coefficients[«2][2]« subset="Individual Threat"¤¤

coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥emp_diff¤$coefficients[«1][2]«se= summary¥emp_diff¤$coefficients[«2][2]« subset="Employer"¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥party_diff¤$coefficients[«1][2]«se= summary¥party_diff¤$coefficients[«2][2]« subset="Party Activist"¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥official_diff¤$coefficients[«1][2]«se= summary¥official_diff¤$coefficients[«2][2]« subset="Government Official"¤¤

coefficients <¬ within¥coefficients«
                       var <¬ ordered¥var« levels = rev¥sort¥unique¥var¤¤¤¤¤
secbreaks = rev¥unique¥as©character¥coefficients$var¤¤¤

# Plot
fig2_panela<¬ggplot¥coefficients« aes¥colour = subset¤¤+ geom_hline¥yintercept = 0« colour = gray¥1/2¤« lty = 2¤+ geom_linerange¥aes¥x = var« ymin = fe ¬ se*interval« ymax = fe + se*interval¤«lwd = 1« position = position_dodge¥width = 1/2¤¤+ geom_pointrange¥aes¥x = var« y = fe« ymin = fe ¬ se*interval«ymax = fe + se*interval¤« lwd = 1/2« position = position_dodge¥width = 1/2¤« shape = 21« fill = "WHITE"¤+ theme_bw¥¤+ ylab¥"Difference in Means"¤+ xlab¥" "¤+ scale_x_discrete¥breaks=secbreaks«labels=c¥'Difference: Between Brokers'«'Difference: Between Inducements'¤¤ + guides¥colour=guide_legend¥ncol=2¤¤+ scale_colour_grey¥name="Inducements                              Brokers"¤+theme¥axis©text = element_text¥size=12¤«axis©title = element_text¥size=12¤«legend©text=element_text¥size=10¤«plot©title = element_text¥hjust = 1©25«size=16¤¤+ggtitle¥"Panel A: Russia Survey"¤

####### Panel B

### Brokers
emp_diff<¬lm¥vote_intent~factor¥employer¤« data=subset¥ven«employed==1¤¤
party_diff<¬lm¥vote_intent~factor¥partyactivist¤« data=subset¥ven«employed==1¤¤
leader_diff<¬lm¥vote_intent~factor¥leader¤« data=subset¥ven«employed==1¤¤

### Strategies
gift_diff<¬lm¥vote_intent~factor¥gift¤« data=subset¥ven«employed==1¤¤
threat_diff<¬lm¥vote_intent~factor¥indthreat¤« data=subset¥ven«employed==1¤¤
benign_diff<¬lm¥vote_intent~factor¥benign¤« data=subset¥ven«employed==1¤¤
org_diff<¬lm¥vote_intent~factor¥orgthreat¤« data=subset¥ven«employed==1¤¤

coefficients<¬data©frame¥var=as©character¥¤«fe = as©numeric¥¤«se= as©numeric¥¤« subset=as©character¥¤¤

coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥benign_diff¤$coefficients[«1][2]«se= summary¥benign_diff¤$coefficients[«2][2]« subset="Simple Ask     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥org_diff¤$coefficients[«1][2]«se= summary¥org_diff¤$coefficients[«2][2]« subset="Organizational Threat     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥gift_diff¤$coefficients[«1][2]«se= summary¥gift_diff¤$coefficients[«2][2]« subset="Turnout¬Buying     "¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="strategies"«fe = summary¥threat_diff¤$coefficients[«1][2]«se= summary¥threat_diff¤$coefficients[«2][2]« subset="Individual Threat"¤¤

coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥emp_diff¤$coefficients[«1][2]«se= summary¥emp_diff¤$coefficients[«2][2]« subset="Employer"¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥party_diff¤$coefficients[«1][2]«se= summary¥party_diff¤$coefficients[«2][2]« subset="Party Activist"¤¤
coefficients<¬rbind¥coefficients«data©frame¥var="brokers"«fe = summary¥leader_diff¤$coefficients[«1][2]«se= summary¥leader_diff¤$coefficients[«2][2]« subset="Neighborhood Leader"¤¤

coefficients <¬ within¥coefficients«
                       var <¬ ordered¥var« levels = rev¥sort¥unique¥var¤¤¤¤¤
secbreaks = rev¥unique¥as©character¥coefficients$var¤¤¤

fig2_panelb<¬ggplot¥coefficients« aes¥colour = subset¤¤+ geom_hline¥yintercept = 0« colour = gray¥1/2¤« lty = 2¤+ geom_linerange¥aes¥x = var« ymin = fe ¬ se*interval« ymax = fe + se*interval¤«lwd = 1« position = position_dodge¥width = 1/2¤¤+ geom_pointrange¥aes¥x = var« y = fe« ymin = fe ¬ se*interval«ymax = fe + se*interval¤« lwd = 1/2« position = position_dodge¥width = 1/2¤« shape = 21« fill = "WHITE"¤+ theme_bw¥¤+ ylab¥"Difference in Means"¤+ xlab¥" "¤+ scale_x_discrete¥breaks=secbreaks«labels=c¥'Difference: Between Brokers'«'Difference: Between Inducements'¤¤ +  guides¥colour=guide_legend¥ncol=2¤¤+ scale_colour_grey¥name="Inducements                              Brokers"¤+theme¥axis©text = element_text¥size=12¤«axis©title = element_text¥size=12¤«legend©text=element_text¥size=10¤«plot©title = element_text¥hjust = 1©25«size=16¤¤+ggtitle¥"\nPanel B: Venezuela Survey"¤

###########################################
########     FIGURE 3      ################
###########################################


df <¬ data©frame¥name= numeric¥0¤«coefs= numeric¥0¤«se= numeric¥0¤«value= numeric¥0¤¤

### Leverage

est1<¬felm¥expoutcome~empparty*perclosejob100+ citysize +  male + logage + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"perclosejob100"«
                   summary¥glht¥est1« linfct = c¥"empparty + empparty:perclosejob100 == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est1« linfct = c¥"empparty + empparty:perclosejob100 == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"perclosejob100"«
                   summary¥glht¥est1« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est1« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

est2<¬felm¥expoutcome~empparty*findnewwork+ citysize +  male + logage + edu + gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"findnewwork"«
                   summary¥glht¥est2« linfct = c¥"empparty + empparty:findnewwork*5 == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est2« linfct = c¥"empparty + empparty:findnewwork*5 == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"findnewwork"«
                   summary¥glht¥est2« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est2« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

rus$num_benefits<¬as©numeric¥as©character¥rus$num_benefits¤¤
est3<¬felm¥expoutcome~empparty*num_benefits+ citysize +  male + logage + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"num_benefits"«
                   summary¥glht¥est3«linfct = c¥"empparty + empparty:num_benefits == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est3« linfct = c¥"empparty + empparty:num_benefits == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"num_benefits"«
                   summary¥glht¥est3« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est3« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

est4<¬felm¥expoutcome~empparty*gov+ citysize +  male + logage + edu+gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"gov"«
                   summary¥glht¥est4« linfct = c¥"empparty + empparty:gov == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est4« linfct = c¥"empparty + empparty:gov == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"gov"«
                   summary¥glht¥est4« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est4« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

### Monitoring

est5<¬felm¥expoutcome~empparty*supervisor+ citysize +  male + logage + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤


df<¬rbind¥df«cbind¥"supervisor"«
                   summary¥glht¥est5« linfct = c¥"empparty + empparty:supervisor*3 == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est5« linfct = c¥"empparty + empparty:supervisor*3 == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"supervisor"«
                   summary¥glht¥est5« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est5« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

est6<¬felm¥expoutcome~empparty*lengthwork+ citysize +  male + logage + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"lengthwork"«
                   summary¥glht¥est6« linfct = c¥"empparty + empparty:lengthwork*50 == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est6« linfct = c¥"empparty + empparty:lengthwork*50 == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"lengthwork"«
                   summary¥glht¥est6« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est6« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤


est7<¬felm¥expoutcome~empparty*coworker_weekly+ citysize +  male + logage + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

df<¬rbind¥df«cbind¥"coworker_weekly"«
                   summary¥glht¥est7« linfct = c¥"empparty + empparty:coworker_weekly*2 == 0"¤¤¤$test$coefficients«
                   summary¥glht¥est7« linfct = c¥"empparty + empparty:coworker_weekly*2 == 0"¤¤¤$test$sigma«"1"¤¤

df<¬rbind¥df«cbind¥"coworker_weekly"«
                   summary¥glht¥est7« linfct = c¥"empparty==0"¤¤¤$test$coefficients«
                   summary¥glht¥est7« linfct = c¥"empparty==0"¤¤¤$test$sigma«"0"¤¤

coefficients <¬ within¥coefficients«
                       var <¬ ordered¥var« levels = rev¥sort¥unique¥var¤¤¤¤¤
secbreaks = rev¥unique¥as©character¥coefficients$var¤¤¤

names¥df¤<¬c¥"variable"«"coef"«"se"«"on"¤
df$leverage<¬c¥rep¥1«8¤«rep¥0«6¤¤
df$coef<¬as©numeric¥as©character¥df$coef¤¤
df$se<¬as©numeric¥as©character¥df$se¤¤
df$variable<¬as©character¥df$variable¤
df$variable[df$variable=="perclosejob100"]<¬"     perclosejob100"
df$variable[df$variable=="findnewwork"]<¬"    findnewwork"
df$variable[df$variable=="num_benefits"]<¬"    num_benefits"
df$variable[df$variable=="gov"]<¬"   gov"
df$variable[df$variable=="supervisor"]<¬"  supervisor"
df$variable[df$variable=="lengthwork"]<¬" lengthwork"
df$variable[df$variable=="coworker_weekly"]<¬"coworker_weekly"
df$leverage<¬as©character¥df$leverage¤

df$variablewhite = factor¥df$variable« levels=c¥"Leverage "«"     perclosejob100"«"    findnewwork"« "    num_benefits"«"   gov"«"Monitoring"«"  supervisor"«" lengthwork"«"coworker_weekly"¤¤

df$variablefill<¬paste¥df$variable«df$on«sep="_"¤

fig3<¬ggplot¥df« aes¥colour = variable«fill=variablefill¤¤+ geom_linerange¥aes¥x = variablefill« ymin = coef ¬ se*interval« ymax = coef + se*interval¤«lwd = 1« position = position_dodge¥width = 1/2¤¤+ geom_pointrange¥aes¥x = variablefill« y = coef« ymin = coef ¬ se*interval«ymax = coef + se*interval¤« lwd = 1/2« position = position_dodge¥width = 1/2¤« shape = 21« fill = "WHITE"¤+ geom_hline¥yintercept = 0« colour = gray¥1/2¤« lty = 2¤+ theme_bw¥¤+ scale_x_discrete¥breaks=df$variablefill«labels=c¥'100'«'0'«'Yes'«'No'«'Yes'«'No'«'Yes'«'No'«'Yes'«'No'«'50'«'0'«'Yes'«'No'¤¤+ ylab¥"Effect of Employer Treatment on Turnout\t"¤+ xlab¥"\nLeverage                            Monitoring"¤+
  guides¥colour=guide_legend¥ncol=2¤¤+ scale_colour_grey¥name=""«labels = c¥"Chance of Job Loss"« "Hard to Find a New Job"«"Receives Benefits"«"Employed in Government"«"Knows Supervisor Well"«"Number of Years Employed"«"Socializes with Coworkers"¤¤+theme¥legend©position="bottom"«axis©text = element_text¥size=16¤«axis©title = element_text¥size=16¤«legend©text=element_text¥size=14¤¤





###########################################
########     TABLE 1      ################
###########################################

### Panel A

rus$induce="Asked You to Vote"
rus$induce[rus$gift4==1]="Offers You a Gift« Money« or Reward for Voting"
rus$induce[rus$org4==1]="Tells You That Your Firm or Org© Will Suffer if Turnout Among Employees is Low"
rus$induce[rus$threat4==1]="Indicates There Will be Negative Consequences For You If You Do Not Vote"

rus$brokertable="  Your Employer"
rus$brokertable[rus$broker=="Party"]=" A Party Activist"
rus$brokertable[rus$broker=="Official"]="A Government Official"

RussiaPercTable<¬table¥rus$brokertable«rus$induce¤

### Panel B

ven$induce="Asked You to Vote"
ven$induce[ven$strategy=="Gift"]="Offers You a Gift« Money« or Reward for Voting"
ven$induce[ven$strategy=="OrgThreat"]="Tells You That Your Firm or Org© Will Suffer if Turnout Among Employees is Low"
ven$induce[ven$strategy=="IndThreat"]="Indicates There Will be Negative Consequences For You If You Do Not Vote"

ven$brokertable="  Your Employer"
ven$brokertable[ven$broker=="Party"]=" A Party Activist"
ven$brokertable[ven$broker=="Neighborhood Leader"]="A Neighborhood Leader"

VenezuelaPercTable<¬table¥ven$brokertable«ven$induce¤


###########################################
########     TABLE 2      ################
###########################################


rus_emp$expoutcome_f<¬factor¥rus_emp$expoutcome¤

### Employer
t<¬polr¥expoutcome_f~employer4«data=rus_emp« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
rus_p<¬cbind¥rus_emp«probs¤
rusemployer_high<¬mean¥rus_p$o4[rus_p$employer4==1]¤ + mean¥rus_p$o5[rus_p$employer4==1]¤
rusemployer_low<¬mean¥rus_p$o1[rus_p$employer4==1]¤ + mean¥rus_p$o2[rus_p$employer4==1]¤

### Activist
t<¬polr¥expoutcome_f~activist4«data=rus_emp« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
rus_p<¬cbind¥rus_emp«probs¤
rusactivist_high<¬mean¥rus_p$o4[rus_p$activist4==1]¤ + mean¥rus_p$o5[rus_p$activist4==1]¤
rusactivist_low<¬mean¥rus_p$o1[rus_p$activist4==1]¤ + mean¥rus_p$o2[rus_p$activist4==1]¤


### Official
t<¬polr¥expoutcome_f~official4«data=rus_emp« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
rus_p<¬cbind¥rus_emp«probs¤
rusofficial_high<¬mean¥rus_p$o4[rus_p$official4==1]¤ + mean¥rus_p$o5[rus_p$official4==1]¤
rusofficial_low<¬mean¥rus_p$o1[rus_p$official4==1]¤ + mean¥rus_p$o2[rus_p$official4==1]¤


###### VENEZUELA PROBABILITY TABLE

ven_emp$vote_intent_f<¬factor¥ven_emp$vote_intent¤
ven_f<¬subset¥ven_emp« is©na¥vote_intent_f¤==FALSE¤

### Employer
t<¬polr¥vote_intent_f~employer«data=ven_f« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
ven_p<¬cbind¥ven_f«probs¤
venemployer_high<¬mean¥ven_p$o4[ven_p$employer==1]¤ + mean¥ven_p$o5[ven_p$employer==1]¤
venemployer_low<¬mean¥ven_p$o1[ven_p$employer==1]¤ + mean¥ven_p$o2[ven_p$employer==1]¤

### Activist
t<¬polr¥vote_intent_f~partyactivist«data=ven_f« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
ven_p<¬cbind¥ven_f«probs¤
venactivist_high<¬mean¥ven_p$o4[ven_p$partyactivist==1]¤ + mean¥ven_p$o5[ven_p$partyactivist==1]¤
venactivist_low<¬mean¥ven_p$o1[ven_p$partyactivist==1]¤ + mean¥ven_p$o2[ven_p$partyactivist==1]¤

### Leader
t<¬polr¥vote_intent_f~leader«data=ven_f« method = "logistic"¤
probs<¬as©data©frame¥predict¥t« type="probs"¤¤
names¥probs¤=c¥"o1"«"o2"«"o3"«"o4"«"o5"¤
ven_p<¬cbind¥ven_f«probs¤
venleader_high<¬mean¥ven_p$o4[ven_p$leader==1]¤ + mean¥ven_p$o5[ven_p$leader==1]¤
venleader_low<¬mean¥ven_p$o1[ven_p$leader==1]¤ + mean¥ven_p$o2[ven_p$leader==1]¤


PanelASummaryTable <¬ data©frame¥brokers= numeric¥0¤«rusprob= numeric¥0¤«venprob= numeric¥0¤¤
PanelASummaryTable[1 «] <¬ c¥"Employer"«specify_decimal¥c¥rusemployer_high*100«venemployer_high*100¤«1¤¤
PanelASummaryTable[2 «] <¬ c¥"Party Activist"«specify_decimal¥c¥rusactivist_high*100«venactivist_high*100¤«1¤¤
PanelASummaryTable[3 «] <¬ c¥"Government Official"«specify_decimal¥rusofficial_high*100«1¤«""¤
PanelASummaryTable[4 «] <¬ c¥"Neighborhood Leader"«""«specify_decimal¥venleader_high*100«1¤¤

colnames¥PanelASummaryTable¤ <¬ c¥" "«"Russia"«"Venezuela"¤



PanelBSummaryTable <¬ data©frame¥brokers= numeric¥0¤«rusprob= numeric¥0¤«venprob= numeric¥0¤¤

PanelBSummaryTable[1 «] <¬ c¥"Employer"«specify_decimal¥c¥rusemployer_low*100«venemployer_low*100¤«1¤¤
PanelBSummaryTable[2 «] <¬ c¥"Party Activist"«specify_decimal¥c¥rusactivist_low*100«venactivist_low*100¤«1¤¤
PanelBSummaryTable[3 «] <¬ c¥"Government Official"«specify_decimal¥rusofficial_low*100«1¤«""¤
PanelBSummaryTable[4 «] <¬ c¥"Neighborhood Leader"«""«specify_decimal¥venleader_low*100«1¤¤

colnames¥PanelBSummaryTable¤ <¬ c¥" "«"Russia"«"Venezuela"¤





###########################################
########     TABLE 3      ################
###########################################

### Leverage

est1<¬felm¥expoutcome~empparty*perclosejob100+ citysize +  male + logage + polinterest  + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

est2<¬felm¥expoutcome~empparty*findnewwork+ citysize +  male + logage + polinterest  + edu + gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

est3<¬felm¥expoutcome~empparty*num_benefits+ citysize +  male + logage + polinterest  + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

est4<¬felm¥expoutcome~empparty*gov+ citysize +  male + logage + polinterest  + edu+gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

### Monitoring

est5<¬felm¥expoutcome~empparty*supervisor+ citysize +  male + logage + polinterest  + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

est6<¬felm¥expoutcome~empparty*lengthwork+ citysize +  male + logage + polinterest  + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤

est7<¬felm¥expoutcome~empparty*coworker_weekly+ citysize +  male + logage + polinterest  + edu+ gov|factor¥strategy¤|0|regionid« data=subset¥rus« employed==1¤¤