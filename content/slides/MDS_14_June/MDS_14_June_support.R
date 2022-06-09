
#install.packages("xaringanthemer")
library(xaringanExtra)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(reshape)
library(tikzDevice)
library(plyr)
library(dplyr)
library(gtable)
library(grid)
library(gridExtra)
library(cowplot)
library(caroline)
library(ggpmisc)
library(tidyr)
library(plotly)
library(tidyquant)
library(lfe)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(arsenal)
library(knitr)
library(GGally)
library(grf)

results = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\results.RDS")
player_table = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\Data_amalgamate\\player_table.RDS")

y = c(#"CV_score.demean"
  #"CV_score.std"
  "CV_score"
)
formula_vars.full = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "applicant.Female",
  "Order",
  "endorsements", 
  #"Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service",
  "age", "participant.Female", #"ethnicity",
  #"highest_education", "marital_status","employment_status", 
  "lib_con", 
  #"Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", "TikTok", 
  #"stayintouch", "toomuchtime", "potentialemployers", 
  #"professional", "lostControl", "advertisers", "employers", 
  "workExperienceInc", "health", #"educationInc",   "crime",  "discrimination", "anythingElse",
  "workExperienceIncLength", "healthLength", #"educationIncLength", "crimeLength",  "discriminationLength",   "anythingElseLength",
  #"teams", "communicate", "administration", "customerService", "ITskills", 
  #"experience", "education", "hobbies", 
  "CVcontents","SMcontents",
  #"SM_active", 
  #"SM_approval", "privacy_no_problem",
  "proSM",
  "graduate", 
  "diffloc",
  #"total_time", "median_length_viewed",
  "TimeSpentInstruc", "mean_no_times_viewed", "mean_notelength",  "min_length_viewed"
  #"TrumpShare", "mental_health_dayIndex", "TrumpShare2", 
  #"TrumpShare3", "mental_health_dayIndex2", 
  #"rural_urban_con", "urban_influence", "economy_type", "stateage", 
  #"time_startednum"
)

cbp <- c("#999999", mh="#E69F00", g="#56B4E9", c="#009E73", nd="#F0E442", gl="#0072B2", b="#D55E00", "#CC79A7")
cbpcolours = c("Bad" = unname(cbp["b"]), "No SM" = unname(cbp["nd"]), "Mental Health" = unname(cbp["mh"]), "Gap Lie" = unname(cbp["gl"]),"Gap" = unname(cbp["g"]),"Control" = unname(cbp["c"]), "Full" = "black")
saveforest = T

bin_function = function(column, number=3){ #column=results$mean_notelength
  breaks = quantile(column, na.rm = T, probs = seq(0,1, length.out=number+1)) 
  breaks = unique(breaks)
  #while(breaks[1] == breaks[2]){
  #  breaks = breaks[-1]
  #}
  tags <- vector(length=(length(breaks)-1))
  if((class(breaks) == "POSIXct")[1]){
    breakschar = strftime(breaks, format="%H:%M:%S")
    for(b in 2:length(breaks)){ #b=2
      tags[b-1] = paste0("[", breakschar[b-1], "-", breakschar[b], ")")
    }
  } else {
    for(b in 2:length(breaks)){ #b=2
      #tags[b-1] = paste0(gsub("0.",".", round(breaks[b-1], 2)), "-", gsub("0.",".", round(breaks[b],2)))
      tags[b-1] = paste0("[", round(breaks[b-1], 2), "-", round(breaks[b],2), ")")
      
    }
  }
  #if(strsplit(tags[1], split = "-")[2] %in% c("",".")){
  #  tags[1] = paste0("0", tags[1])
  #}
  group_tags <- cut(column, 
                    breaks=breaks, 
                    include.lowest=TRUE, 
                    right=FALSE, 
                    labels=tags)
  out <- factor(group_tags, 
                levels = tags,
                ordered = TRUE)
  return(out)
}

#var="notelength";  y.label = "Note Length"; y.positions = c(140, 145,150); tip.compar = 0.0005
bar.by.treat = function(var="notelength",  y.label = "Note Length", y.positions = c(140, 145,150), tip.compar = 0.0005){
  formula = as.formula(paste(var, "~", "treatment"))
  control.level = max(aggregate(formula, results, mean)[,2])*1.1
  stat.test <- compare_means(formula, data = results, method = "wilcox.test", ref.group = "control",
                             symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                                symbols = c("****", "***", "**", "*", "ns")))
  stat.test$y.position = c(control.level,
                           control.level,
                           control.level,
                           control.level,
                           control.level)  #y.positions
  stat.test$p.signif = gsub("ns", "", stat.test$p.signif)
  #stat.test$p.signif <- paste0(stat.test$p.signif, "*")
  #stat.test <- stat.test[c(1,4),]
  p1 = ggbarplot(results,
                 x = "treatment", 
                 y = var, 
                 fill= "treatment",
                 add = "mean_ci",
                 add.params = list(group ="treatment"),
                 alpha = 0.5
  )+
    theme_minimal() +
    stat_pvalue_manual(data = stat.test, 
                       label = "p.signif",
                       remove.bracket = T,
                       #  hide.ns = T,
                       tip.length = tip.compar
    ) +
    #lims(y=c(0,200)) +
    scale_fill_manual(values = c("control" = unname(cbp["c"]),
                                 "gap" = unname(cbp["g"]),
                                 "gap_lie" = unname(cbp["gl"]),
                                 "mental_health" = unname(cbp["mh"]),
                                 "no_data" = unname(cbp["nd"]),
                                 "bad" = unname(cbp["b"]))) +
    labs(fill="", y= y.label, x="") +
    theme(text = element_text(size=20),
          axis.text.x=element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank())
  p1 = p1 + coord_cartesian(ylim=c(0,(control.level*1.2)))
  return(p1)
}
seq(0,10,length.out=11)

#var=y; y.label = "CV score"; by="treatment"; y.positions=NA;  facet="experience_years"; refgroup="control"; x.label=""
#var=y; y.label = "CV score"; by="workExperienceInc"; y.positions=NA;  df=subset(results); facet="treatment"; refgroup = "0"; x.label=""
#var=y; y.label = "CV score"; x.label = "lib_con"; by="lib_con"; y.positions=NA;  df=subset(results, mental_health|control); facet="treatment"; refgroup = "1"

boxplot = function(var=y, by="experience_years", df=results, facet=NA, refgroup=".all.", y.label = "CV score", x.label="", y.positions=NA, include.count=T, paired.=T, tip.compar = 0.0005){
  df[[by]] = factor(df[[by]], ordered=F)
  formula = as.formula(paste(var, "~", by))
  control.level = max(aggregate(formula, df, FUN = function(x){min(quantile(x, .75) + 0*IQR(x), max(x))})[,2])*1.1
  if(is.na(facet)){groupvar = NULL} else {groupvar = facet}
  stat.test = compare_means(formula, data = df, method = "wilcox.test", paired = paired., ref.group = refgroup, group.by = groupvar,
                             symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                                symbols = c("****", "***", "**", "*", "ns")))
  if(is.na(y.positions)){stat.test$y.position = rep(control.level, nrow(stat.test))} else {stat.test$y.position = y.positions}
  stat.test$p.signif = gsub("ns", "", stat.test$p.signif)
  #stat.test$p.signif <- paste0(stat.test$p.signif, "*")
  #stat.test <- stat.test[c(1,4),]
  p1 = ggplot(df, aes_string(x=by, y=var, fill="treatment", group=by)) + 
    geom_boxplot(outlier.shape = NA, coef = 0, alpha = 0.5) +
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0) +
    theme_minimal()
  if(var=="CV_score") p1 = p1 + scale_y_continuous(breaks=seq(min(df[[var]]), max(df[[var]]), length.out=11))
  p1 = p1 + stat_pvalue_manual(data = stat.test, 
                               inherit.aes = FALSE,
                               label = "p.signif",
                               position = "identity",
                               remove.bracket = T,
                               #  hide.ns = T,
                               tip.length = tip.compar) # + lims(y=c(0,200)) 
  if(!is.na(facet)){p1 = p1 + facet_grid(as.formula(paste(".~", facet)), switch="both")}
  p1 = p1 + coord_cartesian(ylim=c(0,(control.level)))
  if("treatment" %in% c(by, facet)){
    p1 = p1 + scale_fill_manual(values = c("control" = unname(cbp["c"]),
                                           "gap" = unname(cbp["g"]),
                                           "gap_lie" = unname(cbp["gl"]),
                                           "mental_health" = unname(cbp["mh"]),
                                           "no_data" = unname(cbp["nd"]),
                                           "bad" = unname(cbp["b"]))) +
      theme(legend.position = "top",
            text = element_text(size=20),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank()) + labs(fill="", y=y.label, x=x.label)
    if("treatment" %in% c(by)){p1 = p1 + theme(axis.text.x=element_blank())} else {p1 = p1 + theme(strip.text = element_blank())}
  } else {
    p1 = p1 + scale_fill_manual(values=c(rep(unname(cbp["a"]), length(unique(df[[by]]))))) +  
      theme(legend.position = "none",
            text = element_text(size=20),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank()) + labs(fill=NULL, y=y.label, x=x.label) 
  }
  if(include.count){
    p1 = p1 + theme(strip.text = element_blank(), axis.text.x=element_blank()) + labs(fill=NULL, y=y.label, x=NULL)
    p2 = ggplot(data = df, aes_string(x=by)) +
      geom_bar(alpha = 0.5) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
    if(!is.na(facet)){p2 = p2 + facet_grid(as.formula(paste(".~", facet)), switch="both")}
    if("treatment" %in% c(by, facet)){
      p2 = p2 + theme(legend.position = "top",
                      text = element_text(size=20),
                      axis.ticks.x = element_blank(),
                      panel.grid.major.x = element_blank()) + labs(fill="", y="count", x=x.label)
      if("treatment" %in% c(by)){p2 = p2 + theme(axis.text.x=element_blank())} else {p2 = p2 + theme(strip.text = element_blank())}
    } else {
      p2 = p2 + scale_fill_manual(values=c(rep(unname(cbp["a"]), length(unique(df[[by]]))))) +  
        theme(legend.position = "none",
              text = element_text(size=20),
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_blank()) + labs(fill="", y="count", x=x.label) 
    }
    
    plot = cowplot::plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(4/4, 1/4))
  } else {
    plot = p1
  }
  
  return(plot)
}
##### lines of best fit
#variable=y;  by="SMcontents"; group="treatment"; df=results; include.count=T; x.label="Contents of the candidate's social media accounts (are important)"; leg.label = "Treatment"; cut.outliers=F
score.means.by.var.line2 <- function(variable, by, group, df, include.count=T, x.label="", leg.label="", cut.outliers=T){
  if(cut.outliers){df = df[!df[[by]] %in% boxplot.stats(df[[by]])$out,]}
  df[[by]] = as.numeric(df[[by]])
  k = length(unique(df[[by]]))
  if(k<10){formula = as.formula(paste0('y ~ s(x, bs = "cs", k=',k,')'))} else {formula = as.formula('y ~ s(x, bs = "cs")')}
  p1 = ggplot(df , aes_string(x=by, y=variable, group=group, colour=group)) + 
    geom_point(position=position_jitter(height=0), alpha=0.025) +
    #geom_ma(ma_fun = SMA, n = 20, linetype = 1, size = 1, na.rm = TRUE) +
    geom_smooth(method="gam", formula=formula, alpha = 0.5) +  # method=lm loess method=lm
    #lims(x=c(0,10)) +
    #geom_hline(yintercept = 0)+
    #facet_grid(treatment~.) +
    labs(y=variable, x=NULL) +
    theme_classic()
  p1 = p1 +  geom_hline(yintercept = mean(df[[variable]], na.rm=T)) + labs(colour = leg.label)
  if(group=="treatment"){
    p1 = p1 + scale_colour_manual(values = c("control" = unname(cbp["c"]),
                                             "gap" = unname(cbp["g"]),
                                             "gap_lie" = unname(cbp["gl"]),
                                             "mental_health" = unname(cbp["mh"]),
                                             "no_data" = unname(cbp["nd"]),
                                             "bad" = unname(cbp["b"])))
  }
  if(include.count){
    p1 = p1 + theme_minimal() +
      theme(#legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
    p2 = ggplot(data = df, aes_string(x=by)) +
      geom_bar(alpha = 0.5) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
    p2 = p2 + labs(x = x.label, colour=leg.label)
    plot = cowplot::plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(4/4, 1/4))
  } else {
    p1 = p1 + theme_minimal() +
      theme(#legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
    plot = p1
  }
  return(plot)
}



pl = c(#"(Intercept)" = "Intercept", 
  "experience_years" = "CV Experience$_i$", 
  "skills_1.2" = "CV Skills$_i$", 
  "interests_1.2" = "CV Interests$_i$", 
  "education_yt" = "CV Education$_i$", 
  "applicant.FemaleTRUE" = "Applic. Female$_i$", 
  "Order" = "Order$_i$",
  "endorsements" = "Endorsements$_i$",
  "gapTRUE" = "Gap$_i$",
  "gap_lieTRUE"= "Lie about Gap$_i$"     ,
  "mental_healthTRUE"= "Mental Health$_i$" ,
  "no_dataTRUE"= "No SM$_i$"      ,
  "badTRUE" = "Bad SM$_i$",
  "age" = "Age$_j$", 
  "participant.FemaleTRUE" = "Eval. Female$_j$", 
  #"ethnicity" = "Eval. Ethnicity,
  #"highest_education2" = "Eval. Education$_j$", 
  #"marital_status2" = "Eval. Marit. Stat.$_j$",
  #"employment_status2" = "Eval.  Employ. Stat.$_j$", 
  "lib_con" = "Eval. 'Conservative'$_j$", 
  "Twitter"="Twitter", 
  "Instagram"="Instagram", 
  "Facebook"="Facebook", 
  "Snapchat"="Snapchat", 
  "LinkedIn"="LinkedIn",
  "TikTok"="TikTok", 
  "stayintouch", 
  "toomuchtime", 
  "potentialemployers", 
  "professional", 
  "lostControl", 
  "advertisers", 
  "employers", 
  "workExperienceInc" = "Notic. Exper. Inc.$_j$", 
  "educationInc"= "Notic. Educ. Inc.$_j$", 
  "health" = "Notic. Health Prob.$_j$",  
  "crime" = "Notic. Crime$_j$",  
  "discrimination" = "Notic. Discrim.$_j$", 
  "anythingElse" = "Notic. Anything Else$_j$",
  "workExperienceIncLength" = "Exper. Inc. Notelength$_j$", 
  "educationIncLength"= "Educ. Inc. Notelength$_j$", 
  "healthLength"= "Health Notelength$_j$",  
  "crimeLength"= "Crime Notelength$_j$",  
  "discriminationLength"= "Discrim. Notelength$_j$", 
  "anythingElseLength"= "Anything Else Notelength$_j$",
  "teams" = "Teamwork Imp.$_j$", 
  "communicate"= "Communi. Imp.$_j$", 
  "administration"= "Admin. Imp.$_j$", 
  "customerService"= "Cust. Serv. Imp.$_j$", 
  "ITskills"= "IT Skills Imp.$_j$", 
  "experience"= "Exper. Imp.$_j$", 
  "education"= "Educ. Imp.$_j$", 
  "hobbies"= "Hobbies Imp.$_j$", 
  "CVcontents"= "CV Contents Imp.$_j$",
  "SMcontents"= "SM Contents Imp.$_j$",
  "SM_active" = "Active on SM$_j$", 
  "SM_approval" = "SM Approval$_j$", 
  "privacy_no_problem" = "Privacy$_j$",
  "SM_approval2" = "SM Approval$_j$",
  "proSM" = "Pro SM$_j$",
  #"graduate", 
  "graduateTRUE" = "Graduate$_j$",
  "TimeSpentInstruc" = "Time on Instruc.$_j$", 
  "mean_no_times_viewed" = "CV Views$_j$", 
  "mean_notelength" = "Av Notelength$_j$",  
  "min_length_viewed" = "Min. Time Viewed$_j$",
  #"TrumpShare", "mental_health_dayIndex", "TrumpShare2", 
  #"TrumpShare3", "mental_health_dayIndex2", 
  #"rural_urban_con", "urban_influence", "economy_type", "stateage", 
  #"time_startednum"
  "treatedTRUE" = "ATE"
)
addvarnames = pl
names(addvarnames) = paste(names(pl), ":treatedTRUE", sep="")
pl = c(pl, addvarnames)
pl2 = pl %>% gsub("\\$_i\\$", "", .) %>% gsub("\\$_j\\$", "", .)
listIDs = subset(player_table, !duplicated(newID))$mturkID 


colour.scale = list(scale_fill_manual(values = c("control" = unname(cbp["c"]),
                                                 "gap" = unname(cbp["g"]),
                                                 "gap_lie" = unname(cbp["gl"]),
                                                 "mental_health" = unname(cbp["mh"]),
                                                 "no_data" = unname(cbp["nd"]),
                                                 "bad" = unname(cbp["b"]))))



length_viewed_hist = gghistogram(data=subset(results, finished), x="length_viewed", bins = 70, title = "Time per Application", xlab = "Minutes") +
  xlim(0,10) 

notelegnth_hist = gghistogram(data=subset(results, finished), x="notelength", bins = 70, title = "Note Length", xlab = "Characters") +
  xlim(0.5,400)  
saveRDS(length_viewed_hist, file = "exhibits\\length_viewed_hist.RDS")
saveRDS(notelegnth_hist, file = "exhibits\\notelegnth_hist.RDS")

gghistogram(data=subset(results, finished), x="CV_score", bins = 11, title = "", xlab = "", fill = "treatment") +
  facet_grid(rows = vars(treatment)) + colour.scale

exp_boxplot = boxplot(var=y, y.label = "CV score", x.label = "Experience", by="experience_years", df=subset(results), y.positions=NA,  facet=NA, refgroup = "4", include.count = F) 
educ_boxplot = boxplot(var=y, y.label = "CV score", x.label = "Education", by="education_yt", df=subset(results), y.positions=NA,  facet=NA, refgroup = "1", include.count = F)
saveRDS(exp_boxplot, file = "exhibits\\exp_boxplot.RDS")
saveRDS(educ_boxplot, file = "exhibits\\educ_boxplot.RDS")

notelength_tr_boxplot = boxplot(var="notelength",  y.label = "Note Length Characters", by="treatment", df=subset(results), y.positions=NA,  facet=NA, refgroup = "control", include.count = F)
lengthViewed_tr_boxplot = boxplot(var="length_viewed",  y.label = "Minutes on CV", by="treatment", df=subset(results), y.positions=NA,  facet=NA, refgroup = "control", include.count = F)
noTimesViewed_tr_boxplot = boxplot(var="no_times_viewed",  y.label = "Times Clicked on CV", by="treatment", df=subset(results), y.positions=NA,  facet=NA, refgroup = "control", include.count = F)
saveRDS(notelength_tr_boxplot, file = "exhibits\\notelength_tr_boxplot.RDS")
saveRDS(lengthViewed_tr_boxplot, file = "exhibits\\lengthViewed_tr_boxplot.RDS")
saveRDS(noTimesViewed_tr_boxplot, file = "exhibits\\noTimesViewed_tr_boxplot.RDS")

legend = get_legend(p1 + theme(legend.position = "top"))
plots = cowplot::plot_grid(
  p1 + theme(legend.position = "none"),  
  p2 + theme(legend.position = "none"), 
  #  p3 + theme(legend.position = "none"),
  nrow = 1)

cowplot::plot_grid(legend, plots, ncol = 1, rel_heights = c(0.2, 2))


tr_boxplot = boxplot(var=y,  y.label  = "CV score", by="treatment", df=subset(results), y.positions=NA,  facet=NA, refgroup = "control", include.count = F)
saveRDS(tr_boxplot, file = "exhibits\\tr_boxplot.RDS")

fitmod_cf.multi.arm = readRDS("C:\\Users\\fo06mafa\\Documents\\AuthenticApplications\\forests\\fitmod_cf.multi.arm.RDS")
df = fitmod_cf.multi.arm$predictions %>% as.data.frame() 
colnames(df) <- c("gap", "gap_lie", "mental_health", "no_data", "bad") 
df = df %>% gather(treatment, CATE)
df$treatment = factor(df$treatment, levels = c("gap", "gap_lie", "mental_health", "no_data", "bad"))

CATE_hist = gghistogram(data=df, x="CATE", fill="treatment", bins=100) +facet_grid(rows = vars(treatment)) + colour.scale
saveRDS(CATE_hist, file = "exhibits\\CATE_hist.RDS")

exp_tr_boxplot =  boxplot(var=y,  y.label = "CV score", by="treatment", df=subset(results), y.positions=NA,  facet="experience_years", refgroup = "control", include.count = F)
saveRDS(exp_tr_boxplot, file = "exhibits\\exp_tr_boxplot.RDS")
exp_tr_boxplot2 =  boxplot(var=y,  y.label = "CV score", by="treatment", df=subset(results, no_data|control), y.positions=NA,  facet="experience_years", refgroup = "control", include.count = F)
saveRDS(exp_tr_boxplot2, file = "exhibits\\exp_tr_boxplot2.RDS")
exp_tr_boxplot3 =  boxplot(var=y,  y.label = "CV score", by="treatment", df=subset(results, no_data|control|bad), y.positions=NA,  facet="experience_years", refgroup = "control", include.count = F)
saveRDS(exp_tr_boxplot3, file = "exhibits\\exp_tr_boxplot3.RDS")
#exp_tr_boxplot3 =  score.means.by.var.line2(variable=y, by="experience_years", group="treatment", df=subset(results, no_data|control|bad))

educ_tr_boxplot = boxplot(var=y,  y.label = "CV score", by="treatment", df=subset(results), y.positions=NA,  facet="education_yt", refgroup = "control", include.count = F)
saveRDS(educ_tr_boxplot, file = "exhibits\\educ_tr_boxplot.RDS")

#v=formula_vars[1]; LM_tab = results #v="normalvar"

#LM_tab$normalvar = rnorm(n=nrow(LM_tab))
#gghistogram(data=LM_tab, x="normalvar")

scaletab = function(LM_tab, formula_vars, scale_ind = F, scale = T, index){
  if(scale){
    if(scale_ind){
      LM_tab = subset(LM_tab, index)
    }
    for(v in formula_vars){
      if(is.numeric(LM_tab[[v]])){
        #LM_tab[[v]] = scale(LM_tab[[v]])[,1]
        #LM_tab[[v]] = arm::standardize(LM_tab[[v]])[,1]
        #LM_tab[[v]] = (LM_tab[[v]] - mean(LM_tab[[v]])) / (2*sd(LM_tab[[v]]))
        #LM_tab[[v]] = (LM_tab[[v]] - mean(LM_tab[[v]])) / (max(LM_tab[[v]]) - min(LM_tab[[v]]))
        LM_tab[[v]] = (LM_tab[[v]] - mean(LM_tab[[v]])) / (quantile(LM_tab[[v]], 0.975) - quantile(LM_tab[[v]], 0.025)) 
      } else if(is.factor(LM_tab[[v]])){
        LM_tab[[v]] = factor(LM_tab[[v]], ordered = F)
      }
    }
    if(!scale_ind){
      LM_tab = subset(LM_tab, index)
    }
  }
  if(!scale){
    LM_tab = subset(LM_tab, index)
  }
  return(LM_tab)
}


############################ LINEAR MODELS ############################

formula_vars = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "endorsements",
  "Order",
  "applicant.Female",
  "gap",
  "gap_lie",
  "mental_health",
  "no_data",
  "bad"
)
cluster_on = "mturkID" #"state" #
for_table<-gsub("_"," ", formula_vars)

formula =  paste(formula_vars, collapse = "+") %>%  paste("|0|0|", cluster_on) %>% paste(y,"~", ., sep="") %>% as.formula()

LM_tab = scaletab(results, formula_vars = formula_vars, index=rep(T,nrow(results)))
fitmod.all = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$control)
fitmod.control = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("gap"))
fitmod.gap = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$gap_lie)
fitmod.gap_lie= felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$mental_health)
fitmod.mental_health = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$bad)
fitmod.bad = felm(keepX=TRUE, formula = formula, data = LM_tab)

#formula_vars[-which(formula_vars=="applicant.Female")]
LM_tab = scaletab(results, formula_vars = formula_vars, index=results$no_data)
fitmod.no_data = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = results
fitmodp2 = felm(keepX=TRUE, formula = formula, data = LM_tab)

modellist = factor(c("Full", "Control", "Gap", "Gap Lie", "Mental Health", "No SM", "Bad"))
linearModel_tr_tab1 = tab_model(fitmod.all, fitmod.control, fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                title="Linear Models by Treatment Group",
                                dv.labels = modellist, 
                                pred.labels = pl2, 
                                show.ci = F, show.p = F, show.se=T, show.aic = T, show.intercept = F,
                                string.pred = " ", string.est = gsub("_", " ", y),
                                p.style = "stars", collapse.se = T)

rmvars = c()
linearModel_tr_plot1 = plot_models(fitmod.all, fitmod.control, fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                   m.labels = modellist, 
                                   rm.terms = rmvars,
                                   axis.labels = pl2,
                                   dot.size = 2,
                                   colors = cbpcolours) +
  guides(color = guide_legend(reverse = F)) +
  geom_hline(yintercept=0, colour = "red", linetype="dashed") +
  labs(title="Linear Models by Treatment Group", x=NULL, y=NULL) +
  theme_minimal() +
  theme(#text = element_text(size=25),
    #axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position="bottom",
    panel.grid.minor.x = element_blank()
  )

saveRDS(linearModel_tr_tab1, file = "exhibits\\linearModel_tr_tab1.RDS")
saveRDS(linearModel_tr_plot1, file = "exhibits\\linearModel_tr_plot1.RDS")


#### TAKE OUT THE OVERALL EFFECT TO FOCUS ON THE INTERACTION ####

formula =  paste(formula_vars[1:7], collapse = "+") %>%  paste("+treated+treated:", paste(formula_vars[1:7], collapse = "+treated:"), sep="") %>%
  paste("|0|0|", cluster_on) %>% paste(y,"~", ., sep="") %>% as.formula()

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("gap", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "gap")] = "treated"
fitmod.gap = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("gap_lie", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "gap_lie")] = "treated"
fitmod.gap_lie= felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("mental_health", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "mental_health")] = "treated"
fitmod.mental_health = felm(keepX=TRUE, formula = formula, data = LM_tab)

#formula_vars[-which(formula_vars=="applicant.Female")]
LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("no_data", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "no_data")] = "treated"
fitmod.no_data = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars, index=results$treatment %in% c("bad", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "bad")] = "treated"
fitmod.bad = felm(keepX=TRUE, formula = formula, data = LM_tab)

modellist = factor(c("Gap", "Gap Lie", "Mental Health", "No SM", "Bad"))
rmvars = c("experience_years", "education_yt", "skills_1.2", 
           "interests_1.2", "applicant.FemaleTRUE", "Order", "endorsements")
linearModel_tr_tab2 = tab_model(fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                title="Interactions with Treatment Group",
                                dv.labels = modellist,
                                rm.terms = rmvars,
                                pred.labels = pl2, 
                                show.ci = F, show.p = F, show.se=T, show.aic = T, show.intercept = F,
                                string.pred = " ", string.est = gsub("_", " ", y),
                                p.style = "stars", collapse.se = T)

rmvars = c("experience_years", "education_yt", "skills_1.2", 
           "interests_1.2", "applicant.FemaleTRUE", "Order", "endorsements", "treatedTRUE")
linearModel_tr_plot2 = plot_models(fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                   m.labels = modellist, 
                                   rm.terms = rmvars,
                                   axis.labels = pl2,
                                   dot.size = 2,
                                   colors = cbpcolours) +
  guides(color = guide_legend(reverse = F)) +
  geom_hline(yintercept=0, colour = "red", linetype="dashed") +
  labs(title="Linear Models by Treatment Group", x=NULL, y=NULL) +
  theme_minimal() +
  theme(#text = element_text(size=25),
    #axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position="bottom",
    panel.grid.minor.x = element_blank()
  )


saveRDS(linearModel_tr_tab2, file = "exhibits\\linearModel_tr_tab2.RDS")
saveRDS(linearModel_tr_plot2, file = "exhibits\\linearModel_tr_plot2.RDS")


#### EXTEND TO PARTICIPANT VARIABLES ####

results$SM_approval2 = results$SM_approval + results$privacy_no_problem
formula_vars.full = c(
  "experience_years",
  "education_yt" ,
  "skills_1.2", 
  "interests_1.2",
  "applicant.Female",
  "Order",
  "endorsements",
  #"Teamwork", "Time.Management", "Microsoft.Office", "Administration", "Customer.Service",
  "age", "participant.Female", #"ethnicity",
  "graduate",
  # "highest_education", "employment_status","marital_status",
  #"Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn", "TikTok",
  #"stayintouch", 
  #"toomuchtime", 
  #"potentialemployers", 
  #"professional", 
  #"lostControl", 
  #"advertisers", 
  #"employers", 
  "workExperienceInc", "health", #"educationInc",   "crime",  "discrimination", "anythingElse",
  # "workExperienceIncLength", "healthLength", #"educationIncLength", "crimeLength",  "discriminationLength",   "anythingElseLength",
  #"teams", "communicate",  "administration", "customerService", "ITskills",  "experience", "education", 
  "lib_con",
  "SM_active",
  "SM_approval2",
  "hobbies",
  #"SM_approval","privacy_no_problem",
  #"proSM",
  #"total_time", "median_length_viewed",
  # "TimeSpentInstruc", "mean_no_times_viewed", "mean_notelength",  "min_length_viewed"
  #"TrumpShare", "mental_health_dayIndex", "TrumpShare2", 
  #"TrumpShare3", 
  #"mental_health_dayIndex2", 
  #"rural_urban_con", "urban_influence", "economy_type", "stateage", 
  #"time_startednum",
  "CVcontents","SMcontents"
)


cluster_on = "mturkID" #"state" #
formula =  paste(formula_vars.full, collapse = "+") %>%  paste("+treated+treated:", paste(formula_vars.full, collapse = "+treated:"), sep="") %>%
  paste("|0|0|", cluster_on) %>% paste(y,"~", ., sep="") %>% as.formula()

LM_tab = scaletab(results, formula_vars = formula_vars.full, index=results$treatment %in% c("gap", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "gap")] = "treated"
fitmod.gap = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars.full, index=results$treatment %in% c("gap_lie", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "gap_lie")] = "treated"
fitmod.gap_lie= felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars.full, index=results$treatment %in% c("mental_health", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "mental_health")] = "treated"
fitmod.mental_health = felm(keepX=TRUE, formula = formula, data = LM_tab)

#formula_vars.full[-which(formula_vars.full=="applicant.Female")]
LM_tab = scaletab(results, formula_vars = formula_vars.full, index=results$treatment %in% c("no_data", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "no_data")] = "treated"
fitmod.no_data = felm(keepX=TRUE, formula = formula, data = LM_tab)

LM_tab = scaletab(results, formula_vars = formula_vars.full, index=results$treatment %in% c("bad", "control"))
colnames(LM_tab)[which(colnames(LM_tab) == "bad")] = "treated"
fitmod.bad = felm(keepX=TRUE, formula = formula, data = LM_tab)


modellist = factor(c("Gap", "Gap Lie", "Mental Health", "No SM", "Bad"))
getcoefnames = names(fitmod.gap$coefficients[,1]) %>% grepl("highest_education|marital_status|employment_status", .)
getcoefnames = names(fitmod.gap$coefficients[,1])[getcoefnames]
rmvars = c(getcoefnames, "experience_years", "education_yt", "skills_1.2", 
           "interests_1.2", "applicant.FemaleTRUE", "Order", "endorsements") %>% c(., paste(., ":treatedTRUE", sep=""))
rmvars = c(formula_vars.full, rmvars, "graduateTRUE", "participant.FemaleTRUE")
linearModel_tr_tab3 = tab_model(fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                title="Linear Model Interactions",
                                dv.labels = modellist, 
                                pred.labels = pl2, 
                                rm.terms = rmvars,
                                show.ci = F, show.p = F, show.se=T, show.aic = T, show.intercept = F,
                                string.pred = " ", string.est = gsub("_", " ", y),
                                p.style = "stars", collapse.se = T)

rmvars = c(rmvars, "treatedTRUE")
linearModel_tr_plot3 = plot_models(fitmod.gap, fitmod.gap_lie, fitmod.mental_health, fitmod.no_data, fitmod.bad,
                                   m.labels = modellist, 
                                   rm.terms = rmvars,
                                   axis.labels = pl2,
                                   dot.size = 2,
                                   colors = cbpcolours) +
  guides(group = guide_legend(reverse = T)) +
  geom_hline(yintercept=0, colour = "red", linetype="dashed") +
  labs(title="Regress CV Score on X - Interactions", x=NULL, y=NULL) +
  theme_minimal() +
  theme(#text = element_text(size=25),
    #axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position="bottom",
    panel.grid.minor.x = element_blank()
  )
linearModel_tr_plot3

saveRDS(linearModel_tr_tab3, file = "exhibits\\linearModel_tr_tab3.RDS")
saveRDS(linearModel_tr_plot3, file = "exhibits\\linearModel_tr_plot3.RDS")


#### INDIVIDUAL FORESTS ####

fitmod_cf.gap = readRDS(file = "forests\\fitmod_cf.gap.RDS")
fitmod_cf.gap_lie = readRDS(file = "forests\\fitmod_cf.gap_lie.RDS")
fitmod_cf.mental_health = readRDS(file = "forests\\fitmod_cf.mental_health.RDS")
fitmod_cf.no_data = readRDS(file = "forests\\fitmod_cf.no_data.RDS")
fitmod_cf.bad = readRDS(file = "forests\\fitmod_cf.bad.RDS")
cf_table = results
cf_table$treated = TRUE

formula =  paste(formula_vars.full, collapse = "+") %>% paste(y,"~", ., sep="") %>% as.formula()
Adf = scaletab(cf_table, formula_vars = formula_vars.full, index=cf_table$control|cf_table$gap)%>% model.matrix(formula, data=.) 
best_linear_projection(fitmod_cf.gap[[1]], A=Adf) 
fitmod.blp.gap = best_linear_projection2(fitmod_cf.gap[[1]], A=Adf)
if(saveforest) saveRDS(fitmod.blp.gap, file = "forests\\fitmod.blp.gap.RDS")
summary(fitmod.blp.gap)
plot_model(fitmod.blp.gap)

Adf = scaletab(cf_table, formula_vars = formula_vars.full, index=cf_table$control|cf_table$gap_lie)%>% model.matrix(formula, data=.) 
best_linear_projection(fitmod_cf.gap_lie[[1]], A=Adf) 
fitmod.blp.gap_lie = best_linear_projection2(fitmod_cf.gap_lie[[1]], A=Adf)
if(saveforest) saveRDS(fitmod.blp.gap_lie, file = "forests\\fitmod_cf.gap_lie.blp.RDS")
summary(fitmod.blp.gap_lie)
plot_model(fitmod.blp.gap_lie)

Adf = scaletab(cf_table, formula_vars = formula_vars.full, index=cf_table$control|cf_table$mental_health)%>% model.matrix(formula, data=.)
best_linear_projection(fitmod_cf.mental_health[[1]], A=Adf) 
fitmod.blp.mental_health = best_linear_projection2(fitmod_cf.mental_health[[1]], A=Adf)
if(saveforest) saveRDS(fitmod.blp.mental_health, file = "forests\\fitmod.blp.mental_health.RDS")
summary(fitmod.blp.mental_health)
plot_model(fitmod.blp.mental_health)

Adf = scaletab(cf_table, formula_vars = formula_vars.full, index=cf_table$control|cf_table$no_data)%>% model.matrix(formula, data=.)
best_linear_projection(fitmod_cf.no_data[[1]], A=Adf) 
fitmod.blp.no_data = best_linear_projection2(fitmod_cf.no_data[[1]], A=Adf)
if(saveforest) saveRDS(fitmod.blp.no_data, file = "forests\\fitmod.blp.no_data.RDS")
summary(fitmod.blp.no_data)
plot_model(fitmod.blp.no_data)

Adf = scaletab(cf_table, formula_vars = formula_vars.full, index=cf_table$control|cf_table$bad)%>% model.matrix(formula, data=.)
best_linear_projection(fitmod_cf.bad[[1]], A=Adf) 
fitmod.blp.bad = best_linear_projection2(fitmod_cf.bad[[1]], A=Adf)
if(saveforest) saveRDS(fitmod.blp.bad, file = "forests\\fitmod.blp.bad.RDS")
summary(fitmod.blp.bad)
plot_model(fitmod.blp.bad)



modellist = factor(c("Gap", "Gap Lie", "Mental Health", "No SM", "Bad"))
cbpcolours = c("Bad" = unname(cbp["b"]), "No SM" = unname(cbp["nd"]), "Mental Health" = unname(cbp["mh"]), "Gap Lie" = unname(cbp["gl"]),"Gap" = unname(cbp["g"]),"Full" = "black","Control" = unname(cbp["c"]))
getcoefnames = names(fitmod.blp.gap$coefficients[,1]) %>% grepl("highest_education|marital_status|employment_status", .)
getcoefnames = names(fitmod.blp.gap$coefficients[,1])[getcoefnames]
rmvars = c(getcoefnames, "experience_years", "education_yt", "skills_1.2", 
           "interests_1.2", "applicant.FemaleTRUE", "Order", "endorsements")
BLP_tr_tab = tab_model(fitmod.blp.gap, fitmod.blp.gap_lie, fitmod.blp.mental_health, fitmod.blp.no_data, fitmod.blp.bad,
                       title="Regress CATE on X",
                       dv.labels = modellist, 
                       pred.labels = pl2, 
                       rm.terms = rmvars,
                       show.ci = F, show.p = F, show.se=T, show.aic = T, show.intercept = F,
                       string.pred = " ", string.est = gsub("_", " ", y),
                       p.style = "stars", collapse.se = T)

BLP_tr_plot = plot_models(fitmod.blp.gap, fitmod.blp.gap_lie, fitmod.blp.mental_health, fitmod.blp.no_data, fitmod.blp.bad,
                          m.labels = modellist, 
                          rm.terms = rmvars,
                          axis.labels = pl2,
                          dot.size = 2,
                          colors = cbpcolours) +
  guides(group = guide_legend(reverse = T)) +
  geom_hline(yintercept=0, colour = "red", linetype="dashed") +
  labs(title="Regress CATE on X", x=NULL, y=NULL) +
  theme_minimal() +
  theme(#text = element_text(size=25),
    #axis.text.x=element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position="bottom",
    panel.grid.minor.x = element_blank()
  )


saveRDS(BLP_tr_tab, file = "exhibits\\BLP_tr_tab.RDS")
saveRDS(BLP_tr_plot, file = "exhibits\\BLP_tr_plot.RDS")


getcoefnames = names(fitmod.blp.mental_health$coefficients[,1]) %>% grepl("highest_education|marital_status|employment_status", .)
getcoefnames = names(fitmod.blp.mental_health$coefficients[,1])[getcoefnames]
getcoefnames2 = names(fitmod.mental_health$coefficients[,1]) %>% grepl("highest_education|marital_status|employment_status", .)
getcoefnames2 = names(fitmod.mental_health$coefficients[,1])[getcoefnames2]
rmvars = c(getcoefnames,getcoefnames2, "experience_years", "education_yt", "skills_1.2", 
           "interests_1.2", "applicant.FemaleTRUE", "Order", "endorsements")
plotmod.settings = list(geom_hline(yintercept=0, colour = "red", linetype="dashed"),
                        theme_minimal(),
                        theme(#text = element_text(size=25),
                          #axis.text.x=element_blank(),
                          #axis.ticks.x = element_blank(),
                          legend.position="bottom",
                          panel.grid.minor.x = element_blank())
)

## need to solve label names problem, so variables match up
if(F){ ## 
  gap_plotmod = plot_models(fitmod.gap, fitmod.blp.gap,
                            m.labels = c("Linear Model", "BLP of CATE"),
                            rm.terms = rmvars,
                            axis.labels = pl2,
                            dot.size = 2) + plotmod.settings + labs(title="Gap", x=NULL, y=NULL)
  saveRDS(gap_plotmod, file = "exhibits\\gap_plotmod.RDS")
  gap_lie_plotmod = plot_models(fitmod.gap_lie, fitmod.blp.gap_lie,
                                m.labels = c("Linear Model", "BLP of CATE"),
                                rm.terms = rmvars,
                                axis.labels = pl2,
                                dot.size = 2) + plotmod.settings + labs(title="Gap Lie", x=NULL, y=NULL)
  saveRDS(gap_lie_plotmod, file = "exhibits\\gap_lie_plotmod.RDS")
  mental_health_plotmod = plot_models(fitmod.mental_health, fitmod.blp.mental_health,
                                      m.labels = c("Linear Model", "BLP of CATE"),
                                      rm.terms = rmvars,
                                      axis.labels = pl2,
                                      dot.size = 2) + plotmod.settings + labs(title="Mental Health", x=NULL, y=NULL)
  saveRDS(mental_health_plotmod, file = "exhibits\\mental_health_plotmod.RDS")
  no_data_plotmod = plot_models(fitmod.no_data, fitmod.blp.no_data,
                                m.labels = c("Linear Model", "BLP of CATE"),
                                rm.terms = rmvars,
                                axis.labels = pl2,
                                dot.size = 2) + plotmod.settings + labs(title="No Data", x=NULL, y=NULL)
  saveRDS(no_data_plotmod, file = "exhibits\\no_data_plotmod.RDS")
  bad_plotmod = plot_models(fitmod.bad, fitmod.blp.bad,
                            m.labels = c("Linear Model", "BLP of CATE"),
                            rm.terms = rmvars,
                            axis.labels = pl2,
                            dot.size = 2) + plotmod.settings + labs(title="Bad", x=NULL, y=NULL)
  saveRDS(bad_plotmod, file = "exhibits\\bad_plotmod.RDS")
}




noticWorkExpInc_boxplot = boxplot(var=y, y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, gap|gap_lie|control), facet="workExperienceInc", refgroup = "control", x.label = "Noticed Work Experience Inconsistencies")
saveRDS(noticWorkExpInc_boxplot, file = "exhibits\\noticWorkExpInc_boxplot.RDS")

noticHealth_boxplot = boxplot(var=y, y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, mental_health|control), facet="health", refgroup = "control", x.label = "Noticed 'Health Problems'")
saveRDS(noticHealth_boxplot, file = "exhibits\\noticHealth_boxplot.RDS")

df. = results$age %>% bin_function(number = 5) %>% cbind(subgroup=. , results) %>% subset(mental_health|control) 
#boxplot(var=y, y.label = "CV score", x.label = "age", by="treatment", y.positions=NA,  df=df., facet="subgroup", refgroup = "control")
ageMentalHelath_boxplot = boxplot(var=y, y.label = "CV score", x.label = "age", by="subgroup", y.positions=NA,  df=df., facet="treatment", refgroup = levels(df.$subgroup)[1], paired.=F)
saveRDS(ageMentalHelath_boxplot, file = "exhibits\\ageMentalHelath_boxplot.RDS")

libConMentalHealth_boxplot = boxplot(var=y, y.label = "CV score", x.label = "lib_con", by="lib_con", y.positions=NA,  df=subset(results, mental_health|control), facet="treatment", refgroup = "1", paired.=F)
saveRDS(libConMentalHealth_boxplot, file = "exhibits\\libConMentalHealth_boxplot.RDS")

potentialEmployersBad_boxplot = boxplot(var=y, y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, bad|control), facet="potentialemployers", refgroup = "control",
                                        x.label = "Employers should use SM")
saveRDS(potentialEmployersBad_boxplot, file = "exhibits\\potentialEmployersBad_boxplot.RDS")

SMContentBad_boxplot = boxplot(var=y, y.label = "CV score", by="treatment", y.positions=NA,  df=subset(results, bad|control), facet="SMcontents", refgroup = "control",
                               x.label = "SM Content is Important for Chossing a Candidate")
saveRDS(SMContentBad_boxplot, file = "exhibits\\SMContentBad_boxplot.RDS")

df. = results$SM_active %>% round() %>% cbind(subgroup=. , results) %>% subset((bad|no_data|control)&subgroup!=7 ) 
SMActiveno_data_boxplot = boxplot(var=y, y.label = "CV score", by="treatment", y.positions=NA,  df=df., facet="subgroup", refgroup = "control",
                                  x.label = "Active on SM", )
saveRDS(SMActiveno_data_boxplot, file = "exhibits\\SMActiveno_data_boxplot.RDS")




############# tableby ################
form_pre = c("experience_years",
             "education_yt" ,
             "skills_1.2", 
             "interests_1.2",
             "Order")
formula = form_pre %>% paste(collapse = "+") %>% paste("treatment", "~", .) %>% as.formula() 
df = subset(results, results$mturkID %in% listIDs)
balance_table = tableby(formula, df, control = tableby.control(numeric.test = "kwt"))
tab1 = as.data.frame(summary(balance_table))

formula = "applicant.gender" %>% paste("treatment", "~", .) %>% as.formula() 
balance_table = tableby(formula, subset(df, !no_data), control = tableby.control(numeric.test = "kwt"))
tab2 = as.data.frame(summary(balance_table))
tab2 = cbind(tab2[,1:5], c("", "n/a", ""), tab2[6:8])
colnames(tab2) = colnames(tab1)
tableby1 = rbind(tab1, tab2)  
saveRDS(tableby1, file = "exhibits\\tableby1.RDS")



redcolssec = match(c("participant.gender", "age",  
                     "highest_education2", "marital_status2", "employment_status2",  
                     "lib_con", 
                     "SM_active",
                     "Twitter", "Instagram", "Facebook", "Snapchat", "LinkedIn","TikTok",
                     #"SM_approval",
                     "stayintouch", "toomuchtime", "potentialemployers","professional", 
                     #"privacy_no_problem",
                     "lostControl", "advertisers", "employers",  #privacy
                     #"teams", "communicate", "administration", "customerService", "ITskills", 
                     #"experience", "education", "hobbies", 
                     "CVcontents", "SMcontents", 
                     #"workExperienceIncLength", "educationIncLength","healthLength", "crimeLength", "discriminationLength", "anythingElseLength",
                     "total_time", "TimeSpentInstruc", "mean_no_times_viewed", "mean_notelength", 
                     "median_length_viewed", "min_length_viewed",
                     "control", "gap", "gap_lie", "mental_health", "no_data", "bad"
),
colnames(player_table))
#redcolssec = match(formula_vars.full, colnames(player_table))

tempdf = subset(player_table, select = redcolssec)
tempdf$participant.gender = revalue(tempdf$participant.gender,
                                    c("Male"=0,
                                      "Female" = 1,
                                      "Other" = NA))

tempdf = apply(tempdf, 2, FUN = as.numeric)

corr_plot = ggcorr(tempdf)
saveRDS(corr_plot, file = "exhibits\\corr_plot.RDS")

educEducImp_lineplot = score.means.by.var.line2(y, "education","education_yt.factor",  results, include.count=T, cut.outliers = T, x.label="Candidate's Education (is important)", leg.label = "Years Education")
saveRDS(educEducImp_lineplot, file = "exhibits\\educEducImp_lineplot.RDS")

expEXpImp_lineplot = score.means.by.var.line2(y, "experience","CV.experience.factor",  results, include.count=T, cut.outliers = T, x.label="Candidate's Experience (is important)", leg.label = "Years Experience")
saveRDS(expEXpImp_lineplot, file = "exhibits\\expEXpImp_lineplot.RDS")

expCVImp_lineplot = score.means.by.var.line2(y, "CVcontents","CV.experience.factor",  results, include.count=T, cut.outliers = T, x.label="Contents of the candidate's CV (is) important)", leg.label = "Years Experience")
saveRDS(expCVImp_lineplot, file = "exhibits\\expCVImp_lineplot.RDS")

trSMImp_lineplot = score.means.by.var.line2(y, "SMcontents","treatment",  results, include.count=T, cut.outliers = T, x.label="Contents of the candidate's social media accounts (are important)", leg.label = "Treatment")
saveRDS(trSMImp_lineplot, file = "exhibits\\trSMImp_lineplot.RDS")

results$agebin = bin_function(results$age, number=6)
gghistogram(data=subset(results, finished), x="stayintouch", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="toomuchtime", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="potentialemployers", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="professional", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="professional", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="lostControl", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="advertisers", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 
gghistogram(data=subset(results, finished), x="employers", bins = 7, title = "", xlab = "", fill = "agebin") +
  facet_grid(rows = vars(agebin)) 





