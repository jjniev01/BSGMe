require(ggplot2)
require(rgdal)
require(data.table)
require(gridExtra)




####  BOXPLOTS OF UNIT LEVEL METRICS BY YEAR  ----------------------------------
iso3 <- c("PAN","VNM","CHE", "UGA")
root <- "E:/Research/BSGMe/"

model_tag <- "2010-2015"
sub_run <- "Observed"


##  Dimensions in mm:
width = 190
height = 90
##Resolution in DPI:
res = 500

##  For every country:

##  Set out directory:
out <- paste0("E:/Research/BSGMeGeneral/","figures/")

##  Pull in the modeled and observed model runs and give them a label fielE:
##  Read in the unit level contingency data from all the countries:
dt <- as.data.table(read.csv(file = paste0(root,
                                           "Observed Input Validation/AdminLevelPixelComparisons_2010-2015.csv"),
                             header = T, stringsAsFactors = F))
##  Make sure we have only unique rows:
dt <- unique(dt, by = c("GID","YEAR"))
##  Correct the DOR calculations:
# dt[,c("DOR","logDOR"):=0]
# dt[,c("DOR.NULL","logDOR.NULL"):=0]
# dt[,c("DOR","logDOR"):=.({{TP*TN}/{{FP+1}*FN}},log({{TP*TN}/{{FP+1}*FN}}))]
# dt[,c("DOR.NULL","logDOR.NULL"):=.(NA,NA)]
dt$SUB.RUN <- "Observed"

# foo <-as.data.table(read.csv(file = paste0(root,
#                                            "Observed Input Validation/AdminLevelPixelComparisons_2010-2015.csv"),
#                              header = T,
#                              stringsAsFactors = F))
# ##  Make sure we have only unique rows:
# foo <- unique(foo, by = c("GID","YEAR"))
# # foo[,c("DOR","logDOR"):=0]
# # foo[,c("DOR","logDOR"):=.({{TP*TN}/{{FP+1}*FN}},log({{TP*TN}/{{FP+1}*FN}}))]
# foo$SUB.RUN <- "Observed"

##  Create a new dataset based upon the NULL models:
nullcols <- grep(".*NULL.*",names(dt),value=T)
k_cols <- c("GID","YEAR", nullcols,"N.TOT","MODEL","ISO")
foonull <- copy(dt[,k_cols,with=F])
##  Modify the names to match the general non-null column names:
names(foonull) <- c("GID","YEAR",
                    sub("(.*)[.]NULL","\\1",
                        grep(".*NULL.*",names(foonull),value=T)),
                    "N.TOT","MODEL","ISO")
# foonull[,c("DOR","logDOR"):=.({{TP*TN}/{{FP+1}*FN}},log({{TP*TN}/{{FP+1}*FN}}))]
foonull$SUB.RUN <- "Null"

##  Remove the NULL model fields from the other datatables:
dt <- dt[,(c(nullcols,"QUANTDIS.0","QUANTDIS.1",
             "ALLOCDIS.0","ALLOCDIS.1","PROP.CORRECT")):=NULL]

# foo <- foo[,(c(nullcols,"QUANTDIS.0","QUANTDIS.1",
#                "ALLOCDIS.0","ALLOCDIS.1","PROP.CORRECT")):=NULL]

##  Combine into single dataset
dt <- copy(rbind(dt,foonull))


##  Calculate a grouping factor:
dt[,ISGROUP := paste(ISO, SUB.RUN,sep = " ")]
dt$ISO <- factor(dt$ISO)
##  Rename the labels of the levels we will use for faceting:
levels(dt$ISO)[levels(dt$ISO)=="CHE"] <- "Switzerland"
levels(dt$ISO)[levels(dt$ISO)=="PAN"] <- "Panama"
levels(dt$ISO)[levels(dt$ISO)=="UGA"] <- "Uganda"
levels(dt$ISO)[levels(dt$ISO)=="VNM"] <- "Vietnam"

##  F1 Plot  ----
tiff(filename=paste0(out,"CHE_PAN_UGA_VNM_F1_Boxplots_BSGMe_",model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)
##  Get programmatic labels for the boxplot sample sizes:
# count_dt <- copy(dt[ISO==i & MODEL==model_tag])
# ##    Remove any null records where data is undefineE:
# count_dt <- count_dt[!is.na(F1)&!is.na(F1.NULL)]
# ##  Get the count of admin IDs:
# count_dt[,COUNT:=.N,by = list(YEAR)]
# setkey(count_dt, YEAR)
# cols <- c("YEAR","COUNT","PERIOD")
# count_dt <- count_dt[,..cols]
# count_dt <- unique(count_dt)

print(ggplot()+
        # geom_violin(data = dt[!is.na(F1)],
        #            aes(x = as.factor(YEAR), 
        #                y = F1, 
        #                fill = SUB.RUN),trim = T)+
        # geom_boxplot(data = dt[!is.na(F1)],
        #              aes(x = as.factor(YEAR), 
        #                  y = F1),
        #              width=0.02,
        #              fill="black",
        #              outlier.color = NA,
        #              inherit.aes = F)+
        # stat_summary(data = dt[!is.na(F1)],
        #              aes(x = as.factor(YEAR), 
        #                  y = F1),
        #              fun.y = "median",geom="point",fill="white",
        #              shape=21,size=1,inherit.aes = F)+
        geom_boxplot(data = dt[!is.na(F1)],
                     aes(x = as.factor(YEAR), y = F1,
                         fill = SUB.RUN),
                     outlier.shape = 1, outlier.alpha = 0.5, outlier.color = "grey50",
                     inherit.aes = F, position = "dodge2", color = "black")+
        xlab("Year of Prediction")+
        ylab("F1 Score")+
        labs(fill = "Input\nSeries")+
        facet_wrap( ~ISO, scales = "free_x")+
        theme_bw()+
        scale_fill_manual(values = c("#403075","#CE89AE","#40817E"))+
        ggtitle("Subnational Unit Comparisons")+
        theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size=8),
              axis.title.y = element_text(size=8),
              axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(size = 8)))        
dev.off()


tiff(filename=paste0(out,"CHE_PAN_UGA_VNM_Recall_Boxplots_BSGMe_",
                     model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

##  Get programmatic labels for the boxplot sample sizes:
# count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(RECALL)&!is.na(RECALL.NULL)])
# count_dt[,COUNT:=.N,by = list(YEAR)]
# setkey(count_dt, YEAR)
# cols <- c("YEAR","COUNT","PERIOD")
# count_dt <- count_dt[,..cols]
# count_dt <- unique(count_dt)


print(ggplot()+
        geom_boxplot(data = dt[!is.na(RECALL)],
                     aes(x = as.factor(YEAR), y = RECALL, 
                         fill = SUB.RUN),
                     outlier.shape = 1, outlier.alpha = 0.5, outlier.color="grey50",
                     inherit.aes = F, position = "dodge2", color = "black")+
        xlab("Year of Prediction")+
        ylab("Recall")+
        labs(fill = "Input\nSeries")+
        facet_wrap( ~ISO, scales = "free_x")+
        theme_bw()+
        scale_fill_manual(values = c("#403075","#CE89AE","#40817E"))+
        ggtitle("Subnational Unit Comparisons")+
        theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size=8),
              axis.title.y = element_text(size=8),
              axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(size = 8)))        
dev.off()


tiff(filename=paste0(out,"CHE_PAN_UGA_VNM_Precision_Boxplots_BSGMe_",model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)
  
##  Get programmatic labels for the boxplot sample sizes:
# count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(PRECISION)&!is.na(PRECISION.NULL)])
# count_dt[,COUNT:=.N,by = list(YEAR)]
# setkey(count_dt, YEAR)
# cols <- c("YEAR","COUNT","PERIOD")
# count_dt <- count_dt[,..cols]
# count_dt <- unique(count_dt)


print(ggplot()+
        geom_boxplot(data = dt[!is.na(PRECISION)],
                     aes(x = as.factor(YEAR), y = PRECISION, 
                         fill = SUB.RUN),
                     outlier.shape = 1, outlier.color = "grey50",outlier.alpha=0.5,
                     inherit.aes = F, position = "dodge2", color = "black")+
        xlab("Year of Prediction")+
        ylab("Precision")+
        labs(fill = "Input\nSeries")+
        facet_wrap( ~ISO, scales = "free_x")+
        theme_bw()+
        scale_fill_manual(values = c("#403075","#CE89AE","#40817E"))+
        ggtitle("Subnational Unit Comparisons")+
        theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size=8),
              axis.title.y = element_text(size=8),
              axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(size = 8)))        
dev.off()


  
  

tiff(filename=paste0(out,"CHE_PAN_UGA_VNM_QuantDis_Boxplots_BSGMe_",model_tag,
                     "_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

##  Get programmatic labels for the boxplot sample sizes:
# count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)])
# count_dt <- count_dt[!is.na(QUANTDIS)]
# count_dt[,COUNT:=.N,by = list(YEAR)]
# setkey(count_dt, YEAR)
# cols <- c("YEAR","COUNT","PERIOD")
# count_dt <- count_dt[,..cols]
# count_dt <- unique(count_dt)

print(ggplot()+
        geom_boxplot(data = dt[!is.na(QUANTDIS)],
                     aes(x = as.factor(YEAR), y = QUANTDIS, 
                         fill = SUB.RUN),
                     outlier.shape = 1, outlier.color = "grey50",outlier.alpha=0.5,
                     inherit.aes = F, position = "dodge2", color = "black")+
        # coord_cartesian(ylim=c(0,0.05))+
        xlab("Year of Prediction")+
        ylab("Quantity Disagreement")+
        labs(fill = "Input\nSeries")+
        facet_wrap( ~ISO, scales = "free_x")+
        theme_bw()+
        scale_fill_manual(values = c("#403075","#CE89AE","#40817E"))+
        ggtitle("Subnational Unit Comparisons")+
        theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size=8),
              axis.title.y = element_text(size=8),
              axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(size = 8)))
dev.off()



  
  
tiff(filename=paste0(out,"CHE_PAN_UGA_VNM_AllocDis_Boxplots_BSGMe_",model_tag,
                     "_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

##  Get programmatic labels for the boxplot sample sizes:
# count_dt <- copy(dt[ISO==i&MODEL == model_tag&!is.na(QUANTDIS)])
# count_dt <- count_dt[!is.na(QUANTDIS)]
# count_dt[,COUNT:=.N,by = list(YEAR)]
# setkey(count_dt, YEAR)
# cols <- c("YEAR","COUNT","PERIOD")
# count_dt <- count_dt[,..cols]
# count_dt <- unique(count_dt)

print(ggplot()+
        geom_boxplot(data = dt[!is.na(ALLOCDIS)],
                     aes(x = as.factor(YEAR), y = ALLOCDIS, 
                         fill = SUB.RUN),
                     outlier.shape = 1, outlier.color ="grey50", outlier.alpha=0.5,
                     inherit.aes = F, position = "dodge2", color = "black")+
        # coord_cartesian(ylim=c(0,0.02))+
        xlab("Year of Prediction")+
        ylab("Allocation Disagreement")+
        labs(fill = "Input\nSeries")+
        # geom_text(data = count_dt,
        #           aes(x = as.factor(YEAR),y = -0.1, label = COUNT),
        #           size = 2,
        #           inherit.aes = F)+
        # stat_summary(data=dt[!is.na(F1)],
        #              fun.y = median, geom= "point", shape=4,size = 3,
        #              aes(x=as.factor(YEAR), y=F1.NULL, group = ISGROUP), color = "red",
        #              na.rm = T)+
        facet_wrap( ~ISO, scales = "free_x")+
        theme_bw()+
        scale_fill_manual(values = c("#403075","#CE89AE","#40817E"))+
        ggtitle("Subnational Unit Comparisons")+
        theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
              axis.title.x = element_text(size=8),
              axis.title.y = element_text(size=8),
              axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 7),
              strip.text = element_text(size = 8)))
dev.off()




### WARNING BELOW HERE HAS NOT BEEN MODIFIED FOR EXTRAPOLATION FIGURES   #######
  
  ##
  
  
  
  # tiff(filename=paste0(out,i,"_QuantPerDiff_Boxplots_",model_tag,".tiff"),
  #      width=width,
  #      height=height,
  #      units="mm",
  #      compression="lzw",
  #      bg="white",
  #      res=res)
  
  ##  Get programmatic labels for the boxplot sample sizes:
  # count_dt <- copy(dt[ISO==i&MODEL==model_tag&!is.na(PER.QUANT.DIFF)])
  # count_dt[,COUNT:=.N,by = list(YEAR)]
  # setkey(count_dt, YEAR)
  # cols <- c("YEAR","COUNT","PERIOD")
  # count_dt <- count_dt[,..cols]
  # count_dt <- unique(count_dt)
  # 
  # print(ggplot(dt[ISO==i&MODEL == model_tag&!is.na(PER.QUANT.DIFF)],
  #              aes(as.factor(YEAR),PER.QUANT.DIFF))+
  #         geom_hline(yintercept = 0)+
  #         geom_boxplot(outlier.shape = NA)+
  #         geom_text(data = count_dt,
  #                   aes(x = as.factor(YEAR),y = -150, label = COUNT),
  #                   size = 2,
  #                   inherit.aes = F)+
  #         xlab("Year of Prediction")+
  #         ylab("Percent Difference")+
  #         coord_cartesian(ylim = c(-150,200))+
  #         facet_wrap( ~PERIOD,scales = "free_x")+
  #         theme_bw()+
  #         ggtitle(i)+
  #         theme(plot.title = element_text(size =10, hjust = 0.5, face = "bold"),
  #               axis.title.x = element_text(size=8),
  #               axis.title.y = element_text(size=8),
  #               axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
  #               axis.text.y = element_text(size = 7),
  #               strip.text = element_text(size = 8)))
  # 
  # dev.off()


##  COUNTRY WIDE PIXEL LEVEL COMPARISON METRIC FIGURES
iso3 <- c("PAN","VNM","CHE", "UGA")
count_tag <- "CHE_PAN_UGA_VNM_"
root <- "E:/Research/BSGMe/"
out <- paste0(root,"figures/")
model_tag <- "2010-2015"
##  Dimensions in mm:
width = 90
height = 75
##Resolution in DPI:
res = 500

##  Read in the data:
comp <- as.data.table(read.csv(paste0(root,
                                      "Modeled Input Validation/CountryLevelPixelComparisons_Extrapolation_Modeled_",
                                      model_tag,"_w_Null.csv"),
                               stringsAsFactors = F))
comp$SUB.RUN <- "BSGMi"
comp[,c("DOR","logDOR","DOR.NULL","logDOR.NULL"):=NULL]
rm(foo)
gc()
foo <-  as.data.table(read.csv(paste0(root,
                                      "/Observed Input Validation/CountryLevelPixelComparisons_Extrapolation_Observed_",
                                      model_tag,"_w_Null.csv"),
                               stringsAsFactors = F))
foo[,c("DOR","logDOR","DOR.NULL","logDOR.NULL"):=NULL]
foo$SUB.RUN <- "Observed"


##  Create a new dataset based upon the NULL models:
nullcols <- grep(".*NULL.*",names(foo),value=T)
k_cols <- c("YEAR", nullcols,"MODEL","ISO")
foonull <- copy(foo[,k_cols,with=F])
##  Modify the names to match the general non-null column names:
names(foonull) <- c("YEAR",
                    sub("(.*)[.]NULL","\\1",
                        grep(".*NULL.*",names(foonull),value=T)),
                    "MODEL","ISO")
##  Create columns that exist for the modeled versions:
foonull[,c("NCELLS",
           "RECALL",
           "PRECISION",
           "SPECIFICITY",
           "F1",
           "OVER.ACC",
           "QUANTDIS",
           "ALLOCDIS"):=
          .({TP+FP+FN+TN},
            {TP/{TP+FN}},
            {TP/{TP+FP}},
            {TN/{FP+TN}},
            {2*{{TP/{TP+FN}}/{{TP/{TP+FN}}+{1}}}},
            {{TP+TN}/{TP+FP+FN+TN}},
            {{{abs({FP-FN}/{TP+FP+FN+TN})}+{abs({FN-FP}/{TP+FP+FN+TN})}}/2},
            {{{2*min({FP/{TP+FP+FN+TN}}, {FN/{TP+FP+FN+TN}})}+{2*min({FN/{TP+FP+FN+TN}}, {FP/{TP+FP+FN+TN}})}}/2})]
# foonull[,c("DOR","logDOR"):=.({{TP*TN}/{{FP+1}*FN}},log({{TP*TN}/{{FP+1}*FN}}))]
foonull$SUB.RUN <- "Null"

##  Remove the NULL model fields from the other datatables:
comp <- comp[,(c(nullcols)):=NULL]

foo <- foo[,(c(nullcols)):=NULL]

##  Combine into single dataset
comp <- copy(rbind(comp,foo,foonull))

comp$ISO <- factor(comp$ISO)
##  Rename the labels of the levels we will use for faceting:
levels(comp$ISO)[levels(comp$ISO)=="CHE"] <- "Switzerland"
levels(comp$ISO)[levels(comp$ISO)=="PAN"] <- "Panama"
levels(comp$ISO)[levels(comp$ISO)=="UGA"] <- "Uganda"
levels(comp$ISO)[levels(comp$ISO)=="VNM"] <- "Vietnam"



##  Create another grouping variable:
comp$MODGROUP <- paste0(comp$ISO," ",
                        comp$SUB.RUN)
# ##  Create a data table of it:
# comp <- as.data.table(comp)
##  Define shapes we want to use:
shapes <- c(22,21,24,25)
names(shapes) <- unique(comp$ISO)

tiff(filename=paste0(out,count_tag,"_Median_F1_DotPlot_Pixel_Level_CountryWide_BSGMe_",
                     model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(comp,
       aes(x=as.factor(YEAR),
           y=F1, 
           shape = ISO, 
           group = SUB.RUN))+
  geom_line(size = 0.2, aes(group = MODGROUP, color = SUB.RUN))+
  # geom_line(data = as.data.frame(comp),
  #           aes(x = as.factor(YEAR),
  #               y = FSCORE.NULL,
  #               group = SUB.RUN),color = "red",
  #           inherit.aes = F, size = 0.2)+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=F1,
                 shape = ISO, fill = NA, color = SUB.RUN, group = ISO),size = 1,
             fill=NA)+
  # geom_point(data = as.data.frame(comp),
  #            aes(x = as.factor(YEAR), y = FSCORE.500,
  #                shape = MODGROUP, group = MPGROUP),
  #            size = 1, fill="white",
  #            inherit.aes = F, color = "red", show.legend = F)+
  scale_shape_manual(values = shapes)+
  scale_color_manual(values = c("#403075","#CE89AE","#40817E"))+
  theme_bw()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Country", color = "Input\nSeries",
       x = "Year", y = "F1 Score")

dev.off()




tiff(filename=paste0(out,count_tag,"_Quant_DotPlot_Pixel_Level_CountryWide_BSGMe_",
                     model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(as.data.frame(comp),
       aes(x=as.factor(YEAR),
           y=QUANTDIS, 
           shape = ISO, 
           color = SUB.RUN,
           group = SUB.RUN))+
  geom_line(size = 0.2, aes(group = MODGROUP, color = SUB.RUN))+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=QUANTDIS,
                 shape = ISO, color = SUB.RUN, group = ISO),size = 1,
             fill=NA)+
  scale_shape_manual(values = shapes)+
  theme_bw()+
  scale_color_manual(values = c("#403075","#CE89AE","#40817E"))+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Country", color = "Input\nSeries",
       x = "Year", y = "Quantity Disagreement")


dev.off()




tiff(filename=paste0(out,count_tag,"_AllocationDis_DotPlot_Pixel_Level_CountryWide_BSGMe_",
                     model_tag,"_w_Null.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

ggplot(as.data.frame(comp),
       aes(x=as.factor(YEAR),
           y=ALLOCDIS, 
           shape = ISO, 
           color = SUB.RUN,
           group = SUB.RUN))+
  geom_line(size = 0.2, aes(group = MODGROUP, color = SUB.RUN))+
  geom_point(data = comp,
             aes(x=as.factor(YEAR),y=ALLOCDIS,
                 shape = ISO, color = SUB.RUN, group = ISO),size = 1,
             fill=NA)+
  scale_shape_manual(values = shapes)+
  scale_color_manual(values = c("#403075","#CE89AE","#40817E"))+
  theme_bw()+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))+
  labs(shape = "Country", color = "Input\nSeries",
       x = "Year", y = "Allocation Disagreement")


dev.off()




####  PIXEL LEVEL DISAG. STACKED BAR CHART - TWO BSGMi RUNS AND NULL MODEL  ----
root <- "E:/Research/"
width <- 190
height <- 120
res <- 500
out <- paste0(root,"BSGMe/figures/")


##  Bring in the observed BSGMe runs pixel level data that already has
##  annual data calculateE:
pixdt <- as.data.table(read.csv(file=paste0(root,
                                            "BSGMe/Observed Input Validation/",
                                            "CountryLevelPixelComparisons_Extrapolation_Observed_2010-2015_w_NULL.csv"),
                                stringsAsFactors = F))

##  Remove the GUF+ run and another grouping column :
pixdt[,MODEL:="Observed Input"]

##  Create a new grouping variable:
pixdt$UNIGROUP <- paste0(pixdt$ISO," ",pixdt$MODEL)


##  Read in the modeled BSGMe runs pixel level data:
tripdt <- as.data.table(read.csv(file=paste0(root,
                                             "BSGMe/Modeled Input Validation/",
                                             "CountryLevelPixelComparisons_Extrapolation_Modeled_2010-2015_w_NULL.csv"),
                                 stringsAsFactors = F))
tripdt[,MODEL := "Modeled Input"]
tripdt$UNIGROUP <- paste0(tripdt$ISO," ",tripdt$MODEL)

##  Combine the two datasets:
fulldt <- rbind(copy(pixdt),copy(tripdt))
dt <- copy(fulldt)

##  Reduce this down to our current columns of interest: 
dt <- dt[,list(UNIGROUP=UNIGROUP, MODEL = MODEL, ISO = ISO, YEAR=YEAR, 
               QUANTDIS=QUANTDIS, ALLOCDIS=ALLOCDIS)]


##  Subset the full dt to only the null model information:
##    NOTE:  The null models are equivalent in both the n=3 and n=4 model runs.
nulldt <- copy(pixdt)
nulldt[,MODEL := "Naive"]
nulldt$UNIGROUP <- as.factor(paste0(nulldt$ISO," Naive"))

##  Subset to our current columns of interest:
nulldt <- nulldt[,list(UNIGROUP=UNIGROUP, MODEL="Naive", 
                       ISO = ISO, YEAR=YEAR, 
                       QUANTDIS={{{abs({FP.NULL-FN.NULL}/{TP.NULL+FP.NULL+FN.NULL+TN.NULL})}+{abs({FN.NULL-FP.NULL}/{TP.NULL+FP.NULL+FN.NULL+TN.NULL})}}/2},
                       ALLOCDIS={{{2*min({FP.NULL/{TP.NULL+FP.NULL+FN.NULL+TN.NULL}}, {FN.NULL/{TP.NULL+FP.NULL+FN.NULL+TN.NULL}})}+{2*min({FN.NULL/{TP.NULL+FP.NULL+FN.NULL+TN.NULL}}, {FP.NULL/{TP.NULL+FP.NULL+FN.NULL+TN.NULL}})}}/2})]

##  Bind the null dataset to the modeled F1 data:
dt <- rbind(copy(dt),copy(nulldt))


##  Melt it:
dt <- melt(dt, id.vars = c("MODEL","ISO","YEAR"),
           measure.vars = c("QUANTDIS","ALLOCDIS"))
levels(dt$variable) <- c("Quantity","Allocation")


dt[MODEL == "Observed Input" & (variable == "Quantity"|variable =="Allocation"), GROUP:=1]
dt[MODEL == "Modeled Input" & (variable == "Quantity"|variable =="Allocation"), GROUP:=2]
dt[MODEL == "Naive" & (variable == "Quantity"|variable =="Allocation"), GROUP:=3]

## Adjust the levels for differring colors (six total, two per model):
dt$varchar <-paste0(dt$variable,", ",dt$MODEL)
dt$varchar <- factor(dt$varchar, levels = c("Allocation, Modeled Input","Allocation, Naive","Allocation, Observed Input",
                                            "Quantity, Modeled Input","Quantity, Naive","Quantity, Observed Input"
                                            ))

##  Go through all the model types to create a custom gridded map
che_plot <- ggplot()+
  geom_col(data = dt[ISO == "CHE"],
           aes(x=factor(GROUP,labels = c("Obs. Input", "BSGMi Input","Naive")),
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.65, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  coord_cartesian(ylim = c(0.0,0.006))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(size=6, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#2085f8","#74b3fb"))
  # scale_fill_manual(values = c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3"))
  # scale_fill_manual(values = c("#9078dd","#CE89AE","#82e3df", # LIGHT - MOD, NULL, OBS
  #                              "#403075","#6c2d4f","#40817E")) #DARK - MOD, NULL, OBS

pan_plot <- ggplot()+
  geom_col(data = dt[ISO == "PAN"],
           aes(x=factor(GROUP,labels = c("Obs. Input", "BSGMi Input","Naive")),
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.65, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  coord_cartesian(ylim = c(0.0,0.006))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#2085f8","#74b3fb"))
  # scale_fill_manual(values = c("#9078dd","#CE89AE","#82e3df", # LIGHT - MOD, NULL, OBS
  #                              "#403075","#6c2d4f","#40817E")) #DARK - MOD, NULL, OBS

uga_plot <- ggplot()+
  geom_col(data = dt[ISO == "UGA"],
           aes(x=factor(GROUP,labels = c("Obs. Input", "BSGMi Input","Naive")),
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.65, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  coord_cartesian(ylim = c(0.0,0.006))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#2085f8","#74b3fb"))
  # scale_fill_manual(values = c("#9078dd","#CE89AE","#82e3df", # LIGHT - MOD, NULL, OBS
  #                              "#403075","#6c2d4f","#40817E")) #DARK - MOD, NULL, OBS

vnm_plot <- ggplot()+
  geom_col(data = dt[ISO == "VNM"],
           aes(x=factor(GROUP,labels = c("Obs. Input", "BSGMi Input","Naive")),
               y=value,
               fill=variable, group = GROUP),
           position = "stack",color="grey20", width =0.65, show.legend = F)+
  facet_wrap( ~ YEAR, ncol = 12)+
  coord_cartesian(ylim = c(0.0,0.006))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6),
        strip.background=element_rect(fill = "white"))+
  xlab("Year")+
  ylab("Disagreement")+ 
  scale_fill_manual(values = c("#2085f8","#74b3fb"))
  # scale_fill_manual(values = c("#9078dd","#CE89AE","#82e3df", # LIGHT - MOD, NULL, OBS
  #                              "#403075","#6c2d4f","#40817E")) #DARK - MOD, NULL, OBS

exm_plot <- ggplot()+
  geom_col(data=data.frame("GROUP"=factor(c(1,2,3), 
                                          labels = c("Obs. Input",
                                                     "BSGMi Input",
                                                     "Naive")),
                           "VAL" = c(7,5,3)),
           aes(x=GROUP, y = VAL,group=GROUP),
           fill="#2085f8")+
  ylab("Key")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=8),
        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        strip.background=element_rect(fill = "white"))

tiff(filename=paste0(out,count_tag,
                     "_ISO_Years_Pixel_Q_and_A_Disagreement_BSGMe_Comps.tiff"),
     width=width,
     height=height,
     units="mm",
     compression="lzw",
     bg="white",
     res=res)

grid.arrange(che_plot,pan_plot,uga_plot,vnm_plot,exm_plot,
             layout_matrix=rbind(c(1,1,1,1,1,1,1,1,1,1,1,1),
                                 c(2,2,2,2,2,2,2,2,2,2,2,2),
                                 c(3,3,3,3,3,3,3,3,3,3,3,3),
                                 c(4,4,4,4,4,4,4,4,4,4,4,4),
                                 c(5,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
             ))


 dev.off()