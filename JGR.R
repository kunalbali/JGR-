
library(data.table)
library(lubridate)
library(plyr)
library(gridExtra)
library(ggtext)
library(ggpmisc)
library(readr)
library(tidyverse)
library(dplyr)
#library(grid.arrange)
library(anytime) 
library(scales)
library(ggpmisc)
library(cowplot)
library(reshape2)
library(gghighlight)
library(yorkregression)
library(devtools)

######  Figure 2 ########

names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  distinct(TheTime, .keep_all= TRUE) %>%   # remove duplicate rows based on header
  mutate(pm2p5 = ifelse(pm2p5 >100, NA, pm2p5)) %>% #0.5
  mutate(CTC_AMS_OA = ifelse(CTC_AMS_OA < 1, NA, CTC_AMS_OA)) %>% #0.5
  mutate(ratio_BC_OA = `MAAP_BC_637_con_mac10.4`/CTC_AMS_OA) %>%
  mutate(ratio_BC_OA = ifelse(ratio_BC_OA > 1, NA, ratio_BC_OA)) %>%
  mutate(BC_ratio_OA = CTC_AMS_OA/(CTC_AMS_OA + MAAP_BC_637_con_mac10.4)) %>% 
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  
  mutate(OAandBC = (`MAAP_BC_637_con_mac10.4` + CTC_AMS_OA)) %>%
  mutate(ratio_BCOA_pm = (OAandBC/pm2p5)) %>%
  mutate(ratio_BCOA_pm = ifelse(ratio_BCOA_pm > 1, NA, ratio_BCOA_pm)) %>%
  mutate(ratio_BCOA_pm = ifelse(ratio_BCOA_pm < 0, NA, ratio_BCOA_pm)) %>%
  select(TheTime,Temp_C, pm2p5,CTC_AMS_OA,`MAAP_BC_637_con_mac10.4`,BC_ratio_OA,ratio_BCOA_pm) %>%
  melt(id.vars=1:1)  -> FIGURE_01


levels(FIGURE_01$variable) <- c(expression(Temperature),
                                "PM[2.5]",
                                "OA[AMS]",
                                "eBC[`MAAP,637`]",
                                "frac(OA, eBC+OA)",
                                "frac(eBC+OA, PM[2.5])")

FIGURE_01 %>% #### ALSO FOR JAN AND FEB ###
  ggplot(aes(x=TheTime, y=value, color=variable, fill = variable)) + 
  geom_point(size=0.5) + geom_line() + 
  #ggplot(aes(x=day, y=value, color=variable, group = day, fill = variable,alpha=0.2)) + 
  #geom_boxplot(notch = F,outlier.shape = NA) +
  xlab("Local Time") +
  #ylab(expression(atop(Temp ~ '(' * degree * C * ')' ~ "", 
  #                     "Conc. (" * mu * g * m^-3 * ")"))) +    
  ylab(expression(`Mass Fraction` ~ '                             ' ~ Concentration ~ '(' * mu * g ~ m^-3 * ')' ~ '                      ' ~ 
                    `T` ~ '(' * degree * C * ')'))+
  theme_bw() + 
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=rel(0.8)))+
  theme(legend.position="none")+
  # theme(legend.position = c(0.8, 0.2)) +
  theme(axis.title = element_text(face="plain",size=14,color="black"),
        axis.text=element_text(size=14,face="plain", color="black"),
        axis.title.x = element_text(vjust=0.1),
        axis.text.y=element_text(hjust=0.5),
        axis.text.x=element_text(hjust=0.0, angle = -45),
        plot.title = element_text(size=16),
        panel.grid.minor = element_line(color = "grey90", size = 0.3),  # Light grey for minor grid
        panel.grid.major = element_line(color = "grey85", size = 0.5),
        axis.ticks.length.x = unit(0.2, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5)) +
  theme(strip.text = element_text(size=14, color="black"))+
  facet_grid(variable ~., switch = "x", scales = "free_y", space = "free_x",
             labeller = label_parsed) +
  facetted_pos_scales(
    y = list(
      Temperature = scale_y_continuous(limits = c(-40, 10)),
      `PM[2.5]` = scale_y_continuous(),
      `OA[(AMS)]` = scale_y_continuous(),
      `eBC[\`(MAAP,637)\`]` = scale_y_continuous(),
      `frac(OA, eBC+OA)` = scale_y_continuous(breaks = c(0.4,0.6,0.8,1),limits = c(0.4, 1)),  # Set to 0-1
      `frac(eBC+OA, PM[2.5])` = scale_y_continuous(breaks = c(0, 0.2,0.5,0.7,1),limits = c(0, 1))  # Set to 0-1
    )
    
  ) +
  scale_x_datetime(expand=c(0,0),
                   date_breaks= "5 days",
                   date_minor_breaks = "1 days",
                   date_labels = "%b-%d",
                   limits = as.POSIXct(c("2022-01-15 00:00:00", "2022-03-01 10:00:00")),
                   guide = "axis_minor") +
  scale_colour_manual( values = c("Temperature" = "magenta3",
                                  "PM[2.5]" = "red", 
                                  "OA[AMS]" = "green4", 
                                  "eBC[`MAAP,637`]" = "black",
                                  "frac(OA, eBC+OA)" = "blue",
                                  "frac(eBC+OA, PM[2.5])" = "tomato3")) +
  scale_fill_manual(values = c("Temperature" = "magenta3",
                               "PM[2.5]" = "red", 
                               "OA[AMS]" = "green4", 
                               "eBC[`MAAP,637`]" = "black",
                               "frac(OA, eBC+OA)" = "blue",
                               "frac(eBC+OA, PM[2.5])" = "tomato3")) +
  annotate("rect", fill = "red", alpha = 0.15, 
           xmin = as.POSIXct("2022-01-27 00:00:00"),
           xmax = as.POSIXct("2022-02-04 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) +
  annotate("rect", fill = "gold", alpha = 0.3, 
           xmin = as.POSIXct("2022-02-23 00:00:00"),
           xmax = as.POSIXct("2022-02-25 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) +
  # Add individual text annotations for each panel
  geom_text(data = data.frame(
    TheTime = as.POSIXct("2022-02-10 00:00:00"),
    value = c(0, 50, 25, 2, 0.4, 0.6),
    variable = factor(c("Temperature", "PM[2.5]", "OA[AMS]", "eBC[`MAAP,637`]", "frac(OA, eBC+OA)", "frac(eBC+OA, PM[2.5])"),
                      levels = c("Temperature", "PM[2.5]", "OA[AMS]", "eBC[`MAAP,637`]", "frac(OA, eBC+OA)", "frac(eBC+OA, PM[2.5])")),
    label = c("NCore", "NCore", "CTC", "CTC", "", "")
  ), aes(x = TheTime, y = value, label = label), 
  hjust = 0.5, vjust = 0.1, size = 6, fontface = "bold", 
  color = "black", inherit.aes = FALSE) +
  coord_cartesian(clip = "off") -> FIGURE_01_PLOT



ggsave("FIGURE_01_PLOT_01_8.5x8.5_31_NEW.pdf", plot = FIGURE_01_PLOT, width = 8.5, height = 8.5, 
       device = "pdf", dpi = 600)

ggsave("FIGURE_01_PLOT_02_revision01.png", 
       plot = FIGURE_01_PLOT, 
       width = 8.5, 
       height = 8.5, 
       units = "in",
       dpi = 600,  # Standard for journals
       #compression = "lzw",
       bg = "white")

ggsave("FIGURE_01_PLOT_01_7x8.5_14.pdf", plot = FIGURE_01_PLOT_NEW, width = 7, height = 8.5, device = "pdf")
ggsave("FIGURE_01_PLOT_01_10x8.5_08.png", plot = FIGURE_01_PLOT, width = 10, height = 8.5, device = "png")







####### FIGURE 3 a #########
########## --> TOTAL absorption  UV   ##########


names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  filter(format(TheTime, "%m") %in% c("01", "02")) %>%
  distinct(TheTime, .keep_all= TRUE) %>%   # remove duplicate rows based on header
  select(TheTime,AE33_Tabs_405,PAAS_total_b_abs_405_Mm,PAX_Tabs_401nm) %>% 
  #select(day,BC_abs_370,BrC_abs_370,ACSM_OA,conc_880_BC6,pm2p5,AAE_370_880,ratio_BC_acsm_OA) %>% 
  melt(id.vars=1:1)  -> AE33_PAX_PAAS_TOTALabs_JAN_FEB_withoutbox

levels(AE33_PAX_PAAS_TOTALabs_JAN_FEB_withoutbox$variable) <- c("AE33[(405 nm)]","PAAS-4*lambda[(405 nm)]","PAX[(401 nm)]")



AE33_PAX_PAAS_TOTALabs_JAN_FEB_withoutbox %>% 
  ggplot(aes(x=TheTime, y=value, color=variable, fill = variable)) + 
  geom_point(size=0.5) + geom_line() + 
  xlab("Local Time") +
  ylab(bquote(italic(b)[`abs`]~'('*M*m^-1*')')) +
  #ylab(bquote(bolditalic(b)[abs]~bold('('*M*m^-1*')')))+
  labs(title= "Hourly variability ") +
  theme_bw() + 
  ylim(0,100) + 
  annotate("text", x = as.POSIXct("2022-02-25 00:00:00"), y = 95, 
           label = "(a)", size = 5, fontface = "bold")+
  theme(legend.title=element_blank()) +
  #theme(legend.position="none")+
  theme(legend.position = c(0.8, 0.85)) +
  theme(legend.box = "horizontal")+
  theme(legend.text=element_text(size=12)) + 
  theme(axis.title = element_text(face="plain",size=16,color="black"),
        axis.text=element_text(size=14,face="plain", color="black"),
        axis.title.x = element_text(vjust=0.1),
        axis.text.y=element_text(hjust=0.5),
        axis.text.x=element_text(hjust=0.0, angle = -45),
        plot.title = element_text(size=16),
        panel.grid.minor = element_line(color = "grey90", size = 0.3),  # Light grey for minor grid
        panel.grid.major = element_line(color = "grey85", size = 0.5),
        axis.ticks.length.x = unit(0.2, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5)) +
  theme(strip.text = element_text(size=16, color="black"))+
  scale_x_datetime(expand=c(0,0),
                   date_breaks= "5 days",
                   date_minor_breaks = "1 days",
                   date_labels = "%b-%d",
                   limits = as.POSIXct(c("2022-01-01 00:00:00", "2022-03-01 10:00:00")),
                   guide = "axis_minor")+
  scale_colour_manual(
    values = c("AE33[(405 nm)]" = "red", "PAAS-4*lambda[(405 nm)]" = "blue", "PAX[(401 nm)]" = "#56B4E9"),
    labels = c(
      "AE33[(405 nm)]" = expression(AE33(`405 nm`)),
      "PAAS-4*lambda[(405 nm)]" = expression(PAAS-4*lambda(`405 nm`)),
      "PAX[(401 nm)]" = expression(PAX(`401 nm`)))) +
  scale_fill_manual(
    values = c("AE33[(405 nm)]" = "red", "PAAS-4*lambda[(405 nm)]" = "blue", "PAX[(401 nm)]" = "#56B4E9"),
    labels = c(
      "AE33[(405 nm)]" = expression(AE33(`405 nm`)),
      "PAAS-4*lambda[(405 nm)]" = expression(PAAS-4*lambda(`405 nm`)),
      "PAX[(401 nm)]" = expression(PAX(`401 nm`)))) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  annotate("rect", fill = "red", alpha = 0.15, 
           xmin = as.POSIXct("2022-01-27 00:00:00"),
           xmax = as.POSIXct("2022-02-04 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) +
  annotate("rect", fill = "gold", alpha = 0.15, 
           xmin = as.POSIXct("2022-02-23 00:00:00"),
           xmax = as.POSIXct("2022-02-25 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) -> hourly_ae33_pass_pax_405





####### FIGURE 3 b #########

####### --> AE33 405 VS PAAS 405 #########

# Clean data (filter out missing values based on your conditions)
clean_data_AE33_PAAS <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  #mutate(PAAS_total_b_abs_405_Mm = ifelse(PAAS_total_b_abs_405_Mm > 50, NA, PAAS_total_b_abs_405_Mm)) %>%
  #mutate(PAAS_total_b_abs_405_Mm = (PAAS_total_b_abs_405_Mm*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(PAAS_total_b_abs_405_Mm), !is.na(AE33_Tabs_405))

# Run the regression function and print the result
regression_results_AE33_PAAS <- york_regression_2nd(clean_data_AE33_PAAS$PAAS_total_b_abs_405_Mm, clean_data_AE33_PAAS$AE33_Tabs_405)




# Extract the regression results
slopeAE33_PAAS <- regression_results_AE33_PAAS$slope
interceptAE33_PAAS <- regression_results_AE33_PAAS$intercept
r_squaredAE33_PAAS <- regression_results_AE33_PAAS$r_squared
rmseAE33_PAAS <- regression_results_AE33_PAAS$rmse
maeAE33_PAAS <- regression_results_AE33_PAAS$mae


# Print each value to check
print(paste("Slope:", slopeAE33_PAAS))
print(paste("Intercept:", interceptAE33_PAAS))
print(paste("R-squared:", r_squaredAE33_PAAS))



# Plot the results with annotation
ggplot(clean_data_AE33_PAAS, aes(x = PAAS_total_b_abs_405_Mm, y = AE33_Tabs_405)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  
  geom_abline(slope = slopeAE33_PAAS, intercept = interceptAE33_PAAS, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,100)+   xlim(0,100)+ 
  labs(title = expression(AE33[`(405 nm)`] ~ "vs" ~ PAAS[`(405 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,405 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,405 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*405~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*405~nm]^{AE33}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", x = 90, y = 95, label = "(b)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 50, y = 25, 
           label = paste0("y = ", round(slopeAE33_PAAS, 2), " x", ifelse(interceptAE33_PAAS >= 0, " + ", " - "), abs(round(interceptAE33_PAAS, 2)),  
                          "\nR² = ", round(r_squaredAE33_PAAS , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_totalabs_UV_01





####### FIGURE 3 c #########

####### --> AE33 405 VS PAX 401 #########

clean_data_AE33_PAX <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` > -8.5, `Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` <11) %>% 
  filter(!is.na(PAX_Tabs_401nm), !is.na(AE33_Tabs_405))

# Run the regression function and print the result
regression_results_AE33_PAX <- york_regression_2nd(clean_data_AE33_PAX$PAX_Tabs_401nm, clean_data_AE33_PAX$AE33_Tabs_405)



# Extract the regression results
slopeAE33_PAX <-     regression_results_AE33_PAX$slope
interceptAE33_PAX <- regression_results_AE33_PAX$intercept
r_squaredAE33_PAX <- regression_results_AE33_PAX$r_squared
rmseAE33_PAX <-      regression_results_AE33_PAX$rmse
maeAE33_PAX <-       regression_results_AE33_PAX$mae


ggplot(clean_data_AE33_PAX, aes(x = PAX_Tabs_401nm, y = AE33_Tabs_405)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  
  #geom_point(size = 2, alpha = 0.9, color = "darkgoldenrod", shape = 1) +
  geom_abline(slope = slopeAE33_PAX, intercept = interceptAE33_PAX, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  xlim(0,100)+   ylim(0,100)+ 
  labs(title = expression(AE33[`(405 nm)`] ~ "vs" ~ PAX[`(401 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  ylab(bquote(italic(b)[abs*", "*405~nm]^{AE33}*"("*Mm^-1*")")) +
  xlab(expression(italic(b)[abs*", "*401~nm]^{PAX}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", x = 90, y = 95, label = "(c)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 50, y = 25, 
           label = paste0("y = ", round(slopeAE33_PAX, 2), " x", ifelse(interceptAE33_PAX >= 0, " + ", " - "), abs(round(interceptAE33_PAX, 2)), 
                          "\nR² = ", round(r_squaredAE33_PAX , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_totalabs_UV_02




####### FIGURE 3 d #########

####### --> PAAS 405 VS PAX 401 #########

clean_data_PAAS_PAX <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_PAAS_PAX_405_DIFF` > -8.7, `Bland-Altman_PAAS_PAX_405_DIFF` <7.3) %>% 
  filter(!is.na(PAAS_total_b_abs_405_Mm), !is.na(PAX_Tabs_401nm))

# Run the regression function and print the result 
regression_results_PAAS_PAX <- york_regression_2nd(clean_data_PAAS_PAX$PAAS_total_b_abs_405_Mm, clean_data_PAAS_PAX$PAX_Tabs_401nm)


# Extract the regression results
slopePAAS_PAX <-     regression_results_PAAS_PAX$slope
interceptPAAS_PAX <- regression_results_PAAS_PAX$intercept
r_squaredPAAS_PAX <- regression_results_PAAS_PAX$r_squared
rmsePAAS_PAX <-      regression_results_PAAS_PAX$rmse
maePAAS_PAX <-       regression_results_PAAS_PAX$mae




ggplot(clean_data_PAAS_PAX, aes(x = PAAS_total_b_abs_405_Mm, y = PAX_Tabs_401nm)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "slateblue2",stroke = 0.3) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  geom_abline(slope = slopePAAS_PAX, intercept = interceptPAAS_PAX, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  xlim(0,40)+   ylim(0,40)+ 
  labs(title = expression(PAX[`(401 nm)`] ~ "vs" ~ PAAS[`(405 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(bquote(italic(b)[abs*", "*405~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*401~nm]^{PAX}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", x = 35, y = 39, label = "(d)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0, y = 40, 
           label = paste0("y = ", round(slopePAAS_PAX, 2), " x", ifelse(interceptPAAS_PAX >= 0, " + ", " - "), abs(round(interceptPAAS_PAX, 2)),
                          "\nR² = ", round(r_squaredPAAS_PAX , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_totalabs_UV_03





# Combine the three plots into a single row for the lower panel
lower_panel <- plot_grid(
  plot_totalabs_UV_01 + theme(legend.position = "none"),
  plot_totalabs_UV_02 + theme(legend.position = "none"),
  plot_totalabs_UV_03 + theme(legend.position = "none"),
  ncol = 3,  # Arrange in a single row with 3 columns
  align = 'h',  # Horizontal alignment
  rel_widths = c(1, 1, 1)  # Equal width for all plots
)

# Combine the upper panel with the lower panel
combined_legend_plot_totalabs_UV2 <- plot_grid(
  hourly_ae33_pass_pax_405,  # Upper panel
  lower_panel,               # Lower panel with 3 plots
  nrow = 2,                  # 2 rows: upper and lower
  rel_heights = c(1, 1)      # Adjust the relative heights
)

# Display the combined plot
print(combined_legend_plot_totalabs_UV2)

ggsave("NON_AMBIENT_TEMP_York10_PAAS_AE33_PAX_total_abs_revision_01.png", 
       plot = combined_legend_plot_totalabs_UV2, 
       width = 11.5, height = 7.5,
       device = "png", dpi = 600)









########## FIGURE 4a ##########
########## --> Total ABSORPTION at longer wavelengths   ##########


names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  filter(format(TheTime, "%m") %in% c("01", "02")) %>%
  distinct(TheTime, .keep_all= TRUE) %>%   # remove duplicate rows based on header
  mutate(ratio_BC_acsm_OA = ifelse(ratio_BC_acsm_OA > 0.5, NA, ratio_BC_acsm_OA)) %>%
  select(TheTime,AE33_Tabs_660, MAAP_BC_abs_637, PAAS_b_total_abs_785_Mm,PAX_Tabs870) %>% 
  melt(id.vars=1:1)  -> AE33_PAX_PAAS_MAAP_totalabs_JAN_FEB_withoutbox

levels(AE33_PAX_PAAS_MAAP_totalabs_JAN_FEB_withoutbox$variable) <- c("AE33[(660 nm)]", "MAAP[(637 nm)]", "PAAS-4*lambda[(785 nm)]","PAX[(870 nm)]")


AE33_PAX_PAAS_MAAP_totalabs_JAN_FEB_withoutbox %>% #### ALSO FOR JAN AND FEB ###
  ggplot(aes(x=TheTime, y=value, color=variable, fill = variable)) + 
  geom_point(size=0.5) + geom_line() + 
  xlab("Local Time") +
  ylab(bquote(italic(b)[`abs`]~'('*M*m^-1*')')) +
  labs(title= "Hourly variability ") +
  theme_bw() + 
  ylim(0,100) + 
  annotate("text", x = as.POSIXct("2022-01-05 00:00:00"), y = 95, 
           label = "(a)", size = 5, fontface = "bold")+
  theme(legend.title=element_blank()) +
  #theme(legend.position="none")+
  theme(legend.position = c(0.76, 0.85)) +
  theme(legend.box = "horizontal")+
  theme(legend.text=element_text(size=12)) + 
  theme(axis.title = element_text(face="plain",size=16,color="black"),
        axis.text=element_text(size=14,face="plain", color="black"),
        axis.title.x = element_text(vjust=0.1),
        axis.text.y=element_text(hjust=0.5),
        axis.text.x=element_text(hjust=0.0, angle = -45),
        plot.title = element_text(size=16),
        panel.grid.minor = element_line(color = "grey90", size = 0.3),  # Very light grey for daily minor gridlines
        panel.grid.major = element_line(color = "grey85", size = 0.5),
        axis.ticks.length.x = unit(0.2, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5) ) + 
  theme(strip.text = element_text(size=16, color="black"))+
  scale_x_datetime(expand=c(0,0),
                   date_breaks= "5 days",
                   date_minor_breaks = "1 days",
                   date_labels = "%b-%d",
                   limits = as.POSIXct(c("2022-01-01 00:00:00", "2022-03-01 10:00:00")),
                   guide = "axis_minor")+
  scale_colour_manual(
    values = c("AE33[(660 nm)]" = "red",   "MAAP[(637 nm)]" = "black", "PAAS-4*lambda[(785 nm)]" = "blue", "PAX[(870 nm)]" = "#56B4E9"),
    labels = c(
      "AE33[(660 nm)]" = expression(AE33(`660 nm`)),
      "MAAP[(637 nm)]" = expression(MAAP(`637 nm`)),
      "PAAS-4*lambda[(785 nm)]" = expression(PAAS-4*lambda(`785 nm`)),
      "PAX[(870 nm)]" = expression(PAX(`870 nm`)))) +
  scale_fill_manual(
    values = c("AE33[(660 nm)]" = "red",   "MAAP[(637 nm)]" = "black", "PAAS-4*lambda[(785 nm)]" = "blue", "PAX[(870 nm)]" = "#56B4E9"),
    labels = c(
      "AE33[(660 nm)]" = expression(AE33(`660 nm`)),
      "MAAP[(637 nm)]" = expression(MAAP(`637 nm`)),
      "PAAS-4*lambda[(785 nm)]" = expression(PAAS-4*lambda(`785 nm`)),
      "PAX[(870 nm)]" = expression(PAX(`870 nm`)))) +
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  annotate("rect", fill = "red", alpha = 0.15, 
           xmin = as.POSIXct("2022-01-27 00:00:00"),
           xmax = as.POSIXct("2022-02-04 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) +
  annotate("rect", fill = "gold", alpha = 0.3, 
           xmin = as.POSIXct("2022-02-23 00:00:00"),
           xmax = as.POSIXct("2022-02-25 23:59:00"),
           ymin = -Inf, 
           ymax = Inf) -> hourly_ae33_pass_pax_longer_wavea




########## FIGURE 4b ##########

######### ---> AE33 660 vs MAAP 637 ##########

# Clean data (filter out missing values based on your conditions)
clean_data_AE33_MAAP <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <= 2) %>% 
  #mutate(MAAP_BC_abs_637 = (MAAP_BC_con*10.4)) %>% 
  #mutate(MAAP_BC_abs_637 = (MAAP_BC_abs_637*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(MAAP_BC_abs_637), !is.na(AE33_Tabs_660))

# Run the regression function and print the result
regression_results_AE33_MAAP <- york_regression_fixed(clean_data_AE33_MAAP$MAAP_BC_abs_637, clean_data_AE33_MAAP$AE33_Tabs_660)
regression_results_AE33_MAAP <- york_regression_2nd(clean_data_AE33_MAAP$MAAP_BC_abs_637, clean_data_AE33_MAAP$AE33_Tabs_660)



# Extract the regression results
slope_AE33_MAAP <-     regression_results_AE33_MAAP$slope
intercept_AE33_MAAP <- regression_results_AE33_MAAP$intercept
r_squared_AE33_MAAP <- regression_results_AE33_MAAP$r_squared
rmse_AE33_MAAP <-      regression_results_AE33_MAAP$rmse
mae_AE33_MAAP <-       regression_results_AE33_MAAP$mae
unperPAX_PAAS_IR <-       regression_results_AE33_MAAP$uncertainty_percent


# Print each value to check
print(paste("Slope:", slope1))
print(paste("Intercept:", intercept1))
print(paste("R-squared:", r_squared1))


# Plot the results with annotation
ggplot(clean_data_AE33_MAAP, aes(x = MAAP_BC_abs_637, y = AE33_Tabs_660)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  #geom_point(size = 2, shape = 1, alpha = 0.5, color = "black", fill = "blue",stroke = 0.3) +
  
  geom_abline(slope = slope_AE33_MAAP, intercept = intercept_AE33_MAAP, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(AE33[`(660 nm)`] ~ "vs" ~ MAAP[`(637 nm)`]))+ 
  #labs(title = expression(AE33[(660)] ~ "vs" ~ MAAP[(637)]~ ","~MAC[MAAP]~ "= 10.4" ~m^2*g^-1))+ 
  
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(MAAP  ~ abs[`(total,637 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33  ~ abs[`(total,660 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(b)[abs*", "*637~nm]^{MAAP}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*660~nm]^{AE33}*"("*Mm^-1*")")) +
  theme(legend.position = "") + 
  annotate("text", x = 35, y = 39, label = "(b)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 20, y = 10, 
           label = paste0("y = ", round(slope_AE33_MAAP, 2), " x", ifelse(intercept_AE33_MAAP >= 0, " + ", " - "), abs(round(intercept_AE33_MAAP, 2)),  
                          "\nR² = ", round(r_squared_AE33_MAAP , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_04a

annotate("text", 
         x = 23, y = 15, 
         label = paste0("y = ", round(slope_AE33_MAAP, 2), " x", ifelse(intercept_AE33_MAAP >= 0, " + ", " - "), abs(round(intercept_AE33_MAAP, 2)),  
                        "\nR² = ", round(r_squared_AE33_MAAP , 2), "\nRMSE = ", round(rmse_AE33_MAAP , 2), "\nMAE = ", round(mae_AE33_MAAP , 2)),
         hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_04a



########## FIGURE 4c ##########

library(IsoplotR)
library(dplyr)
library(ggplot2)

####### --->  PAX 870 VS PAAS 785 #########

# Clean data (filter out missing values based on your conditions)
clean_data_PAX_PAAS_IR <- AE33_NCORE_hourly %>%
  #filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>%
  #filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  #filter(`Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` > -8.5, `Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` <11) %>% 
  filter(`Bland-Altman_PAAS_PAX_405_DIFF` > -8.7, `Bland-Altman_PAAS_PAX_405_DIFF` <7.3) %>% 
  #mutate(PAAS_b_total_abs_785_Mm = ifelse(PAAS_b_total_abs_785_Mm > 20, NA, PAAS_b_total_abs_785_Mm)) %>%
  #mutate(PAX_Tabs870 = ifelse(PAX_Tabs870 > 15, NA, PAX_Tabs870)) %>%
  #mutate(PAAS_b_total_abs_785_Mm = (PAAS_b_total_abs_785_Mm*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(PAAS_b_total_abs_785_Mm), !is.na(PAX_Tabs870))

# Run the regression function and print the result
#regression_results_PAX_PAAS_IR <- york_regression_fixed(clean_data_PAX_PAAS_IR$PAAS_b_total_abs_785_Mm, clean_data_PAX_PAAS_IR$PAX_Tabs870)
regression_results_PAX_PAAS_IR <- york_regression_2nd(clean_data_PAX_PAAS_IR$PAAS_b_total_abs_785_Mm, clean_data_PAX_PAAS_IR$PAX_Tabs870)


# Extract the regression results
slopePAX_PAAS_IR <-     regression_results_PAX_PAAS_IR$slope
slope_se_PAX_PAAS_IR <- regression_results_PAX_PAAS_IR$slope_se
interceptPAX_PAAS_IR <- regression_results_PAX_PAAS_IR$intercept
r_squaredPAX_PAAS_IR <- regression_results_PAX_PAAS_IR$r_squared
p_value_PAX_PAAS_IR <- regression_results_PAX_PAAS_IR$p_value
rmsePAX_PAAS_IR <-      regression_results_PAX_PAAS_IR$rmse
maePAX_PAAS_IR <-       regression_results_PAX_PAAS_IR$mae



# Plot the results with annotation
ggplot(clean_data_PAX_PAAS_IR, aes(x = PAAS_b_total_abs_785_Mm, y = PAX_Tabs870)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  #geom_point(size = 2, shape = 1, alpha = 0.5, color = "black", fill = "blue",stroke = 0.3) +
  geom_abline(slope = slopePAX_PAAS_IR, intercept = interceptPAX_PAAS_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(PAX[`(870 nm)`] ~ "vs" ~ PAAS[`(785 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(PAX~abs[`(total,870 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*785~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*870~nm]^{PAX}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", x = 35, y = 39, label = "(c)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 20, y = 10, 
           label = paste0("y = ", round(slopePAX_PAAS_IR, 2), 
                         # " (±", round(slope_se_PAX_PAAS_IR, 3), ")", 
                          " x", ifelse(interceptPAX_PAAS_IR >= 0, " + ", " - "), 
                          abs(round(interceptPAX_PAAS_IR, 2)),  
                          "\nR² = ", round(r_squaredPAX_PAAS_IR , 2)),
                        #  ", p ", ifelse(p_value_PAX_PAAS_IR < 0.001, "< 0.001", 
                        #                 paste0("= ", round(p_value_PAX_PAAS_IR, 3)))),
           hjust = 0, vjust = 1, size = 4.5, color = "black")  -> plot_totalabs_IR_03a


lower_panel_longer <-  plot_grid(plot_totalabs_IR_04a + theme(legend.position = "none"), #1
                                 plot_totalabs_IR_03a + theme(legend.position = "none"), #2
                                 ncol = 2,  # Arrange in a single row with 3 columns
                                 align = 'h',  # Horizontal alignment
                                 rel_widths = c(1, 1)  # Equal width for all plots
)

# Combine the upper panel with the lower panel
combined_legend_plot_totalabs_longer <- plot_grid(
  hourly_ae33_pass_pax_longer_wavea,  # Upper panel
  lower_panel_longer,               # Lower panel with 3 plots
  nrow = 2,
  rel_heights = c(1, 1) # Adjust the relative heights
)

# Display the combined plot
print(combined_legend_plot_totalabs_UV2)

ggsave("combined_legend_plot_totalabs_longer_W8H8_revision_01.png", 
       plot = combined_legend_plot_totalabs_longer, 
       width = 8, height = 8, 
       device = "png", dpi=600)






######## Figure (6a)  ############
names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(AE33_eBC_conc_CF_660 = AE33_Tabs_CF_660/7.77) %>% 
  mutate(BC_ratio_OA = CTC_AMS_OA/(CTC_AMS_OA + MAAP_BC_637_con_mac10.4)) %>% 
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  #mutate(AE33_AAE_370_880 = ifelse(CTC_AMS_OA <= 5, NA, AE33_AAE_370_880)) %>%
  mutate(AE33_AAE_370_880 = ifelse(`MAAP_BC_637_con_mac10.4` <1 , NA, AE33_AAE_370_880)) %>%
  select(TheTime,`MAAP_BC_637_con_mac10.4`,PAX_SSA_401_based_hour,delta_co,MCE_CTC_430,AE33_AAE_370_880,CTC_AMS_OA,BC_ratio_OA) %>% 
  filter(!is.na(CTC_AMS_OA)) %>%
  filter(!is.na(AE33_AAE_370_880)) %>%
  ggplot(aes(x = PAX_SSA_401_based_hour, y = AE33_AAE_370_880, color = BC_ratio_OA)) +
  geom_point(shape = 16, size = 3, alpha=0.9) +
  theme_bw() + 
  scale_color_stepsn(
    colours = c("black", "#56B4E9", "blue", "red","yellow"),
    limits = c(0.6, 1),
    breaks = c(0.6, 0.7, 0.8, 0.9, 0.95,1),
    labels = c("0.6", "0.7", "0.8", "0.9", "0.95","1.0"),
    name = expression(frac(OA, eBC+OA))
  ) +
  scale_y_continuous(breaks = c(0,1, 1.5, 2, 3), limits = c(0,3))+
  scale_x_continuous(breaks = c(0.7,0.8,0.9,1.0), limits = c(0.65,1.15))+
  #scale_x_continuous(breaks = c(0.6,0.7,0.8,0.9,1.0), limits = c(0.5,1.15))+
  
  
  labs(title = expression(AAE[370/880]^{AE33} ~ "vs" ~ SSA[""*401~nm]^{PAX}*""**"")) + 
  annotate("text", x = 0.65, y = (0.1 + 0.3), parse = TRUE,
           label = as.character(expression(paste(OA[AMS] > 5*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  annotate("text", x = 0.65, y = (0.001 + 0.1), parse = TRUE,
           label = as.character(expression(paste(eBC[`MAAP`] > 1*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=16),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.2, "inch"),
    legend.key.width = unit(0.05, "inch"),
    legend.text = element_text(size=12),
    legend.position = c(0.81, 0.5)
  ) + 
  annotate("text", x = 0.65, y = 3, label = "(a)", size = 5, fontface = "bold") +
  xlab(expression(italic(SSA)[""*401~nm]^{PAX}*""**"")) +
  ylab(expression(italic(AAE)[""*370/880]^{"AE33"}*""**"")) -> AAE_AE33_SSA_OA_BC_RATIO



######## Figure (6b)  ############


AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(BC_ratio_OA = CTC_AMS_OA/(CTC_AMS_OA + MAAP_BC_637_con_mac10.4)) %>%
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(CTC_AMS_OA <= 5, NA, PAAS_AAE_405_785)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(`MAAP_BC_637_con_mac10.4` <1 , NA, PAAS_AAE_405_785)) %>%
  select(TheTime,`MAAP_BC_637_con_mac10.4`,PAX_SSA_401_based_hour,delta_co,MCE_CTC_430,PAAS_AAE_405_785,CTC_AMS_OA,BC_ratio_OA) %>% 
  filter(!is.na(CTC_AMS_OA)) %>%
  filter(!is.na(PAAS_AAE_405_785)) %>%
  ggplot(aes(x = PAX_SSA_401_based_hour, y = PAAS_AAE_405_785, 
             color = BC_ratio_OA)) +
  geom_point(shape = 16, size = 3, alpha=0.9) +
  theme_bw() + 
  scale_color_stepsn(
    colours = c("black", "#56B4E9", "blue", "red2","yellow"),
    limits = c(0.6, 1),
    breaks = c(0.6, 0.7, 0.8, 0.9, 0.95,1),
    labels = c("0.6", "0.7", "0.8", "0.9", "0.95","1.0"),
    name = expression(frac(OA, eBC+OA))
  )+
  scale_y_continuous(breaks = c(0,1, 1.5, 2, 3), limits = c(0,3))+
  scale_x_continuous(breaks = c(0.7,0.8,0.9,1.0), limits = c(0.65,1.15))+
  labs(title = expression(AAE[405/785]^{"PAAS-4"*lambda} ~ "vs" ~ SSA[PAX])) + 
  annotate("text", x = 0.65, y = (0.1 + 0.3), parse = TRUE,
           label = as.character(expression(paste(OA[AMS] > 5*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  annotate("text", x = 0.65, y = (0.001 + 0.1), parse = TRUE,
           label = as.character(expression(paste(eBC[`MAAP*`] > 1*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=14),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.2, "inch"),
    legend.key.width = unit(0.05, "inch"),
    legend.text = element_text(size=12),
    legend.position = c(0.81, 0.5)
  ) + 
  annotate("text", x = 0.65, y = 3, label = "(b)", size = 5, fontface = "bold") +
  xlab(expression(italic(SSA)[""*401~nm]^{PAX}*""**"")) +
  ylab(expression(italic(AAE)[""*405/785]^{"PAAS-4"*lambda}*""**"")) -> AAE_PAAS_SSA_OA_BC_RATIO



ggsave("AAE_PAAS_SSA_OA_BC_RATIO_revision02.png", 
       plot = AAE_PAAS_SSA_OA_BC_RATIO, 
       width = 6, height = 4, 
       device = "png", dpi = 600)


######## Figure (6b1)  ############

names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(BC_ratio_OA = CTC_AMS_OA/(CTC_AMS_OA + MAAP_BC_637_con_mac10.4)) %>%
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(CTC_AMS_OA <= 5, NA, PAAS_AAE_405_785)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(`MAAP_BC_637_con_mac10.4` <1 , NA, PAAS_AAE_405_785)) %>%
  select(TheTime,`MAAP_BC_637_con_mac10.4`,PAX_SSA_401_based_hour,delta_co,MCE_CTC_430,PAAS_AAE_405_785,CTC_AMS_OA,BC_ratio_OA,PAX_AAE_401_870_WITH_negative) %>% 
  filter(!is.na(CTC_AMS_OA)) %>%
  filter(!is.na(PAAS_AAE_405_785)) %>%
  ggplot(aes(x = PAX_SSA_401_based_hour, y = PAX_AAE_401_870_WITH_negative, 
             color = BC_ratio_OA)) +
  geom_point(shape = 16, size = 3, alpha=0.9) +
  theme_bw() + 
  scale_color_stepsn(
    colours = c("black", "#56B4E9", "blue", "red2","yellow"),
    limits = c(0.6, 1),
    breaks = c(0.6, 0.7, 0.8, 0.9, 0.95,1),
    labels = c("0.6", "0.7", "0.8", "0.9", "0.95","1.0"),
    name = expression(frac(OA, eBC+OA))
  )+
  scale_y_continuous(breaks = c(0,1, 1.5, 2, 3), limits = c(0,3))+
  scale_x_continuous(breaks = c(0.7,0.8,0.9,1.0), limits = c(0.65,1.15))+
  labs(title = expression(AAE[401/870]^{PAX} ~ "vs" ~ SSA[""*401~nm]^{PAX}*""**"")) + 
  annotate("text", x = 0.65, y = (0.1 + 0.3), parse = TRUE,
           label = as.character(expression(paste(OA[AMS] > 5*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  annotate("text", x = 0.65, y = (0.001 + 0.1), parse = TRUE,
           label = as.character(expression(paste(eBC[`MAAP*`] > 1*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=16),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.2, "inch"),
    legend.key.width = unit(0.05, "inch"),
    legend.text = element_text(size=12),
    legend.position = c(0.81, 0.5)
  ) + 
  annotate("text", x = 0.65, y = 3, label = "(b)", size = 5, fontface = "bold") +
  xlab(expression(italic(SSA)[""*401~nm]^{PAX}*""**"")) +
  ylab(expression(italic(AAE)[""*401/870]^{PAX}*""**"")) -> AAE_PAX_SSA_OA_BC_RATIO


ggsave("AAE_PAX_SSA_OA_BC_RATIO_revision02.png", 
       plot = AAE_PAX_SSA_OA_BC_RATIO, 
       width = 6, height = 4, 
       device = "png", dpi = 600)



######## Figure (6b2)  ############

names(AE33_NCORE_hourly)
AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(pm2p5_ratio_OA = pm2p5/(pm2p5 + MAAP_BC_637_con_mac10.4)) %>% 
  mutate(pm2p5_ratio_OA = ifelse(pm2p5_ratio_OA <= 0, NA, pm2p5_ratio_OA)) %>%
  mutate(pm2p5_ratio_OA = ifelse(pm2p5_ratio_OA <= 0, NA, pm2p5_ratio_OA)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(CTC_AMS_OA <= 5, NA, PAAS_AAE_405_785)) %>%
  mutate(PAAS_AAE_405_785 = ifelse(`MAAP_BC_637_con_mac10.4` <1 , NA, PAAS_AAE_405_785)) %>%
  select(TheTime,`MAAP_BC_637_con_mac10.4`,AE33_AAE_370_880,PAX_SSA_401_based_hour,delta_co,MCE_CTC_430,PAAS_AAE_405_785,CTC_AMS_OA,PAX_AAE_401_870_WITH_negative, pm2p5_ratio_OA) %>% 
  filter(!is.na(CTC_AMS_OA)) %>%
  filter(!is.na(PAAS_AAE_405_785)) %>%
  ggplot(aes(x = PAX_SSA_401_based_hour, y = AE33_AAE_370_880, 
             color = pm2p5_ratio_OA)) +
  geom_point(shape = 16, size = 3, alpha=0.9) +
  theme_bw() + 
  scale_color_stepsn(
    colours = c("black", "#56B4E9", "blue", "red2","yellow"),
    limits = c(0.6, 1),
    breaks = c(0.6, 0.7, 0.8, 0.9, 0.95,1),
    labels = c("0.6", "0.7", "0.8", "0.9", "0.95","1.0"),
    name = expression(frac(PM[2.5] , PM[2.5] + eBC[`MAAP`]))
  )+
  scale_y_continuous(breaks = c(0,1, 1.5, 2, 3), limits = c(0,3))+
  scale_x_continuous(breaks = c(0.7,0.8,0.9,1.0), limits = c(0.65,1.15))+
  labs(title = expression(AAE[405/785]^{"AE33"} ~ "vs" ~ SSA[PAX])) + 
  annotate("text", x = 0.65, y = (0.1 + 0.3), parse = TRUE,
           label = as.character(expression(paste(OA[AMS] > 5*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  annotate("text", x = 0.65, y = (0.001 + 0.1), parse = TRUE,
           label = as.character(expression(paste(eBC[`MAAP*`] > 1*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=14),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.2, "inch"),
    legend.key.width = unit(0.05, "inch"),
    legend.text = element_text(size=12),
    legend.position = c(0.81, 0.5)
  ) + 
  annotate("text", x = 0.65, y = 3, label = "(b)", size = 5, fontface = "bold") +
  xlab(expression(italic(SSA)[""*401~nm]^{PAX}*""**"")) +
  ylab(expression(italic(AAE)[""*370/880]^{"AE33"}*""**"")) #-> AAE_PAX_SSA_OA_BC_RATIO



######## Figure (6c) #########
abs_scat_coating_Miea <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/40nm_CoreBC_abs_scat_coating_Mie.csv")
abs_scat_coating_MieaA <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/40nm_CoreBC_abs_scat_coating_Mie_01.csv")

head(abs_scat_coating_MieaA)

abs_scat_coating_MieaA %>% 
  #select(coating_fraction, AAE_370_880_BrC_coating, AAE_520_880_BrC_coating, 
  #       AAE_370_880_scatter_coating, AAE_520_880_scatter_coating) %>% 
  select(coating_fraction, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = coating_fraction, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 0.65, xend = 0.65, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
              # "AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
              # "AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
              # "AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
              # "AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
             #  "AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
              # "AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Fraction[Mie]),
    x = "coating mass fraction",
    y = "AAE"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0.5, 1, 1.5, 2, 3), limits = c(0, 3), expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.7, 1), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 3))+
  #xlim(0, 1) +    
  theme(legend.text = element_text(size = 9)) + #10
  #theme(legend.position = c(0.28, 0.18)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 16)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(~`mass fraction = `~frac(italic(m)[`coating,OA`], 
                                       italic(m)[`core,BC`] + italic(m)[`coating,OA`]))) +
  annotate("text", x = 0.2, y = 0.2, label = "(c)", size = 5, fontface = "bold") +
  annotate("text", x = 0.3, y = 0.5,
           label = "BC[core]~`= 40nm`",
           parse = TRUE, color = "black", size = 5) +
  annotate("text", x = 0.3, y = 2.1,
           label = "k[`BrC,370`]~`= 0.02`",
           parse = TRUE, color = "blue", size = 5) +
  geom_rect(aes(xmin = 0.95, xmax = 1, ymin = 1.3, ymax = 2.5), 
            fill = "brown", alpha = 0.005, inherit.aes = FALSE) +
  geom_rect(aes(ymin = 1.4, ymax = 2.0, xmin = 0.8, xmax = 0.95), 
            fill = "#56B4E9", alpha = 0.005, inherit.aes = FALSE) +
  annotate("label", x = 0.86, y = 2.3, label = "moderate to thick coating", 
           size = 3, fontface = "bold", color = "black", angle = 50,
           fill = "white", label.size = 0) +
  geom_rect(aes(ymin = 1.2, ymax = 1.5, xmin = 0.2, xmax = 0.8), 
            fill = "black", alpha = 0.005, inherit.aes = FALSE) +
  annotate("label", x = 0.65, y = 1.15, label = "thin  coating", 
           size = 3, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) #-> plot_AAE_k_02a 


######## Figure (6d) #########
abs_scat_coating_Miea <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/40nm_CoreBC_abs_scat_coating_Mie.csv")

names(abs_scat_coating_Miea)

abs_scat_coating_Miea %>% 
  #select(core_cot, AAE_370_880_BrC_coating, AAE_520_880_BrC_coating, AAE_370_880_scatter_coating, AAE_520_880_scatter_coating) %>% 
  select(core_cot, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = core_cot, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 55, xend = 55, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 95, xend = 95, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
              # "AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               #"AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Thickness[Mie]),
    x = "OA coating thickness (nm)"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 150) +    
  theme(legend.text = element_text(size = 9)) + #10
  #theme(legend.position = c(0.3, 0.80)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 16)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(atop(
    "OA coating thickness",
    paste("(", nm, ")")
  )))+ 
  annotate("text", x = 0.0, y = 0.2, label = "(d)", size = 5, fontface = "bold") +
  #annotate("text", x = 25, y = 0.5,
  #         label = "BC[core]~`= 40nm`",
  #         parse = TRUE, color = "black", size = 5) +
  annotate("label", x = 75, y = 0.75, label = "OA:BC fraction", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 75, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 75, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) -> plot_mie_AAE_OA_coating_thick 



library(ggpubr)

# Upper panel  AAE_PAAS_SSA_OA_BC_RATIO
upper <- ggarrange(AAE_AE33_SSA_OA_BC_RATIO, AAE_PAAS_SSA_OA_BC_RATIO, 
                   ncol = 2, nrow = 1)



# Lower panel (centered) 
#lower1 <- ggarrange(NULL, plot_AAE_k_02a, NULL,  ncol = 3, nrow = 1,  widths = c(1, 4, 1))

#lower2 <- ggarrange(plot_AAE_k_02a, plot_mie_AAE_OA_coating_thick,  ncol = 2, nrow = 1)
lower2 <- ggarrange(AE33_AAE_OA_BC_RATIO, PAAS_AAE_OA_BC_RATIO,  ncol = 2, nrow = 1)

# Combine
#ggarrange(upper, lower1, ncol = 1, nrow = 2) -> AAE_SSA_winter_Mie_AAE_01a_NEW
ggarrange(upper, lower2, ncol = 1, nrow = 2) -> AAE_SSA_winter_Mie_AAE_01a_NEWaa

ggsave("AAE_SSA_winter_Mie_AAE_01a_NEW_8x8_revision06a.png", 
       plot = AAE_SSA_winter_Mie_AAE_01a_NEWaa, 
       width = 8, height = 8, 
       device = png, dpi =600)






######### ---> ########### 

######## Figure (6c) with RI = 1.5 #########
abs_scat_coating_Mie1.5a <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/40nm_1.5_CoreBC_abs_scat_coating_Mie.csv")

names(abs_scat_coating_Mie1.5a)

abs_scat_coating_Mie1.5a %>% 
  select(coating_fraction, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = coating_fraction, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 0.65, xend = 0.65, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               #"AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               #"AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Fraction[Mie]),
    x = "coating mass fraction",
    y = "AAE"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 1) +    
  theme(legend.text = element_text(size = 10)) + #10
  #theme(legend.position = c(0.28, 0.18)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 16)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(~`mass fraction = `~frac(italic(m)[`coating,OA`], 
                                       italic(m)[`core,BC`] + italic(m)[`coating,OA`]))) +
  annotate("text", x = 0.0, y = 0.2, label = "(a)", size = 5, fontface = "bold") +
  annotate("text", x = 0.15, y = 1.8, label = "for RI, n = 1.5 ", size = 3.5, fontface = "bold") +
  annotate("text", x = 0.3, y = 0.5,
           label = "BC[core]~`= 40nm`",
           parse = TRUE, color = "red", size = 5) +
  annotate("label", x = 0.86, y = 2.3, label = "moderate to thick coating", 
           size = 3, fontface = "bold", color = "black", angle = 50,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.65, y = 1.15, label = "thin  coating", 
           size = 3, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) -> plot_40NM_mie_AAE_oa_bc_RI_1.5_01 



######## Figure (6d) with RI = 1.5 #########
abs_scat_coating_Mie1.5a <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/40nm_1.5_CoreBC_abs_scat_coating_Mie.csv")

names(abs_scat_coating_Mie1.5a)

abs_scat_coating_Mie1.5a %>% 
  select(core_cot, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = core_cot, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 55, xend = 55, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 95, xend = 95, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               #"AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               #"AAE_520_880_BrC_coating" = "red",
               "AAE_370_880_scatter_coating" = "purple"),
               #"AAE_520_880_scatter_coating" = "orange"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               #"AAE_520_880_BrC_coating" = expression(`(BrC coating,520/880)`),
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`)
               #"AAE_520_880_scatter_coating" = expression(`(non absorbing coating, 520/880)`))
  )) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Thickness[Mie]),
    x = "OA coating thickness (nm)"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 150) +    
  theme(legend.text = element_text(size = 10)) + #10
  #theme(legend.position = c(0.3, 0.80)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 16)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(atop(
    "OA coating thickness",
    paste("(", nm, ")")
  )))+ 
  annotate("text", x = 0.0, y = 0.2, label = "(b)", size = 5, fontface = "bold") +
  annotate("text", x = 25, y = 1.8, label = "for RI, n = 1.5 ", size = 3.5, fontface = "bold") +
  annotate("text", y = 1, x = 25,
           label = "BC[core]~`= 40nm`",
           parse = TRUE, color = "red", size = 5) +
  annotate("label", x = 75, y = 0.75, label = "OA:BC fraction", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 75, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 75, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) -> plot_40NM_mie_AAE_OA_coating_thick_RI_1.5_01 



ggarrange(plot_40NM_mie_AAE_oa_bc_RI_1.5_01, plot_40NM_mie_AAE_OA_coating_thick_RI_1.5_01, 
          ncol = 2, nrow = 1) -> Mie_RI_1.5_40NM_AAE_OA_FRACTION_OA_COATING_01

ggsave("Mie_RI_1.5_40NM_AAE_OA_FRACTION_OA_COATING_01_8x4_revision02.png", 
       plot = Mie_RI_1.5_40NM_AAE_OA_FRACTION_OA_COATING_01, 
       width = 8, height = 4, 
       device = png, dpi =600)


######## PROPOSED Figure (6c) 70 nm  RI = 1.45 #########
#abs_scat_coating_Mie_70nm <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/70nm_CoreBC_abs_scat_coating_Mie.csv")
abs_scat_coating_Mie_70nma <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/70nm_CoreBC_abs_scat_coating_Mie_02.csv")

names(abs_scat_coating_Mie_70nma)

abs_scat_coating_Mie_70nma %>% 
  select(coating_fraction, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = coating_fraction, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 0.65, xend = 0.65, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Fraction[Mie]),
    x = "coating mass fraction",
    y = "AAE"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 1) +    
  theme(legend.text = element_text(size = 10)) + 
  #theme(legend.position = c(0.28, 0.18)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 16)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(~`mass fraction = `~frac(italic(m)[`coating,OA`], 
                                       italic(m)[`core,BC`] + italic(m)[`coating,OA`]))) +
  annotate("text", x = 0.0, y = 0.1, label = "(c)", size = 5, fontface = "bold") +
  annotate("text", x = 0.15, y = 1.8, label = "for RI, n = 1.45 ", size = 3.5, fontface = "bold") +
  annotate("text", x = 0.3, y = 0.5,
           label = "BC[core]~`= 70nm`",
           parse = TRUE, color = "red", size = 5) +
  annotate("label", x = 0.78, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) #-> plot_70NM_mie_AAE_oa_bc_RI_1.45_01 




######## PROPOSED Figure (6c) 70 nm  RI = 1.5 #########
#abs_scat_coating_Mie_70nm <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/70nm_CoreBC_abs_scat_coating_Mie.csv")
abs_scat_coating_Mie_70_1.5nma <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/70nm_1.5_CoreBC_abs_scat_coating_Mie_02.csv")

names(abs_scat_coating_Mie_70_1.5nma)

abs_scat_coating_Mie_70_1.5nma %>% 
  select(coating_fraction, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = coating_fraction, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 0.65, xend = 0.65, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Fraction[Mie]),
    x = "coating mass fraction",
    y = "AAE"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 1) +    
  theme(legend.text = element_text(size = 10)) + 
  #theme(legend.position = c(0.28, 0.18)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 14)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(~`mass fraction = `~frac(italic(m)[`coating,OA`], 
                                       italic(m)[`core,BC`] + italic(m)[`coating,OA`]))) +
  annotate("text", x = 0.0, y = 0.1, label = "(d)", size = 5, fontface = "bold") +
  annotate("text", x = 0.15, y = 1.8, label = "for RI, n = 1.5 ", size = 3.5, fontface = "bold") +
  annotate("text", x = 0.3, y = 0.5,
           label = "BC[core]~`= 70nm`",
           parse = TRUE, color = "red", size = 5) +
  annotate("label", x = 0.78, y = 0.5, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 0.25, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) -> plot_70NM_mie_AAE_oa_bc_RI_1.5_01 





ggarrange(plot_40NM_mie_AAE_oa_bc_RI_1.5_01, plot_40NM_mie_AAE_OA_coating_thick_RI_1.5_01, 
          plot_70NM_mie_AAE_oa_bc_RI_1.45_01,plot_70NM_mie_AAE_oa_bc_RI_1.5_01,
          ncol = 2, nrow = 2) -> Mie_RI_1.5_40_70nm_AAE_OA_FRACTION_OA_COATING_01

ggsave("Mie_RI_1.5_40NM_70NM_AAE_OA_FRACTION_OA_COATING_01_8x4_revision04.png", 
       plot = Mie_RI_1.5_40_70nm_AAE_OA_FRACTION_OA_COATING_01, 
       width = 8, height =8, 
       device = png, dpi =600)





######## PROPOSED Figure (6c) 100 nm  #########
abs_scat_coating_Mie_100nma <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/coating/100nm_CoreBC_abs_scat_coating_Mie_01.csv")

names(abs_scat_coating_Mie_100nma)

abs_scat_coating_Mie_100nma %>% 
  select(coating_fraction, AAE_370_880_BrC_coating, 
         AAE_370_880_scatter_coating) %>% 
  melt(id.vars = 1:1) %>% 
  mutate(variable_type = ifelse(grepl("scatter", variable), "scatter", "BrC")) %>%
  ggplot(aes(x = coating_fraction, y = value, color = variable, fill = variable)) + 
  geom_line(size = 0.3) +
  # geom_point(aes(size = variable_type), alpha = 0.6, shape = 21, color = "black") +
  geom_point(aes(size = variable_type), alpha = 0.9, shape = 16) +
  scale_size_manual(values = c("BrC" = 3, "scatter" = 3), guide = "none") +
  geom_segment(aes(x = 0.65, xend = 0.65, y = 0.0, yend = 2.0), #0.8
               linetype = "dashed", color = "black", size = 1) +  
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0.0, yend = 2.0), #0.95
               linetype = "dashed", color = "black", size = 1) + 
  geom_hline(yintercept = 1.5, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  scale_fill_manual(
    values = c("AAE_370_880_BrC_coating" = "blue", 
               "AAE_370_880_scatter_coating" = "purple"),
    labels = c("AAE_370_880_BrC_coating" = expression(`(BrC coating,370/880)`), 
               "AAE_370_880_scatter_coating" = expression(`(non absorbing coating, 370/880)`))
  ) + 
  labs(color = NULL, fill = NULL) +
  labs(
    title = expression(AAE[Mie] ~ "vs" ~ Coating~Fraction[Mie]),
    x = "coating mass fraction",
    y = "AAE"
  ) +  
  theme_bw() + 
  scale_y_continuous(breaks = c(0, 1, 1.5, 2, 3), limits = c(0, 3)) +
  xlim(0, 1) +    
  theme(legend.text = element_text(size = 10)) + 
  #theme(legend.position = c(0.28, 0.18)) +
  theme(legend.position = c(0.37, 0.85)) +
  
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(override.aes = list(size = 3))) +
  theme(axis.title = element_text(face = "plain", size = 14, color = "black"),
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(lineheight = .8, face = "plain", size = 14)) +
  theme(legend.key.height = unit(0.2, "inch")) +
  ylab(bquote(italic(AAE[`Mie, calculation`]))) +
  xlab(bquote(~`mass fraction = `~frac(italic(m)[`coating,OA`], 
                                       italic(m)[`core,BC`] + italic(m)[`coating,OA`]))) +
  #annotate("text", x = 0.0, y = 3, label = "(c)", size = 5, fontface = "bold") +
  annotate("text", x = 0.3, y = 1.0,
           label = "BC[core]~`= 100nm`",
           parse = TRUE, color = "black", size = 5) +
  annotate("label", x = 0.78, y = 1.25, label = "Fairbanks", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) +
  annotate("label", x = 0.78, y = 1.0, label = "region", 
           size = 3.7, fontface = "bold", color = "black", angle = 10,
           fill = "white", label.size = 0) #-> plot_AAE_k_02a 






######### ---> ###########

######### Figure 8 (a)   ##########


#######  --> 2022 WINTER EPA EC, MAAP, AE33,PAAS,PAX BC   #########

WINTER_EC_EPA_ALPACA <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/WINTER_EC_EPA_ALPACA.csv")


names(WINTER_EC_EPA_ALPACA)
WINTER_EC_EPA_ALPACA %>%
  #mutate(`AE33_BC_CF_660_MAC_10.35` = AE33_BC_CF_660_MAC_10.35 * 1.3) %>%  # Create the modified variable first
  #mutate(
  #  EPA_EC_TOR = (EPA_EC_TOR/3),  # EPA TOR based on AE33 eBC correction factor applied
  #  EPA_EC_TOT = (EPA_EC_TOR/1.1)  # EPA TOT based on AE33 eBC correction factor applied
  #) %>%  
  select(date_local, 
         EPA_EC_TOR, ###### EPA OA = OC*1.6 same for TCA
         EPA_EC_TOT,
         `MAAP_BC_10.4_660` = MAAP_BC_637_MAC_10.4,
         `PAAS_BC_660` = PAAS_BC_con_660_MAC_11.2,
         `AE33_BC_CF_660` = AE33_BC_CF_660_MAC_10.35,  # Fixed: removed extra parentheses
         PAX_BC_870 = PAX_BC_conc_870) %>%
  # Filter for January-February 2022
  filter(date_local >= as.Date("2022-01-02") & date_local <= as.Date("2022-02-28")) %>%
  mutate(date_local = as.Date(date_local)) %>%  # Add this line
  pivot_longer(cols = -date_local, 
               names_to = "variable", 
               values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("EPA_EC_TOR", "EPA_EC_TOT", 
                                                "AE33_BC_CF_660", "PAAS_BC_660", 
                                                "PAX_BC_870", "MAAP_BC_10.4_660"))) %>%
  ggplot(aes(x = date_local, y = value, color = variable)) +
  geom_line(size = 1) + #geom_line(size = 1.5) +  # Slightly thicker lines for better visibility
  geom_point(size = 4) + #geom_point(size = 4) +
  #geom_point(aes(shape = variable), size = 4)+
  labs(title = "",
       #title = expression(OA[EPA] ~ "," ~ OA[`TCA-08`] ~ "," ~ OA[`AMS`] ~ "and" ~ OA[`ACSM`] ~ ("2022,  Jan and Feb")),
       x = "Date") +
  theme_bw() +  
  ylim(0,4)+
  ylab(expression("(" * mu * g ~ m^-3 * ")")) +
  annotate("text", x = as.Date("2022-01-07"), y = 4, label = "(a)", size = 5, fontface = "bold")+
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.position = c(0.6, 0.8),
    axis.title = element_text(face = "plain", size = 14, color = "black"),
    # axis.text = element_text(size = 14, face = "plain", color = "black"),
    axis.text = element_text(size = 14, face = "plain", color = "black"),
    axis.title.x = element_text(vjust = 0.1),
    axis.text.y = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.0, angle = -45),
    plot.title = element_text(size = 16),
    panel.grid.minor = element_line(color = "grey90", size = 0.3),
    panel.grid.major = element_line(color = "grey85", size = 0.5),
    axis.ticks.length.x = unit(0.3, "cm"),              # Major tick length (longer)
    ggh4x.axis.ticks.length.minor = rel(0.5)
  ) +
  scale_color_manual(name = NULL,
                     values = c("EPA_EC_TOR" = "orange",      # yellow
                                "EPA_EC_TOT" = "#999999",      # gray
                                "AE33_BC_CF_660" = "red",      # Vermillion 
                                "PAAS_BC_660" = "blue",        #  Bluish green
                                "PAX_BC_870" = "#56B4E9",      # Blue
                                "MAAP_BC_10.4_660" = "#000000"), # Black
                     labels = c("EPA_EC_TOR" = expression(EPA~EC[paste("(TOR)")]),
                                "EPA_EC_TOT" = expression(EPA~EC[paste("(TOT)")]),
                                "AE33_BC_CF_660" = expression(`AE33*`~eBC[paste("(660 nm, MAC="~"10.35"~"m"^2*" "*"g"^{-1}*")")]),
                                "PAAS_BC_660" = expression({PAAS-4*lambda~eBC}[paste("(660 nm, MAC="~"11.2"~"m"^2*" "*"g"^{-1}*")")]),
                                "PAX_BC_870" = expression(PAX~eBC[paste("(870 nm, MAC="~"4.74"~"m"^2*" "*"g"^{-1}*")")]),
                                "MAAP_BC_10.4_660" = expression(MAAP~eBC[paste("(637 nm, MAC="~"10.4"~"m"^2*" "*"g"^{-1}*")")]))) +
  scale_shape_manual(name = NULL, 
                     values = c("EPA_EC_TOR" = 5,      # Fixed: was EPA_OC
                                "EPA_EC_TOT" = 16,    # Fixed: was TCA-08_OC  
                                "AE33_BC_CF_660" = 16,
                                "PAAS_BC_660" = 16,
                                "PAX_BC_870" = 16,
                                "MAAP_BC_10.4_660" = 16)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  scale_x_date(
    breaks = seq(as.Date("2022-01-02"), as.Date("2022-02-28"), by = "5 days"),
    minor_breaks = seq(as.Date("2022-01-02"), as.Date("2022-02-28"), by = "1 day"),
    date_labels = "%m/%d",
    limits = c(as.Date("2021-12-30"), as.Date("2022-03-03")),
    expand = c(0, 0),
    guide = "axis_minor")   -> time_series_EPA_EC_MAAP_PAAS_PAX_AE33_eBC


ggsave("time_series_EPA_EC_MAAP_PAAS_PAX_AE33_eBC_6x6_revision01.png", 
       plot = time_series_EPA_EC_MAAP_PAAS_PAX_AE33_eBC, 
       width = 6, height =6, 
       device = png, dpi =600)

######### Figure 8 (b)   ##########

########### --> YORK REGRESSION EPA TOR  EC, AE33 WINTER  #############


names(WINTER_EC_EPA_ALPACA)


names(WINTER_EC_EPA_ALPACA)
clean_data_WINTER_AE33_EPA_TOR_EC <- WINTER_EC_EPA_ALPACA %>%
  select(EPA_EC_TOR, AE33_BC_CF_660_MAC_10.35,MAAP_BC_637_MAC_10.4,PAX_BC_conc_870,PAAS_BC_con_660_MAC_11.2) %>%  # Add your date column name here
  filter(!is.na(EPA_EC_TOR), !is.na(AE33_BC_CF_660_MAC_10.35)) 

# Run the regression function and print the result
#regression_results_EPA_TOR_AE33 <- york_regression(clean_data_WINTER_AE33_EPA_TOR_EC$EPA_EC_TOR, clean_data_WINTER_AE33_EPA_TOR_EC$AE33_BC_CF_660_MAC_10.35)
regression_results_EPA_TOR_AE33 <- york_regression_2nd(clean_data_WINTER_AE33_EPA_TOR_EC$EPA_EC_TOR, clean_data_WINTER_AE33_EPA_TOR_EC$AE33_BC_CF_660_MAC_10.35)


# Extract the regression results
slopeEPA_TOR_AE33 <- regression_results_EPA_TOR_AE33$slope
interceptEPA_TOR_AE33 <- regression_results_EPA_TOR_AE33$intercept
r_squaredEPA_TOR_AE33 <- regression_results_EPA_TOR_AE33$r_squared
rmseEPA_TOR_AE33 <- regression_results_EPA_TOR_AE33$rmse
maeEPA_TOR_AE33 <- regression_results_EPA_TOR_AE33$mae


# Plot the results with annotation
ggplot(clean_data_WINTER_AE33_EPA_TOR_EC, aes(x = EPA_EC_TOR, y = AE33_BC_CF_660_MAC_10.35)) +
  geom_point(size = 3, alpha = 0.8, color = "black") +
  geom_abline(slope = slopeEPA_TOR_AE33, intercept = interceptEPA_TOR_AE33, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,4)+   xlim(0,4)+ 
  labs(title = expression(eBC[`AE33*`] ~ "vs" ~ EC_TOR[`EPA`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 12, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 12, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(bquote(italic(EC_TOR)[NCore]^{`EPA`}*~"("*mu*g~m^-3*")")) +
  ylab(expression(italic(eBC)[NCore]^{`AE33*`}*~"("*mu*g~m^-3*")")) +
  theme(legend.position = "") +
  annotate("text", x = 3.5, y = 4, label = "(b)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0.1, y = 4, 
           label = paste0("y = ", round(slopeEPA_TOR_AE33, 2), " x", ifelse(interceptEPA_TOR_AE33 >= 0, " + ", " - "), abs(round(interceptEPA_TOR_AE33, 2)),
                          "\nR² = ", round(r_squaredEPA_TOR_AE33 , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_AE33_EPA_TORun



######### Figure 8 (c)   ##########

########### --> YORK REGRESSION EPA TOT  EC, AE33 WINTER  #############


names(WINTER_EC_EPA_ALPACA)


names(WINTER_EC_EPA_ALPACA)
clean_data_WINTER_AE33_EPAtot_EC <- WINTER_EC_EPA_ALPACA %>%
  select(date_local,EPA_EC_TOT, EPA_EC_TOR, AE33_BC_CF_660_MAC_10.35) %>%  # Add your date column name here
  filter(!is.na(EPA_EC_TOT), !is.na(AE33_BC_CF_660_MAC_10.35)) 

# Run the regression function and print the result
#regression_results_EPAtot_AE33 <- york_regression(clean_data_WINTER_AE33_EPAtot_EC$EPA_EC_TOT, clean_data_WINTER_AE33_EPAtot_EC$AE33_BC_CF_660_MAC_10.35)
regression_results_EPAtot_AE33 <- york_regression_2nd(clean_data_WINTER_AE33_EPAtot_EC$EPA_EC_TOT, clean_data_WINTER_AE33_EPAtot_EC$AE33_BC_CF_660_MAC_10.35)


# Extract the regression results
slopeEPAtot_AE33 <- regression_results_EPAtot_AE33$slope
interceptEPAtot_AE33 <- regression_results_EPAtot_AE33$intercept
r_squaredEPAtot_AE33 <- regression_results_EPAtot_AE33$r_squared
rmseEPAtot_AE33 <- regression_results_EPAtot_AE33$rmse
maeEPAtot_AE33 <- regression_results_EPAtot_AE33$mae


# Plot the results with annotation
ggplot(clean_data_WINTER_AE33_EPAtot_EC, aes(x = EPA_EC_TOT, y = AE33_BC_CF_660_MAC_10.35)) +
  geom_point(size = 3, alpha = 0.8, color = "black") +
  geom_abline(slope = slopeEPAtot_AE33, intercept = interceptEPAtot_AE33, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,4)+   xlim(0,4)+ 
  labs(title = expression(eBC[`AE33*`] ~ "vs" ~ EC_TOT[`EPA`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 12, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 12, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(bquote(italic(EC_TOT)[NCore]^{`EPA`}*~"("*mu*g~m^-3*")")) +
  ylab(expression(italic(eBC)[NCore]^{`AE33*`}*~"("*mu*g~m^-3*")")) +
  theme(legend.position = "") +
  annotate("text", x = 3.5, y = 4, label = "(c)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0.1, y = 4, 
           label = paste0("y = ", round(slopeEPAtot_AE33, 2), " x", ifelse(interceptEPA_TOR_AE33 >= 0, " + ", " - "), abs(round(interceptEPA_TOR_AE33, 2)), 
                          "\nR² = ", round(r_squaredEPAtot_AE33 , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_AE33_EPA_TOTun



grid.arrange(plot_AE33_EPA_TORun, plot_AE33_EPA_TOTun,ncol = 1) -> plot_AE33_EPA_TORun_TORun

grid.arrange(time_series_EPA_EC_MAAP_PAAS_PAX_AE33_eBC, plot_AE33_EPA_TORun_TORun,ncol = 2,
             widths = c(2, 1)) -> plot_TIMES_SERIES_AE33_EPA_TORun_TORun

ggsave("plot_TIMES_SERIES_AE33_EPA_TORun_TORun_8.5x6E_revision01.png", 
       plot = plot_TIMES_SERIES_AE33_EPA_TORun_TORun, 
       width = 8.5, height = 6, 
       device = "png", dpi=600)



######### --> ############

######### Supporting Material Figures ############

######### --> ############





####### Figure S1 (a)  #########

####### --> PM2.5  VS BC  #########

names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_pm_maap_bc <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(!is.na(pm2p5), !is.na(MAAP_BC_637_con_mac10.4))

# Run the regression function and print the result
regression_results_pm_maap_bc <- york_regression_2nd(clean_data_pm_maap_bc$pm2p5, clean_data_pm_maap_bc$MAAP_BC_637_con_mac10.4)


# Extract the regression results
slopepm_maap_bc <- regression_results_pm_maap_bc$slope
interceptpm_maap_bc <- regression_results_pm_maap_bc$intercept
r_squaredpm_maap_bc <- regression_results_pm_maap_bc$r_squared
rmsepm_maap_bc <- regression_results_pm_maap_bc$rmse
maepm_maap_bc <- regression_results_pm_maap_bc$mae




# Plot the results with annotation
ggplot(clean_data_pm_maap_bc, aes(x = pm2p5, y = MAAP_BC_637_con_mac10.4)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  
  geom_abline(slope = slopepm_maap_bc, intercept = interceptpm_maap_bc, color = "red", size = 0.3) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,5)+   xlim(0,100)+ 
  labs(title = expression(PM[`2.5`] ~ "vs" ~ eBC[`MAAP, 637 nm`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  labs(
    x = expression("PM"[2.5] * "  (" * mu * "g m"^-3 * ")"),
    y = expression("eBC"[`MAAP,637 nm`] * "  (" * mu * "g m"^-3 * ")"))+
  theme(legend.position = "") +
  annotate("text", x = 90, y = 5, label = "(a)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0, y = 5, 
           label = paste0("y = ", round(slopepm_maap_bc, 2), " x", ifelse(interceptpm_maap_bc >= 0, " + ", " - "), abs(round(interceptpm_maap_bc, 2)),  
                          "\nR² = ", round(r_squaredpm_maap_bc , 2)),
           hjust = 0, vjust = 1, size = 3.9, color = "black") -> plot_pm_ebc_con_01



####### Figure S1 (b)  #########

#######  --> PM2.5  VS OA  #########

names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_pm_ams_oa <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  #mutate(PAAS_total_b_abs_405_Mm = (PAAS_total_b_abs_405_Mm*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(pm2p5), !is.na(CTC_AMS_OA))

# Run the regression function and print the result
regression_results_pm_ams_oa <- york_regression_2nd(clean_data_pm_ams_oa$pm2p5, clean_data_pm_ams_oa$CTC_AMS_OA)



# Extract the regression results
slopepm_ams_oa <- regression_results_pm_ams_oa$slope
interceptpm_ams_oa <- regression_results_pm_ams_oa$intercept
r_squaredpm_ams_oa <- regression_results_pm_ams_oa$r_squared
rmsepm_ams_oa <- regression_results_pm_ams_oa$rmse
maepm_ams_oa <- regression_results_pm_ams_oa$mae


# Print each value to check
print(paste("Slope:", slopeAE33_PAAS))
print(paste("Intercept:", interceptAE33_PAAS))
print(paste("R-squared:", r_squaredAE33_PAAS))



# Plot the results with annotation
ggplot(clean_data_pm_ams_oa, aes(x = pm2p5, y = CTC_AMS_OA)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  
  geom_abline(slope = slopepm_ams_oa, intercept = interceptpm_ams_oa, color = "red", size = 0.3) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,50)+   xlim(0,100)+ 
  labs(title = expression(PM[`2.5`] ~ "vs" ~ OA[`AMS`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  labs(
    x = expression("PM"[2.5] * "  (" * mu * "g m"^-3 * ")"),
    y = expression("OA"[`AMS`] * "  (" * mu * "g m"^-3 * ")"))+
  theme(legend.position = "") +
  annotate("text", x = 90, y = 50, label = "(b)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0, y = 50, 
           label = paste0("y = ", round(slopepm_ams_oa, 2), " x", ifelse(interceptpm_ams_oa >= 0, " + ", " - "), abs(round(interceptpm_ams_oa, 2)),  
                          "\nR² = ", round(r_squaredpm_ams_oa , 2)),
           hjust = 0, vjust = 1, size = 3.9, color = "black") -> plot_pm_OA_con_01


# Arrange the three UV plots into a single column
combined_PM_EBC_OA <- plot_grid(
  plot_pm_ebc_con_01 + theme(legend.position = "none"),
  plot_pm_OA_con_01 + theme(legend.position = "none"),
  ncol = 1,  # Stack vertically
  align = "v"
)

# Print the final plot
print(combined_legend_plot_totalabs_UV1)
ggsave("York10_combined_PM_EBC_OA_W4H8_revision01.png", 
       plot = combined_PM_EBC_OA, 
       width = 4, height = 8, 
       device = "png", dpi = 600)

#







####### Figure S2  #########

####### --> AMS OA  VS TCA OC  #########

names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_ams_tca_oa <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  #mutate(PAAS_total_b_abs_405_Mm = (PAAS_total_b_abs_405_Mm*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(ACSM_OA), !is.na(CTC_AMS_OA))

# Run the regression function and print the result
regression_results_ams_tca_oa <- york_regression_2nd(clean_data_ams_tca_oa$ACSM_OA, clean_data_ams_tca_oa$CTC_AMS_OA)



# Extract the regression results
slopeams_tca_oa <- regression_results_ams_tca_oa$slope
interceptams_tca_oa <- regression_results_ams_tca_oa$intercept
r_squaredams_tca_oa <- regression_results_ams_tca_oa$r_squared
rmseams_tca_oa <- regression_results_ams_tca_oa$rmse
maeams_tca_oa <- regression_results_ams_tca_oa$mae



# Plot the results with annotation
ggplot(clean_data_ams_tca_oa, aes(x = ACSM_OA, y = CTC_AMS_OA)) +
  geom_point(size = 2, alpha = 0.9, color = "black", shape=1) +
  #geom_point(size = 2, shape = 21, alpha = 0.7, color = "black", fill = "gray34",stroke = 0.3) +
  
  geom_abline(slope = slopeams_tca_oa, intercept = interceptams_tca_oa, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,50)+   xlim(0,50)+ 
  labs(title = expression(ACSM_OA ~ "vs" ~ AMS_OA))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  labs(
    x = expression("ACSM_OA" * "  (" * mu * "g m"^-3 * ")"),
    y = expression("AMS_OA" * "  (" * mu * "g m"^-3 * ")"))+
  theme(legend.position = "") +
  #annotate("text", x = 50, y = 50, label = "(a)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0, y = 50, 
           label = paste0("y = ", round(slopeams_tca_oa, 2), " x", ifelse(interceptams_tca_oa >= 0, " + ", " - "), abs(round(interceptams_tca_oa, 2)),  
                          "\nR² = ", round(r_squaredams_tca_oa , 2)),
           hjust = 0, vjust = 1, size = 3.9, color = "black") -> plot_pm_acsm_ams_OA_con_01


ggsave("plot_pm_acsm_ams_OA_con_01_revision01.png", 
       plot = plot_pm_acsm_ams_OA_con_01, 
       width = 4, height = 4, 
       device = "png", dpi = 600)



####### Figure S3   #########

####### BEFORE CORRECTION #########

####### ---> AE33 785 VS PAAS  785 #########

# Clean data (filter out missing values based on your conditions)
clean_data_AE33_PAAS_IR <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  filter(!is.na(PAAS_b_total_abs_785_Mm), !is.na(AE33_Tabs_785))

# Run the regression function and print the result
regression_results_AE33_PAAS_IR <- york_regression_2nd(clean_data_AE33_PAAS_IR$PAAS_b_total_abs_785_Mm, clean_data_AE33_PAAS_IR$AE33_Tabs_785)



# Extract the regression results
slopeAE33_PAAS_IR <-     regression_results_AE33_PAAS_IR$slope
interceptAE33_PAAS_IR <- regression_results_AE33_PAAS_IR$intercept
r_squaredAE33_PAAS_IR <- regression_results_AE33_PAAS_IR$r_squared
rmseAE33_PAAS_IR <-      regression_results_AE33_PAAS_IR$rmse
maeAE33_PAAS_IR <-       regression_results_AE33_PAAS_IR$mae


# Plot the results with annotation
ggplot(clean_data_AE33_PAAS_IR, aes(x = PAAS_b_total_abs_785_Mm, y = (AE33_Tabs_785))) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeAE33_PAAS_IR, intercept = interceptAE33_PAAS_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(AE33[`(785 nm)`] ~ "vs" ~ PAAS[`(785 nm)`]))+
  theme(plot.title = element_text(color = "tomato3", size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 16, face = "plain", color = "black")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*785~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*785~nm]^{AE33}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 15, 
           label = paste0("y = ", round(slopeAE33_PAAS_IR, 2), " x", ifelse(interceptAE33_PAAS_IR >= 0, " + ", " - "), abs(round(interceptAE33_PAAS_IR, 2)),  
                          "\nR² = ", round(r_squaredAE33_PAAS_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_01a


####### ---> AE33 880 VS PAX 870 #########

# Clean data (filter out missing values based on your conditions)
clean_data_AE33_PAX_IR <- AE33_NCORE_hourly %>%
  #filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(`Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` > -8.5, `Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` <11) %>% 
  filter(!is.na(PAX_Tabs870), !is.na(AE33_Tabs_880))

# Run the regression function and print the result
regression_results_AE33_PAX_IR <- york_regression_2nd(clean_data_AE33_PAX_IR$PAX_Tabs870, clean_data_AE33_PAX_IR$AE33_Tabs_880)


# Extract the regression results
slopeAE33_PAX_IR <-     regression_results_AE33_PAX_IR$slope
interceptAE33_PAX_IR <- regression_results_AE33_PAX_IR$intercept
r_squaredAE33_PAX_IR <- regression_results_AE33_PAX_IR$r_squared
rmseAE33_PAX_IR <-      regression_results_AE33_PAX_IR$rmse
maeAE33_PAX_IR <-       regression_results_AE33_PAX_IR$mae


# Plot the results with annotation
ggplot(clean_data_AE33_PAX_IR, aes(x = PAX_Tabs870, y = (AE33_Tabs_880))) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeAE33_PAX_IR, intercept = interceptAE33_PAX_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(AE33[`(880 nm)`] ~ "vs" ~ PAX[`(870 nm)`]))+
  theme(plot.title = element_text(color = "tomato3", size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 16, face = "plain", color = "black")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAX~abs[`(total,870 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,880 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*870~nm]^{PAX}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*880~nm]^{AE33}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 15, 
           label = paste0("y = ", round(slopeAE33_PAX_IR, 2), " x", ifelse(interceptAE33_PAX_IR >= 0, " + ", " - "), abs(round(interceptAE33_PAX_IR, 2)), 
                          "\nR² = ", round(r_squaredAE33_PAX_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_05a



####### ---> MAAP 637 VS PAAS  660 #########

# Clean data (filter out missing values based on your conditions)
clean_data_MAAP_PAAS_IR <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(PAAS_Total_b_abs_660_Mm = ifelse(PAAS_Total_b_abs_660_Mm > 20, NA, PAAS_Total_b_abs_660_Mm)) %>%
  filter(!is.na(PAAS_Total_b_abs_660_Mm), !is.na(MAAP_BC_abs_637))


# Run the regression function and print the result
regression_results_MAAP_PAAS_IR <- york_regression_2nd(clean_data_MAAP_PAAS_IR$PAAS_Total_b_abs_660_Mm, clean_data_MAAP_PAAS_IR$MAAP_BC_abs_637)



# Extract the regression results
slopeMAAP_PAAS_IR <-     regression_results_MAAP_PAAS_IR$slope
interceptMAAP_PAAS_IR <- regression_results_MAAP_PAAS_IR$intercept
r_squaredMAAP_PAAS_IR <- regression_results_MAAP_PAAS_IR$r_squared
rmseMAAP_PAAS_IR <-      regression_results_MAAP_PAAS_IR$rmse
maeMAAP_PAAS_IR <-       regression_results_MAAP_PAAS_IR$mae


# Plot the results with annotation
ggplot(clean_data_MAAP_PAAS_IR, aes(x = PAAS_Total_b_abs_660_Mm, y = MAAP_BC_abs_637)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeMAAP_PAAS_IR, intercept = interceptMAAP_PAAS_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(MAAP[`(637 nm)`] ~ "vs" ~ PAAS[`(660 nm)`]))+
  theme(plot.title = element_text(color = "tomato3", size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 16, face = "plain", color = "black")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,660 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(MAAP~abs[`(total,637 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*660~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*637~nm]^{MAAP}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 15, 
           label = paste0("y = ", round(slopeMAAP_PAAS_IR, 2), " x", ifelse(interceptMAAP_PAAS_IR >= 0, " + ", " - "), abs(round(interceptMAAP_PAAS_IR, 2)),
                          "\nR² = ", round(r_squaredMAAP_PAAS_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_02a




####### ---> MAAP 637 VS PAX 660  #########

# Clean data (filter out missing values based on your conditions)
clean_data_MAAP_PAX_IR <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(PAX_Tabs_660 = ifelse(PAX_Tabs_660 > 20, NA, PAX_Tabs_660)) %>%
  filter(!is.na(PAX_Tabs_660), !is.na(MAAP_BC_abs_637))


# Run the regression function and print the result
regression_results_MAAP_PAX_IR <- york_regression_2nd(clean_data_MAAP_PAX_IR$PAX_Tabs_660, clean_data_MAAP_PAX_IR$MAAP_BC_abs_637)


# Extract the regression results
slopeMAAP_PAX_IR <-     regression_results_MAAP_PAX_IR$slope
interceptMAAP_PAX_IR <- regression_results_MAAP_PAX_IR$intercept
r_squaredMAAP_PAX_IR <- regression_results_MAAP_PAX_IR$r_squared
rmseMAAP_PAX_IR <-      regression_results_MAAP_PAX_IR$rmse
maeMAAP_PAX_IR <-       regression_results_MAAP_PAX_IR$mae


# Plot the results with annotation
ggplot(clean_data_MAAP_PAX_IR, aes(x = PAX_Tabs_660, y = (MAAP_BC_abs_637))) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeMAAP_PAX_IR, intercept = interceptMAAP_PAX_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(MAAP[`(637 nm)`] ~ "vs" ~ PAX[`(660 nm)`]))+
  theme(plot.title = element_text(color = "tomato3", size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 16, face = "plain", color = "black")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAX~abs[`(total,660 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(MAAP~abs[`(total,637 nm)`]~ '('*Mm^-1*')')) +
  xlab(bquote(italic(b)[abs*", "*660~nm]^{PAX}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*637~nm]^{MAAP}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 15, 
           label = paste0("y = ", round(slopeMAAP_PAX_IR, 2), " x", ifelse(interceptMAAP_PAX_IR >= 0, " + ", " - "), abs(round(interceptMAAP_PAX_IR, 2)),
                          "\nR² = ", round(r_squaredMAAP_PAX_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_totalabs_IR_06a



AE33_and_MAAP_vs_PAX_and_PAAS_longer <- plot_grid(
  plot_grid(plot_totalabs_IR_01a + theme(legend.position = "none"), #1
            plot_totalabs_IR_05a + theme(legend.position = "none"), #2
            plot_totalabs_IR_02a + theme(legend.position = "none"), #4
            plot_totalabs_IR_06a + theme(legend.position = "none"), #5
            ncol = 2, align = 'v'),
  rel_widths = c(5, 0.5)  # Adjust as necessary
)

print(AE33_and_MAAP_vs_PAX_and_PAAS_longer)
ggsave("YORK_AE33_and_MAAP_vs_PAX_and_PAAS_longer_revision01.png", 
       plot = AE33_and_MAAP_vs_PAX_and_PAAS_longer, 
       width = 8, height = 8, 
       device = "png", dpi=600)


####### Figure S4   #########

####### AFTER CORRECTION #########

####### ---> AE33 405 vs PAAS 405   #########

# Clean data (filter out missing values based on your conditions)
clean_data_CFAE33_405_PAAS <- AE33_NCORE_hourly %>%
  #filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  #mutate(PAAS_total_b_abs_405_Mm = ifelse(PAAS_total_b_abs_405_Mm > 50, NA, PAAS_total_b_abs_405_Mm)) %>%
  #mutate(PAAS_total_b_abs_405_Mm = (PAAS_total_b_abs_405_Mm*((Temp_C+273.15)/(298.15)))) %>%
  #mutate(AE33_Tabs_CF_3.6_405a = AE33_Tabs_405/2.8) %>%
  filter(!is.na(PAAS_total_b_abs_405_Mm), !is.na(AE33_Tabs_CF_3.6_405))

# Run the regression function and print the result
regression_results_CFAE33_405_PAAS <- york_regression_fixed(clean_data_CFAE33_405_PAAS$PAAS_total_b_abs_405_Mm, clean_data_CFAE33_405_PAAS$AE33_Tabs_CF_3.6_405)
regression_results_CFAE33_405_PAAS <- york_regression_2nd(clean_data_CFAE33_405_PAAS$PAAS_total_b_abs_405_Mm, clean_data_CFAE33_405_PAAS$AE33_Tabs_CF_3.6_405)


# Extract the regression results
slopeCFAE33_405_PAAS <-     regression_results_CFAE33_405_PAAS$slope
interceptCFAE33_405_PAAS <- regression_results_CFAE33_405_PAAS$intercept
r_squaredCFAE33_405_PAAS <- regression_results_CFAE33_405_PAAS$r_squared
rmseCFAE33_405_PAAS <-      regression_results_CFAE33_405_PAAS$rmse
maeCFAE33_405_PAAS <-       regression_results_CFAE33_405_PAAS $mae



# Plot the results with annotation
ggplot(clean_data_CFAE33_405_PAAS, aes(x = PAAS_total_b_abs_405_Mm, y = AE33_Tabs_CF_3.6_405)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeCFAE33_405_PAAS, intercept = interceptCFAE33_405_PAAS, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(`AE33*`[`(405 nm)`] ~ "vs" ~ PAAS[`(405 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,405 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,405 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(b)[abs*", "*405~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*405~nm]^{`AE33*`}*"("*Mm^-1*")")) +
  # ylab(expression(italic(b)[abs*", "*405~nm]^{AE33*bold("*")}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 21, y = 10, 
           label = paste0("y = ", round(slopeCFAE33_405_PAAS, 2), " x",  ifelse(interceptCFAE33_405_PAAS >= 0, " + ", " - "), abs(round(interceptCFAE33_405_PAAS, 2)),
                          "\nR² = ", round(r_squaredCFAE33_405_PAAS , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PAAS_405_02


annotate("text", 
         x = 21, y = 15, 
         label = paste0("y = ", round(slopeCFAE33_405_PAAS, 2), " x",  ifelse(interceptCFAE33_405_PAAS >= 0, " + ", " - "), abs(round(interceptCFAE33_405_PAAS, 2)),
                        "\nR² = ", round(r_squaredCFAE33_405_PAAS , 2), "\nRMSE = ", round(rmseCFAE33_405_PAAS , 2), "\nMAE = ", round(maeCFAE33_405_PAAS , 2)),
         hjust = 0, vjust = 1, size = 4.5, color = "black") #-> plot_CFAE33_PAAS_405_02





####### ---> AE33 405 VS PAX 401 #########

clean_data_CFAE33405_PAX <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  #filter(`Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` > -8.5, `Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` <11) %>% 
  mutate(PAX_Tabs_401nm = ifelse(PAX_Tabs_401nm > 40, NA, PAX_Tabs_401nm)) %>%
  #mutate(PAX_Tabs_401nm = (PAX_Tabs_401nm*((Temp_C+273.15)/(298.15)))) %>%
  #mutate(AE33_Tabs_CF_3.6_405a = AE33_Tabs_405/2.8) %>%
  filter(!is.na(PAX_Tabs_401nm), !is.na(AE33_Tabs_CF_3.6_405))

# Run the regression function and print the result
regression_results_CFAE33405_PAX <- york_regression_fixed(clean_data_CFAE33405_PAX$PAX_Tabs_401nm, clean_data_CFAE33405_PAX$AE33_Tabs_CF_3.6_405)
regression_results_CFAE33405_PAX <- york_regression_2nd(clean_data_CFAE33405_PAX$PAX_Tabs_401nm, clean_data_CFAE33405_PAX$AE33_Tabs_CF_3.6_405)


# Extract the regression results
slopeCFAE33405_PAX <-     regression_results_CFAE33405_PAX$slope
interceptCFAE33405_PAX <- regression_results_CFAE33405_PAX$intercept
r_squaredCFAE33405_PAX <- regression_results_CFAE33405_PAX$r_squared
rmseCFAE33405_PAX <-      regression_results_CFAE33405_PAX$rmse
maeCFAE33405_PAX <-       regression_results_CFAE33405_PAX$mae


ggplot(clean_data_CFAE33405_PAX, aes(x = PAX_Tabs_401nm, y = AE33_Tabs_CF_3.6_405)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeCFAE33405_PAX, intercept = interceptCFAE33405_PAX, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(`AE33*`[`(405 nm)`] ~ "vs" ~ PAX[`(401 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #ylab(bquote(AE33~abs[`(total,405 nm)`]~ '('*Mm^-1*')')) +
  #xlab(bquote(PAX~abs[`(total,401 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(b)[abs*", "*405~nm]^{PAX}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*405~nm]^{`AE33*`}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 23, y = 10, 
           label = paste0("y = ", round(slopeCFAE33405_PAX, 2), " x",  ifelse(interceptCFAE33405_PAX >= 0, " + ", " - "), abs(round(interceptCFAE33405_PAX, 2)),
                          "\nR² = ", round(r_squaredCFAE33405_PAX , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PAX_405_02

annotate("text", 
         x = 23, y = 15, 
         label = paste0("y = ", round(slopeCFAE33405_PAX, 2), " x",  ifelse(interceptCFAE33405_PAX >= 0, " + ", " - "), abs(round(interceptCFAE33405_PAX, 2)),
                        "\nR² = ", round(r_squaredCFAE33405_PAX , 2), "\nRMSE = ", round(rmseCFAE33405_PAX , 2), "\nMAE = ", round(maeCFAE33405_PAX , 2)),
         hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PAX_405_02




####### ---> AE33 785 vs PAAS 785   #########
names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_CFAE33_PAAS_IR <- AE33_NCORE_hourly %>%
  #filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  # mutate(PAAS_b_total_abs_785_Mm = ifelse(PAAS_b_total_abs_785_Mm > 20, NA, PAAS_b_total_abs_785_Mm)) %>%
  #mutate(PAAS_b_total_abs_785_Mm = (PAAS_b_total_abs_785_Mm*((Temp_C+273.15)/(298.15)))) %>%
  filter(!is.na(PAAS_b_total_abs_785_Mm), !is.na(AE33_Tabs_CF_3.6_785))



# Run the regression function and print the result
regression_results_CFAE33_PAAS_IR <- york_regression_fixed(clean_data_CFAE33_PAAS_IR$PAAS_b_total_abs_785_Mm, clean_data_CFAE33_PAAS_IR$AE33_Tabs_CF_3.6_785)
regression_results_CFAE33_PAAS_IR <- york_regression_2nd(clean_data_CFAE33_PAAS_IR$PAAS_b_total_abs_785_Mm, clean_data_CFAE33_PAAS_IR$AE33_Tabs_CF_3.6_785)


# Extract the regression results
slopeCFAE33_PAAS_IR <-     regression_results_CFAE33_PAAS_IR$slope
interceptCFAE33_PAAS_IR <- regression_results_CFAE33_PAAS_IR$intercept
r_squaredCFAE33_PAAS_IR <- regression_results_CFAE33_PAAS_IR$r_squared
rmseCFAE33_PAAS_IR <-      regression_results_CFAE33_PAAS_IR$rmse
maeCFAE33_PAAS_IR <-       regression_results_CFAE33_PAAS_IR$mae


# Plot the results with annotation
ggplot(clean_data_CFAE33_PAAS_IR, aes(x = PAAS_b_total_abs_785_Mm, y = AE33_Tabs_CF_3.6_785)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeCFAE33_PAAS_IR, intercept = interceptCFAE33_PAAS_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(`AE33*`[`(785 nm)`] ~ "vs" ~ PAAS[`(785 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(b)[abs*", "*785~nm]^{PAAS-4*lambda}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*785~nm]^{`AE33*`}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 10, 
           label = paste0("y = ", round(slopeCFAE33_PAAS_IR, 2), " x", ifelse(interceptCFAE33_PAAS_IR >= 0, " + ", " - "), abs(round(interceptCFAE33_PAAS_IR, 2)),  
                          "\nR² = ", round(r_squaredCFAE33_PAAS_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PASS_785_01

annotate("text", 
         x = 20, y = 15, 
         label = paste0("y = ", round(slopeCFAE33_PAAS_IR, 2), " x", ifelse(interceptCFAE33_PAAS_IR >= 0, " + ", " - "), abs(round(interceptCFAE33_PAAS_IR, 2)),  
                        "\nR² = ", round(r_squaredCFAE33_PAAS_IR , 2), "\nRMSE = ", round(rmseCFAE33_PAAS_IR , 2), "\nMAE = ", round(maeCFAE33_PAAS_IR , 2)),
         hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PASS_785_01



####### ---> AE33 880 vs PAX 870   #########
names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_CFAE33_PAX_IR <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  #filter(`Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` > -8.5, `Bland-Altman_CF_3.6_AE33_PAX_405_DIFF` <11) %>% 
  mutate(PAX_Tabs870 = ifelse(PAX_Tabs870 > 30, NA, PAX_Tabs870)) %>%
  #mutate(PAX_Tabs870 = (PAX_Tabs870*((Temp_C+273.15)/(298.15)))) %>%
  # mutate(AE33_Tabs_CF_3.6_880a = AE33_Tabs_880/2.8) %>%
  filter(!is.na(PAX_Tabs870), !is.na(AE33_Tabs_CF_3.6_880))


# Run the regression function and print the result
regression_results_CFAE33_PAX_IR <- york_regression_fixed(clean_data_CFAE33_PAX_IR$PAX_Tabs870, clean_data_CFAE33_PAX_IR$AE33_Tabs_CF_3.6_880)
regression_results_CFAE33_PAX_IR <- york_regression_2nd(clean_data_CFAE33_PAX_IR$PAX_Tabs870, clean_data_CFAE33_PAX_IR$AE33_Tabs_CF_3.6_880)


# Extract the regression results
slopeCFAE33_PAX_IR <-     regression_results_CFAE33_PAX_IR$slope
interceptCFAE33_PAX_IR <- regression_results_CFAE33_PAX_IR$intercept
r_squaredCFAE33_PAX_IR <- regression_results_CFAE33_PAX_IR$r_squared
rmseCFAE33_PAX_IR <-      regression_results_CFAE33_PAX_IR$rmse
maeCFAE33_PAX_IR <-       regression_results_CFAE33_PAX_IR$mae


# Plot the results with annotation
ggplot(clean_data_CFAE33_PAX_IR, aes(x = PAX_Tabs870, y = AE33_Tabs_CF_3.6_880)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeCFAE33_PAX_IR, intercept = interceptCFAE33_PAX_IR, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(`AE33*`[`(880 nm)`] ~ "vs" ~ PAX[`(870 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAX~abs[`(total,870 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,880 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(b)[abs*", "*870~nm]^{PAX}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*880~nm]^{`AE33*`}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 20, y = 10, 
           label = paste0("y = ", round(slopeCFAE33_PAX_IR, 2), " x", ifelse(interceptCFAE33_PAX_IR >= 0, " + ", " - "), abs(round(interceptCFAE33_PAX_IR, 2)), 
                          "\nR² = ", round(r_squaredCFAE33_PAX_IR , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PAX_880_02


annotate("text", 
         x = 20, y = 15, 
         label = paste0("y = ", round(slopeCFAE33_PAX_IR, 2), " x", ifelse(interceptCFAE33_PAX_IR >= 0, " + ", " - "), abs(round(interceptCFAE33_PAX_IR, 2)), 
                        "\nR² = ", round(r_squaredCFAE33_PAX_IR , 2), "\nRMSE = ", round(rmseCFAE33_PAX_IR , 2), "\nMAE = ", round(maeCFAE33_PAX_IR , 2)),
         hjust = 0, vjust = 1, size = 4.5, color = "black") -> plot_CFAE33_PAX_880_02





AE33_vs_PAX_and_PAAS_uv_longer <- plot_grid(
  plot_grid(plot_CFAE33_PAAS_405_02 + theme(legend.position = "none"), #1
            plot_CFAE33_PASS_785_01 + theme(legend.position = "none"), #4
            plot_CFAE33_PAX_405_02 + theme(legend.position = "none"), #2
            plot_CFAE33_PAX_880_02 + theme(legend.position = "none"), #5
            ncol = 2, align = 'v'),
  rel_widths = c(5, 0.5)  # Adjust as necessary
)

# To view the combined plot
print(AE33_vs_PAX_and_PAAS_uv_longer)
ggsave("YORK10_AE33_vs_PAX_and_PAAS_uv_longer5_revision01.png", 
       plot = AE33_vs_PAX_and_PAAS_uv_longer, 
       width = 7.7, height = 7.7, 
       device = "png", dpi=600)




####### Figure S5   #########



loading_effect <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/loading_effect.csv")
loading_effect <- read_csv("~/")

names(loading_effect)


loading_effect %>% #### ALSO FOR JAN AND FEB ###
  ggplot(aes(x = hour, y = SSA_450)) + 
  geom_point(size=1, colour = "blue") + geom_line(size=0.5, colour = "blue") + 
  xlab("Local Time") +
  ylab(expression(italic(SSA)[(`470 nm`)]^{AE33}*~"")) +
  labs(title= "January and Februrary [2022]") +
  # ylim(0.55,0.9) + 
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=rel(0.8)))+
  theme(legend.position="none")+
  # theme(legend.position = c(0.8, 0.2)) +
  theme(axis.title = element_text(face="plain",size=16,color="black"),
        axis.text=element_text(size=12,face="plain", color="black"),
        axis.title.x = element_text(vjust=0.1),
        axis.text.y=element_text(hjust=0.5),
        axis.text.x=element_text(hjust=0.0, angle = -45),
        plot.title = element_text(size=14)) +
  theme(strip.text = element_text(size=8, color="black"))+
  scale_x_datetime(expand=c(0,0),
                   date_breaks= "1 days",
                   date_minor_breaks = "7 days",
                   date_labels = "%m/%d",
                   limits = as.POSIXct(c("2022-01-28 00:00:00", "2022-02-05 10:00:00"))) -> AE33_SSA_01A


loading_effect %>% #### ALSO FOR JAN AND FEB ###
  ggplot(aes(x = TheTime, y = K_470)) + 
  geom_point(size=1, colour = "blue") + geom_line(size=0.5, colour = "blue") + 
  xlab("Local Time") +
  #ylab(bquote((italic)~K[`(AE33, 470nm)`])) +
  ylab(expression(italic(K)[(`470 nm`)]^{AE33}*~"")) +
  labs(title= "January and Februrary [2022]") +
  ylim(0.002,0.0055) + 
  #ylim(0,0.0065) + 
  
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=rel(0.8)))+
  theme(legend.position="none")+
  # theme(legend.position = c(0.8, 0.2)) +
  theme(axis.title = element_text(face="plain",size=16,color="black"),
        axis.text=element_text(size=12,face="plain", color="black"),
        axis.title.x = element_text(vjust=0.1),
        axis.text.y=element_text(hjust=0.5),
        axis.text.x=element_text(hjust=0.0, angle = -45),
        plot.title = element_text(size=14)) +
  theme(strip.text = element_text(size=8, color="black"))+
  scale_x_datetime(expand=c(0,0),
                   date_breaks= "1 days",
                   date_minor_breaks = "7 days",
                   date_labels = "%m/%d",
                   #date_labels = "%b-%d",
                   limits = as.POSIXct(c("2022-01-28 00:00:00", "2022-02-05 10:00:00"))) -> AE33_loading470_01A


grid.arrange(AE33_SSA_01A, AE33_loading470_01A,ncol = 2) -> AE33_SSA_loading470_01

ggsave("AE33_SSA_loading470_01_10x4E.png", plot = AE33_SSA_loading470_01, width = 10, height = 4,
       dpi =600, device = "png")

####### Figure S6   #########


######### ---> AE33 660 vs MAAP 637  ##########



# Clean data (filter out missing values based on your conditions)
clean_data_CFAE33_MAAP <- AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <= 2) %>% 
  #mutate(MAAP_BC_abs_637 = (MAAP_BC_abs_637*((Temp_C+273.15)/(298.15)))) %>%
  #select(MAAP_BC_abs_637,AE33_Tabs_CF_660) %>% 
  #mutate(MAAP_BC_abs_637 = (MAAP_BC_con*10.4)) %>% 
  # mutate(AE33_Tabs_660a = (AE33_Tabs_660/(2.32))) %>%
  #filter(!is.na(MAAP_BC_abs_637), !is.na(AE33_Tabs_660a))
  filter(!is.na(MAAP_BC_abs_637), !is.na(AE33_Tabs_CF_660))


# Run the regression function and print the result
regression_results_CFAE33_MAAP <- york_regression_fixed(clean_data_CFAE33_MAAP$MAAP_BC_abs_637, clean_data_CFAE33_MAAP$AE33_Tabs_CF_660)
regression_results_CFAE33_MAAP <- york_regression_2nd(clean_data_CFAE33_MAAP$MAAP_BC_abs_637, clean_data_CFAE33_MAAP$AE33_Tabs_CF_660)


# Extract the regression results
slope_CFAE33_MAAP <-     regression_results_CFAE33_MAAP$slope
intercept_CFAE33_MAAP <- regression_results_CFAE33_MAAP$intercept
r_squared_CFAE33_MAAP <- regression_results_CFAE33_MAAP$r_squared
rmse_CFAE33_MAAP <-      regression_results_CFAE33_MAAP$rmse
mae_CFAE33_MAAP <-       regression_results_CFAE33_MAAP$mae



# Plot the results with annotation
ggplot(clean_data_CFAE33_MAAP, aes(x = MAAP_BC_abs_637, y = AE33_Tabs_CF_660)) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slope_CFAE33_MAAP, intercept = intercept_CFAE33_MAAP, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,40)+   xlim(0,40)+ 
  labs(title = expression(`AE33*`[`(660 nm)`] ~ "vs" ~ MAAP[`(637 nm)`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 16, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(expression(italic(b)[abs*", "*637~nm]^{MAAP}*"("*Mm^-1*")")) +
  ylab(expression(italic(b)[abs*", "*660~nm]^{`AE33*`}*"("*Mm^-1*")")) +
  theme(legend.position = "") +
  annotate("text", 
           x = 0, y = 40, 
           label = paste0("y = ", round(slope_CFAE33_MAAP, 2), " x", ifelse(intercept_CFAE33_MAAP >= 0, " + ", " - "), abs(round(intercept_CFAE33_MAAP, 2)), 
                          "\nR² = ", round(r_squared_CFAE33_MAAP , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black") -> CF_AE33_MAAP_02a



ggsave("plot_CF_AE33_MAAP_02a_revision01.png", 
       plot = CF_AE33_MAAP_02a, 
       width = 4, height = 4, 
       device = "png", dpi = 600)



########## Figure S7 #########


####### OVERLAY OA PLOT, MAAP AND AE33 RATION AND BOX PLOT ############ 


# Prepare the data with filter
plot_data_OA <- AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>%
  select(MAAP_BC_abs_637, AE33_Tabs_CF_660, CTC_AMS_OA, Temp_C) %>%
  mutate(MAAP_PAASratio = (MAAP_BC_abs_637 / AE33_Tabs_CF_660)) %>%
  mutate(MAAP_PAASratio = ifelse(MAAP_PAASratio > 6, NA, MAAP_PAASratio)) %>%
  # Create OA bins for boxplot grouping
  #mutate(CTC_OA_bin = cut(CTC_AMS_OA, 
  #                        breaks = c(0,5,10, 15,20,25,30,35,40,45,50),
  #                        include.lowest = TRUE)) %>%
  mutate(CTC_OA_bin = cut(CTC_AMS_OA, 
                          breaks = c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 
                                     22.5, 25, 27.5, 30, 32.5, 35, 37.5, 40, 
                                     42.5, 45, 47.5, 50),
                          include.lowest = TRUE)) %>% 
  filter(!is.na(CTC_OA_bin))

# Create combined plot with boxplot and colored scatter points
library(ggplot2)
ggplot(plot_data_OA, aes(x = CTC_AMS_OA, y = MAAP_PAASratio)) +
  # Add transparent boxplots by bin
  geom_boxplot(aes(group = CTC_OA_bin), fill = NA, color = "black", 
               outlier.shape = NA, alpha = 0.5, width = 8) +
  # Add colored scatter points on top
  geom_point(aes(color = Temp_C), size = 1.5, shape = 16, alpha = 0.5) +
  # Color scale for temperature
  scale_color_gradientn(
    colours = c("darkslateblue", "blue", "deepskyblue", "cyan2", "yellow", "orange", "red2"),
    values = scales::rescale(c(-40, -30, -20, -10, 0)),
    breaks = c(-40, -30, -20, -10, 0),
    limits = c(-40, 0),
    labels = c("-40", "-30", "-20", "-10", "0"),
    name = expression("T"~(degree*C))
  ) +
  ylab(expression(italic(b)[abs*", "*637~nm]^{MAAP} ~ `:` ~ italic(b)[abs*", "*660~nm]^{`AE33*`}*"")) +
  xlab(expression(OA[AMS]*~"("*mu*g~m^-3*")")) +
  labs(title = expression(MAAP[`637 nm`] ~ `:` ~ `AE33*`[`660 nm`] ~ vs ~ OA[AMS]))+ 
  scale_y_continuous(breaks = c(0,2,4,6), limits = c(0, 6)) +
  xlim(0, 30) +
  theme_bw() + 
  annotate("text", x = 25, y = 0.5, label = "(c)", size = 5, fontface = "bold") +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=12),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.10, "inch"),
    legend.key.width = unit(0.26, "inch"),
    legend.text = element_text(size=10),
    legend.position = c(0.75, 0.75),
    legend.direction = "horizontal",
    legend.title.position = "top") -> MAAP_CF_AE33_RATIO_OA_BOX_overlay

# Display the plot
MAAP_CF_AE33_RATIO_OA_BOX_overlay


ggarrange(MAAP_CF_AE33_RATIO_pm_BOX_overlay, MAAP_CF_AE33_RATIO_OA_BOX_overlay, ncol = 1, nrow = 2) -> MAAP_CF_AE33_RATIO_OA_PM_OVERLAY

ggsave("MAAP_CF_AE33_RATIO_OA_PM_OVERLAY_5x7_NEW_08.pdf", 
       plot = MAAP_CF_AE33_RATIO_OA_PM_OVERLAY, width = 5, height = 7, device = "pdf")




####### OVERLAY PM2.5/BC PLOT, MAAP AND AE33 RATION AND BOX PLOT ############ 

# Prepare the data with filter
plot_data_pm2p5_ratio_OA <- AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>%
  select(MAAP_BC_abs_637, AE33_Tabs_CF_660, pm2p5, Temp_C,MAAP_BC_637_con_mac10.4) %>%
  mutate(MAAP_PAASratio = (MAAP_BC_abs_637 / AE33_Tabs_CF_660)) %>%
  mutate(MAAP_PAASratio = ifelse(MAAP_PAASratio > 6, NA, MAAP_PAASratio)) %>%
  mutate(pm2p5_ratio_OA = pm2p5/(MAAP_BC_637_con_mac10.4)) %>% 
  mutate(pm2p5_ratio_OA = ifelse(pm2p5_ratio_OA <= 0, NA, pm2p5_ratio_OA)) %>%
  # Create PM2.5 bins for boxplot grouping
  #mutate(CTC_pm_bin = cut(pm2p5, 
  #                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
  #                        include.lowest = TRUE)) %>%
  mutate(pm2p5_ratio_OA_pm_bin = cut(pm2p5_ratio_OA, 
                                     breaks = seq(0, 80, by = 4),
                                     include.lowest = TRUE)) %>% 
  filter(!is.na(pm2p5_ratio_OA_pm_bin))

# Create combined plot
library(ggplot2)
ggplot(plot_data_pm2p5_ratio_OA, aes(x = pm2p5_ratio_OA, y = MAAP_PAASratio)) +
  # Add transparent boxplots by bin
  geom_boxplot(aes(group = pm2p5_ratio_OA_pm_bin), fill = NA, color = "black", 
               outlier.shape = NA, alpha = 0.5, width = 8) +
  # Add colored scatter points on top
  geom_point(aes(color = Temp_C), size = 1.5, shape = 16, alpha = 0.5) +
  # Color scale for temperature
  scale_color_gradientn(
    colours = c("darkslateblue", "blue", "deepskyblue", "cyan2", "yellow", "orange", "red2"),
    values = scales::rescale(c(-40, -30, -20, -10, 0)),
    breaks = c(-40, -30, -20, -10, 0),
    limits = c(-40, 0),
    labels = c("-40", "-30", "-20", "-10", "0"),
    name = expression("T"~(degree*C))
  ) +
  ylab(expression(italic(b)[abs*", "*637~nm]^{MAAP} ~ `:` ~ italic(b)[abs*", "*660~nm]^{`AE33*`}*"")) +
  xlab(bquote(frac(PM[2.5], eBC[`MAAP, 637 nm`]~ ''**'')))+ 
  
  labs(title = expression(MAAP[`637 nm`] ~ `:` ~ `AE33*`[`660 nm`] ~ vs ~ PM[2.5]:eBC[MAAP]))+ 
  scale_y_continuous(breaks = c(0,2,4,6), limits = c(0, 6)) +
  #scale_x_continuous(breaks = c(0,10,20,30,40,60), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(0,20,40,60), limits = c(0, 60)) +
  
  theme_bw() + 
  annotate("text", x = 50, y = 0.5, label = "(b)", size = 5, fontface = "bold") +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=12),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.10, "inch"),
    legend.key.width = unit(0.26, "inch"),
    legend.text = element_text(size=10),
    legend.position = c(0.75, 0.75),
    legend.direction = "horizontal",
    legend.title.position = "top") -> MAAP_CF_AE33_RATIO_pm_BC_RATIO_overlay



####### OVERLAY OA/BC RATIO  , MAAP AND AE33 RATION AND BOX PLOT ############ 

names(AE33_NCORE_hourly)
# Prepare the data with filter
plot_data_BC_ratio_OA <- AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>%
  select(MAAP_BC_abs_637, AE33_Tabs_CF_660, CTC_AMS_OA, Temp_C,MAAP_BC_637_con_mac10.4) %>%
  mutate(MAAP_PAASratio = (MAAP_BC_abs_637 / AE33_Tabs_CF_660)) %>%
  mutate(BC_ratio_OA = CTC_AMS_OA/(MAAP_BC_637_con_mac10.4)) %>% 
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  
  mutate(MAAP_PAASratio = ifelse(MAAP_PAASratio > 6, NA, MAAP_PAASratio)) %>%
  # Create OA bins for boxplot grouping
  #mutate(CTC_OA_bin = cut(CTC_AMS_OA, 
  #                        breaks = c(0,5,10, 15,20,25,30,35,40,45,50),
  #                        include.lowest = TRUE)) %>%
  mutate(BC_ratio_OA_bin = cut(BC_ratio_OA, 
                               breaks = c(0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, 12.0, 13.5, 15.0, 
                                          16.5, 18.0, 19.5, 21.0, 22.5, 24.0, 25.5, 27.0, 28.5, 30.0, 
                                          31.5, 33.0, 34.5, 36.0, 37.5, 39.0, 40.5, 42.0, 43.5, 45.0, 
                                          46.5, 48.0, 49.5, 50),
                               include.lowest = TRUE)) %>% 
  filter(!is.na(BC_ratio_OA_bin))

# Create combined plot with boxplot and colored scatter points
library(ggplot2)
ggplot(plot_data_BC_ratio_OA, aes(x = BC_ratio_OA, y = MAAP_PAASratio)) +
  # Add transparent boxplots by bin
  geom_boxplot(aes(group = BC_ratio_OA_bin), fill = NA, color = "black", 
               outlier.shape = NA, alpha = 0.5, width = 8) +
  # Add colored scatter points on top
  geom_point(aes(color = Temp_C), size = 1.5, shape = 16, alpha = 0.5) +
  # Color scale for temperature
  scale_color_gradientn(
    colours = c("darkslateblue", "blue", "deepskyblue", "cyan2", "yellow", "orange", "red2"),
    values = scales::rescale(c(-40, -30, -20, -10, 0)),
    breaks = c(-40, -30, -20, -10, 0),
    limits = c(-40, 0),
    labels = c("-40", "-30", "-20", "-10", "0"),
    name = expression("T"~(degree*C))
  ) +
  ylab(expression(italic(b)[abs*", "*637~nm]^{MAAP} ~ `:` ~ italic(b)[abs*", "*660~nm]^{`AE33*`}*"")) +
  xlab(bquote(frac(OA[`AMS`], eBC[`MAAP, 637 nm`]~ ''**'')))+ 
  labs(title = expression(MAAP[`637 nm`] ~ `:` ~ `AE33*`[`660 nm`] ~ vs ~ OA[AMS]:eBC[MAAP]))+ 
  scale_y_continuous(breaks = c(0,2,4,6), limits = c(0, 6)) +
  xlim(0, 30) +
  theme_bw() + 
  annotate("text", x = 25, y = 0.5, label = "(d)", size = 5, fontface = "bold") +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=12),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.10, "inch"),
    legend.key.width = unit(0.26, "inch"),
    legend.text = element_text(size=10),
    legend.position = c(0.75, 0.75),
    legend.direction = "horizontal",
    legend.title.position = "top") -> MAAP_CF_AE33_RATIO_OA_BC_RATIO_overlay

# Display the plot 
MAAP_CF_AE33_RATIO_OA_BOX_overlay


ggarrange(MAAP_CF_AE33_RATIO_pm_BC_RATIO_overlay, 
          MAAP_CF_AE33_RATIO_OA_BC_RATIO_overlay, ncol = 1, nrow = 2) -> MAAP_CF_AE33_RATIO_pm_OA_BC_RATIO_overlay

ggsave("MAAP_CF_AE33_RATIO_pm_OA_BC_RATIO_overlay_5x7_NEW_01.pdf", 
       plot = MAAP_CF_AE33_RATIO_pm_OA_BC_RATIO_overlay, width = 5, height = 7, 
       device = "pdf", dpi=300)



ggarrange(MAAP_CF_AE33_RATIO_OA_PM_OVERLAY, 
          MAAP_CF_AE33_RATIO_pm_OA_BC_RATIO_overlay, ncol = 2, nrow = 1) -> FigureS7A

ggsave("MAAP_CF_AE33_RATIO_pm_OA_BC_RATIO_overlay_8x8_revision01.png", 
       plot = FigureS7A, width = 8, height = 8, 
       device = "png", dpi=300)



######### Figure S10 a   ##########

####### 2022 WINTER (2ND) EPA EC, MAAP, AE33,PAAS,PAX BC   #########

WINTER_EC_EPA_ALPACA <- read_csv("~/Google Drive/My Drive/alaska/Magee/new_2022/WINTER_EC_EPA_ALPACA.csv")


names(WINTER_EC_EPA_ALPACA)
WINTER_EC_EPA_ALPACA %>%
  mutate(
    EPA_EC_TOR_adj = EPA_EC_TOR * 0.8,  # EPA correction factor applied
    EPA_EC_TOT_adj = EPA_EC_TOT - 0.025,  # Total EC calculation using original EPA_EC_TOR
    `AE33_BC_abs_CF_660` = (AE33_BC_CF_660_MAC_10.35*7.77),
  ) %>%  
  select(date_local, 
         EPA_EC_TOR_adj,  
         EPA_EC_TOT_adj, 
         `MAAP_BC_10.4_660` = MAAP_BC_637_MAC_10.4,
         `PAAS_BC_660` = PAAS_BC_con_660_MAC_11.2,
         `AE33_BC_CF_660` = (AE33_BC_CF_660_MAC_10.35),  # Fixed: removed extra parentheses
         PAX_BC_870 = PAX_BC_conc_870) %>%
  # Filter for January-February 2022
  #filter(date_local >= as.Date("2021-12-28") & date_local <= as.Date("2022-03-02")) %>%
  filter(date_local >= as.Date("2022-01-02") & date_local <= as.Date("2022-02-28")) %>%
  pivot_longer(cols = -date_local, 
               names_to = "variable", 
               values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("EPA_EC_TOR_adj", "EPA_EC_TOT_adj", 
                                                "AE33_BC_CF_660", "PAAS_BC_660", 
                                                "PAX_BC_870", "MAAP_BC_10.4_660"))) %>%
  ggplot(aes(x = date_local, y = value, color = variable)) +
  geom_line(size = 1) +  # Slightly thicker lines for better visibility
  geom_point(size = 4) +
  labs(title = "Comparison with Corrected EPA* EC",
       #title = expression(OA[EPA] ~ "," ~ OA[`TCA-08`] ~ "," ~ OA[`AMS`] ~ "and" ~ OA[`ACSM`] ~ ("2022,  Jan and Feb")),
       x = "Date") +
  theme_bw() +  ylim(0,4)+
  ylab(expression("(" * mu * g ~ m^-3 * ")")) +
  annotate("text", x = as.Date("2022-01-07"), y = 4, label = "(a)", size = 5, fontface = "bold")+
  #annotate("text", x = 1, y = 15, label = "TCA ",  color = "black", size = 5, hjust = 0) +  # Text label for mean
  theme(
    legend.text = element_text(size = rel(1.2)),
    legend.position = c(0.6, 0.8),
    axis.title = element_text(face = "plain", size = 14, color = "black"),
    axis.text = element_text(size = 14, face = "plain", color = "black"),
    axis.title.x = element_text(vjust = 0.1),
    axis.text.y = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.0, angle = -45),
    plot.title = element_text(size = 16),
    panel.grid.minor = element_line(color = "grey90", size = 0.3),
    panel.grid.major = element_line(color = "grey85", size = 0.5),
    axis.ticks.length.x = unit(0.3, "cm"),              # Major tick length (longer)
    ggh4x.axis.ticks.length.minor = rel(0.5)
  ) +
  scale_color_manual(name = NULL,
                     values = c("EPA_EC_TOR_adj" = "orange",       # orange
                                "EPA_EC_TOT_adj" = "#999999",      # gray
                                "AE33_BC_CF_660" = "red",          # red 
                                "PAAS_BC_660" = "blue",            #  Bluish green
                                "PAX_BC_870" = "#56B4E9",          # Blue
                                "MAAP_BC_10.4_660" = "#000000"),   # Black
                     labels = c("EPA_EC_TOR_adj" = expression(`EPA*`~EC[paste("(TOR)")]),
                                "EPA_EC_TOT_adj" = expression(`EPA*`~EC[paste("(TOT)")]),
                                "AE33_BC_CF_660" = expression(`AE33*`~eBC[paste("(660 nm, MAC="~"4.46"~"m"^2*" "*"g"^{-1}*")")]),
                                "PAAS_BC_660" = expression({PAAS-4*lambda}~eBC[paste("(660 nm, MAC="~"11.2"~"m"^2*" "*"g"^{-1}*")")]),
                                "MAAP_BC_10.4_660" = expression(MAAP~eBC[paste("(637 nm, MAC="~"10.4"~"m"^2*" "*"g"^{-1}*")")]), 
                                "PAX_BC_870" = expression(PAX~eBC[paste("(870 nm, MAC="~"4.74"~"m"^2*" "*"g"^{-1}*")")]))) +
  scale_shape_manual(name = NULL, 
                     values = c("EPA_EC_TOR_adj" = 16,      # Fixed: was EPA_OC
                                "EPA_EC_TOT_adj" = 16,    # Fixed: was TCA-08_OC  
                                "AE33_BC_CF_660" = 16,
                                "PAAS_BC_660" = 16,
                                "MAAP_BC_10.4_660" = 16, 
                                "PAX_BC_870" = 16)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  scale_x_date(
    breaks = seq(as.Date("2022-01-02"), as.Date("2022-02-28"), by = "5 days"),
    minor_breaks = seq(as.Date("2022-01-02"), as.Date("2022-02-28"), by = "1 day"),
    date_labels = "%m/%d",  # Show month/day format
    limits = c(as.Date("2021-12-30"), as.Date("2022-03-03")),  # 3 days before and after
    expand = c(0, 0),
    guide = "axis_minor") -> time_series_EPA_EC_correction_MAAP_PAAS_PAX_AE33_eBC


######### Figure S10 b   ##########

########### --> YORK REGRESSION CORRECTED EPA TOR  EC, AE33 WINTER   #############


names(WINTER_EC_EPA_ALPACA)


names(WINTER_EC_EPA_ALPACA)
clean_data_WINTER_AE33_corr_EPA_TOR_EC <- WINTER_EC_EPA_ALPACA %>%
  mutate(
    EPA_EC_TOR_adj = EPA_EC_TOR * 0.8,  # EPA correction factor applied
    EPA_EC_TOT_adj = EPA_EC_TOT - 0.025  # Total EC calculation using original EPA_EC_TOR
  ) %>% 
  select(date_local,EPA_EC_TOT_adj, EPA_EC_TOR_adj, AE33_BC_CF_660_MAC_10.35) %>%  # Add your date column name here
  filter(!is.na(EPA_EC_TOR_adj), !is.na(AE33_BC_CF_660_MAC_10.35)) 

# Run the regression function and print the result 
regression_results_corrEPA_TOR_AE33 <- york_regression_2nd(clean_data_WINTER_AE33_corr_EPA_TOR_EC$EPA_EC_TOR_adj, clean_data_WINTER_AE33_corr_EPA_TOR_EC$AE33_BC_CF_660_MAC_10.35)


# Extract the regression results
slopeEPAcorr_AE33 <- regression_results_corrEPA_TOR_AE33$slope
interceptEPAcorr_AE33 <- regression_results_corrEPA_TOR_AE33$intercept
r_squaredEPAcorr_AE33 <- regression_results_corrEPA_TOR_AE33$r_squared
rmseEPAcorr_AE33 <- regression_results_corrEPA_TOR_AE33$rmse
maeEPAcorr_AE33 <- regression_results_corrEPA_TOR_AE33$mae


# Plot the results with annotation
ggplot(clean_data_WINTER_AE33_corr_EPA_TOR_EC, aes(x = EPA_EC_TOR_adj, y = AE33_BC_CF_660_MAC_10.35)) +
  geom_point(size = 3, alpha = 0.8, color = "black") +
  geom_abline(slope = slopeEPAcorr_AE33, intercept = interceptEPAcorr_AE33, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,4)+   xlim(0,4)+ 
  labs(title = expression(eBC[`AE33*`] ~ "vs" ~ EC_TOR[`EPA*`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 12, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 10, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(bquote(italic(EC_TOR)[NCore]^{`EPA*`}*~"("*mu*g~m^-3*")")) +
  ylab(expression(italic(eBC)[NCore]^{`AE33*`}*~"("*mu*g~m^-3*")")) +
  theme(legend.position = "") +
  annotate("text", x = 3.5, y = 4, label = "(b)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0.1, y = 4, 
           label = paste0("y = ", round(slopeEPAcorr_AE33, 2), " x", ifelse(interceptEPAcorr_AE33 >= 0, " + ", " - "), abs(round(interceptEPAcorr_AE33, 2)), 
                          "\nR² = ", round(r_squaredEPAcorr_AE33 , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") #-> plot_AE33_EPA_TORcorr



######### Figure S10 c   ##########

########### --> YORK REGRESSION CORRECTED EPA TOT  EC, AE33 WINTER   #############



names(WINTER_EC_EPA_ALPACA)


names(WINTER_EC_EPA_ALPACA)
clean_data_WINTER_AE33_corr_EPAtot_EC <- WINTER_EC_EPA_ALPACA %>%
  mutate(
    EPA_EC_TOR_adj = EPA_EC_TOR * 0.8,  # EPA correction factor applied
    EPA_EC_TOT_adj = EPA_EC_TOT - 0.025  # Total EC calculation using original EPA_EC_TOR
  ) %>% 
  select(date_local,EPA_EC_TOT_adj, EPA_EC_TOR_adj, AE33_BC_CF_660_MAC_10.35) %>%  # Add your date column name here
  filter(!is.na(EPA_EC_TOT_adj), !is.na(AE33_BC_CF_660_MAC_10.35)) 

# Run the regression function and print the result
regression_results_corrEPAtot_AE33 <- york_regression_2nd(clean_data_WINTER_AE33_corr_EPAtot_EC$EPA_EC_TOT_adj, clean_data_WINTER_AE33_corr_EPAtot_EC$AE33_BC_CF_660_MAC_10.35)


# Extract the regression results
slopeEPAcorrtot_AE33 <- regression_results_corrEPAtot_AE33$slope
interceptEPAcorrtot_AE33 <- regression_results_corrEPAtot_AE33$intercept
r_squaredEPAcorrtot_AE33 <- regression_results_corrEPAtot_AE33$r_squared
rmseEPAcorrtot_AE33 <- regression_results_corrEPAtot_AE33$rmse
maeEPAcorrtot_AE33 <- regression_results_corrEPAtot_AE33$mae


# Plot the results with annotation
ggplot(clean_data_WINTER_AE33_corr_EPAtot_EC, aes(x = EPA_EC_TOT_adj, y = AE33_BC_CF_660_MAC_10.35)) +
  geom_point(size = 3, alpha = 0.8, color = "black") +
  geom_abline(slope = slopeEPAcorrtot_AE33, intercept = interceptEPAcorrtot_AE33, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,4)+   xlim(0,4)+ 
  labs(title = expression(eBC[`AE33*`] ~ "vs" ~ EC_TOT[`EPA*`]))+ 
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 12, color = "black"), 
        axis.text = element_text(size = 14, face = "plain", color = "black")) +
  theme(plot.title = element_text(color = "tomato3", size = 10, hjust = 0.5, face = "bold")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  xlab(bquote(italic(EC_TOT)[NCore]^{`EPA*`}*~"("*mu*g~m^-3*")")) +
  ylab(expression(italic(eBC)[NCore]^{`AE33*`}*~"("*mu*g~m^-3*")")) +
  theme(legend.position = "") +
  annotate("text", x = 3.5, y = 4, label = "(c)",size = 5,fontface = "bold")+
  annotate("text", 
           x = 0.1, y = 4, 
           label = paste0("y = ", round(slopeEPAcorrtot_AE33, 2), " x", ifelse(interceptEPAcorrtot_AE33 >= 0, " + ", " - "), abs(round(interceptEPAcorrtot_AE33, 2)),  
                          "\nR² = ", round(r_squaredEPAcorrtot_AE33 , 2)),
           hjust = 0, vjust = 1, size = 4.2, color = "black") -> plot_AE33_EPA_TOTcorr


grid.arrange(plot_AE33_EPA_TORcorr, plot_AE33_EPA_TOTcorr,ncol = 1) -> plot_AE33_EPA_TORcorr_TOTcorr

grid.arrange(time_series_EPA_EC_correction_MAAP_PAAS_PAX_AE33_eBC, plot_AE33_EPA_TORcorr_TOTcorr,ncol = 2,
             widths = c(2, 1)) -> plot_TIMES_SERIES_AE33_EPA_TORcorr_TORcorr

ggsave("plot_TIMES_SERIES_AE33_EPA_TORcorr_TORcorr_8.5x6a_revision01.png", 
       plot = plot_TIMES_SERIES_AE33_EPA_TORcorr_TORcorr, 
       width = 8.5, height = 6, 
       device = "png",dpi=600)



####### --> AAE AE33 vs AAE vs PPAS >  ########

names(AE33_NCORE_hourly)
# Clean data (filter out missing values based on your conditions)
clean_data_AE33_PAAS_AAE <- AE33_NCORE_hourly %>%
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  filter(`Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` > -10, `Bland-Altman_CF_3.6_AE33_PAAS_405_DIFF` <10.2) %>% 
  filter(!is.na(PAAS_AAE_405_785), !is.na(AE33_AAE_370_880))

# Run the regression function and print the result
regression_results_AE33_PAAS_AAE <- york_regression_2nd(clean_data_AE33_PAAS_AAE$PAAS_AAE_405_785, 
                                                       clean_data_AE33_PAAS_AAE$AE33_AAE_370_880)



# Extract the regression results
slopeAE33_PAAS_AAE <-     regression_results_AE33_PAAS_AAE$slope
interceptAE33_PAAS_AAE <- regression_results_AE33_PAAS_AAE$intercept
r_squaredAE33_PAAS_AAE <- regression_results_AE33_PAAS_AAE$r_squared
rmseAE33_PAAS_AAE <-      regression_results_AE33_PAAS_AAE$rmse
maeAE33_PAAS_AAE <-       regression_results_AE33_PAAS_AAE$mae


# Plot the results with annotation
ggplot(clean_data_AE33_PAAS_AAE, aes(x = PAAS_AAE_405_785, y = (AE33_AAE_370_880))) +
  geom_point(size = 2, alpha = 0.9, color = "blue", shape=1) +
  geom_abline(slope = slopeAE33_PAAS_AAE, intercept = interceptAE33_PAAS_AAE, color = "red", size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_bw() + 
  ylim(0,3)+   xlim(0,3)+ 
  labs(title = expression(AAE[370/880]^{AE33} ~ "vs" ~ AAE[405/785]^{"PAAS-4"*lambda}))+
  theme(plot.title = element_text(color = "tomato3", size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) + 
  theme(axis.title = element_text(face = "plain", size = 16, color = "black"), 
        axis.text = element_text(size = 16, face = "plain", color = "black")) +
  theme(legend.key.height = unit(0.4, "inch")) + 
  theme(legend.key.width = unit(0.1, "inch")) + 
  #xlab(bquote(PAAS-4*lambda*~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  #ylab(bquote(AE33~abs[`(total,785 nm)`]~ '('*Mm^-1*')')) +
  xlab(expression(italic(AAE)[""*405/785]^{"PAAS-4"*lambda}*""**""))+  
  ylab(expression(italic(AAE)[""*370/880]^{"AE33"}*""**""))+
  theme(legend.position = "") +
  annotate("text", 
           x = 1.5, y = 1.7, 
           label = paste0("y = ", round(slopeAE33_PAAS_AAE, 2), " x", ifelse(interceptAE33_PAAS_AAE >= 0, " + ", " - "), 
                          abs(round(interceptAE33_PAAS_AAE, 2)),  
                          "\nR² = ", round(r_squaredAE33_PAAS_AAE , 2)),
           hjust = 0, vjust = 1, size = 4.5, color = "black")



######## FOR OBJECTIVE 3RD --> ###########





AE33_NCORE_hourly %>% 
  filter(`Bland-Altman_MAAP_AE33_diff` > -1, `Bland-Altman_MAAP_AE33_diff` <=2) %>% 
  mutate(AE33_eBC_conc_CF_660 = AE33_Tabs_CF_660/7.77) %>% 
  mutate(BC_ratio_OA = CTC_AMS_OA/(CTC_AMS_OA + MAAP_BC_637_con_mac10.4)) %>% 
  mutate(BC_ratio_OA = ifelse(BC_ratio_OA <= 0, NA, BC_ratio_OA)) %>%
  #mutate(AE33_AAE_370_880 = ifelse(CTC_AMS_OA <= 5, NA, AE33_AAE_370_880)) %>%
  mutate(AE33_AAE_370_880 = ifelse(`MAAP_BC_637_con_mac10.4` <1 , NA, AE33_AAE_370_880)) %>%
  select(TheTime,`MAAP_BC_637_con_mac10.4`,PAX_SSA_401_based_hour,delta_co,MCE_CTC_430,AE33_AAE_370_880,CTC_AMS_OA,BC_ratio_OA) %>% 
  filter(!is.na(CTC_AMS_OA)) %>%
  filter(!is.na(AE33_AAE_370_880)) %>%
  #ggplot(aes(y = PAX_SSA_401_based_hour, x = AE33_AAE_370_880, color = BC_ratio_OA)) +
  ggplot(aes(y = PAX_SSA_401_based_hour, x = AE33_AAE_370_880)) +
  geom_point(size = 3, color = "black", fill = "deepskyblue", shape = 21, alpha = 0.6) +
  #geom_point(shape = 16, size = 3, alpha=0.9) +
  theme_bw() + 
  #scale_color_stepsn(
  #  colours = c("black", "#56B4E9", "blue", "red","yellow"),
  #  limits = c(0.6, 1),
  #  breaks = c(0.6, 0.7, 0.8, 0.9, 0.95,1),
  #  labels = c("0.6", "0.7", "0.8", "0.9", "0.95","1.0"),
  #  name = expression(frac(OA, eBC+OA))) +
  scale_x_continuous(breaks = c(0,1, 1.5, 2, 3), limits = c(0,3))+
  #scale_x_continuous(breaks = c(0.7,0.8,0.9,1.0), limits = c(0.65,1.15))+
  scale_y_continuous(breaks = c(0.5,0.6,0.7,0.8,0.9,1.0), limits = c(0.5,1))+
  
  labs(title = expression(paste(AAE[370/880]^{AE33} ~ " vs " ~ SSA[PAX], "  (Winter = Jan-Feb)"))) +
  #labs(title = expression(AAE[370/880]^{AE33} ~ "vs" ~ SSA[PAX])~ "Winter =Jan-Feb") +
  annotate("text", y = 0.6, x = (0.1 + 0.1), parse = TRUE,
           label = as.character(expression(paste(OA[AMS] > 5*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  annotate("text", y = 0.55, x = (0.1 + 0.1), parse = TRUE,
           label = as.character(expression(paste(eBC[`MAAP`] > 1*" "*mu*g*m^-3))), 
           color = "black", size = 4.5, hjust = 0) +
  theme(
    plot.title = element_text(lineheight=.8, face="bold", size=14),
    legend.title = element_text(size=12),
    axis.title = element_text(face="plain", size=16, color="black"),
    axis.text = element_text(size=14, face="plain", color="black"),
    legend.key.height = unit(0.2, "inch"),
    legend.key.width = unit(0.05, "inch"),
    legend.text = element_text(size=12),
    legend.position = c(0.81, 0.5)
  ) + 
  theme(legend.position="none")+
  annotate("text", y = 0.98, x = 2.8, label = "(b)", size = 6.5, fontface = "bold") +
  annotate("text", y = 0.98, x = 1.5, label = "2022 Jan-Feb", size = 5, fontface = "bold") +
  
  ylab(expression(italic(SSA)[""*401~nm]^{PAX}*""**"")) +
  xlab(expression(italic(AAE)[""*370/880]^{"AE33"}*""**"")) #-> winter_aae_ssa_2022


ggsave("winter_aae_ssa_2022_5x4_04.pdf", winter_aae_ssa_2022,
       width = 5, height = 4, dpi = 300, bg = "white")















# Test if slope is significantly different from 1
# Using the York regression results and assuming you have access to the slope uncertainty

# Method 1: If your york_regression_2nd function provides slope uncertainty
# Install and load the IsoplotR package which has York regression
# install.packages("IsoplotR")
library(IsoplotR)

# Prepare data for York regression (needs uncertainties for both x and y)
# If you don't have measurement uncertainties, estimate them from scatter
x <- clean_data_PAX_PAAS_IR$PAAS_b_total_abs_785_Mm
y <- clean_data_PAX_PAAS_IR$PAX_Tabs870

x_err <- abs(x * 0.10)
y_err <- abs(y * 0.10)
york_fit <- york(cbind(x, x_err, y, y_err))

# Estimate uncertainties (you can adjust these if you have actual measurement errors)
x_err <- rep(sd(x) * 0.05, length(x))  # 5% of SD as example
y_err <- rep(sd(y) * 0.05, length(y))  # 5% of SD as example

# Run York regression
york_fit <- york(cbind(x, x_err, y, y_err))

# Correct extraction from York regression
slope_york <- york_fit$b[1]          # slope is in $b
slope_se_york <- york_fit$b[2]       # slope SE
intercept_york <- york_fit$a[1]      # intercept is in $a
intercept_se_york <- york_fit$a[2]   # intercept SE

# Test if slope = 1
t_stat <- (slope_york - 1) / slope_se_york
p_val <- 2 * pt(abs(t_stat), df = york_fit$df, lower.tail = FALSE)

# Calculate 95% confidence interval for slope
ci_lower <- slope_york - qt(0.975, york_fit$df) * slope_se_york
ci_upper <- slope_york + qt(0.975, york_fit$df) * slope_se_york

cat("\n=== York Regression Results ===\n")
cat("Slope:", slope_york, "±", slope_se_york, "\n")
cat("t-statistic (H0: slope=1):", t_stat, "\n")
cat("p-value:", p_val, "\n")
cat("Significantly different from 1?", ifelse(p_val < 0.05, "YES", "NO"), "\n")



differences <- clean_data_PAX_PAAS_IR$PAX_Tabs870 - 
  clean_data_PAX_PAAS_IR$PAAS_b_total_abs_785_Mm

# Paired t-test
paired_t_test <- t.test(clean_data_PAX_PAAS_IR$PAX_Tabs870, 
                        clean_data_PAX_PAAS_IR$PAAS_b_total_abs_785_Mm, 
                        paired = TRUE)

# Print results
cat("\n=== PAIRED T-TEST RESULTS ===\n")
cat("Mean difference (PAX - PAAS):", round(mean(differences), 3), "Mm⁻¹\n")
cat("SD of differences:", round(sd(differences), 3), "Mm⁻¹\n")
cat("t-statistic:", round(paired_t_test$statistic, 3), "\n")
cat("p-value:", format(paired_t_test$p.value, scientific = TRUE, digits = 3), "\n")
cat("95% CI for difference: [", round(paired_t_test$conf.int[1], 3), ",", 
    round(paired_t_test$conf.int[2], 3), "]\n")
