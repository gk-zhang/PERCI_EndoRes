library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)
library(ggalluvial)


### Figure 2
# Fig. 2A
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2A" to a data frame "df.plot"
df.plot$`Treatment response` <- gsub(" ", "\n", df.plot$`Treatment response`)
colnames(df.plot)[1] <- "Treatment\nresponse"
gg <- ggplot(data = df.plot,
             aes(axis1 = Histology_grade_baseline,
                 axis2 = `Treatment\nresponse`,
                 axis3 = `Histology_grade_post-pET`, 
                 y = Number_of_Samples)) +
  geom_alluvium(aes(fill = `Treatment\nresponse`)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 3, lineheight = 0.8) +
  coord_cartesian(ylim = c(10, 380),expand = TRUE, clip = "off") +
  scale_x_discrete(limits = colnames(data)[c(2,1,3)],
                   expand = c(.15, .05),
                   labels = c("Grade", "Treatment response", "Grade")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 11, colour = "black"),
        axis.title.x = element_blank(),
        # axis.title.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box.margin = margin(t = -8, r = 0, b = 0, l = 0),
        panel.border = element_blank(), axis.line = element_line()
  ) +
  scale_fill_manual(values = colorfill) +
  ylab("Number of Samples") +
  annotate(geom = "text", x = c(1, 3), y = 390, label = c("baseline", "post-pET"), size = 4)

jpeg("dis_grade_change.jpg", height = 6.3, width = 3.6, res = 600, units = "in")
print(gg)
dev.off()


# Fig. 2B
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2B" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("NST" = "darkseagreen4", "ILBC" = "gold1")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "Histology\nType") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -130),
    legend.position = "bottom"
  ) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("dis_histologytype_baseline_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg 
dev.off()

# Fig. 2C
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2C" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("less equal 10" = "#fdd0a2", "more than 10" = "#fdae6b"), labels=c(expression(""<=10), "> 10")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "PR Group") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 10, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -16, r = 0, b = -12, l = 0),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent")) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "AI_NR_baseline",
                            "TAM_R_post-pET","TAM_NR_post-pET","AI_R_post-pET","AI_NR_post-pET"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "AI\nNR",
                            "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5, 7.5), y = 137, label = unique(df.plot$group), size = 4)

jpeg("dis_pr_all_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()

# Fig. 2D
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2D" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("LumA" = "#fdae6b", "LumB" = "#003366")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "Luminal\nSubtype") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -130),
    legend.position = "bottom"
    ) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("dis_luminal_baseline_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2E
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2E" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("1"="#ec8686", "2"="#ee3b3b", "3"="#8a0707")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "RS Group") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -140),
    legend.position = "bottom"
  ) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("dis_recurrencescore_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2F
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2F" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("1" = "rosybrown1", "2" = "rosybrown3", "3" = "rosybrown4"), labels=c(expression(""<=9), "10-40", "> 40")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "PaTILs Group") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = 0),
    legend.position = "bottom") +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "AI_NR_baseline",
                            "TAM_R_post-pET","TAM_NR_post-pET","AI_R_post-pET","AI_NR_post-pET"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "AI\nNR",
                            "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5, 7.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("dis_patils_all_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2G
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2G" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond, y=ki67)) +
  geom_boxplot(aes(fill=group.treatment.respond), outlier.size = 0.5) +
  theme(
    axis.text.x = element_text(size = 10),
    # axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -32),
    legend.position="bottom",
  ) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_x_discrete(limits=c("TAM_R","TAM_NR","AI_R","AI_NR"),
                   labels=c("TAM R", "TAM NR", "AI R", "AI NR")) +
  scale_fill_manual(values = c("baseline_TAM_R" = "#4d769e", "post-pET_TAM_R" = "#003366", "baseline_TAM_NR" = "#a4eced", "post-pET_TAM_NR" = "#00CED1", 
                               "baseline_AI_R" = "#de90c9", "post-pET_AI_R" = "#ae017e", "baseline_AI_NR" = "#fad5e4", "post-pET_AI_NR" = "#f768a1"),
                    labels = c("baseline_TAM_R" = "baseline", "post-pET_TAM_R" = "post-pET", "baseline_TAM_NR" = "baseline", "post-pET_TAM_NR" = "post-pET",
                               "baseline_AI_R" = "baseline", "post-pET_AI_R" = "post-pET", "baseline_AI_NR" = "baseline", "post-pET_AI_NR" = "post-pET")) +
  labs(y = "Ki67 staining (%)", fill = "Group") +
  coord_cartesian(ylim = c(0, 110))

jpeg("dis_ki67_boxplot.jpg", height = 2.2, width = 3.96, res = 600, units = "in")
gg
dev.off()

# Fig. 2H
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2H" to a data frame "df.plot"
gg <- ggplot(df.plot,
             aes(axis1 = Histology_grade_baseline,
                 axis2 = `Treatment\nresponse`,
                 axis3 = `Histology_grade_post-pET`, 
                 y = Number_of_Samples)) +
  geom_alluvium(aes(fill = `Treatment\nresponse`)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 3, lineheight = 0.8) +
  coord_cartesian(ylim = c(10, 300),expand = TRUE, clip = "off") +
  scale_x_discrete(limits = colnames(data)[c(2,1,3)],
                   expand = c(.15, .05),
                   labels = c("Grade", "Treatment response", "Grade")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 11, colour = "black"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.box.margin = margin(t = -8, r = 0, b = 0, l = 0),
        panel.border = element_blank(), axis.line = element_line(),
        # plot.margin = margin(t = 0, r = 8, b = 0, l = 2, unit = "mm")
  ) +
  scale_fill_manual(values = colorfill) +
  ylab("Number of Samples") +
  annotate(geom = "text", x = c(1, 3), y = 290, label = c("baseline", "post-pET"), size = 4)

jpeg("val_grade_change.jpg", height = 6.3, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2I
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2I" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("NST" = "darkseagreen4", "ILBC" = "gold1", "Others" = "orchid4")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "Histology\nType") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -95),
    legend.position = "bottom"
  ) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("val_histologytype_baseline_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2J
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2J" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("less equal 10" = "#fdd0a2", "more than 10" = "#fdae6b"), labels=c(expression(""<=10), "> 10")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "PR Group") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 10, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -16, r = 0, b = -12, l = 0),
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent")) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "AI_NR_baseline",
                            "TAM_R_post-pET","TAM_NR_post-pET","AI_R_post-pET","AI_NR_post-pET"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "AI\nNR",
                            "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5, 7.5), y = 137, label = unique(df.plot$group), size = 4)

jpeg("val_pr_all_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2J
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2K" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("LumA" = "#fdae6b", "LumB" = "#003366")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "Luminal\nSubtype") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -130),
    legend.position = "bottom"
  ) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("val_luminal_baseline_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2L
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2L" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("1"="#ec8686", "2"="#ee3b3b", "3"="#8a0707")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "RS Group") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -140),
    legend.position = "bottom"
  ) +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "TAM_R_baseline", "TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "TAM\nR", "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("val_recurrencescore_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2M
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2M" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond.group, y=counts)) +
  geom_bar(position="stack", stat="identity", aes(fill=type), width = 0.7) +
  scale_fill_manual(values=c("1" = "rosybrown1", "2" = "rosybrown3", "3" = "rosybrown4"), labels=c(expression(""<=9), "10-40", "> 40")) +
  theme_classic() +
  labs(x = NULL, y = "Patient count", fill = "PaTILs Group") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text( 
      margin = margin(t = 0, r = 0, b = 0, l = 0)), 
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = 0),
    legend.position = "bottom") +
  scale_x_discrete(limits=c("TAM_R_baseline","TAM_NR_baseline","AI_R_baseline","AI_NR_baseline", "AI_NR_baseline",
                            "TAM_R_post-pET","TAM_NR_post-pET","AI_R_post-pET","AI_NR_post-pET"),
                   labels=c("TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR", "AI\nNR",
                            "TAM\nR", "TAM\nNR", "AI\nR", "AI\nNR")) +
  coord_cartesian(ylim = c(0, 130),expand = TRUE, clip = "off") +
  annotate(geom = "text", x = c(2.5, 7.5), y = 130, label = unique(df.plot$group), size = 4)

jpeg("val_patils_all_stackbar.jpg", height = 2, width = 3.6, res = 600, units = "in")
gg
dev.off()


# Fig. 2N
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2N" to a data frame "df.plot"
gg <- ggplot(df.plot, aes(x=treatment.respond, y=ki67)) +
  geom_boxplot(aes(fill=group.treatment.respond), outlier.size = 0.5) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.box.margin = margin(t = -13, r = 0, b = 0, l = -32),
    legend.position="bottom",
  ) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_x_discrete(limits=c("TAM_R","TAM_NR","AI_R","AI_NR"),
                   labels=c("TAM R", "TAM NR", "AI R", "AI NR")) +
  scale_fill_manual(values = c("baseline_TAM_R" = "#4d769e", "post-pET_TAM_R" = "#003366", "baseline_TAM_NR" = "#a4eced", "post-pET_TAM_NR" = "#00CED1", 
                               "baseline_AI_R" = "#de90c9", "post-pET_AI_R" = "#ae017e", "baseline_AI_NR" = "#fad5e4", "post-pET_AI_NR" = "#f768a1"),
                    labels = c("baseline_TAM_R" = "baseline", "post-pET_TAM_R" = "post-pET", "baseline_TAM_NR" = "baseline", "post-pET_TAM_NR" = "post-pET",
                               "baseline_AI_R" = "baseline", "post-pET_AI_R" = "post-pET", "baseline_AI_NR" = "baseline", "post-pET_AI_NR" = "post-pET")) +
  labs(y = "Ki67 staining (%)", fill = "Group") +
  coord_cartesian(ylim = c(0, 110))

jpeg("/home/t15g2/work/phd/publication/plots/clinic/val_ki67_boxplot.jpg", height = 2.2, width = 3.96, res = 600, units = "in")
gg
dev.off()
