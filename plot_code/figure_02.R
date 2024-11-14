library(ggplot2)
library(ggpubr)
library(rstatix)

### Figure 2
# Fig 2A
# load the data from "Supporting_data_values.xlsx" Tab "Fig. 2A" to a data frame "data"
df.plot$`Treatment response` <- gsub(" ", "\n", df.plot$`Treatment response`)
colnames(df.plot)[1] <- "Treatment\nresponse"
gg <- ggplot(data = data,
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


# Fig 2B
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
