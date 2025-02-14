#volcano plot
volcano <- read.csv("Vulcano_data_V2.csv")
head(volcano)
str(volcano)
library(ggplot2)
library(ggrepel)

diff_line <- 2

#changing colour and assigning UP or down
volcano$diffexpressed <- "NO"
volcano$diffexpressed[volcano$log2_PAD4_vs_YFP > diff_line & volcano$pval_PAD4_vs_YFP < 0.01] <- "UP"
volcano$diffexpressed[volcano$log2_PAD4_vs_YFP < -diff_line & volcano$pval_PAD4_vs_YFP < 0.01] <- "DOWN"

#Same code as last part, but for names (2 -> 10)
volcano$diffexpressed2 <- "NO"
volcano$diffexpressed2[volcano$log2_PAD4_vs_YFP > 10 & volcano$pval_PAD4_vs_YFP < 0.01] <- "UP"
volcano$diffexpressed2[volcano$log2_PAD4_vs_YFP < -10 & volcano$pval_PAD4_vs_YFP < 0.01] <- "DOWN"

#uses last bit of code to generate labels
volcano$volcanolabel <- NA
volcano$volcanolabel[volcano$diffexpressed2 != "NO"] <- volcano$Majority.protein.IDs[volcano$diffexpressed2 != "NO"]

ggplot(data = volcano, aes(x=log2_PAD4_vs_YFP,y=-log10(pval_PAD4_vs_YFP), col=diffexpressed, label= volcanolabel)) + 
  geom_point() + 
  theme_minimal() +
  geom_text_repel(size = 2) +
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-diff_line, diff_line), col="red") +
  geom_hline(yintercept=-log10(0.01), col="red") +
  labs(title = "Volcano plot PAD4 vs YFP", x = "log2 abundance PAD4 vs YFP", y = "-log10(p-value)", colour = "differential expression")
  
#box plot
protein = "AT3G52430.1"

#pulling the values from the df
PAD4_1 <- volcano$Norm_abundance_PAD4_rep1[volcano$Majority.protein.IDs == protein]
PAD4_2 <- volcano$Norm_abundance_PAD4_rep2[volcano$Majority.protein.IDs == protein]
PAD4_3 <- volcano$Norm_abundance_PAD4_rep3[volcano$Majority.protein.IDs == protein]
PAD4_4 <- volcano$Norm_abundance_PAD4_rep4[volcano$Majority.protein.IDs == protein]
YFP_1 <- volcano$Norm_abundance_YFP_rep1[volcano$Majority.protein.IDs == protein]
YFP_2 <- volcano$Norm_abundance_YFP_rep2[volcano$Majority.protein.IDs == protein]
YFP_3 <- volcano$Norm_abundance_YFP_rep3[volcano$Majority.protein.IDs == protein]
YFP_4 <- volcano$Norm_abundance_YFP_rep4[volcano$Majority.protein.IDs == protein]

#Putting the values into another df
abundance <- c(PAD4_1, PAD4_2, PAD4_3, PAD4_4, YFP_1, YFP_2, YFP_3, YFP_4)
gene_names <- rep(c("PAD4", "YFP"), each = 4)
df <- data.frame(abundance, gene_names)

#Getting useful information for the plot title
log2ratio = round(volcano$log2_PAD4_vs_YFP[volcano$Majority.protein.IDs == protein], digits = 2)
pval <- volcano$pval_PAD4_vs_YFP[volcano$Majority.protein.IDs == protein]

#Making the plot title in advance
plot_title <- paste("Normalized abundance of ", protein, ", ", "log2 ratio ", log2ratio, ", " ,"p=", pval, sep = "") 

ggplot(df, aes(x = gene_names, y = abundance)) + 
  geom_boxplot() +
  theme_minimal() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
  labs(title = plot_title, x = "sample", y = "Normalized abundance")

test <- read.delim("Vulcano_data.txt")
