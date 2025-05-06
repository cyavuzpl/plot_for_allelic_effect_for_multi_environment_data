# Load required libraries
library(readxl)  # For reading Excel files
library(ggplot2) # For creating plots
library(dplyr)   # For data manipulation

# Read data from Excel
data <- read_excel("protein_snp_results.xlsx")

# Clean and manipulate data
data <- data %>%
  mutate(
    Change = as.numeric(gsub(",", ".", Change)),  # Convert commas to dots in 'Change'
    P_Value = as.numeric(gsub(",", ".", P_Value)) # Convert commas to dots in 'P_Value'
  ) %>%
  mutate(Outname = recode(Outname,  # Recode Outname for readability
                          "AH2022.vcf.phe" = "Ames_High_2022", 
                          "AL2022.vcf.phe" = "Ames_Low_2022",
                          "AM2022.vcf.phe" = "Ames_Medium_2022",
                          "april_misso.vcf.phe" = "Missouri_2022",
                          "CL2022.phe" = "Crawfordsville_Low_2022", 
                          "CM2022.phe" = "Crawfordsville_Medium_2022", 
                          "LM2022.phe" = "Lincoln_Medium_2022", 
                          "LM2023.phe" = "Lincoln_Medium_2023",
                          "MM2022.phe" = "Missouri_Medium_2022", 
                          "MM2023.phe" = "Missouri_Medium_2023", 
                          "nocov_2021.vcf.phe" = "Nebraska_2021", 
                          "SH2022.phe" = "Scottsbluff_High_2022"
  )) %>%
  mutate(
    Significance = if_else(P_Value < 0.05, "*", NA_character_), # Mark significant SNPs with "*"
    Size = abs(Change)  # Size points based on absolute 'Change'
  )

# Order SNPs for plotting
data$SNP <- factor(data$SNP, levels = unique(data$SNP))

# Create plot
p <- ggplot(data, aes(x = Change, y = SNP)) +
  geom_point(aes(fill = Outname, size = Size), shape = 21, color = "black") +  # Plot points
  geom_text(
    data = filter(data, !is.na(Significance)),
    aes(label = Significance), hjust = -0.6, vjust = -0.6, size = 3.2
  ) +  # Add significance labels
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +  # Add vertical line at x = 0
  scale_size_continuous(range = c(2, 7), guide = "none") +  # Adjust point sizes
  scale_x_continuous(limits = c(-20, 20), breaks = seq(-30, 30, by = 5)) +  # Set x-axis limits and breaks
  scale_fill_manual(values = c(  # Custom colors for different environments
    "Ames_High_2022" = "#FF6347", 
    "Ames_Low_2022" = "#4682B4", 
    "Ames_Medium_2022" = "#32CD32", 
    "Missouri_2022" = "#FFD700", 
    "Crawfordsville_Low_2022" = "#8A2BE2",  
    "Crawfordsville_Medium_2022" = "#7FFF00", 
    "Lincoln_Medium_2022" = "#D2691E", 
    "Lincoln_Medium_2023" = "#B22222", 
    "Missouri_Medium_2022" = "#20B2AA", 
    "Missouri_Medium_2023" = "#FF4500", 
    "Nebraska_2021" = "#008080",  
    "Scottsbluff_High_2022" = "#A52A2A"
  )) +
  labs(
    x = "Percent Change", y = "SNP", fill = "Environment"  # Axis labels
  ) +
  theme_minimal() +  # Minimal theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    legend.position = "bottom",  # Move legend to bottom
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 9)  # Adjust legend text size
  )

# Save plot
ggsave("SNP_Effect_FinalPlot_UniqueColors.png", plot = p, width = 10, height = 8, dpi = 300, bg = "white")  # Save plot as PNG
