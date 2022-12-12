# Vital Signs Case Study
# Kyle Bragg
# University of Florida
# kylebragg@ufl.edu

# loading packages -------

# loading tidyverse
library(tidyverse)

# getting and filtering data ------

# getting yield and sales
crop_data <- read_csv("/Users/kylebragg/downloads/rstudio-export/household_fieldcrop_TZA.csv")
str(crop_data)
crop_data <- crop_data %>% 
  select(landscape_no, eplot_no, crop_name, ag5a_02_1, ag5a_03) %>% 
  rename(kg_sold = ag5a_02_1, value_sales = ag5a_03)
crop_data

# getting nitrogen and carbon contents
soil_data <- read_csv("/Users/kylebragg/downloads/rstudio-export/eplot_depth_TZA.csv")
str(soil_data)
soil_data <- soil_data %>% 
  filter(depth_class == "top", landscape_no != 0) %>% 
  select(landscape_no, eplot_no, proc_total_nitrogen, proc_total_carbon)
soil_data

# getting species diversity
veg_data <- read_csv("/Users/kylebragg/downloads/rstudio-export/eplot_subplot_tree_TZA.csv")
str(veg_data)
veg_data <- veg_data %>% 
  filter(landscape_no != 0) %>% 
  select(landscape_no, eplot_no, genus, species)
veg_data

# cleaning data -----

# condensing by landscape, plot, and crop and converting to USD
crop_data[is.na(crop_data)] = 0 
crop_data_clean <- crop_data %>% 
  group_by(landscape_no, eplot_no, crop_name) %>% 
  summarize(sum_crop = sum(kg_sold), sum_value = sum(value_sales)) %>% 
  mutate(sum_value_usd = sum_value*0.000601, 
         value_usd_kg = sum_value / sum_crop * .000601)
crop_data_clean

# determining different species and their prevalence in each plot
veg_data_clean <- veg_data %>% 
  mutate(plant_name = paste(genus, species)) %>% 
  group_by(landscape_no, eplot_no) %>% 
  summarize(species_richness = length(unique(plant_name)))
veg_data_clean


# joining data -----

# joining crop soil and veg data on crop, landscape, eplot
all_data <- left_join(crop_data_clean, soil_data) %>% 
  left_join(veg_data_clean)
all_data

# analyzing crop distributions and monetary value -------

# grouping smaller crops for readability
other_data <- all_data %>% 
  group_by(crop_name) %>% 
  summarize(total_yield = sum(sum_crop), total_value = sum(sum_value_usd)) %>% 
  filter(total_yield < 2000, total_value < 500)
big_crop_data <-  all_data
for (crop in unique(other_data$crop_name)) {
  big_crop_data[big_crop_data == crop] <- "Other"
}

# plotting crop distributions for each landscape  
ggplot(big_crop_data, aes(x = factor(landscape_no), y = sum_crop, 
                          fill = crop_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Crop",
                    values = c("brown4", "chocolate4", "darkgoldenrod1", 
                                "deeppink2", "darkgray", "darkolivegreen1", 
                                "chartreuse", "darkolivegreen4", "brown2",
                                "darkgoldenrod4")) +
  labs(x = "Landscape", y = "Sum of agricultural products (kg)") +
  theme_bw()

# plotting crop value distribution for each landscape
ggplot(big_crop_data, aes(x = factor(landscape_no), y = sum_value_usd, 
                     fill = crop_name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Crop",
                    values = c("brown4", "chocolate4", "darkgoldenrod1", 
                                "deeppink2", "darkgray", "darkolivegreen1", 
                                "chartreuse", "darkolivegreen4", "brown2",
                                "darkgoldenrod4"))+
  labs(x = "Landscape", y = "Value of agricultural products sold (USD)") +
  theme_bw()

# analyzing nitrogen contents and yield ------

# standardizing yield dependent upon crop type
standardize <- function(crop)
{
    data <- all_data %>% 
      ungroup() %>% 
      filter(crop_name == crop) %>% 
      mutate(zscore_yield = (sum_crop - mean(sum_crop))/sd(sum_crop)) 
  return(data)  
  }
standardized_yield_data <- rbind(standardize("Maize"), standardize("Beans")) %>% 
  rbind(standardize("Paddy"))

# plotting relative yield against nitrogen content for larger crops
ggplot(standardized_yield_data %>% 
         filter(proc_total_nitrogen != 0), 
       aes(x = proc_total_nitrogen, y = zscore_yield,  color = crop_name)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("green4", "yellow3", "grey28")) + 
  facet_wrap(~ crop_name) +
  theme_bw() +
  labs(x = "Nitrogen", y = "Yield (kg)", color = "Crop")

# analyzing species biodiversity and monetary value -------

# plotting species richness against value of products sold
ggplot(all_data %>% 
         filter(sum_value != 0, landscape_no %in% c("L11", "L19", "L20"), 
                (species_richness != 0)),
       aes(x = species_richness, y = sum_value_usd, color = landscape_no)) +
  geom_point() +
  scale_color_manual(values = c("slategray3", "deepskyblue1", "deepskyblue4")) +
  labs(x = "Species richness", y = "Value of agricultural products sold (USD)",
       color = "Landscape") + 
  theme_bw()
