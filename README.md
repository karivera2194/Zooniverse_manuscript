# Optimizing community science contributions in ecology: a case study on Zooniverse’s ‘Chicago Wildlife Watch’
**Kimberly Rivera 1* (Orcid; kimberly.rivera22194@gmail.com; 2001 N. Clark St. Chicago, IL 60614), Mason Fidino 1, Elizabeth W. Lehrer 1, Holly R. Torsey 2, Sarah Allen 3, Laura Trouille 4, and Seth B. Magle 1 

1 Department of Conservation and Science, Lincoln Park Zoo, Chicago, IL, USA
2 Community Scientist, Zooniverse, c/o Department of Conservation and Science, Lincoln Park Zoo, Chicago, IL, USA
3 Senior Front-end Engineer, Team Manager, 3dna Corp. dba NationBuilder, Los Angeles, CA, USA
4 Science Engagement Division and Zooniverse, The Adler Planetarium, Chicago, IL, USA

**corresponding author*

### 'Zooniverse_class.mod.git.R' 
This script loads in cleaned data and formats it for a binomial generalized linear model with a logit link. It also has code to plot variation in user accuracy for two retirement rules.

### 'Zooniverse_empty.mod.git.R' 
This script loads in cleaned data and formats it for a binomial generalized linear mixed model using only empty images. It also has code to plot accuracy across user agreement, or evenness, and animal weight.

### 'Zooniverse_sp.mod.git.R' 
This script loads in cleaned data and formats it for a binomial generalized linear mixed model using species-present images. It also has code to plot accuracy across user agreement, or evenness.

### 'animal_weights.csv' 
This is a .csv of animal weights used in the bionial generalized linear mixed model of species-present images.

### 'zoo_merged.zip' 
This is a .csv of raw data extracted from Chicago Wildlife Watch.



