
<!-- PROJECT LOGO AND INTRO SECTION -->
<br />
<div align="center">
  <h2 align="center">Heart Disease Indicators by Sex</h2>

  <p align="center">
    Heart disease is a leading cause of death in the United States. Gender disparities in studying heart disease throughout history are well documented and frequent. This can lead to unequal treatment and diagnosis for patients based on gender.

This project identifies indicators for heart disease, and understanding how patient sex plays a role in which indicators to focus on for diagnosis.
    <br />
    <br />
    :heart:
    
  </p>
  
</div>

### Quick Links:

* [Analysis](writeup.md) Take a look at my code and analysis!
* [Data Source](https://archive.ics.uci.edu/dataset/45/heart+disease)


### Summary 
__This project explores heart disease indicators with a focus on gender disparities using decision tree models.__

My primary questions are:

1. How do heart disease indicators differ between men and women?
2. Which indicators are most significant for predicting heart disease in each sex?
3. How can we ensure equal representation of women in medical research?

By analyzing a heart disease dataset, I built decision trees to uncover patterns in symptoms and risk factors. The results reveal distinct differences in how heart disease presents itself in men and women, highlighting the need for gender-specific diagnostic criteria. 

Notably, the decision tree trained on the entire dataset __did not effectively cover women__. In fact, it __incorrectly classified 50% of women who actually had heart disease,__ underscoring the inadequacy of the models. By training individual decision trees by sex, we immediately see the difference in which indicators are most important for men and women, and they are not the same. __The decision tree trained on women achieves an accuracy of 80%__ and illustrates the stark difference in which indicators medical professionals must focus on depending on sex.

This analysis emphasizes the importance of including equal numbers of male and female participants in medical studies, as well as creating accuracy metrics that account for both groups individually. Future work will focus on exploring additional models, incorporating more diverse data sources, and optimizing our models with cross-validation techniques.

### References
Centers for Disease Control and Prevention. (2021, January 19). Heart disease. Centers for Disease Control
and Prevention. Retrieved September 29, 2021, from https://www.cdc.gov/heartdisease/index.htm

Doyal, L. (2001). Sex, gender, and health: The need for a new approach. BMJ, 323(7320), 1061–1063.
https://doi.org/10.1136/bmj.323.7320.1061

Weisz, D., Gusmano, M. K., & Rodwin, V. G. (2004). Gender and the treatment of heart disease in older persons
in the United States, France, and England: A Comparative, Population-based view of a clinical phenomenon. Gender Medicine, 1(1), 29–40. https://doi.org/10.1016/s1550-8579(04)80008-1

Janosi, A., Steinbrunn, W., Pfisterer, M., Detrano, R. (1988). Heart Disease Data Set, electronic dataset, UCI 
Machine Learning Repository. https://archive.ics.uci.edu/ml/datasets/Heart+Disease

