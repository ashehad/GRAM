# GRAM
GRAM is a trait-based grassland plant production model, that was fully described in Ali et al. (2013). The model  simulates 
plant nitrogen  (N) and carbon  (C) dynamics using a set of difference equations. The processes simulated include 
photosynthesis, respiration, carbon  and  nitrogen  allocation,  turnover,  and  nitrogen uptake. 
At the leaf scale, the response of photosynthesis to variations in light, temperature, and CO2 concentration
is represented using the standard biochemical model of C3 photosynthesis (Farquhar and von Caemmerer 1982) and depends
on the maximum Rubisco activity (Vcmax), which is a function of leaf nitrogen content. The leaf intercellular CO2
concentration, Ci, is calculated from the optimal stomatal conductance model of Medlyn et al.(2011). Instantaneous leaf 
photosynthesis is calculated for sunlit and shaded leaf separately (Medlyn et al. 2000) using leaf area index 
(LAI, m2 m-2) and incident radiation. 
Daily canopy photosynthesis was calculated as the integral of the instantaneous photosynthesis. Whole-plant
respiration is assumed to be proportional to whole-plant photosynthesis. Biomass increment of leaves and roots is
a function of C allocation and turnover rates. N uptake is represented as a saturating function of root biomass
(Br, gC m-2). 
For this study, the net soil N mineralization is held constant and equal to 3 g N m-2 yr-1. The model is deterministic,
in common with other models examining grassland community dynamics (Parton et al.1994; Cannell and Thornely 1998).
The model has twelve parameters that represent plant traits. In the model, a species is characterized as a vector of 
values for these plant trait parameters. Thus, growth rate of a range of different species can be simulated by varying
the input parameters to the model.





	
	

