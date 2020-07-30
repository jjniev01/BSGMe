#  BSGMe Methods
##  Extrapolation
###  Overview
Here we are extrapolating for every year between an intial timepoint, _t0_, and a second time point, _t1_, which is at least one time unit away (e.g. months, years, etc.) and for which we have observed built settlement (BS) extents. We start by retrieving and combining any previously created Random Forest (RF) objects created for the study area during interpolation using the [BSGMi][l2]. This hybrid RF is then used to produce a continuous surface across the study area representing the probability of a given pixel transitioning from non-BS to BS given the covariates at _t0_ and the observed relationships for interpolated periods prior to _t0_. 

We continue by creating a population map for every year of the interpolation period represented by the RF objects, e.g. if the RF objects combined represented the years 2000-2012 and 2012-2014 we would create population surfaces for all years between 2012 and 2014 with 2014 being our _t0_. We then take the previously modeled annual extents for those same years, which were produced by the [BSGMi][l2], and calculate three values for every subnational unit and year:

1.  Number of BS cells within the unit
2.  The sum of population within BS cells of the unit
3.  The average BS population density of each unit 

Then, independently for each subnational unit across all the extracted years, we fit one of four models, based upon which best fits the observed data, for the average BS population density and for the **U**rban **R**ural **R**atio (URR). These models are then used to extrapolate these values out to time _t1_ and derive the expected BS population of each unit. Lastly, we simply divide the unit and year specific BS population by the average BS population density to obtain the total number of projected cells that are BS in each unit for that corresponding year.


Then for every time step, and on a unit by unit basis we take the RF transition probability layer and the estimated number of cells that transition. The most basic model simply chooses from the cells which were known to transition by selecting the _n_-th highest probabilities for transitioning, converts them to "BS", writes out the new BS extents, and uses those extents as the basemap for the next time step of transitioning. However, the default modeling setting utilizes admin.-unit normalized lagged lights-at-night (LAN) data, if available for that year, to annually adjust the base RF-derived transition probabilities. The assumption behind this being that areas which underwent the largest increase in brightness, relative to the rest of the admin. unit, in a single time step have a higher probability of transitioning and vice versa. The transitioning procedure follows the same as above.

This results in a series of regularly spaced time-specific binary spatial predictions of the BS extents in raster format.




###  Random Forest (RF) Estimation of the Probability of Transition
In the base interpolation models [BSGMi][l2], to create a probability of transition surface for such a complex and non-linear phenomena as BS transition, we chose to utilize a RF model to accurately and efficiently determine this across an entire country at the pixel (3 arc second) level in an automatable and paralellizable fashion. We trained the classification RF on whether a cell was observed to have transitioned (value of 1) or not transitioned (value of 0) between time _t0_ and _t1_ along with the corresponding values of a reduced set of time-specific, or assumed to be time invariant, covariates at _t0_ which are as follows:

* Distance to Nearest Edge of BS
* Proportion of BS within 1 pixel
* Proportion of BS within 5 pixels
* Proportion of BS within 10 pixels
* Proportion of BS within 15 pixels
* Elevation (assumed temporally invariant)
* Slope (assumed temporally invariant)
* Distance to Nearest Edge of Level 1 Protected Areas ([WDPA][r1]) (assumed temporally invariant from _t0_)

The covariate choice was based upon the Africa-specific urban growth model by [Linard et al. 2013][r2].

For the extrapolation model, we take advantage of the ensemble nature of RF models and combine the RF models used for previous interpolation periods. The purpose behind this is to assume that future growth will follow the relationships observed from the previous time periods and, because of the varying length of modeled periods and the data driving the transitions, to make sure that our projections are not entirely driven by the most recently modeled period, which in our case is a period of three years. We then take the merged RF to predict across the entire modeling area. Rather than accepting the default output of a RF classifier, which puts out the single majority predicted class as indicated by the predictions of its individual constituent trees, we wanted a continuous, 0.00 to 1.00, probability of transitioning. Therefore, given that the RF is trained as a binary classifier, we took the mean of the individual binary predictions and output them in a spatially explicit manner as a raster. More specifically, this value between 0.00 and 1.00 represents the posterior probability of a cell being classified by the RF as having transitioned between _t0_ and _t1_.  
  
  
  
  
###  Lights-At-Night (LAN) Processing
Here we utilize a corrected database of annually aggregated DMSP LAN data and a collection of annually aggregated VIIRS LAN data. The DMSP data covers the time period of 2000 to 2011 and the VIIRS data covers from 2012 to 2016. The processing which occurs only happens on DMSP or on VIIRS; there is no crossover between the datasets even though they are temporally adjacent as that would cause undue difficulties in resolving method and instrumentation differences between the datasets.
  
For a given LAN dataset, covering the set of regular time points \{ _T_ \} = \{1, 2, ..., _t_\}, we begin by lagging every LAN dataset at a given time point _t_ within \{ _T_ \}, from _t_ = 2 to _t_, such that:

  
![Alt Text][i1]


Then for every _t_, we approach reweighting the lagged LAN data to values between 0.00 and 1.00 for every admin unit _j_ using the following equation for every pixel _i_ and where _x_ is a member of the set \{ _X_ \}:


![Alt Text][i2]


These year specific reweighted data are then output as rasters. The script responsible for this processing can be found in the `/accessories/` folder of the [BSGMi][l2] repository and is titled [LANClusterProcessing.R][https://bitbucket.org/jjnieves/bsgm/src/master/accessories/LANClusterProcessing.R].




###  Population Mapping
For any given interpolation period we first need to have a best estimate of the admin. unit specific BS population at the time points bracketing the modeling period. To get this we create a "first draft" population using the available time-specific and, assumed, time-invariant covariates and the WorldPop RF method of dasymetrically redistributing the time-specific population totals from the admin. unit level to 100 meter resolution rasters, as detailed in [Stevens et al. 2015][r5]. Based upon findings from [Gaughan et al. 2016][r6] for a given time point, the distance to nearest BS edge for the previous year was included. For example, if we were to model the  population map of 2012 we would include not only the distance to nearest BS edge for 2012 as one of the predictive covariates, but would also include the distance to nearest BS edge corresponding to the 2011 BS extents. This is done to show that older parts of a BS agglomeration are different from newer ones and, from a practical modelling stance, to avoid the appearance of "doughnuts" where centers of agglomerations appear to have exceedingly low population densities relative to the preceeding modeled timepoint. Once these population models are created, then the total population spatially coincident with the BS extents are extracted and summed by subnational unit and the corresponding BS population density is derived as well for use in the BSGMe predictive phases.




###  Extrapolation Model Fitting Process
The process of determining what type of model best fits the previously interpolated average BS population density and the URR is done independently for each subnational unit. The five possible model classes are:

-  Historical Mean
-  Linear Constant Rate
-  Exponential Constant Rate
-  [Theta Model][r3]
-  [**A**uto**R**egressive **I**ntegrated **M**oving **A**verage (ARIMA) Models][r4]
-  [Exponential Smoothing Models (**E**rror **T**rend **S**easonality (ETS))][r4]

Given that we are working with an extremely short time series, less than 20 points, and comparing models with different base assumptions, assessing model predictive performance becomes a challenge. We opted to carry out a predictive _k_-fold cross-validation, also known as a 'rolling forecast origin,' where we make the _k_ equal to one-third of the time series and we compare the models based upon the resulting **M**ean **A**bsolute **E**rror (MAE). The model with the lowest MAE is then selected to be trained upon the entire timeseries and project out to time _t1_.

Given that we define the URR at time _t0_ as:


![Alt text][i3]


Where _BSPOP\_o_ is the BS Population of the admin unit and _POP\_o_ is the total admin. population at time _t0_.

And BS population density ( _BSD_ ) at time _t0_ is given by the equation:


![Alt text][i5]


where _BSCNT\_o_ is the number of BS cells in the admin. unit at time _t0_. 

Having interpolated the BS Population and corresponding BS population density for each time _t_ we can the derive the projected number of cells which should transition for each time step, after translating _URR_ into the "Proportion Urban" ( _PU_ ), and based upon the simple calculation of:


![Alt text][i7]


####  Handling of Negative Cases
Given that the above equations can result in "negative" predicted growth in a given year, and the input BS extent data assumes that once an area has transitioned to BS it remains BS, we are faced with a contradiction. Here we must side with the extent data and limit the model to only be able to show "stagnation," i.e. no growth, or growth. Therefore, for any year showing negative predicted growth, we simply change the predicted growth to '0.'




###  Predicted BS Extents Map Production
In order to turn those predicted transitions into timestep specific BS extent maps, i.e. spatially allocate the transitions within each subnational unit, we have two options. We can allocate based solely upon the RF-derived probability transition surface or, if the data is available, we can utilize the same RF-derived probability surface in conjunction with the time step specific [unit weighted Lights at Night (LAN)][#markdown-header--lights-at-night-(lan)-processing] data.


####  Unit-specific RF-derived Probability Surface for Disaggregating Transitions
The procedure using only the RF-derived probability is carried out on an unit by unit basis. Given that the transition process is iterative in nature, we begin by taking the extents of the previous time step, or the extents of _t0_ if it is the first time step. For any non-BS cell, we retrieve the probability of transition as calculated by the RF-derived surface. We make the assumption that cells with a higher probability of transition are more likely to transition before cells with lower probabilities. We select the _n_ highest probabilities from the subset of potential transition cells, change their value to represent BS in addition to the pre-existing extents, and output the union of the new transitions and preceding BS as the predicted BS extents at that time step. This output is then used as the base BS extents for the next time step's transition procedure until all time steps have been processed.


####  Time Step & Unit-specific LAN Modified RF-derived Probability Surface for Disaggregating Transitions
The procedure for using time step specific LAN adjusted probabilities of transition is also carried out on an unit by unit basis and largely follows the same procedure given for the [RF-only disaggregation of transitions][#markdown-header--admin-unit-specific-rf-derived-probability-surface-for-disaggregating-transitions]. Where the methods differ is that the base probability of transition for every cell _i_ is adjusted by the weighted LAN data, for time step _t_, of the subnational unit _j_ using the following:


![Alt text][i9]


The procedure after that is identical.




[Return to Overview/Table of Contents][l1]

[r1]: /docs/References.md "World Database of Protected Areas"
[r2]: /docs/References.md ""
[r3]: https://www.sciencedirect.com/science/article/pii/S0169207000000662 ""
[r4]: https://www.jstatsoft.org/article/view/v027i03/v27i03.pdf ""
[r5]:  ""
[r6]:  ""

[i1]: /figures/lag_equation.gif ""
[i2]: /figures/LAN_reweighting.gif ""
[i3]: /figures/URR_o.gif ""
[i5]: /figures/BSD_o.gif ""
[i7]: /figures/BSCNT_t.gif ""
[i9]: /figures/reweightingLANtrans.gif ""

[l1]: https://bitbucket.org/jjnieves/bsgme/src/master/
[l2]: https://bitbucket.org/jjnieves/bsgm/src/master/