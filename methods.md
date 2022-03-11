# Bayesian Methods 

To determine the emplacement age of the MGS, we combine our new eruption-age of the HMT, and the pseudotachylyte generation age with zircon ^206^Pb/^238^U data from the same tuff and from the basal layer of the gravity slide (Figure 4). Because the pseudotachylyte was generated during the gravity slide, it should provide the most accurate emplacement age. However, the low potassium content of the glass (see supplemental material), and the potential to entrain atmospheric argon during generation of the pseudotachylyte, resulted in large uncertainties for each heating step of our experiment preclude a precise age determination. Indeed, these problems have been previously recognized for other pseudotachylite localities (e.g., Kelly et al., 1994; Reimold et al., 1990; Sherlock and Hetzle, 2001; Spray et al., 1995). To overcome these challenges, we developed a probabilistic bayesian model of age for the pseudotachylyte, that uses our knowledge about the formation order of each of our dated samples as prior information to constrain the age of the pseudotachylyte. 

Bayesian models attempt to estimate the probable values of unknown parameters based on prior information about these parameters, which can be conditioned by observed data (*d*), or likelihoods. Since we are interested in estimating age, we use the radioisotopic dates for sample as our likelihood distribution where the probability of an age (*θ*) is defined as: 

$$P(d| \theta) = \mathcal{N}(\mu, \sigma^2)$$ 

where μ is the weighed mean age and σ^2 is the variance of each radioisotopic date. 

Bayesian calibration of ranked-age information is commonly used on radiocarbon analyses to reduce the uncertainty of dates from archaeological sites where the relative ordering of dates is known [@blaauw2010; @bronkramsey2008; @buck1991].  And we apply a version of this technique here. In our case our prior information is the rank order of formation for each sample. Clearly, the pseudotachylyte must be older than the overlying HMT, and younger than the underlying basal layers (PBL, HBL, BV HCD). Furthermore since the Panguitch Lake andesite blocks included in the MGS, they must be slightly older than the slide. By ranking each event from oldest to youngest we assume that age (*θ*), decreases with increasing rank (i.e., more recent events must be younger, and therefore the probability of *θ* for each event is defined as:

$$P(\theta) = \begin{cases}
  1 = \theta_{basal} ≥ \theta_{PL~andesite} ≥ \theta_{PST} ≥ \theta_{HMT~sanidine} \\   
  0 = otherwise    
\end{cases}$$



