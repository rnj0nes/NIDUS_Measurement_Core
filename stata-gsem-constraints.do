* Example of Stata/gsem and parameter constraints
* Rich Jones (rnjones@brown.edu) 2 July 2019
* --------------------------------------------
* IRT example data that Stata provides
* see https://www.stata.com/manuals/irt.pdf page 14/256
use https://www.stata-press.com/data/r16/masc1, clear
* --------------------------------------------
* the following two are the same because 
* family(bernoulli) link(logit) is the default when
* we call simply "logit"
* local famlink ", family(bernoulli) link(logit)"
local famlink ", logit"
*local is stored in active memory, if referring to it in code
*need to run local everytime to get models to run

* first estimate model normally
* This is a single factor confirmatory factor
* analysis model where all factor loadings
* are estimated and the variance of the
* latent trait is assumed to be 1
* and the mean is assumed to be 0 by default
gsem ///
   (q1 <- g `famlink') ///
   (q2 <- g `famlink') ///
   (q3 <- g `famlink') ///
   (q4 <- g `famlink') ///
   (q5 <- g `famlink') ///
   (q6 <- g `famlink') ///
   (q7 <- g `famlink') ///
   (q8 <- g `famlink') ///
   (q9 <- g `famlink') ///	
	, latent(g) var(g@1) mean(g@0) nocapslatent
	
* let's look at a quick example
* cap drop gish
* gen gish = log((q1+q2+q3+q4+q5+q6+q7+q8+q9+.025)/9+.05)
* su gish
* replace gish=(gish-`r(mean)')/`r(sd)'
* su gish
* logit q9 gish

	
* That's great now let's see what the parameter estimates
* are called
gsem , coeflegend

* Super now let's extract those coefficient estimates into some
* local macros that are easier to remember their names
forvalues i=1/9 { // loop over 9 items
	local lambda`i' = _b[q`i':g]
	local cons`i' = _b[q`i':_cons]
	local model "`model' (q`i' <- f@`lambda`i'' _cons@`cons`i'' `famlink') " // this is short hand for second model below
}


* Now let's re-run the mode with the first time through
* parameter estiamates as constraints
* Note we also remove the constraint on
* the variance of the latent trait (f)
* This should be freely estimated, 
* and is possible to estimate because 
* at least 1 (and in fact, all) of the
* factor loadings is constrained to some
* value. We also add a command to estimate the 
* mean of f. This is now possible because
* at least 1 (and in fact, all) of the
* thresholds are constrained.
gsem ///
   (q1 <- f@`lambda1' _cons@`cons1' `famlink') ///
   (q2 <- f@`lambda2' _cons@`cons2' `famlink') ///
   (q3 <- f@`lambda3' _cons@`cons3' `famlink') ///
   (q4 <- f@`lambda4' _cons@`cons4' `famlink') ///
   (q5 <- f@`lambda5' _cons@`cons5' `famlink') ///
   (q6 <- f@`lambda6' _cons@`cons6' `famlink') ///
   (q7 <- f@`lambda7' _cons@`cons7' `famlink') ///
   (q8 <- f@`lambda8' _cons@`cons8' `famlink') ///
   (q9 <- f@`lambda9' _cons@`cons9' `famlink') ///
	, latent(f) nocapslatent ///
		var(f) means(f) 

*below is same model as above, just even more shorthand	
di in red "`model'"

gsem `model' , latent(f) nocapslatent var(f) means(f) 

		
* Here is what it would look like if we only
* put the constraints on the first 4 items
* Note we can still get a mean of f and variance of f
gsem ///
   (q1 <- f@`lambda1' _cons@`cons1' `famlink') ///
   (q2 <- f@`lambda2' _cons@`cons2' `famlink') ///
   (q3 <- f@`lambda3' _cons@`cons3' `famlink') ///
   (q4 <- f@`lambda4' _cons@`cons4' `famlink') ///
   (q5 <- f `famlink') ///
   (q6 <- f `famlink') ///
   (q7 <- f `famlink') ///
   (q8 <- f `famlink') ///
   (q9 <- f `famlink') ///	
	, latent(f) nocapslatent ///
		var(f) means(f) 


*below is code to draw an item characteristic curve for q1
gr tw function y = invlogit(`cons1' + x*`lambda1') , range(-4 4)
