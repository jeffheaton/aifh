
#  Artificial Intelligence for Humans
## Volume 1: Fundamental Algorithms
## R Version
## http://www.aifh.org
## http://www.jeffheaton.com
##
## Code repository:
## https://github.com/jeffheaton/aifh
##
## Copyright 2013 by Jeff Heaton
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## For more information on Heaton Research copyrights, licenses
## and trademarks visit:
## http://www.heatonresearch.com/copyright

## Chapter 4 Example: Inscribed Circle 
## 
## This example shows how random numbers can be used to estimate pi 
## using random sampling (Monte Carlo method)
##
## Area of a circle = pi/(r^2)
## Area of a square = (2r)^2
## circle/square = (pi/(r^2))/(2r)^2 = pi/4

estimate_pi <- function(samples, r){
	tries <- 0
	successes <- 0
	for (i in 1:samples){

		# pick a point randomly	
		x = r*runif(1)
		y = r*runif(1)
		tries <- tries + 1
		# check if point is in the circle
		if (x*x + y*y <= r*r){
			successes <- successes + 1
		}
	}	
	pi_est <- 4*successes/tries
	return(pi_est)
}

## After a million samples, Monte Carlo comes pretty close!
## Returns: 3.144756 (but will be different unless seed is the same)
print(estimate_pi(1000000,4))


