# Experiments for DISCOLI 2022

This repository contains the experiments done for the integration of Reinforcement Learning in the Aggregate Computing Stack. In particular, here we deploy Distributed Q-Learning to reduce the power consumption of the entire system.
## Requirements:
- a working version of java, > 9 < 17
- sbt >= 1.6.2
## How To lunch
There are three simulations and one of each produces data (in res/) and the plots of each simulation configuration (in img/). Furthermore, in img/ there is a file (analysis.csv) that is used to choose what configuration is the best.
The command to launch are:
- ```sbt startGradient```: run the simulation with the gradient program and the standard scenario
- ```startGradientMulti```: run the simulation above but with the multiswap scenario
- ```startBlockC```: run the simulation with the plain configuration but wiht the block c

## Miscellaneous
This repository used an ad-hoc ScaFi version with the support of DES-like simulations.
This will probably add in the main repository, currently this work is [here](https://github.com/cric96/scafi/tree/des-simulator).