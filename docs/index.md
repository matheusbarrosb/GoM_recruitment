---
title: The modelling framework
output:
  html_document: default
  pdf_document: default
---

<nav>
  <ul>
    <li><a href="index.html">Home</a></li>
    <li>
      <a href="#">Sections</a>
      <ul>
        <li><a href="results.html">Results</a></li>
      </ul>
    </li>
  </ul>
</nav>

# **The Multivariate Autoregressive State Space Model (MARSS)**

This project aims to model the interannual variation in recruitment dynamics for several ecologically, economically important fish species. 
I'm using a Multivariate Autoregressive State Space Model (MARSS) with covariates to estimate recruitment trends. State Space models are useful
to model time series because it differentiates between process and observation components by estimating separate variances.
In a population dynamics context, the process component refers to true changes in population abundance through time.
The observation compoenent is then modelled with additional error to account for the inherent variability related to sampling errors.
The model also includes the effects of covariates on the unobserved population abundance.

The model can be described as follows:



$$y_{t,s} = x_{t,s} + \nu_{t}$$

$$\nu_{t} \sim N(0, \sigma_{obs})$$

$$\sigma_{obs} \sim \Gamma(2, 2)$$

$$x_{t} = x_{t-1} + \beta_{k,s}X_{t,k} + \epsilon_{t}$$

$$\epsilon_{t} \sim N(0, \sigma_{pr})$$

$$\sigma_{pr} \sim \Gamma(2, 2)$$

$$\beta_{k,s} \sim N(0, 2)$$

Where:

$y_{t}$ = observed mean count at year _t_ \
$\nu_{t}$ = observation error at year _t_\
$\sigma_{obs}$ = observation error standard deviation \
$x_{t}$ = unobserved state (_i.e._ the true population abundance at year _t_) \
$\epsilon_{t}$ = process error at year _t_ \
$\sigma_{pr}$ = process standard deviation \
$\beta_{k,s}$ = effects of covariate _k_ on species _s_ \
$X_{t,k}$ = design matrix of covariates \

The model is fitted using a Bayesian approach through the NUTS (No U-Turn Sampler) algorithm in the "Stan" software.
The full joint likelihood of the model can be expressed as follows:

$$L(\theta, x_{1:N,1:S}|y_{1:N,1:S}) = \prod_{t=1}^{N}\prod_{s=1}^{S}g(y_{t,s} | x_{t,s}, \theta_{obs})f(x_{t,s} | x_{t-1,s}, \theta_{p})$$

Where:

$\theta$ = model parameters \
$\theta_{obs}$ = observation component parameters \
$\theta_{p}$ = observation component parameters \

Average growth rates through time can be easily estimated from the posterior state predictions by calculating the finite differences
between predicted states at each time step as follows:


\begin{align}

\lambda_{t,s} = \frac{x_{t,s} - x_{t-1,s}}{t - (t-1)}

\end{align}


$$\bar{\lambda}_{s} = \sum_{t=1}^{N}\frac{\lambda_{t,s}}{N}$$

Where:

$\lambda_{t,s}$ = growth rate of species _s_ at year _t_ \
$x_{t,s}$ = predicted state of species _s_ at year _t_






