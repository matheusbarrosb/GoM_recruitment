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

\begin{align}

y_{t} = Z_{y}x_{t} + \sigma_{obs}

\end{align}

\begin{align}

\sigma_{obs} \sim MVN(0, Q_{t})

\end{align}

\begin{align}

x_{t} = x_{t-1} + \beta_{k,s}X_{t,k} + \sigma_{pr}

\end{align}

\begin{align}

\sigma_{pr} \sim MVN(0, R_{t})

\end{align}

$y_{t}$ = observed mean count at year _t_ \
$\sigma_{obs}$ = observation variance \
$Q_{t}$ = observation variance covariance matrix \
$x_{t}$ = unobserved state (_i.e._ the true population abundance at year _t_) \
$\beta_{k,s}$ = effects of covariate _k_ on species _s_ \
$X_{t,k}$ = design matrix of covariates \
$\sigma_{pr}$ = process variance \
$R_{t}$ = process variance covariance matrix \


