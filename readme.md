# Nonlinear Regression using Generalized Additive Models

This course provides a general introduction to nonlinear regression analysis
using generalized additive models. As an introduction, we begin by covering
practically and conceptually simple extensions to the general and generalized
linear models framework using polynomial regression. We will then cover more
powerful and flexible extensions of this modelling framework by way of the
general concept of _basis functions_, which includes spline and radial basis
functions. We then move on to the major topic of generalized additive models
(GAMs) and generalized additive mixed models (GAMMs), which can be viewed as
the generalization of all the basis function regression topics, but cover a
wider range of topic including nonlinear spatial and temporal models and
interaction models.

## Software requirements

R/Rstudio and a set of R packages are required.

Instructions on how to install all the software is [here](software.md).

# Course programme

- Topic 1: _Regression modelling overview_. We begin with a brief overview and summary of regression modelling in general. The purpose of this is to provide a brief recap of general and generalized linear models, and to show how nonlinear regression fits into this very widely practiced framework.
- Topic 2: _Polynomial regression_. Polynomial regression is both a conceptually and practically simple extension of linear modelling and so provides a straightforward and simple means to perform nonlinear regression. Polynomial regression also leads naturally to the concept of basis function function regression and thus is bridge between the general or generalized linear models and nonlinear regression modelling using generalized additive models.
- Topic 3: _Spline and basis function regression_: Nonlinear regression using splines is a powerful and flexible non-parametric or semi-parametric nonlinear regression method. It is also an example of a basis function regression method. Here, we will cover spline regression using the `splines::bs` and `splines::ns` functions that can be used with `lm`, `glm`, etc. We also look at regression using radial basis functions, which is closely related to spline regression. Understanding basis functions is vital for understanding Generalized Additive Models.
- Topic 4: _Generalized additive models_. We now turn to the major topic of generalized additive models (GAMs). GAMs generalize many of concepts and topics covered so far and represent a powerful and flexible framework for nonlinear modelling. In R, the `mgcv` package provides a extensive set of tools for working with GAMs. Here, we will provide an in-depth coverage of `mgcv` including choosing smooth terms, controlling overfitting and complexity, prediction, model evaluation, and so on.
- Topic 5: _Generalized additive mixed models_. GAMs can also be used in linear mixed effects, aka multilevel, models where they are known as generalized additive mixed models (GAMMs). GAMMs can also be used with the `mgcv` package.
- Topic 6: _Bayesian Generalized additive mixed models_. GAMs and GAMMs may be conducted using Bayesian methods. This provides both theoretical and practical advantages. In this section, we will introduce how GAMs and GAMMs can be carried out in a Bayesian manner using the powerful `brms`/Stan software.
