# Binomial Mixture Model 

## Introduction

This R package is developed for simulating case-control data under phenotypic heterogeity and fitting a Binomaial Mixture Model. A binomial mixture model (BMM) uses the ancestry clusters of the samples and fit an identifiable mixture model to account for heterogeneity in a case-control study. The purpose and results of the study is given as the abstract of the paper. 

### Abstract

Most common human diseases and complex traits are etiologically heterogeneous. Genome-wide Association Studies (GWAS) aim to discover common genetic variants that are associated with complex traits, typically without considering heterogeneity. Heterogeneity, as well as imprecise phenotyping, significantly reduces the power to find genetic variants associated with human diseases and complex traits. Disease subtyping through unsupervised clustering techniques such as latent class analysis can explain some of the heterogeneity; however, subtyping methods do not typically incorporate heterogeneity into the association framework. Here, we use a finite mixture model with logistic regression to incorporate heterogeneity into the association testing framework for a case-control study. In the proposed method, the disease outcome is modeled as a mixture of two binomial distributions. One of the component distributions refers to the subgroup of the population for which the genetic variant is not associated with the disease outcome and another component distribution corresponds to the subgroup for which the genetic variant is associated with the disease outcome. The mixing parameter corresponds to the proportion of the population for which the genetic variant is associated with the disease outcome. A simulation study of a trait with differing levels of prevalence, SNP minor allele frequency, and odds ratio was performed, and effect size estimates compared between the models with and without incorporating heterogeneity. The proposed mixture model yields lower bias of odds ratios while having comparable power compared to classical logistic regression. 

## Package Description

This package contains codes for fitting the BMM and plotting figures that was necessary for the papers. It also contains codes that were used to simulate data for the study. Simulation results are also included for lazy loading. 

