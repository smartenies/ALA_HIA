# 2017_ALA_HIA
Health impact assessment sponsored by the American Lung Association evaluating the potential impacts of shutting down two coal-fired power plants in the Front Range region of Colorado

Project: ALA Health Impact Assessment
Date created: September 18, 2017
Created by: Sheena Martenies
Sheena.Martenies@colostate.edu

Description:
Health impact assessment of the potential mortality and morbitidy benefits
of shutting down two coal-fired powerplants in the Front Range region of 
Colorado

Project is sponsored by the American Lung Association

The methods used in this analysis were based on those for the study done by Jacob Pratt, Ryan Gan, and others here at CSU. In this original study, they estimated the impacts of wildfire-smoke related ozone for the entire United States. Because we had a smaller study area, we were able to include more sources of uncertainty.

The scripts are numbered for the order in which they should be run.


###Major deadlines:
1) Report due to ALA in January, 2018

###Analytical Notes:
09.18.17	Created GitHub repository

09.26.17	Summarized NEI 2014 data for the two power plants
		Estimated impacts per ton emitted
		NOTE: The impacts per ton analysis is HIGHLY UNCERTAIN and 
		should really not be used or communicated to outside parties

10.12.17	Created spatial objects for the analysis
		Mapped census variables at the census tract level

10.31.17	5-year population estimates at the census tract and ZTCA to 
		Ryan to calculate hospitalization and mortality rates

11.02.17	Summarized NEI 2008, 2011, and 2014 for the Comanche and Drake 
		plants
		Started the CR coefficient database for the health-impact 
		functions-- PM2.5 first
		Emailed Gordon Pierce at CDPHE to see if the state maintains its
		own emissions inventory

11.13.17	Downloaded monitoring data for the SFR area for 2014:
		Daily data from US EPA's AQS DataMart
		Hourly data from CDPHE