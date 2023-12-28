# Group 8 dashboard

[View the full report here]()

## Alcohol consumption v Development status
We wanted to see if there was a difference in alcohol consumption between developed and developing nations.
We plotted this data as a boxplot animated over time as shown:
![Animated boxplot for alcohol consumption](resources/animated_boxplot_alcohol.gif)

We can see that people in developed nations drink considerably more than people in developing nations.
We speculate that this is due to people in developing nations having less access to alcohol.
<br>

## Life expectancy v Development status
We also checked how life expectancies compare between developed and developing countries.
As before, we use an animated boxplot to show this.

![Animated boxplot for life expectancy](resources/animated_boxplot_life_expectation.gif)

We see that developing nations have a significantly lower life expectancy than developed nations.
We think this is caused by a wide range of factors including quality of healthcare, working conditions, water access, and prevalence of deadly diseases.
<br>

## Alcohol consumption v Adult mortality
Does drinking alcohol correlate with mortality?
We've shown alcohol comsumption and adult mortality in this bubbleplot.

<embed type="text/html" src="resources/plot_bubble_alcohol_deaths.html" style="width: 100%;height:50vh;"/>

[View in Fullscreen](resources/plot_bubble_alcohol_deaths.html)

We conclude that while it seems that there is no broad correlation, if we look only at developed nations, we see that there is some correlation between the two.
<br>

## Alcohol consumption v Hepatitis B immunization
Since Hepatitis is a disease of the liver, and alcohol also affects the liver, we wanted to see if there was some sort of correlation between them.
We don't have data for Hep B cases, so we'll use immunization instead.

<embed type="text/html" src="resources/plot_bubble_alcohol_hepatitis.html" style="width: 100%;height:50vh;"/>

[View in Fullscreen](resources/plot_bubble_alcohol_hepatitis.html)

We were unable to see any obvious correlation of the two parameters.
<br>

## Life expectancy v GDP per capita
To show how GDP affects Life expectancy, we've made this bubbleplot.

<embed type="text/html" src="resources/plot_bubble_life_expectancy_gdp.html" style="width: 100%;height:50vh;"/>

[View in Fullscreen](resources/plot_bubble_life_expectancy_gdp.html)

We can clearly see that in general, the higher the country's GDP, the higher it's Life expectancy.
<br>


## Infant mortality v Hepatitis B immunization
<embed type="text/html" src="resources/plot_scatter_infant_mortality_hepatitis.html" style="width: 100%;height:50vh;"/>

[View in Fullscreen](resources/plot_scatter_infant_mortality_hepatitis.html)
<br>

## AIDS cases v Adult mortality
To visualize the deadlyness of AIDS, this scatterplot shows the linear regression for african countries' AIDS cases over adult mortality.
<embed type="text/html" src="resources/plot_scatter_aids_deaths.html" style="width: 100%;height:50vh;"/>

[View in Fullscreen](resources/plot_scatter_aids_deaths.html)

Except for Swaziland being an outlier, it seems that the higher the AIDS prevalence, the higher the adult mortality is for any given african country.
<br>

## BMI v Adult mortality
To showcase the effect of BMI on adult mortality, we present the following barchart:

<iframe src="https://jakoblm.shinyapps.io/barchart/" style="width: 100%;height:50vh;"/>
It looks like your browser doesn't support iframes.
</iframe>

[View in Fullscreen](https://jakoblm.shinyapps.io/barchart/)

This doesn't show anything conclusive, as there are many other factors related to adult mortality, that we can't take into account here.
<br>

## Disease over time
We have made a worldplot for different diseases to showcase where each is most prevalent.
We use Immunization-percentage for all diseases except for HIV/AIDS.
<iframe src="https://jakoblm.shinyapps.io/worldplot/" style="width: 100%;height:50vh;"/>
It looks like your browser doesn't support iframes.
</iframe>

[View in Fullscreen](https://jakoblm.shinyapps.io/worldplot/)

We see that AIDS is most prevalent in the south of Africa, and most other diseases are least immunized against in central Africa.