# Comments: _Supervised Machine Learning with R Case Studies_

### A note on finishing the course

After almost two weeks spending on trying out the course by [@juliasilge](https://twitter.com/juliasilge) on [supervised ML Case Studies with R](https://supervised-ml-course.netlify.com/), this is indeed a **beginner-friendly** course for those looking for real-life application of machine learning techniques, coupled with data analytics. The course consists of 4 case studies, 2 using regression and 2 using binary classification. The hands-on procedures are also considered standard for those working in data science industry: explore, visualize, tidy/transform data before working your way to build a machine learning model -- train, (validate), test and pick the best model from the model pool. Other than the **clear structure** of course, it comes with other few highlights:

##### It's free.

Yes. It's free. Thanks to the course creator, Julia. Anyone could have access to the course.

##### It's interactive.

The course is hosted on _mybinder.org_ docker and because of that, users are able to execute R code in a stand-alone environment.

##### It has new datasets.

The course utilizes more recent datasets instead of the good'ol `mtcars` or `iris`, which we are all familiar about. The new datasets surely provide users some freshness.

##### It's light and breezy.

Just like I mentioned above, this is a beginner-friendly course. No daunting mathematics or greek symbols, no bombardents of fancy jargons, just light and breezy.
<br></br>

### caret vs tidymodels

Maybe some of you wonder: what took me so long to finish a four-chapter course?

It's all because I am a _wanderlust_ in the **tidyverse**. Instead using `caret` by [@topedos](https://twitter.com/topepos), a machine learning toolkit, suggested in the course, I turned to trying out his recent package [`parsnip`](https://tidymodels.github.io/parsnip/index.html) as part of the [`tidymodels`](https://github.com/tidymodels). While I am still getting accustomed to this whole new API, it works just fine like caret, aside from its rather immature lifecycle. I recommend his workshop **Applied Machine Learning** on [github](https://github.com/tidymodels/aml-training) if you would also like to have taste of tidiness. **It's worth it.**
<br></br>

### Suggestions & Questions

Although the course is great for a lot of reasons, it can be better if:

-   Font size for text in slides is bigger so people have a better reading experience

That's it. That's the only suggestion coming from me.

In addition, some questions are formed in my head during taking the course and most of them are ML-related. It would be great if Julia could address them in an updated version of the course.

-   When we train our models, do we usually skip the step looking at raw model performance on training set and test set since resampling makes an improvement?
![resampling performance](https://raw.githubusercontent.com/chuckleong21/supervisedml/master/resampling.png)

-   In [case study 3](https://supervised-ml-course.netlify.com/chapter3), the event we evaluate with model is the _likehood to vote_, meaning the odds of voted in 2016 for `imiss_a_2016` should increase about 48% since `exp(.397) = 1.487356` and that for `imiss_n_2016` should otherwise decrease about 34% for `exp(-.4) = 0.67032`. Is there a mistake in the slide?
