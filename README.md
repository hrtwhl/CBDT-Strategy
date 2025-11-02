# CBDT-Strategy

Trading strategy based on Empirical Similarity (ES), an offshoot of Case Based Decision Theory (CBDT). CBDT was introduced in the 1990s by Gilboa & Schmeidler as a model of decision making under uncertainty. Instead of relying on subjective probabilities, CBDT posits that people recall past situations that look similar to today’s and judge actions based on how well they worked back then.

In other words: do not guess the future, compare with the past.

ES adapts this principle to data. At each evaluation point, we represent a market with a set of features: price dynamics, volatility, drawdowns, and macro changes (unemployment, term spread, CPI, industrial production, etc.). If today looks statistically similar to some past cluster of weeks, that fact alone carries information. In finance, where regimes shift and models decay, this parsimony is a strength. Where many machine learning models try to generalize from millions of samples, ES is content with “eyeballing” the closest cases.

I will expand the README as soon as I am done with the project. 