# CBDT EM Strategy

Trading strategy based on Empirical Similarity (ES), an offshoot of Case Based Decision Theory (CBDT).

See [Macro Trend](https://substack.com/home/post/p-174152862) by Moritz Heiden: CBDT was introduced in the 1990s by Gilboa & Schmeidler as a model of decision making under uncertainty. Instead of relying on subjective probabilities, CBDT posits that people recall past situations that look similar to today’s and judge actions based on how well they worked back then. In other words: do not guess the future, compare with the past. ES adapts this principle to data. At each evaluation point, we represent a market with a set of features: price dynamics, volatility, drawdowns, and macro changes (unemployment, term spread, CPI, industrial production, etc.). If today looks statistically similar to some past cluster of weeks, that fact alone carries information. In finance, where regimes shift and models decay, this parsimony is a strength. Where many machine learning models try to generalize from millions of samples, ES is content with “eyeballing” the closest cases.

## Asset Universe

### Single-Country EM Equity ETFs

| Country | ETF | TER | Acc/Dis | ISIN | YF Ticker | Data Since | OeKB |
|---------|---------|---------|---------|---------|---------|---------|---------|
| China | Xtrackers MSCI China UCITS ETF | 0.65% | Acc | LU0514695690 | XCS6.DE | 10/4/11 | Yes |
| Taiwan | iShares MSCI Taiwan UCITS ETF | 0.74% | Dis | IE00B0M63623 | ITWN.MI | 1/1/08 | Yes |
| India | iShares MSCI India UCITS ETF (Acc) | 0.65% | Acc | IE00B2QP8B85 | QDV5.DE | 5/31/18 | Yes |
| South Korea | Xtrackers MSCI Korea UCITS ETF 1C | 0.45% | Acc | LU0292100046 | XMKO.MI | 1/1/08 | Yes |
| Brazil | Xtrackers MSCI Brazil UCITS ETF | 0.25% | Acc | LU0292109344 | DBX6.DE | 2/6/09 | Yes |
| Saudi Arabia | iShares MSCI Saudi Arabia Capped UCITS ETF | 0.60% | Acc | IE00BYX8BR88 | IUSV.DE | 5/6/19 | Yes |
| South Africa | iShares MSCI South Africa UCITS ETF | 0.65% | Acc | IE00B52XQP83 | SRSA.MI | 7/8/15 | Yes |
| Mexico | Xtrackers MSCI Mexico UCITS ETF | 0.65% | Acc | LU0476289466 | XMEX.MI | 7/26/10 | Yes |
| Malaysia | Xtrackers MSCI Malaysia UCITS ETF 1C | 0.50% | Acc | LU0516494370 | XCS3.DE | 8/24/11 | Yes |
| Poland | iShares MSCI Poland UCITS ETF USD (Acc) | 0.74% | Acc | IE00B4M7GH52 | IBCJ.DE | 1/21/11 | Yes |
| Thailand | Xtrackers MSCI Thailand UCITS ETF 1C | 0.50% | Acc | LU0516494701 | XCS4.DE | 8/19/11 | Yes |
| Türkiye | iShares MSCI Turkey UCITS ETF | 0.74% | Dis | IE00B1FZS574 | ITKY.MI | 1/10/08 | Yes |
| Philippines | Xtrackers MSCI Philippines UCITS ETF 1C | 0.65% | Acc | LU0592215043 | XPQP.F | 8/23/12 | Yes |
| Vietnam | Xtrackers Vietnam Swap UCITS ETF | 0.85% | Acc | LU0322252924 | XFVT.DE | 1/15/2008 | Yes |

### Regional / Broad EM Equity ETFs

| Region | ETF | TER | Acc/Dis | ISIN | YF Ticker | Data Since | OeKB |
|---------|---------|---------|---------|---------|---------|---------|---------|
| EM broad | iShares Core MSCI Emerging Markets IMI UCITS ETF (Acc) | 0.18% | Acc | IE00BKM4GZ66 | EUNM.DE | 10/20/2009 | Yes |
| EM Asia | iShares MSCI EM Asia UCITS ETF (Acc) | 0.20% | Acc | IE00B5L8K969 | CSEMAS.MI | 8/10/2010 | Yes |
| EM Latin America | iShares MSCI EM Latin America UCITS ETF (Dist) | 0.20% | Dis | IE00B27YCK28 | IUSC.DE | 3/27/2008 | Yes |
| EM Small Cap | iShares MSCI Emerging Markets Small Cap UCITS ETF | 0.74% | Dis | IE00B3F81G20 | EUNI.DE | 3/5/2009 | Yes |

### EM Factor / Style ETFs

| Factor | ETF | TER | Acc/Dis | ISIN | YF Ticker | Data Since | OeKB |
|---------|---------|---------|---------|---------|---------|---------|---------|
| EM Value | iShares Edge MSCI EM Value Factor UCITS ETF USD(Acc) | 0.40% | Acc | IE00BG0SKF03 | 5MVL.DE | 1/1/2019 | Yes |
| EM Min Vol | iShares Edge MSCI EM Minimum Volatility UCITS ETF | 0.40% | Acc | IE00B8KGV557 | EUNZ.F | 2/18/2013 | Yes |

### EM Bond ETFs

| Factor | ETF | TER | Acc/Dis | ISIN | YF Ticker | Data Since | OeKB |
|---------|---------|---------|---------|---------|---------|---------|---------|
| EM Sovereign USD Bonds | iShares J.P. Morgan USD Emerging Markets Bond UCITS ETF (Dist) | 0.45% | Dis | IE00B2NPKV68 | IUS7.DE | 3/20/2009 | Yes |
| EM Local Currency Bonds | iShares J.P. Morgan EM Local Govt Bond ETF | 0.50% | Dis | IE00B5M4WH52 | IUSP.DE | 8/15/2011 | Yes |
| EM Corporate Bonds | iShares J.P. Morgan USD EM Corporate Bond UCITS ETF (Dist) | 0.50% | Dis | IE00B6TLBW47 | IS0Q.DE | 4/17/2012 | Yes |

***I will expand the README as soon as I am done with the project.***