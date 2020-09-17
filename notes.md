# modeling notes 2020-09-17

conensus findings:

- those who post to chats more are more positive
- teachers are more positive 
- sentiment becomes more positive over time

things that are a bit ambiguous:

- NGSSchat chats are more negative by senti_scale, more positive by others
- generally, after adoption, sentiment is slightly more negative than during years around initial adoption and before it

things that are ambiguous: 

- after chat, sometimes more positive and some times negative

things that are unimportant:

- how long someone has been on Twitter
- quadratic growth term

# older modeling notes

## preliminary

1. check distribution as normal for `senti_scale` as normally distributed
1. check correlations for variable selection and interpretation

## model-building

What is our modeling approach?

- First determine what multi-level structure we have.
- Then, possibly add all at once

### null models

1. model with state (doesn't seem to be highly explanatory)
1. null model with user (seems to be explanatory)
1. null models w/ state and user (State_ICC = .01 (1%), User_ICC = .20 (20%))

### key variables

1. keep state, only add *adoption status* (possibly remove random effect, possibly don't include fixed effects for state)
1. add core, *chat-related vars* (NGSSchat chat, NGSSchat non-chat, non-NGSSchat - we need better names for these)
1. add time (*year*)

### between key and control

1. add user-level vars (participating in a chat or not, whether the tweet is before or after participated in a chat)

### control variables

1. add tweet-level vars

## model-checking and evaluation

1. check assumptions - especially independent and identically distributed (iid) residuals: https://easystats.github.io/performance/reference/check_model.html
1. look at indices for over-fitting/fit (AIC, BIC)
1. check with a fully standardized model - time, count variables may not be as sensible to standardize - maybe report only the variables that aren't otherwise interpretable
1. check r^2 and r^2

## report

1. reports package (ludecke)

## major contribution

1. methodological - data collection, data analysis with multiple techniques
1. it's positive, quite positive
1. user measures - what the user does in terms of participating in #NGSSchat

## other

- to make scale: positive plus negative (gives you a possible range of 1-9), and then center on 5
- for correlations, just positive plus negative
- LIWC manual: https://liwc.wpengine.com/wp-content/uploads/2015/11/LIWC2015_LanguageManual.pdf

- look at the role of moderators; JLA special issue: https://learning-analytics.info/index.php/JLA/announcement/view/167