## How to (possibly) employ trinary classification in SentiStrength

* check data mining/Export as Trinary [-1, 0, 1]

* go to Sentiment Strength Analysis/Analyse ALL Texts in File "each line seperately (...)

* feed in "_senti_full.txt"

* echo header = yes

* text column = 1

## What has worked so far

* open windows powershell and run the following commands over the Java version of sentistrength (see tutorial http://sentistrength.wlv.ac.uk/#Java for help)

> java -jar SentiStrengthCom.jar sentidata C:/SentiStrength_Data/ input _senti_full.txt binary

> java -jar SentiStrengthCom.jar sentidata C:/SentiStrength_Data/ input _senti_full.txt trinary