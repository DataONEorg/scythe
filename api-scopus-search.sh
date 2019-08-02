
APIKEY=985f23cdfde84bdcb15229504fb644b4

# multi-page ADC
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.18739-2009-2019-pg${pg}.json; done

# multi-page KNB
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:10.5063\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.5063-2009-2019-pg${pg}.json; done

# multi-page DBO
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:%22Distributed%20Biological%20Observatory%22\&date=2000-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-DBO-pg${pg}.json; done

# multi-page ADC - loose
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:%22Arctic%20Data%20Center%22\&date=2000-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-ADC-pg${pg}.json; done

 

# APIKEY=de69d7664651081fab89bb0f101e9a9e
# 
# # KNB
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.5063\&date=2010-2013\&APIKey=${APIKEY} -o results/scopus-10.5063-2010-2013.json
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.5063\&date=2014-2016\&APIKey=${APIKEY} -o results/scopus-10.5063-2014-2016.json
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.5063\&date=2017-2019\&APIKey=${APIKEY}\&start=0 -o results/scopus-10.5063-2017-2019-pg1.json
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.5063\&date=2017-2019\&APIKey=${APIKEY}\&start=26 -o results/scopus-10.5063-2017-2019-pg2.json
# 
#  # ADC
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2010-2019\&APIKey=${APIKEY} -o results/scopus-10.18739-2010-2019.json
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2010-2019\&APIKey=${APIKEY}\&start=26 -o results/scopus-10.18739-2010-2019-pg2.json
#  curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2000-2009\&APIKey=${APIKEY} -o results/scopus-10.18739-2000-2009.json
# 
#  # LTER / EDI
#  for pg in 0 26 51 76 101 126 151; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.6073\&date=2010-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.6073-2010-2019-pg${pg}.json; done
# 
