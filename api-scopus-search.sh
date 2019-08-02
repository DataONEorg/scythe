APIKEY=985f23cdfde84bdcb15229504fb644b4

# multi-page ADC
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.18739-2009-2019-pg${pg}.json; done

# multi-page KNB
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:10.5063\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.5063-2009-2019-pg${pg}.json; done

# multi-page DBO
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:%22Distributed%20Biological%20Observatory%22\&date=2000-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-DBO-pg${pg}.json; done

# multi-page ADC - loose
 for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:%22Arctic%20Data%20Center%22\&date=2000-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-ADC-pg${pg}.json; done

 