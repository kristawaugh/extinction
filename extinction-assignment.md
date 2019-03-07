Are We Experiencing the Sixth Great Extinction?
================
Krista Waugh & Tanya Hanson & Vanessa Garcia

Background Information
----------------------

### Our References

-   [Anthony Barnosky and Kaitlin Maguire Measure Mammal Extinctions](https://youtu.be/QsH6ytm89GI)
-   [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253)

Our report is heavily inspired by the paper cited above titled "Accelerated modern human–induced species losses: Entering the sixth mass extinction". This report will attempt to replicate the figures and use the same estimated background rate of: 2 mammal extinctions per 10,000 species per 100 years.

In the video cited above, Barnosky and Maguire stated that within these last 100 years, we've seen mammalian species going extinct 28-67 times too fast. In order to assess whether we are entering the 6th mass extinction or not, we need to conclude that current extinction rates are far above the “background” rates prevailing between the five previous mass extinctions.

### Additional references:

-   <http://www.hhmi.org/biointeractive/biodiversity-age-humans> (Video)
-   [Barnosky et al. (2011)](http://doi.org/10.1038/nature09678)
-   [Pimm et al (2014)](http://doi.org/10.1126/science.1246752)
-   [Sandom et al (2014)](http://dx.doi.org/10.1098/rspb.2013.3254)

Loading in our API Data
-----------------------

For this report we created a function that reads in the api data from `http://api.iucnredlist.org/index/species/Acaena-exigua.json` and distinguishes between genus, species, and rational (with the caveat that if the status code reads an error, then the rationale coulumn should read as N/A)

``` r
genus <- "Acaena"
species <- "exigua"
api_fn <- function(genus, species){
  
  api_call <- paste0("http://api.iucnredlist.org/index/species/", genus, "-", species, ".json")
  Sys.sleep(0.5)
  resp <- GET(api_call)
  status <- httr::status_code(resp)
  if ( status > 300) {
    return(data_frame(Genus = genus, Species = species, 
                      rationale = NA, status_code = status))
  }
  out <- content(resp, as = "text")
  df <- jsonlite::fromJSON(out)
  rationale <- df$rationale

  data_frame(Genus = genus, Species = species, 
             rationale = rationale, status_code = status)
  }
```

### Testing our function

``` r
api_fn("Acaena", "exigua")
```

    # A tibble: 1 x 4
      Genus  Species rationale                                     status_code
      <chr>  <chr>   <chr>                                               <int>
    1 Acaena exigua  The last known individual of <em>A. exigua</…         200

Finding Dates of Extinction
---------------------------

Here, we can see our test was successful and we were able to pull the genus, species, and rationale all into their respective columns. Next we will apply this to all of the data. For the purposes of our report the rationale column will be used to extract dates of extinction. To continue with our analysis, we will need to extract these dates and match them with their respective genus and species.

### Downloading Extinction Data: Putting it all together

In order to understand what queries to make to the IUCN REST API, we need a list of extinct species names. This information can be downloaded from the IUCN website. To simplify, we download the data file using a copy that was already prepared and name it "extinct".

``` r
extinct <- suppressMessages(read_csv("https://espm-157.github.io/extinction-module/extinct.csv"))

species <- extinct$Species
genus <- extinct$Genus
output <- map2_dfr( genus, species, api_fn)

output
```

    # A tibble: 841 x 4
       Genus      Species   rationale                              status_code
       <chr>      <chr>     <chr>                                        <int>
     1 Acaena     exigua    The last known individual of <em>A. e…         200
     2 Acalypha   dikuluwe… "<span style=\"font-style: italic;\">…         200
     3 Acalypha   rubriner… <NA>                                           200
     4 Acalypha   wilderi   "<span style=\"font-style: italic;\">…         200
     5 Acanthame… pecatoni… <NA>                                           200
     6 Achatinel… abbrevia… <NA>                                           200
     7 Achatinel… buddii    <NA>                                           200
     8 Achatinel… caesia    <NA>                                           200
     9 Achatinel… casta     <NA>                                           200
    10 Achatinel… decora    <NA>                                           200
    # ... with 831 more rows

### Extracting Extinction Dates from Data set

Now that we have applied our "api\_fn" function to the extinct data, we need to write a function to extract the rationale for the extinction data for all extinct species.

``` r
rationale <- output %>%
   select("rationale") %>% 
  na.omit
```

    [[1]]
      [1] "1990" "1959" "1929" "1980" "1964" "1951" "1982" "1996" "1996" "2000"
     [11] "2000" "2008" "1996" "1996" "1996" "1996" "1955" "1996" "2003" "1832"
     [21] "1690" "1774" "1927" "1995" "1671" "1672" "1710" "1693" "1698" "1779"
     [31] "1779" "1981" "1983" "1984" "2011" "1793" "1696" "1906" "1938" "1693"
     [41] "1980" "1828" "1880" "1923" "1968" "1825" "1975" "1885" "1945" "1996"
     [51] "1909" "1988" "1989" "1502" "1980" "1028" "1980" "1603" "1500" "1906"
     [61] "1975" "1627" "1892" "1502" "2002" "1887" "1970" "1887" "1970" "1893"
     [71] "1895" "1928" "1935" "1994" "2009" "1875" "1903" "1840" "1930" "1894"
     [81] "1901" "1950" "1859" "1828" "1500" "1875" "1912" "1894" "1877" "1860"
     [91] "1980" "1990" "2003" "1892" "1996" "2000" "1965" "2005" "1871" "1870"
    [101] "1964" "1936" "1730" "1889" "1860" "1862" "1910" "1960" "1800" "1920"
    [111] "1960" "1970" "1900" "1923" "1969" "1940" "2005" "1890" "1950" "1950"
    [121] "1920" "1875" "1834" "1986" "1962" "1928" "1800" "1909" "1990" "1917"
    [131] "1773" "1844" "1977" "1999" "1800" "1800" "1800" "1981" "1983" "1984"
    [141] "2011" "1934" "1934" "1934" "1987" "1895" "1909" "2002" "2011" "1907"
    [151] "1898" "1802" "1836" "1802" "1805" "1926" "1961" "2003" "1690" "1833"
    [161] "1834" "1800" "1876" "1918" "1900" "1910" "1996" "2004" "2011" "1996"
    [171] "2012" "2000" "2004" "1996" "1996" "2004" "1996" "2000" "2005" "1996"
    [181] "1996" "1996" "1996" "1996" "1996" "1996" "2000" "2005" "1967" "1965"
    [191] "1901" "1996" "1996" "1726" "1761" "1980" "1892" "1900" "1936" "1671"
    [201] "1672" "1911" "1916" "1850" "1693" "1983" "1960" "1960" "1998" "1951"
    [211] "2001" "1950" "1918" "1993" "1874" "1928" "1936" "1950" "2000" "1970"
    [221] "1988" "1992" "2000" "2009" "2009" "1996" "1996" "1996" "1996" "1996"
    [231] "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1913" "1940" "1850"
    [241] "2000" "1969" "1940" "1901" "1907" "1999" "1999" "1799" "1800" "1908"
    [251] "1741" "1768" "1938" "1938" "1979" "2003" "2004" "1971" "1972" "1996"
    [261] "1840" "1872" "1930" "1973" "1940" "1945" "1946" "1979" "1989" "1990"
    [271] "2009" "1890" "1960" "1965" "1967" "1970" "1988" "1993" "1990" "2000"
    [281] "2003" "2005" "1758" "1995" "2013" "2014" "1967" "1932" "1960" "1890"
    [291] "1996" "1996" "1900" "1830" "1600" "1928" "1878" "1878" "1878" "1922"
    [301] "1970" "2000" "2008" "2013" "2013" "1917" "1950" "1996" "1996" "1996"
    [311] "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1996"
    [321] "1996" "1996" "1996" "1996" "1996" "1847" "1764" "1673" "1675" "1693"
    [331] "1888" "1995" "1924" "1937" "1931" "1960" "1905" "2006" "2004" "1960"
    [341] "2010" "1940" "1944" "1726" "1837" "1859" "1775" "1804" "1996" "1996"
    [351] "1897" "1881" "1875" "1800" "2001" "1902" "1864" "1902" "1910" "1892"
    [361] "1904" "1950" "1975" "1837" "1890" "1981" "1987" "1934" "1656" "1985"
    [371] "1995" "1997" "1825" "1983" "1912" "1726" "1761" "1761" "1864" "1970"
    [381] "1952" "2004" "1926" "1931" "1950" "1894" "1988" "1999" "2000" "1860"
    [391] "1983" "1986" "1994" "2003" "1730" "1674" "1500" "1906" "1930" "1930"
    [401] "1934" "1930" "1800" "1500" "1999" "1896" "1901" "1902" "1843" "1844"
    [411] "1840" "1975" "1999" "1674" "1700" "1693" "1700" "1726" "1761" "1902"
    [421] "1996" "1983" "2009" "2009" "1997" "2003" "1892" "1950" "1964" "1894"
    [431] "1877" "1897" "1800" "1800" "1972" "1994" "1620" "1961" "1963" "1979"
    [441] "1990" "1994" "2000" "1980" "1992" "1994" "2000" "1977" "1980" "2005"
    [451] "1980" "1992" "1994" "2000" "1980" "1994" "1980" "1992" "1994" "2000"
    [461] "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000" "1980" "1992"
    [471] "1994" "2000" "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000"
    [481] "1977" "1980" "1990" "2003" "2005" "1980" "1992" "1994" "2000" "1980"
    [491] "1992" "1994" "2000" "1980" "1994" "1977" "1980" "2005" "1977" "1980"
    [501] "1990" "2003" "2005" "1980" "1992" "1994" "2000" "1980" "1992" "1994"
    [511] "2000" "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000" "1980"
    [521] "1992" "1994" "2000" "2002" "1980" "1992" "1994" "2000" "1980" "1992"
    [531] "1994" "2000" "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000"
    [541] "1986" "1991" "2006" "1980" "1992" "1994" "2000" "1980" "1992" "1994"
    [551] "2000" "1980" "1994" "1977" "1980" "1990" "2003" "2005" "1980" "1992"
    [561] "1994" "2000" "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000"
    [571] "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000" "1980" "1994"
    [581] "1980" "1992" "1994" "2000" "1980" "1992" "1994" "2000" "1996" "1980"
    [591] "1994" "1980" "1992" "1994" "2000" "1895" "1994" "1996" "1930" "2009"
    [601] "1926" "1994" "1943" "1892" "1931" "1761" "1778" "1840" "1850" "1852"
    [611] "1975" "1977" "2000" "1894" "1964" "1996" "1996" "1996" "1996" "1996"
    [621] "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1996" "1996" "2005"
    [631] "1996" "1996" "1834" "1888" "1977" "1981" "1983" "1986" "1970" "1977"
    [641] "1987" "1930" "1972" "1823" "1790" "1834" "1730" "1860" "1894" "1937"
    [651] "1999" "1774" "2008" "2008" "2008" "1850" "1777" "1777" "1860" "1870"
    [661] "1920" "1930" "1928" "1970" "1864" "1970" "1970" "1970" "1878" "2008"
    [671] "1956" "1856" "1857" "1927" "1980" "1875" "1876" "1893" "1906" "1502"
    [681] "1874" "1859" "1864" "1873" "1862" "1860" "1968" "1922" "1962" "1960"
    [691] "1994" "1910" "1662" "1902" "1904" "1897" "1898" "1970" "1980" "2000"
    [701] "1997" "2006" "2007" "1997" "1981" "1985" "1891" "1893" "1896" "1906"
    [711] "1911" "1909" "1990" "1960" "2001" "2002" "2003" "1932" "1938" "1930"
    [721] "1904" "1950" "2003" "1960" "1970" "1995" "1977" "1980" "1990" "2003"
    [731] "2005" "1914" "1835" "1908" "1902" "2003" "1886" "1968" "1990" "1991"
    [741] "1940" "1894" "1897" "1933" "2004" "1933" "1968" "2004" "1989" "2004"
    [751] "2009" "1982" "1979" "1988" "1912" "1913" "1908" "1761" "1933" "1936"
    [761] "1895" "1975" "1975" "1954" "1970" "1950" "1989" "1980" "1938" "1965"
    [771] "1905" "1902" "1970" "1502" "1855" "1988" "2001" "1886" "1988" "1913"
    [781] "1936" "1988" "1980" "1874" "1927" "1972" "1700" "1950" "1951" "1998"
    [791] "1974" "1975" "1502" "1827" "1828" "1784" "1944" "1884" "1828" "1889"
    [801] "1908" "1928"

Next we created a function that extracts the date from the rationale. We also create another column with these dates. For the purpose of our report we extracted all dates within the rationale, to make your analysis more accurate you must extract the oldest year from the rationale.

``` r
datejoin <- output %>%
  mutate(year = as.numeric(str_extract(rationale, "\\d{4}")))

datejoin
```

    # A tibble: 841 x 5
       Genus     Species   rationale                         status_code  year
       <chr>     <chr>     <chr>                                   <int> <dbl>
     1 Acaena    exigua    The last known individual of <em…         200  1990
     2 Acalypha  dikuluwe… "<span style=\"font-style: itali…         200  1959
     3 Acalypha  rubriner… <NA>                                      200    NA
     4 Acalypha  wilderi   "<span style=\"font-style: itali…         200  1929
     5 Acantham… pecatoni… <NA>                                      200    NA
     6 Achatine… abbrevia… <NA>                                      200    NA
     7 Achatine… buddii    <NA>                                      200    NA
     8 Achatine… caesia    <NA>                                      200    NA
     9 Achatine… casta     <NA>                                      200    NA
    10 Achatine… decora    <NA>                                      200    NA
    # ... with 831 more rows

Histogram of Extinction Dates
-----------------------------

We can get a sense for the tempo of extinctions by plotting extinctions since 1500 in 25-year interval bins.

``` r
datefilter <- datejoin %>%
  filter("year" >= 1500)


date25 <- hist(datefilter$year,main = "Distribution of Extinction Dates", breaks = 25, xlab = "Year", col = "red")
```

![](extinction-assignment_files/figure-markdown_github/unnamed-chunk-8-1.png)

Detailed Analysis
-----------------

### Extinctions by group

Next we computed the number of extinctions from 1500 - 1900 and from 1900 to present of each of the following taxonomic groups:

-   Vertebrates: CHORDATA
-   Mammals: MAMMALIA
-   Birds: AVES
-   Fish: ACTINOPTERYGII
-   Amphibians: AMPHIBIA
-   Reptiles: REPTILIA
-   Insects: INSECTA
-   Plants:PLANTAE

``` r
classes <- datejoin %>% 
  left_join(extinct, by = c("Genus", "Species")) %>%
  mutate( mammals = ifelse(Class == "MAMMALIA", 1, 0), 
         birds = ifelse(Class == "AVES", 1, 0),
         fish = ifelse( Class == "ACTINOPTERYGII", 1, 0),
         amphibians = ifelse(Class == "AMPHIBIA", 1, 0),
         insects = ifelse(Class == "INSECTA", 1,0),
         plants = ifelse(Kingdom == "PLANTAE", 1, 0),
         reptiles = ifelse(Class == "REPTILIA", 1, 0)) %>%
  mutate(Category = case_when(mammals == 1 ~ "Mammals", 
                                 birds == 1 ~ "Birds",
                                 fish == 1 ~ "Fish",
                                 amphibians == 1 ~ "Amphibians",
                                 reptiles == 1 ~ "Reptiles",
                                 insects == 1 ~ "Insects",
                                  plants == 1 ~ "Plants",
                                 TRUE ~ "Other"
                                ))%>%
  group_by(Category) %>% 
  summarize(
    "Since 1500" = sum(year >= 1500, year < 1900, na.rm = TRUE), 
    "Since 1900" = sum(year >= 1900, na.rm = TRUE))


 vertebrate_count<-datejoin%>%
   left_join(extinct, by = c("Genus", "Species")) %>%
   filter(Kingdom == "ANIMALIA", Phylum == "CHORDATA")%>%
   group_by(Phylum, year)%>%
   tally()%>%
   summarize(
    "Since 1500" = sum(year >= 1500, year < 1900, na.rm = TRUE), 
    "Since 1900" = sum(year >= 1900, na.rm = TRUE))
 
 
 classes
```

    # A tibble: 8 x 3
      Category   `Since 1500` `Since 1900`
      <chr>             <int>        <int>
    1 Amphibians           10           10
    2 Birds               213           45
    3 Fish                 38           34
    4 Insects              11            9
    5 Mammals              79           29
    6 Other               153          113
    7 Plants               48           32
    8 Reptiles             19            5

``` r
 vertebrate_count
```

    # A tibble: 1 x 3
      Phylum   `Since 1500` `Since 1900`
      <chr>           <int>        <int>
    1 CHORDATA          189           61

### Weighing by number of species

The number of species going extinct per century in a given taxonomic group will be influenced by how many species are present in the group to begin with. Overall, these numbers do not change greatly over a period of a few hundred years, so we were able to make the relative comparisons between the roughly pre-industrial and post-industrial periods above.

As discussed by Tony Barnosky in the introductory video (or in [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253) paper), if we want to compare these extinction rates against the long-term palentological record, it is necessary to weigh the rates by the total number of species. That is, to compute the number of extinctions per million species per year (MSY; equivalently, the number extinctions per 10,000 species per 100 years).

-First, we will compute how many species are present in each of the taxonomic groups. To do so, we need a table that has not only extinct species, but all assessed species. We will once again query this information from the IUCN API. Since there is a lot of data, the API breaks the returns up into groups of 10,000 species per page, and the API also tells us the total number of species: <http://apiv3.iucnredlist.org/api/v3/speciescount?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee>

-Below we attempt to collect all of the data into a single DataFrame.

``` r
test_fn1 <- function(page_number) {
  paste0("http://apiv3.iucnredlist.org/api/v3/species/page/",page_number,"?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee")
  }
  test1<- test_fn1(1)
  resp1<- httr::GET(test1)
  status1 <- httr::status_code(resp1)
  output1<-content(resp1, as= "text") 
  df<- fromJSON(output1)
  
  page_func <- function(page_number){
   test_fn1(page_number)%>%
     httr::GET()%>%
      content(as= "text")%>%
      fromJSON()
    }
  num_pages<- c(0:9)
  pg_output<- map(num_pages, page_func)
```
