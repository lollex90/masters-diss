region_names = ['Kherson Oblast',
 'Volyn Oblast',
 'Rivne Oblast',
 'Zhytomyr Oblast',
 'Kyiv Oblast',
 'Chernihiv Oblast',
 'Sumy Oblast',
 'Kharkiv Oblast',
 'Luhansk Oblast',
 'Donetsk Oblast',
 'Zaporizhia Oblast',
 'Lviv Oblast',
 'Ivano-Frankivsk Oblast',
 'Zakarpattia Oblast',
 'Ternopil Oblast',
 'Chernivtsi Oblast',
 'Odessa Oblast',
 'Mykolaiv Oblast',
 'Autonomous Republic of Crimea',
 'Vinnytsia Oblast',
 'Khmelnytskyi Oblast',
 'Cherkasy Oblast',
 'Poltava Oblast',
 'Dnipropetrovsk Oblast',
 'Kirovohrad Oblast',
 'Kyiv',
 'Sevastopol']

region_names_gdp = ['Autonomous Republic of Crimea',
 'Vinnytsya',
 'Volyn',
 'Dnipropetrovsk',
 'Donetsk',
 'Zhytomyr',
 'Zakarpattya',
 'Zaporizhzhya',
 'Ivano-Frankivsk',
 'Kyiv',
 'Kirovohrad',
 'Luhansk',
 'Lviv',
 'Mykolayiv',
 'Odesa',
 'Poltava',
 'Rivne',
 'Sumy',
 'Ternopyl',
 'Kharkiv',
 'Kherson',
 'Khmelnytskiy',
 'Cherkasy',
 'Chernivtsi',
 'Chernihiv',
 'Kyiv city',
 'Sevastopol city']

# create a dictionary to map the region names
region_map = {
    'Kherson': 'Kherson Oblast',
    'Volyn': 'Volyn Oblast',
    'Rivne': 'Rivne Oblast',
    'Zhytomyr': 'Zhytomyr Oblast',
    'Kyiv': 'Kyiv Oblast',
    'Chernihiv': 'Chernihiv Oblast',
    'Sumy': 'Sumy Oblast',
    'Kharkiv': 'Kharkiv Oblast',
    'Luhansk': 'Luhansk Oblast',
    'Donetsk': 'Donetsk Oblast',
    'Zaporizhzhya': 'Zaporizhia Oblast',
    'Lviv': 'Lviv Oblast',
    'Ivano-Frankivsk': 'Ivano-Frankivsk Oblast',
    'Zakarpattya': 'Zakarpattia Oblast',
    'Ternopyl': 'Ternopil Oblast',
    'Chernivtsi': 'Chernivtsi Oblast',
    'Odesa': 'Odessa Oblast',
    'Mykolayiv': 'Mykolaiv Oblast',
    'Autonomous Republic of Crimea': 'Autonomous Republic of Crimea',
    'Vinnytsya': 'Vinnytsia Oblast',
    'Khmelnytskiy': 'Khmelnytskyi Oblast',
    'Cherkasy': 'Cherkasy Oblast',
    'Poltava': 'Poltava Oblast',
    'Dnipropetrovsk': 'Dnipropetrovsk Oblast',
    'Kirovohrad': 'Kirovohrad Oblast',
    'Kyiv city': 'Kyiv',
    'Sevastopol city': 'Sevastopol'
}

years = ["2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"]

composites = ["AllAngle_Composite_Snow_Covered", "AllAngle_Composite_Snow_Free", 
              "NearNadir_Composite_Snow_Covered", "NearNadir_Composite_Snow_Free", 
              "OffNadir_Composite_Snow_Covered", "OffNadir_Composite_Snow_Free"]