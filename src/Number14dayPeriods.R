chron::dates(sapply( chron::seq.dates("01/01/1965", "12/31/2016", by="years") ,
                     function(x) chron::seq.dates(x, to=x+365, by=14, length=26)))
