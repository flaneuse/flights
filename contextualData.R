# Exploring DCA and IAD airports ----------------------------------------
# Pulling in external data on important changes to IAD and DCA airports

# Laura Hughes, laura.d.hughes@gmail.com
# 22 June 2016
# (c) 2016 via MIT License



# Create dataset ----------------------------------------------------------
context = data.frame(airport = c('IAD', 'IAD', 'IAD', 'IAD', 'IAD', 'IAD', 'IAD',
                                 'DCA'),
                     year = c(2006, 2003, 2008, 2004, 2006, 2008, 2009,
                              2002, 2005
                              ),
                     month = c(10, 04, 01, 04, 01, 11, 09,
                               04, 10),
                     day = c(05, 01, 01, 16, 05, 20, 22,
                             01,18), # Note: using 1st day of month if unknown
                     event = c('Southwest Airlines opens at IAD',
                               '4 gates added to Concourse B',
                               '15 gates added to Concourse B',
                               'Independence Air launches',
                               'Independence Air shuts',
                               'New runway opened',
                               'Expanded international arrivals building',
                               'Ban on large aircraft lifted',
                               'Reopened to general aviation on limited basis')
                     )


# Sources -----------------------------------------------------------------
# https://en.wikipedia.org/wiki/Washington_Dulles_International_Airport
# http://www.swamedia.com/channels/Southwest-News/releases/Southwest-Airlines-Does-Dulles-Airline-Announces-Fares-and-Flights-for-Newest-Destination?mode=print
# https://en.wikipedia.org/wiki/Independence_Air
# http://www.mwaa.com/sites/default/files/archive/mwaa.com/file/conbexpansion.pdf
# https://en.wikipedia.org/wiki/Ronald_Reagan_Washington_National_Airport

