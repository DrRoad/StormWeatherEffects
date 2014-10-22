## ---------------------------------------------------------------------------
## Simply a scratchpad R file used to check code for the StormWeatherEffects 
## report
## ---------------------------------------------------------------------------

## Download data if necessary
if (! file.exists('data/repdata-data-StormData.csv.bz2')) {
    fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
    
    download.file(fileUrl,
                  dest = 'data/repdata-data-StormData.csv.bz2',
                  method = 'curl',
                  quiet = TRUE)
}

## Load the data
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(car))

data <- read.csv('data/repdata-data-StormData.csv.bz2')
head(data)

## Clean up headers
data.titles <- c('state.__',              # ????
                 'begin.date',            # Event Beginning Date
                 'begin.time',            # Event Beginning Time
                 'time.zone',             # Event Time Zone
                 'county',                # Begin Event County Code
                 'county.name',           # Begin Event County Name
                 'state',                 # Begin Event State
                 'evtype',                # Event Type
                 'begin.range',           # Beginning Range
                 'begin.azi',             # Beginning Azimuth
                 'begin.location',        # Beginning Location
                 'end.date',              # Event Ending Date
                 'end.time',              # Event Ending Time
                 'county.end',            # End Event County Code
                 'county.end.name',       # End Event County Name
                 'end.range',             # Ending Range
                 'end.azi',               # Ending Azimuth
                 'end.location',          # Ending Location
                 'length',                # Event Length
                 'width',                 # Event Width
                 'force',                 # Wind Force of Event
                 'mag',                   # Event Magnitude
                 'fatalities',            # Number of Event Fatalities
                 'injuries',              # Number of Event Injuries
                 'prop.damage',           # Cost of Property Damage by Event
                 'prop.damage.exp',       # Property Damage Cost Category
                 'crop.damage',           # Cost of Crop Damage by Event
                 'crop.damage.exp',       # Crop Damage Cost Category
                 'wfo',                   # Weather Forecasting Office??
                 'state.office',          # State Office Name
                 'zone.names',            # Zone Names
                 'latitude',              # Beginning Latitude
                 'longitude',             # Beginning Longitude
                 'latitude.end',          # Ending Latitute
                 'longitude.end',         # Ending Longitude
                 'remarks',               # Event Remarks
                 'ref.num')               # Reference Number

colnames(data) <- data.titles
data <- tbl_df(data)                      # Make this easier to use for manipulation
data


## Cleanup the evtype column

# first take a backup of the original data as it takes a long time to 
# reconsitute
StormData.backup <- data

# get rid of any '; or \\' in the data as they cause recode issues
data <- data %>% 
    mutate(evtype = str_replace_all(evtype, ';', ''))

# Recode the evtype column values to be more consistent
# Astronomicasl Low Tide Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ASTRONOMICAL LOW TIDE', 'BLOW-OUT TIDE', 'BLOW-OUT TIDES')
    = 'Astronomical Low Tide'")

# Avalanche Category
data$evtype <- recode(str_trim(data$evtype),
    "c('AVALANCE', 'AVALANCHE') = 'Avalanche'")

# Blizzard Categgory
data$evtype <- recode(str_trim(data$evtype), 
    "c('BLIZZARD AND EXTREME WIND CHIL', 'BLIZZARD AND HEAVY SNOW',
    'Blizzard Summary', 'BLIZZARD WEATHER', 'BLIZZARD/FREEZING RAIN',
    'BLIZZARD/HEAVY SNOW', 'BLIZZARD/HIGH WIND', 'BLIZZARD',
    'BLIZZARD/WINTER STORM', 'blowing snow', 'Blowing Snow',
    'BLOWING SNOW', 'BLOWING SNOW & EXTREME WIND CH', 
    'BLOWING SNOW- EXTREME WIND CHI', 'BLOWING SNOW/EXTREME WIND CHIL',
    'GROUND BLIZZARD') = 'Blizzard'")

# Coastal Flood Category
data$evtype <- recode(str_trim(data$evtype), 
    "c('COASTAL FLOOD', 'BEACH FLOOD', 'COASTAL  FLOODING/EROSION',
    'Coastal Flooding', 'coastal flooding', 'coastal flooding',
    'COASTAL FLOODING/EROSION', 'COASTAL/TIDAL FLOOD',
    'CSTL FLOODING/EROSION', 'Erosion/Cstl Flood', 'COASTAL FLOODING',
    'COASTALFLOOD', 'BEACH EROSION/COASTAL FLOOD') = 'Coastal Flood'")

# Cold/Wind Chill Category
data$evtype <- recode(str_trim(data$evtype), 
    "c('BITTER WIND CHILL', 'BITTER WIND CHILL TEMPERATURES', 'Cold',
    'COLD', 'COLD AND WET CONDITIONS', 'Cold Temperature',
    'COLD TEMPERATURES', 'COLD WAVE', 'COLD WEATHER',
    'COLD WIND CHILL TEMPERATURES', 'COLD/WIND CHILL', 'COLD/WINDS',
    'COOL AND WET', 'COOL SPELL', 'Cold and Frost', 'COLD AND FROST',
    'COLD AND SNOW') = 'Cold/Wind Chill'")                      

# Dense Fog Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DENSE FOG', 'FOG', 'FOG AND COLD TEMPERATURES') = 'Dense Fog'")

# Dense Smoke Category
data$evtype <- recode(str_trim(data$evtype),
  "c('DENSE SMOKE') = 'Dense Smoke'")

# Drought Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ABNORMALLY DRY', 'BELOW NORMAL PRECIPITATION', 'DRIEST MONTH',
    'DROUGHT', 'DROUGHT/EXCESSIVE HEAT', 'DRY', 'DRY CONDITIONS',
    'DRY HOT WEATHER', 'DRY PATTERN', 'DRY SPELL', 'DRY WEATHER',
    'DRYNESS', 'EXCESSIVELY DRY', 'VERY DRY', 'WARM DRY CONDITIONS',
    'HEAT DROUGHT', 'HEAT/DROUGHT') = 'Drought'")

# Dust Devil Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DUST DEVIL', 'Dust Devil', 'DUST DEVEL', 'DUST DEVIL WATERSPOUT')
    = 'Dust Devil'")

# Dust Storm Category
data$evtype <- recode(str_trim(data$evtype),
    "c('BLOWING DUST', 'DUST STORM', 'DUST STORM/HIGH WINDS', 'DUSTSTORM') 
    = 'Dust Storm'")

# Excessive Heat Category
data$evtype <- recode(str_trim(data$evtype),
    "c('EXCESSIVE HEAT', 'EXCESSIVE HEAT/DROUGHT', 'EXTREME HEAT',
    'HEAT WAVE DROUGHT', 'HEAT WAVES', 'HIGH TEMPERATURE RECORD')
    = 'Excessive Heat'")

# Extreme Cold/Wind Chill Category
data$evtype <- recode(str_trim(data$evtype),
    "c('Black Ice', 'BLACK ICE', 'Excessive Cold', 'Extended Cold', 
    'Extreme Cold', 'EXTREME COLD', 'EXTREME COLD/WIND CHILL',
    'EXTREME WIND CHILL', 'EXTREME WIND CHILL/BLOWING SNO', 
    'EXTREME WIND CHILLS', 'EXTREME WINDCHILL',
    'EXTREME WINDCHILL TEMPERATURES', 'EXTREME/RECORD COLD',
    'ICE', 'ICE AND SNOW', 'ICE FLOES', 'Ice Fog', 'ICE JAM',
    'Ice jam flood (minor', 'ICE JAM FLOODING', 'ICE ON ROAD',
    'ICE PELLETS', 'ICE ROADS', 'Ice/Snow', 'ICE/SNOW', 'ICE/STRONG WINDS') 
    = 'Extreme Cold/Wind Chill'")

# Flash Flood Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DAM FAILURE', 'DAM BREAK', 'FLASH FLOOD', 'FLASH FLOOD - HEAVY RAIN',
    'FLASH FLOOD FROM ICE JAMS', 'FLASH FLOOD LANDSLIDES', 'FLASH FLOOD WINDS',
    'FLASH FLOOD/', 'FLASH FLOOD/ FLOOD', 'FLASH FLOOD/ STREET',
    'FLASH FLOOD/FLOOD', 'FLASH FLOOD/HEAVY RAIN', 'FLASH FLOOD/LANDSLIDE',
    'FLASH FLOODING', 'FLASH FLOODING/FLOOD',
    'FLASH FLOODING/THUNDERSTORM WI', 'FLASH FLOODS', 'FLASH FLOOODING',
    'FLOOD FLASH', 'FLOOD/FLASH') = 'Flash Flood'")

# Flood Category
data$evtype <- recode(str_trim(data$evtype),
    "c('BREAKUP FLOODING', 'FLOOD', 'FLOOD & HEAVY RAIN', 'FLOOD FLOOD/FLASH',
    'FLOOD WATCH/', 'Flood/Flash Flood', 'FLOOD/FLASH FLOOD', 
    'FLOOD/RAIN/WIND', 'FLOOD/RAIN/WINDS', 'FLOOD/RIVER FLOOD', 
    'Flood/Strong Wind', 'FLOODING', 'FLOODING/HEAVY RAIN',
    'FLOODS', 'FLOOD/FLASH FLOODING', 'FLOOD/FLASH/FLOOD', 'FLOOD/FLASHFLOOD',
    'HIGHWAY FLOODING') = 'Flood'")

# Freezing Fog Category
data$evtype <- recode(str_trim(data$evtype),
    "c('FREEZING FOG') = 'Freezing Fog'")

# Frost/Freeze Category
data$evtype <- recode(str_trim(data$evtype),
    "c('AGRICULTURAL FREEZE', 'Damaging Freeze', 'DAMAGING FREEZE', 
    'EARLY FREEZE', 'EARLY FROST', 'Early Frost',
    'FIRST FROST', 'Freeze', 'FREEZE', 'Freezing drizzle', 'Freezing Drizzle',
    'FREEZING DRIZZLE', 'FREEZING DRIZZLE AND FREEZING', 'Freezing rain',
    'Freezing Rain', 'FREEZING RAIN', 'FREEZING RAIN AND SLEET',
    'FREEZING RAIN AND SNOW', 'FREEZING RAIN SLEET AND', 
    'FREEZING RAIN SLEET AND LIGHT', 'FREEZING RAIN/SLEET', 'FREEZING RAIN/SNOW',
    'Freezing Spray', 'Frost', 'FROST', 'FROST/FREEZE', 'Frost/Freeze', 'Glaze',
    'GLAZE', 'GLAZE ICE', 'GLAZE/ICE STORM', 'HARD FREEZE',
    'HYPERTHERMIA/EXPOSURE', 'HYPERTHERMIA', 'HYPOTHERMIA',
    'Hypothermia/Exposure', 'HYPOTHERMIA/EXPOSURE') 
    = 'Frost/Freeze'")

# Funnel Cloud Category
data$evtype <- recode(str_trim(data$evtype),
    "c('COLD AIR FUNNEL', 'COLD AIR FUNNELS', 'FUNNEL', 'FUNNEL CLOUD', 'FUNNEL CLOUD.',
    'FUNNEL CLOUD/HAIL', 'FUNNEL CLOUDS', 'FUNNELS') = 'Funnel Cloud'")

# Hail Category
data$evtype <- recode(str_trim(data$evtype),
    "c('HAIL', 'HAIL 0.75', 'HAIL 0.88', 'HAIL 075', 'HAIL 088', 'HAIL 1.00',
    'HAIL 1.75', 'HAIL 1.75)', 'HAIL 100', 'HAIL 125', 'HAIL 150', 'HAIL 175',
    'HAIL 200', 'HAIL 225', 'HAIL 275', 'HAIL 450', 'HAIL 75', 'HAIL 80',
    'HAIL 88', 'HAIL ALOFT', 'HAIL DAMAGE', 'HAIL FLOODING', 'HAIL STORM',
    'Hail(0.75)', 'HAIL/ICY ROADS', 'HAIL/WIND', 'HAIL/WINDS', 'HAILSTORM',
    'HAILSTORMS', 'DEEP HAIL') = 'Hail'")

# Heat Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ABNORMAL WARMTH', 'HEAT', 'Heat Wave', 'HEAT WAVE', 
    'Heatburst', 'Hot and Dry', 'HOT PATTERN', 'HOT SPELL', 
    'HOT WEATHER', 'HOT/DRY PATTERN') = 'Heat'")

# Heavy Rain Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ABNORMALLY WET', 'DOWNBURST', 'DOWNBURST WINDS',
    'EXCESSIVE PRECIPITATION', 'EXCESSIVE RAIN', 'EXCESSIVE RAINFALL', 
    'EXCESSIVE WETNESS', 'EXTREMELY WET', 'Heavy Precipitation',
    'HEAVY PRECIPITATION', 'Heavy rain', 'HEAVY RAIN', 'HEAVY RAIN AND FLOOD',
    'HEAVY RAIN URBAN FLOOD WINDS', 'HEAVY RAIN/FLOODING', 
    'Heavy Rain and Wind', 'HEAVY RAIN EFFECTS', 
    'Heavy Rain/High Surf', 'HEAVY RAIN/LIGHTNING', 
    'HEAVY RAIN/MUDSLIDES/FLOOD', 'HEAVY RAIN/SEVERE WEATHER',
    'HEAVY RAIN/SMALL STREAM URBAN', 'HEAVY RAIN/SNOW', 
    'HEAVY RAIN/URBAN FLOOD', 'HEAVY RAIN/WIND', 'HEAVY RAINFALL', 
    'HEAVY RAINS', 'HEAVY RAINS/FLOODING', 'HEAVY SHOWER',
    'HEAVY SHOWERS', 'HVY RAIN', 'LOCALLY HEAVY RAIN',
    'Mixed Precipitation', 'MIXED PRECIPITATION', 'Monthly Rainfall',
    'MONTHLY RAINFALL', 'NORMAL PRECIPITATION', 'PROLONGED RAIN',
    'RAIN', 'RAIN (HEAVY)', 'RAIN AND WIND', 'Rain Damage', 'RAIN/SNOW',
    'RAIN/WIND', 'RAINSTORM', 'RECORD PRECIPITATION', 'RECORD RAINFALL',
    'TORRENTIAL RAIN', 'Torrential Rainfall', 'UNSEASONABLY COOL AND WET',
    'UNSEASONABLY WARM AND WET', 'UNSEASONABLY WARM/WET', 
    'UNSEASONABLY WET', 'UNSEASONABLE RAIN', 'wet microburst', 
    'WET MICROBURST', 'Wet Month', 'WET WEATHER', 'Wet Year',
    'HEAVY PRECIPITATION', 'HEAVY PRECIPATATION') = 'Heavy Rain'")

# Heavy Snow Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ACCUMULATED SNOWFALL', 'EXCESSIVE SNOW', 'HEAVY SNOW',
    'HEAVY SNOW   FREEZING RAIN', 'HEAVY SNOW & ICE', 'HEAVY SNOW AND',
    'HEAVY SNOW AND HIGH WINDS', 'HEAVY SNOW AND ICE',
    'HEAVY SNOW AND ICE STORM', 'HEAVY SNOW AND STRONG WINDS',
    'HEAVY SNOW ANDBLOWING SNOW', 'Heavy snow shower', 'HEAVY SNOW SQUALLS',
    'HEAVY SNOW-SQUALLS', 'HEAvy SNOW/BLIZZARD', 
    'HEAVY SNOW/BLIZZARD/AVALANCHE', 'HEAVY SNOW/BLOWING SNOW', 
    'HEAVY SNOW/FREEZING RAIN', 'HEAVY SNOW/HIGH', 'HEAVY SNOW/HIGH WIND',
    'HEAVY SNOW/HIGH WINDS', 'HEAVY SNOW/HIGH WINDS & FLOOD',
    'HEAVY SNOW/HIGH WINDS/FREEZING', 'HEAVY SNOW/ICE', 'HEAVY SNOW/ICE STORM',
    'HEAVY SNOW/SLEET', 'HEAVY SNOW/SQUALLS', 'HEAVY SNOW/WIND', 
    'HEAVY SNOW/WINTER STORM', 'HEAVY SNOWPACK',
    'HEAVY SNOW/BLIZZARD', 'HEAVY WET SNOW') = 'Heavy Snow'")

# High Surf Category
data$evtype <- recode(str_trim(data$evtype),
    "c('HAZARDOUS SURF', 'HEAVY SEAS', 'HEAVY SURF', 'Heavy Surf',
    'Heavy surf and wind', 'HEAVY SURF COASTAL FLOODING', 'HEAVY SURF/HIGH SURF',
    'HEAVY SWELLS', 'HIGH SWELLS', 'HIGH SEAS', 'HIGH SURF',
    'HIGH  SWELLS', 'HIGH SURF ADVISORIES', 'HIGH SURF ADVISORY',
    'HIGH TIDES', 'HIGH WATER', 'HIGH WAVES') = 'High Surf'")

# High Wind Category
data$evtype <- recode(str_trim(data$evtype),
    "c('gradient wind', 'Gradient wind', 'GRADIENT WIND', 'GRADIENT WINDS',
    'GUSTY LAKE WIND', 'Gusty Wind', 'GUSTY WIND', 'GUSTY WIND/HAIL',
    'GUSTY WIND/HVY RAIN', 'Gusty wind/rain', 'Gusty winds', 'Gusty Winds',
    'GUSTY WINDS', 'HIGH WINDS', 'HIGH WIND', 'HIGH WIND (G40)',
    'HIGH WIND 48', 'HIGH WIND 63', 'HIGH WIND 70', 'HIGH WIND AND HEAVY SNOW',
    'HIGH WIND DAMAGE', 'HIGH WIND/ BLIZZARD',
    'HIGH WIND/BLIZZARD/FREEZING RA', 'HIGH WIND/HEAVY SNOW',
    'HIGH WIND/LOW WIND CHILL', 'HIGH WIND/WIND CHILL/BLIZZARD', 'HIGH WINDS',
    'HIGH WINDS 55', 'HIGH WINDS 57', 'HIGH WINDS 58', 'HIGH WINDS 63',
    'HIGH WINDS 66', 'HIGH WINDS 67', 'HIGH WINDS 73', 'HIGH WINDS 76',
    'HIGH WINDS 80', 'HIGH WINDS 83', 'HIGH WINDS AND WIND CHILL',
    'HIGH WINDS HEAVY RAINS', 'HIGH WINDS/', 'HIGH WINDS/COASTAL FLOOD',
    'HIGH WINDS/COLD', 'HIGH WINDS/FLOODING', 'HIGH WINDS/HEAVY RAIN', 
    'HIGH WINDS/SNOW', 'Wind', 'WIND', 'WIND ADVISORY', 'WIND AND WAVE',
    'WIND CHILL', 'WIND CHILL/HIGH WIND', 'Wind Damage', 'WIND DAMAGE',
    'WIND GUSTS', 'WIND STORM', 'WIND/HAIL', 'WINDS', 'WND',
    'HIGH  WINDS', 'HIGH WIND AND HIGH TIDES','HIGH WIND AND SEAS',
    'HIGH WIND/BLIZZARD', 'HIGH WIND/SEAS', 'HIGH WIND/WIND CHILL', 
    'HIGH WINDS 82', 'HIGH WINDS DUST STORM') = 'High Wind'")

# Hurricane (Typhoon) Category
data$evtype <- recode(str_trim(data$evtype), 
    "c('HURRICANE', 'Hurricane Edouard', 'HURRICANE EMILY', 'HURRICANE ERIN',
    'HURRICANE FELIX', 'HURRICANE GORDON', 'HURRICANE OPAL', 
    'HURRICANE OPAL/HIGH WINDS', 'HURRICANE-GENERATED SWELLS', 
    'HURRICANE/TYPHOON') = 'Hurricance (Typhoon)'")

# Ice Storm Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ICE STORM', 'ICE STORM AND SNOW', 'ICE STORM/FLASH FLOOD') 
    = 'Ice Storm'");

# Lake Effect Snow Category
data$evtype <- recode(str_trim(data$evtype),
    "c('HEAVY LAKE SNOW') = 'Lake Effect Snow'")

# Other Category (for those observations that don't seem to fit elsewhere)
data$evtype <- recode(str_trim(data$evtype),
    "c('APACHE COUNTY', 'DROWNING', 'EXCESSIVE', 'HEAVY MIX', 'HIGH') 
    = 'Other'")

# Storm Surge/Tide Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ASTRONOMICAL HIGH TIDE', 'BEACH EROSIN', 'Beach Erosion', 
    'BEACH EROSION', 'STORM SURGE/TIDE', 'COASTAL EROSION', 'COASTAL STORM',
    'COASTAL SURGE', 'Coastal Storm', 'COASTALSTORM') = 'Storm Surge/Tide'")

# Thunderstorm Wind Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DRY MICROBURST', 'DRY MICROBURST 50', 'DRY MICROBURST 53', 
    'DRY MICROBURST 58', 'DRY MICROBURST 61', 'DRY MICROBURST 84',
    'DRY MICROBURST WINDS', 'DRY MIRCOBURST WINDS', 'GUSTY THUNDERSTORM WIND',
    'GUSTY THUNDERSTORM WINDS', 'Microburst', 'MICROBURST', 
    'MICROBURST WINDS', 'SEVERE THUNDERSTORM', 'SEVERE THUNDERSTORM WINDS',
    'SEVERE TURBULENCE', 'THUDERSTORM WINDS', 'THUNDERSTORM WINDS',
    'THUNDERESTORM WINDS', 'THUNDERSNOW', 'Thundersnow shower',
    'THUNDERSTORM', 'THUNDERSTORM WINDS', 'THUNDERSTORM DAMAGE',
    'THUNDERSTORM DAMAGE TO', 'THUNDERSTORM HAIL', 'THUNDERSTORM W INDS',
    'THUNDERSTORM WIND', 'THUNDERSTORM WIND (G40)', 'THUNDERSTORM WIND 50',
    'THUNDERSTORM WIND 52', 'THUNDERSTORM WIND 56', 'THUNDERSTORM WIND 59',
    'THUNDERSTORM WIND 59 MPH', 'THUNDERSTORM WIND 59 MPH.',
    'THUNDERSTORM WIND 60 MPH', 'THUNDERSTORM WIND 65 MPH',
    'THUNDERSTORM WIND 65MPH', 'THUNDERSTORM WIND 69', 
    'THUNDERSTORM WIND 98 MPH', 'THUNDERSTORM WIND G50', 
    'THUNDERSTORM WIND G51', 'THUNDERSTORM WIND G52', 'THUNDERSTORM WIND G55',
    'THUNDERSTORM WIND G60', 'THUNDERSTORM WIND G61', 
    'THUNDERSTORM WIND TREES', 'THUNDERSTORM WIND.', 'THUNDERSTORM WIND/ TREE',
    'THUNDERSTORM WIND/ TREES', 'THUNDERSTORM WIND/AWNING', 
    'THUNDERSTORM WIND/HAIL', 'THUNDERSTORM WIND/LIGHTNING', 
    'THUNDERSTORM WINDS', 'THUNDERSTORM WINDS      LE CEN',
    'THUNDERSTORM WINDS 13', 'THUNDERSTORM WINDS 2', 'THUNDERSTORM WINDS 50',
    'THUNDERSTORM WINDS 52', 'THUNDERSTORM WINDS 53', 'THUNDERSTORM WINDS 60',
    'THUNDERSTORM WINDS 61', 'THUNDERSTORM WINDS 62', 
    'THUNDERSTORM WINDS 63 MPH', 'THUNDERSTORM WINDS AND',
    'THUNDERSTORM WINDS FUNNEL CLOU', 'THUNDERSTORM WINDS G',
    'THUNDERSTORM WINDS G60', 'THUNDERSTORM WINDS HAIL', 'THUNDEERSTORM WINDS',
    'THUNDERSTORM  WINDS', 'THUNDERSTORM WINDS HEAVY RAIN', 
    'THUNDERSTORM WINDS LIGHTNING', 'THUNDERSTORM WINDS SMALL STREA',
    'THUNDERSTORM WINDS URBAN FLOOD', 'THUNDERSTORM WINDS.',
    'THUNDERSTORM WINDS/ FLOOD', 'THUNDERSTORM WINDS/ HAIL',
    'THUNDERSTORM WINDS/FLASH FLOOD', 'THUNDERSTORM WINDS/FLOODING',
    'THUNDERSTORM WINDS/FUNNEL CLOU', 'THUNDERSTORM WINDS/HAIL',
    'THUNDERSTORM WINDS/HEAVY RAIN', 'THUNDERSTORM WINDS53',
    'THUNDERSTORM WINDSHAIL', 'THUNDERSTORM WINDSS', 'THUNDERSTORM WINS',
    'THUNDERSTORMS', 'THUNDERSTORMS WIND', 'THUNDERSTORMS WINDS',
    'THUNDERSTORMW', 'THUNDERSTORMW 50', 'THUNDERSTORMW WINDS',
    'THUNDERSTORMWINDS', 'THUNDERSTROM WIND', 'THUNDERSTROM WINDS',
    'THUNDERTORM WINDS', 'THUNDERTSORM WIND', 'THUNDESTORM WINDS',
    'THUNERSTORM WINDS', 'TSTM', 'TSTM HEAVY RAIN', 'Tstm Wind', 
    'TSTM WIND', 'TSTM WIND  (G45)', 'TSTM WIND (41)', 'TSTM WIND (G35)',
    'TSTM WIND (G40)', 'TSTM WIND (G45)', 'TSTM WIND 40', 'TSTM WIND 45',
    'TSTM WIND 50', 'TSTM WIND 51', 'TSTM WIND 52', 'TSTM WIND 55',
    'TSTM WIND 65)', 'TSTM WIND AND LIGHTNING', 'TSTM WIND DAMAGE',
    'TSTM WIND G45', 'TSTM WIND G58', 'TSTM WIND/HAIL', 'TSTM WINDS',
    'TSTM WND', 'TSTMW', 'TUNDERSTORM WIND', 'DOWNBURST WINDS',
    'GUSTNADO', 'GUSTNADO AND') = 'Thunderstorm Wind'")

# Tornado Category
data$evtype <- recode(str_trim(data$evtype),
    "c('COLD AIR TORNADO') = 'Tornado'")

# Wildfire Category
data$evtype <- recode(str_trim(data$evtype),
    "c('WILDFIRES', 'WILDFIRE', 'WILD/FOREST FIRES', 'WILD/FOREST FIRE',
    'WILD FIRES', 'BRUSH FIRE', 'BRUSH FIRES', 'FOREST FIRES',
    'GRASS FIRES') = 'Wildfire'")

# Winter Storm Category
data$evtype <- recode(str_trim(data$evtype),
    "c('WINTER STORMS', 'WINTER STORM/HIGH WINDS', 'WINTER STORM/HIGH WIND',
    'WINTER STORM', 'WINTER STORM HIGH WINDS') = 'Winter Storm'")

# Winter Weather Category
data$evtype <- recode(str_trim(data$evtype),
    "c('WINTRY MIX', 'Wintry Mix', 'Wintry mix', 'WINTERY MIX', 
    'WINTER WEATHER/MIX', 'WINTER WEATHER', 'WINTER MIX', 
    'Winter Weather Mix', 'WINTER WEATHER MIX', 'EARLY SNOW', 'Early snowfall',
    'EARLY SNOWFALL', 'FALLING SNOW/ICE', 'FIRST SNOW', 'Drifting Snow',
    'EARLY RAIN') = 'Winter Weather'")


