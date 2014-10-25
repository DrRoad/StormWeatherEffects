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
suppressPackageStartupMessages(library(Hmisc))
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

# get rid of any ':, ;, ?,  or \\' in the data as they cause recode issues
data <- data %>%
    mutate(evtype = str_replace_all(evtype, ';', ''))

data <- data %>%
mutate(evtype = str_replace_all(evtype, ':', ''))

# These will eliminate only 3 rows so wont adversly affect the result
#data <- data[data$evtype != "?",]
data <- subset(data, data$evtype != "?")
data <- subset(data, data$evtype != "FROST\\FREEZE")
data <- subset(data, data$evtype != "SNOW\\COLD")


             
# Recode the evtype column values to be more consistent
# Astronomicasl Low Tide Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ASTRONOMICAL LOW TIDE', 'BLOW-OUT TIDE', 'BLOW-OUT TIDES')
    = 'Astronomical Low Tide'")

# Avalanche Category
data$evtype <- recode(str_trim(data$evtype),
    "c('AVALANCE', 'AVALANCHE', 'LANDSLIDE', 'LANDSLIDE/URBAN FLOOD',
    'LANDSLIDES', 'LANDSLUMP', 'Landslump', 'MUD SLIDE', 'MUD SLIDES',
    'MUD SLIDES URBAN FLOODING', 'MUD/ROCK SLIDE', 'Mudslide', 
    'MUDSLIDE', 'MUDSLIDE/LANDSLIDE', 'Mudslides', 'MUDSLIDES', 
    'ROCK SLIDE') = 'Avalanche'")

# Blizzard Categgory
data$evtype <- recode(str_trim(data$evtype), 
    "c('BLIZZARD AND EXTREME WIND CHIL', 'BLIZZARD AND HEAVY SNOW',
    'Blizzard Summary', 'BLIZZARD WEATHER', 'BLIZZARD/FREEZING RAIN',
    'BLIZZARD/HEAVY SNOW', 'BLIZZARD/HIGH WIND', 'BLIZZARD',
    'BLIZZARD/WINTER STORM', 'blowing snow', 'Blowing Snow',
    'BLOWING SNOW', 'BLOWING SNOW & EXTREME WIND CH', 
    'BLOWING SNOW- EXTREME WIND CHI', 'BLOWING SNOW/EXTREME WIND CHIL',
    'GROUND BLIZZARD', 'SNOW SQUALL', 'Snow squalls', 'Snow Squalls',
    'SNOW- HIGH WIND- WIND CHILL', 'SNOW SQUALLS',
    'SNOW/ICE STORM', 'SNOWSTORM') = 'Blizzard'")

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
    "c('DENSE FOG', 'FOG', 'FOG AND COLD TEMPERATURES',
    'PATCHY DENSE FOG') = 'Dense Fog'")

# Dense Smoke Category
data$evtype <- recode(str_trim(data$evtype),
  "c('DENSE SMOKE', 'SMOKE') = 'Dense Smoke'")

# Drought Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ABNORMALLY DRY', 'BELOW NORMAL PRECIPITATION', 'DRIEST MONTH',
    'DROUGHT', 'DROUGHT/EXCESSIVE HEAT', 'DRY', 'DRY CONDITIONS',
    'DRY HOT WEATHER', 'DRY PATTERN', 'DRY SPELL', 'DRY WEATHER',
    'DRYNESS', 'EXCESSIVELY DRY', 'VERY DRY', 'WARM DRY CONDITIONS',
    'HEAT DROUGHT', 'HEAT/DROUGHT', 'Record Dry Month',
    'RECORD DRYNESS', 'RECORD LOW RAINFALL', 'Record dry month',
    'SNOW DROUGHT', 'UNSEASONABLY DRY') = 'Drought'")

# Dust Devil Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DUST DEVIL', 'Dust Devil', 'DUST DEVEL', 'DUST DEVIL WATERSPOUT', 
    'Whirlwind', 'WHIRLWIND') = 'Dust Devil'")

# Dust Storm Category
data$evtype <- recode(str_trim(data$evtype),
    "c('BLOWING DUST', 'DUST STORM', 'DUST STORM/HIGH WINDS', 'DUSTSTORM',
    'Saharan Dust', 'SAHARAN DUST') = 'Dust Storm'")

# Excessive Heat Category
data$evtype <- recode(str_trim(data$evtype),
    "c('EXCESSIVE HEAT', 'EXCESSIVE HEAT/DROUGHT', 'EXTREME HEAT',
    'HEAT WAVE DROUGHT', 'HEAT WAVES', 'HIGH TEMPERATURE RECORD',
    'PROLONG WARMTH', 'Record Heat', 'RECORD HEAT', 'RECORD HEAT WAVE',
    'RECORD HIGH', 'Record High', 'RECORD HIGH TEMPERATURE',
    'RECORD HIGH TEMPERATURES', 'RECORD WARM TEMPS.', 'RECORD WARM',
    'Record Warmth', 'RECORD WARMTH', 'RECORD/EXCESSIVE HEAT', 
    'UNSEASONABLY HOT', 'UNSEASONABLY WARM', 'UNSEASONABLY WARM & WET',
    'UNSEASONABLY WARM AND DRY', 'UNSEASONABLY WARM YEAR',
    'UNUSUAL/RECORD WARMTH', 'UNUSUALLY WARM') = 'Excessive Heat'")

# Extreme Cold/Wind Chill Category
data$evtype <- recode(str_trim(data$evtype),
    "c('Black Ice', 'BLACK ICE', 'Excessive Cold', 'Extended Cold', 
    'Extreme Cold', 'EXTREME COLD', 'EXTREME COLD/WIND CHILL',
    'EXTREME WIND CHILL', 'EXTREME WIND CHILL/BLOWING SNO', 
    'EXTREME WIND CHILLS', 'EXTREME WINDCHILL',
    'EXTREME WINDCHILL TEMPERATURES', 'EXTREME/RECORD COLD',
    'ICE', 'ICE AND SNOW', 'ICE FLOES', 'Ice Fog', 'ICE JAM',
    'Ice jam flood (minor', 'ICE JAM FLOODING', 'ICE ON ROAD',
    'ICE PELLETS', 'ICE ROADS', 'Ice/Snow', 'ICE/SNOW', 'ICE/STRONG WINDS',
    'Icy Roads', 'ICY ROADS', 'LOW TEMPERATURE', 'LOW TEMPERATURE RECORD',
    'LOW WIND CHILL', 'PATCHY ICE', 'Prolong Cold', 'PROLONG COLD',
    'PROLONG COLD/SNOW', 'RECORD COLD', 'Record Cold', 'RECORD  COLD',
    'RECORD COLD AND HIGH WIND', 'RECORD COLD/FROST', 'RECORD COOL',
    'RECORD LOW', 'SEVERE COLD', 'Unseasonably Cold', 'UNSEASONABLY COLD',
    'UNSEASONABLY COOL', 'UNSEASONABLY COOL & WET', 
    'UNSEASONABLY LOW TEMP', 'UNUSUALLY COLD', 'Unseasonable Cold', 
    'UNSEASONAL LOW TEMP') = 'Extreme Cold/Wind Chill'")

# Flash Flood Category
data$evtype <- recode(str_trim(data$evtype),
    "c('DAM FAILURE', 'DAM BREAK', 'FLASH FLOOD', 'FLASH FLOOD - HEAVY RAIN',
    'FLASH FLOOD FROM ICE JAMS', 'FLASH FLOOD LANDSLIDES', 'FLASH FLOOD WINDS',
    'FLASH FLOOD/', 'FLASH FLOOD/ FLOOD', 'FLASH FLOOD/ STREET',
    'FLASH FLOOD/FLOOD', 'FLASH FLOOD/HEAVY RAIN', 'FLASH FLOOD/LANDSLIDE',
    'FLASH FLOODING', 'FLASH FLOODING/FLOOD',
    'FLASH FLOODING/THUNDERSTORM WI', 'FLASH FLOODS', 'FLASH FLOOODING',
    'FLOOD FLASH', 'FLOOD/FLASH', 'LOCAL FLASH FLOOD') = 'Flash Flood'")

# Flood Category
data$evtype <- recode(str_trim(data$evtype),
    "c('BREAKUP FLOODING', 'FLOOD', 'FLOOD & HEAVY RAIN', 'FLOOD FLOOD/FLASH',
    'FLOOD WATCH/', 'Flood/Flash Flood', 'FLOOD/FLASH FLOOD', 
    'FLOOD/RAIN/WIND', 'FLOOD/RAIN/WINDS', 'FLOOD/RIVER FLOOD', 
    'Flood/Strong Wind', 'FLOODING', 'FLOODING/HEAVY RAIN',
    'FLOODS', 'FLOOD/FLASH FLOODING', 'FLOOD/FLASH/FLOOD', 'FLOOD/FLASHFLOOD',
    'HIGHWAY FLOODING', 'LOCAL FLOOD', 'MAJOR FLOOD', 'MINOR FLOOD',
    'Minor Flooding', 'MINOR FLOODING', 'RAPIDLY RISING WATER',
    'RIVER AND STREAM FLOOD', 'RIVER FLOOD', 'River Flooding', 
    'RIVER FLOODING', 'RURAL FLOOD', 'SMALL STREAM', 'SMALL STREAM AND',
    'SMALL STREAM AND URBAN FLOOD', 'SMALL STREAM AND URBAN FLOODIN',
    'SMALL STREAM FLOOD', 'SMALL STREAM FLOODING', 'SMALL STREAM URBAN FLOOD',
    'SMALL STREAM/URBAN FLOOD', 'Sml Stream Fld', 'SNOWMELT FLOODING',
    'STREAM FLOODING', 'STREET FLOOD', 'STREET FLOODING',
    'URBAN AND SMALL STREAM', 'URBAN AND SMALL STREAM FLOOD', 
    'URBAN AND SMALL STREAM FLOODIN', 'Urban flood', 'Urban Flood',
    'URBAN FLOOD', 'URBAN FLOOD LANDSLIDE', 'Urban Flooding', 'URBAN FLOODING',
    'URBAN FLOODS', 'URBAN SMALL', 'URBAN SMALL STREAM FLOOD', 'URBAN/SMALL', 
    'URBAN/SMALL FLOODING', 'URBAN/SMALL STREAM', 'URBAN/SMALL STREAM  FLOOD',
    'URBAN/SMALL STREAM FLOOD', 'URBAN/SMALL STREAM FLOODING', 'URBAN AND SMALL',
    'URBAN/SMALL STRM FLDG', 'URBAN/SML STREAM FLD', 'URBAN/SML STREAM FLDG',
    'URBAN/STREET FLOODING') = 'Flood'")

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
    'Hypothermia/Exposure', 'HYPOTHERMIA/EXPOSURE', 'LATE FREEZE') = 'Frost/Freeze'")

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
    'HAILSTORMS', 'DEEP HAIL', 'LATE SEASON HAIL', 'NON SEVERE HAIL',
    'small hail', 'Small Hail', 'SMALL HAIL') = 'Hail'")

# Heat Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ABNORMAL WARMTH', 'HEAT', 'Heat Wave', 'HEAT WAVE', 
    'Heatburst', 'Hot and Dry', 'HOT PATTERN', 'HOT SPELL', 
    'HOT WEATHER', 'HOT/DRY PATTERN', 'UNUSUAL WARMTH', 'UNUSUALLY WARM', 
    'VERY WARM', 'WARM WEATHER') = 'Heat'")

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
    'HEAVY PRECIPITATION', 'HEAVY PRECIPATATION', 'MIXED PRECIP', 
    'MONTHLY PRECIPITATION', 'RECORD/EXCESSIVE RAINFALL') = 'Heavy Rain'")

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
    'HEAVY SNOW/BLIZZARD', 'HEAVY WET SNOW', 'MODERATE SNOW', 'MODERATE SNOWFALL',
    'Monthly Snowfall', 'MONTHLY SNOWFALL', 'Mountain Snows',
    'NEAR RECORD SNOW', 'Record May Snow', 'RECORD SNOW', 'RECORD SNOW/COLD',
    'RECORD SNOWFALL', 'Record Winter Snow', 'Snow Accumulation', 
    'SNOW ACCUMULATION', 'SNOW ADVISORY', 'SNOW SHOWERS', 'SNOW SLEET',
    'SNOW AND HEAVY SNOW', 'SNOWFALL RECORD', 'SNOW/HEAVY SNOW') = 'Heavy Snow'")

# High Surf Category
data$evtype <- recode(str_trim(data$evtype),
    "c('HAZARDOUS SURF', 'HEAVY SEAS', 'HEAVY SURF', 'Heavy Surf',
    'Heavy surf and wind', 'HEAVY SURF COASTAL FLOODING', 'HEAVY SURF/HIGH SURF',
    'HEAVY SWELLS', 'HIGH SWELLS', 'HIGH SEAS', 'HIGH SURF',
    'HIGH  SWELLS', 'HIGH SURF ADVISORIES', 'HIGH SURF ADVISORY',
    'HIGH TIDES', 'HIGH WATER', 'HIGH WAVES', 'ROGUE WAVE', 'ROUGH SEAS',
    'ROUGH SURF') = 'High Surf'")

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
    'HIGH WINDS 82', 'HIGH WINDS DUST STORM', 'NON TSTM WIND', 
    'NON-SEVERE WIND DAMAGE', 'NON-TSTM WIND') = 'High Wind'")

# Hurricane (Typhoon) Category
data$evtype <- recode(str_trim(data$evtype), 
    "c('HURRICANE', 'Hurricane Edouard', 'HURRICANE EMILY', 'HURRICANE ERIN',
    'HURRICANE FELIX', 'HURRICANE GORDON', 'HURRICANE OPAL', 
    'HURRICANE OPAL/HIGH WINDS', 'HURRICANE-GENERATED SWELLS', 
    'HURRICANE/TYPHOON', 'REMNANTS OF FLOYD', 'TYPHOON') = 'Hurricance (Typhoon)'")

# Ice Storm Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ICE STORM', 'ICE STORM AND SNOW', 'ICE STORM/FLASH FLOOD',
    'Icestorm/Blizzard') = 'Ice Storm'");

# Lake Effect Snow Category
data$evtype <- recode(str_trim(data$evtype),
    "c('HEAVY LAKE SNOW', 'LAKE EFFECT SNOW', 'LAKE-EFFECT SNOW') 
    = 'Lake Effect Snow'")

# Lakeshore Flood Category
data$evtype <- recode(str_trim(data$evtype),
    "c('LAKE FLOOD', 'LAKESHORE FLOOD') = 'Lakeshore Flood'")

# Lightning Category
data$evtype <- recode(str_trim(data$evtype),
    "c('LIGHTING', 'LIGHTNING', 'LIGHTNING  WAUSEON', 'LIGHTNING AND HEAVY RAIN',
    'LIGHTNING AND THUNDERSTORM WIN', 'LIGHTNING AND WINDS', 'LIGHTNING DAMAGE',
    'LIGHTNING FIRE', 'LIGHTNING INJURY', 'LIGHTNING THUNDERSTORM WINDS',
    'LIGHTNING THUNDERSTORM WINDSS', 'LIGHTNING.', 'LIGHTNING/HEAVY RAIN',
    'LIGNTNING') = 'Lightning'")

# Marine Hail Category
data$evtype <- recode(str_trim(data$evtype),
    "c('MARINE HAIL') = 'Marine Hail'")

# Marine High Wind Category
data$evtype <- recode(str_trim(data$evtype),
    "c('MARINE HIGH WIND') = 'Marine High Wind'")

# Marine Strong Wind Category
data$evtype <- recode(str_trim(data$evtype),
    "c('MARINE STRONG WIND') = 'Marine Strong Wind'")

# Marine Thunderstorm Wind Category
data$evtype <- recode(str_trim(data$evtype),
    "c('MARINE THUNDERSTORM', 'MARINE TSTM WIND', 
    'MARINE THUNDERSTORM WIND') = 'Marine Thunderstorm Wind'")

# Other Category (for those observations that don't seem to fit elsewhere)
data$evtype <- recode(str_trim(data$evtype),
    "c('APACHE COUNTY', 'DROWNING', 'EXCESSIVE', 'HEAVY MIX', 'HIGH',
    'Marine Accident', 'MARINE MISHAP', 'Mild and Dry Pattern', 'MILD PATTERN',
    'MILD/DRY PATTERN', 'MONTHLY TEMPERATURE', 'NONE', 'NORTHERN LIGHTS',
    'No Severe Weather', 'OTHER', 'Record temperature', 'RECORD TEMPERATURE',
    'Record Temperatures', 'RECORD TEMPERATURES', 'OTHER', 'SOUTHEAST',
    'Temperature Record', 'Temperature record', 'UNSEASONAL RAIN',
    'UNUSUALLY LATE SNOW', 'URBAN AND SMALL', 'VOG') = 'Other'")

# Rip Current Category
data$evtype <- recode(str_trim(data$evtype),
    "c('RIP CURRENT', 'RIP CURRENTS', 'RIP CURRENTS HEAVY SURF', 
    'RIP CURRENTS/HEAVY SURF') = 'Rip Current'")

# Seiche Category
data$evtype <- recode(str_trim(data$evtype),
    "c('SEICHE') = 'Seiche'")

# Sleet Category
data$evtype <- recode(str_trim(data$evtype),
    "c('LIGHT SNOW AND SLEET', 'LIGHT FREEZING RAIN', 'SLEET',
    'SLEET & FREEZING RAIN', 'SLEET STORM', 'SLEET/FREEZING RAIN',
    'SLEET/ICE STORM', 'SLEET/RAIN/SNOW', 'SLEET/SNOW') = 'Sleet'")

# Storm Surge/Tide Category
data$evtype <- recode(str_trim(data$evtype),
    "c('ASTRONOMICAL HIGH TIDE', 'BEACH EROSIN', 'Beach Erosion', 
    'BEACH EROSION', 'STORM SURGE/TIDE', 'COASTAL EROSION', 'COASTAL STORM',
    'COASTAL SURGE', 'Coastal Storm', 'COASTALSTORM', 'STORM SURGE', 
    'Tidal Flooding', 'TIDAL FLOODING', 'TIDAL FLOOD') = 'Storm Surge/Tide'")

# Strong Wind Category
data$evtype <- recode(str_trim(data$evtype), 
    "c('STORM FORCE WINDS', 'STRONG WIND', 'STRONG WIND GUST', 'Strong winds',
    'Strong Winds', 'STRONG WINDS', 'WAKE LOW WIND') = 'Strong Wind'")

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
    'GUSTNADO', 'GUSTNADO AND', 'Metro Storm, May 26', 
    'SEVERE THUNDERSTORMS', 'wet microburst') = 'Thunderstorm Wind'")

# Tornado Category
data$evtype <- recode(str_trim(data$evtype),
    "c('COLD AIR TORNADO', 'LANDSPOUT', 'LARGE WALL CLOUD',
    'ROTATING WALL CLOUD', 'TORNADO', 'TORNADO DEBRIS',
    'TORNADO F0', 'TORNADO F1', 'TORNADO F2', 'TORNADO F3', 'TORNADOES',
    'TORNADOES, TSTM WIND, HAIL', 'TORNADO/WATERSPOUT', 'TORNADOS',
    'TORNDAO', 'WALL CLOUD', 'WALL CLOUD/FUNNEL CLOUD') = 'Tornado'")

# Tropical Depression Category
data$evtype <- recode(str_trim(data$evtype),
    "c('TROPICAL DEPRESSION') = 'Tropical Depression'")

# Tropical Storm Category
data$evtype <- recode(str_trim(data$evtype),
  "c('TROPICAL STORM', 'TROPICAL STORM DEAN', 'TROPICAL STORM GORDON', 
  'TROPICAL STORM JERRY', 'TROPICAL STORM ALBERTO') = 'Tropical Storm'")

# Tsnunami Category
data$evtype <- recode(str_trim(data$evtype),
    "c('TSUNAMI') = 'Tsunami'")

# Volcanic Ash Category
data$evtype <- recode(str_trim(data$evtype),
    "c('VOLCANIC ASH', 'Volcanic Ash Plume', 'VOLCANIC ASHFALL',
    'VOLCANIC ERUPTION') = 'Volcanic Ash'")

# Waterspout Category
data$evtype <- recode(str_trim(data$evtype),
    "c('WATER SPOUT', 'WATERSPOUT', 'WATERSPOUT FUNNEL CLOUD', 
    'WATERSPOUT TORNADO', 'WATERSPOUT-', 'WATERSPOUT-TORNADO', 'WATERSPOUT/',
    'WATERSPOUT/ TORNADO', 'WATERSPOUT/TORNADO', 'WATERSPOUTS', 'WAYTERSPOUT') 
    = 'Waterspout'")

# Wildfire Category
data$evtype <- recode(str_trim(data$evtype),
    "c('WILDFIRES', 'WILDFIRE', 'WILD/FOREST FIRES', 'WILD/FOREST FIRE',
    'WILD FIRES', 'BRUSH FIRE', 'BRUSH FIRES', 'FOREST FIRES',
    'GRASS FIRES', 'RED FLAG CRITERIA', 'RED FLAG FIRE WX') = 'Wildfire'")

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
    'EARLY RAIN', 'LACK OF SNOW', 'LATE SEASON SNOW', 'LATE SNOW',
    'Late Season Snowfall', 'Late-season Snowfall', 'Light snow', 'Light Snow',
    'LIGHT SNOW', 'LIGHT SNOW AND SLEET', 'Light Snow/Flurries',
    'LIGHT SNOW/FREEZING PRECIP', 'Light Snowfall',
    'Seasonal Snowfall', 'SNOW/ BITTER COLD', 'SNOW/ ICE', 'Snow', 'SNOW',
    'SNOW AND COLD','SNOW AND HAVY SNOW', 'Snow and Ice', 'SNOW AND ICE', 
    'SNOW AND ICE STORM', 'Snow and sleet', 'SNOW AND SLEET', 'SNOW AND WIND', 
    'SNOW FREEZING RAIN', 'SNOW/BLOWING SNOW', 'SNOW/COLD', 'SNOW/FREEZING RAIN',
    'SNOW/HIGH WINDS', 'SNOW/ICE', 'SNOW/RAIN', 'SNOW/RAIN/SLEET', 'SNOW/SLEET',
    'SNOW/SLEET/FREEZING RAIN', 'SNOW/SLEET/RAIN', 'UNUSUALLY LATE SNOW',
    'WET SNOW') = 'Winter Weather'")

# Gather all the summary rows together and delete them
data$evtype = recode(str_trim(data$evtype),
    "c('Summary August 10', 'Summary August 11', 'Summary August 17', 
    'Summary August 2-3', 'Summary August 21', 'Summary August 28', 
    'Summary August 4', 'Summary August 7', 'Summary Jan 17', 
    'Summary July 23-24', 'Summary June 18-19', 
    'Summary August 9', 'Summary June 5-6', 'Summary June 6',
    'Summary of April 12', 'Summary of April 13', 'Summary of April 21',
    'Summary of April 27', 'Summary of April 3rd', 'Summary of August 1',
    'Summary of July 11', 'Summary of July 2', 'Summary of July 22',
    'Summary of July 26', 'Summary of July 29', 'Summary of July 3',
    'Summary of June 10', 'Summary of June 11', 'Summary of June 12',
    'Summary of June 13', 'Summary of June 15', 'Summary of June 16',
    'Summary of June 18', 'Summary of June 23', 'Summary of June 24',
    'Summary of June 3', 'Summary of June 30', 'Summary of June 4',
    'Summary of June 6', 'Summary of March 14', 'Summary of March 23',
    'Summary of March 24', 'SUMMARY OF MARCH 24-25', 'SUMMARY OF MARCH 27',
    'SUMMARY OF MARCH 29', 'Summary of May 10', 'Summary of May 13',
    'Summary of May 14', 'Summary of May 22', 'Summary of May 22 am',
    'Summary of May 22 pm', 'Summary of May 26 am', 'Summary of May 26 pm',
    'Summary of May 31 am', 'Summary of May 31 pm', 'Summary of May 9-10',
    'Summary of Sept. 25-26', 'Summary September 20', 'Summary September 23',
    'Summary September 3', 'Summary September 4', 'Summary Sept. 25-26',
    'Summary Nov. 16', 'Summary Nov. 6-7', 'Summary October 31',
    'Summary Oct. 20-21', 'Summary Sept. 18') = 'SUMMARY'")

data <- subset(data, data$evtype != "SUMMARY")


# Now that recoding is done lets have a quick look at a plot of injuries / fatalties
# by event

# First sum up the figures.
event.health <- data %>%
    group_by(evtype) %>%
    summarize(injuries = sum(injuries), fatalities = sum(fatalities))

event.health

# Then plot injuries and fatalities in turn due different scales
ggplot(event.health, aes(x=evtype, y=injuries)) +
    geom_bar(stat='identity') +
    ggtitle('Injuries by Event Type (1950 - 2011)') + 
    xlab('Event Type') +
    ylab('Number of Injuries') + 
    theme(axis.text.x=element_text(angle=90))

ggplot(event.health, aes(x=evtype, y=fatalities)) +
    geom_bar(stat='identity') +
    ggtitle('Fatalities by Event Type (1950 - 2011)') + 
    xlab('Event Type') +
    ylab('Number of Fatalities') + 
    theme(axis.text.x=element_text(angle=90))

# All looks good, so moving on to the economic aspects of analysis
data.damage = data %>%
    select(begin.date, evtype, prop.damage, prop.damage.exp, crop.damage, crop.damage.exp)

# will need to do some recoding here to calculate the costs
Damage.Backup <- data.damage

# Set total prop damage and crop damage costs to prop.damage value
# then we will recode the differences as this will take care of 
# blank exp cols
data.damage <- data.damage %>%
    mutate(prop.cost = prop.damage, crop.cost = crop.damage)
glimpse(data.damage)

data.damage$prop.damage.exp <- as.character(data.damage$prop.damage.exp)
data.damage$crop.damage.exp <- as.character(data.damage$crop.damage.exp)
#data.damage$begin.date <- 
#    strptime(as.character(data.damage$begin.date), format = "%m/%d/%Y %H:%M:%S")
data.damage$begin.year <- year(strptime(as.character(data.damage$begin.date), 
                                        format = "%m/%d/%Y %H:%M:%S"))
data.damage$begin.date <- NULL
glimpse(data.damage)


# recalc the cost values depending on the prop,exe and crop.exp cols
# dealing only with H, K, M and B codes. All others except blank
# which we've already dealt with seem to be data entry errors so just go 
# with the initial value as per code book

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('+', '-', '?') = '0'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('0') = '1'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('1') = '10'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('h', 'H', '2') = '100'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('k', 'K', '3') = '1000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('4') = '10000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('5') = '100000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('m', 'M', '6') = '1000000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('7') = '10000000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('8') = '100000000'")

data.damage$prop.damage.exp <- recode(str_trim(data.damage$prop.damage.exp),
    "c('b', 'B', '9') = '1000000000'")

data.damage$crop.damage.exp <- recode(str_trim(data.damage$crop.damage.exp),
    "c('?') = '0'")

data.damage$crop.damage.exp <- recode(str_trim(data.damage$crop.damage.exp),
    "c('2') = '100'")

data.damage$crop.damage.exp <- recode(str_trim(data.damage$crop.damage.exp),
    "c('k', 'K') = '1000'")

data.damage$crop.damage.exp <- recode(str_trim(data.damage$crop.damage.exp),
    "c('m', 'M') = '1000000'")

data.damage$crop.damage.exp <- recode(str_trim(data.damage$crop.damage.exp),
    "c('b', 'B') = '1000000000'")

# convert exponents to bnumeric
data.damage$prop.damage.exp <- as.numeric(data.damage$prop.damage.exp)
data.damage$crop.damage.exp <- as.numeric(data.damage$crop.damage.exp)
glimpse(data.damage)

# Create a data frame of inflation factors at 2011. figures were derived from
# a number of questions on stackoverflow and other resources
begin.year <- c(1950:2011)
inflation.factor <- c(0.107, 0.116, 0.118, 0.119, 0.12, 0.119, 0.121, 0.125, 
                    0.128, 0.129, 0.132, 0.133, 0.134, 0.136, 0.138, 0.14, 
                    0.144, 0.148, 0.155, 0.163, 0.172, 0.18, 0.186, 0.197,
                    0.219, 0.239, 0.253, 0.269, 0.29, 0.323, 0.366, 0.404,
                    0.429, 0.433, 0.462, 0.478, 0.487, 0.505, 0.526, 0.551,
                    0.581, 0.605, 0.624, 0.642, 0.659, 0.678, 0.698, 0.714,
                    0.725, 0.741, 0.766, 0.787, 0.8, 0.818, 0.84, 0.868,
                    0.896, 0.922, 0.957, 0.954, 0.969, 1)
inflation <- data.frame(begin.year, inflation.factor)
inflation

# join the 2 dataframes
data.damage$begin.year <- as.character(data.damage$begin.year)
inflation$begin.year <- as.character(inflation$begin.year)
data.damage <- tbl_df(join(data.damage, inflation))
data.damage

# Now calculate the full property and crop costs
data.damage <- data.damage %>%
    mutate(prop.cost = ifelse( is.na(prop.damage.exp), 0, 
                    prop.damage * prop.damage.exp / inflation.factor),
           crop.cost = ifelse(is.na(crop.damage.exp), 0, 
                    crop.damage * crop.damage.exp / inflation.factor))
data.damage

# And lets summarise what we've got by event type and put in as a value
#in $100,000 proportions
event.damage <- data.damage %>%
    group_by(evtype) %>%
    summarize(prop.cost = sum(prop.cost), crop.cost = sum(crop.cost)) %>%
    mutate( prop.cost = prop.cost/100000, crop.cost = crop.cost / 100000)
event.damage

# and lets plot it
ggplot(event.damage, aes(x=evtype)) +
    geom_bar(aes(y=prop.cost, fill='prop.cost'), stat='identity') +
    geom_bar(aes(y=crop.cost, fill='crop.cost'), stat='identity') +    
    ggtitle('Economic Cost of Damage by Event Type (1950 - 2011)') + 
    xlab('Event Type') +
    ylab('Cost in $100,000 (2011)') + 
    theme(axis.text.x=element_text(angle=90)) +
    labs(legend="Damage Type")