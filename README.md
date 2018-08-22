# EcdcColors
Development of an R package for colour palettes following March 2018 ECDC guidelines for presentation of surveillance data. Contains green, blue, red and grey scales, and qualitative colours, as well as hotcold (warm-cold) scale. Function returns a vector of colours with defined length, which can be used directly in plots.

The choice of a colour palette is important as it has a strong impact on the perception of your data by the reader.
The perception will be different depending on whether an area is depicted in red or in green, and this in turn will
result in an initial positive or negative perception of your data. It seems obvious but common mistakes are often
made. Therefore, the palette used should be chosen carefully depending on the message to be conveyed.

The colour palette implemented in EcdcColors will help you to create clean, readable and consistent graphs and maps. A palette
usually consists of a maximum of seven steps as the human eye is not able to distinguish between many shades of
the same colour. Limiting the number of steps is also a good way to make your graphs and maps clear for the reader.
To ensure optimal readability, use the different colours according to the number of steps in your scale.

Colours are defined by three characteristics:
- their hue, corresponding to the wave length of the light emitted, as in a rainbow;
- heir luminance, corresponding to the amount of light emitted;
- their saturation, corresponding to the purity of the colour.

When choosing a colour to represent data, some of the basic rules to consider are set out in the ECDC guidelines for presentation of surveillance data. The colours chosen are the result of a combination of different requirements in order of importance: adherence to
the basic rules and standard practices for data visualisation, being easily understandable, offering maximum
accessibility to those who are colour-blind, being compatible with the ECDC corporate identity and being pleasing
to the eye.

The colours implemented in EcdcColors fit ECDC’s requirements but are also valid for any other organisation and can be used widely. It
is also possible to adapt these colours to suit different needs. 

Citation:  European Centre for Disease Prevention and Control. Guidelines for presentation of surveillance
data. Stockholm: ECDC; 2018. Available from: https://ecdc.europa.eu/en/publications-data/guidelines-presentation-surveillance-data
