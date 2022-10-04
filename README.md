# airline_consolidation_analysis
Shiny dashboard using Eurocontrol R&D data to assess consolidation of European airline sector

This app allows you to explore how airline mergers or acquisitions would change the baseline 2019 market structure.

Some health warnings: this app is just for fun and speculation. It is not designed for business or policy considerations. Airline market concentration is better analysed at the route or country level and considering growth through fleet expansion or increased utilisation rates, in addition to acquisitions.

The data comes from Eurocontrol's Flights R&D Archive. Thank you to Eurocontrol for making this data available publicly.

EUROCONTROL's Disclaimer: "This document_R&D product has been created with or contains elements of ATM Datasets made available by EUROCONTROL. 2020, EUROCONTROL; EUROCONTROL does not necessarily support and/or endorse the conclusion of this document/R&D product. EUROCONTROL shall not be liable for any direct, indirect, incidental or consequential damages arising out of or in connection with this document/product and/or underlying the ATM Datasets."

In the app, the data consists of only the flights recorded by Eurocontrol during March 2019. To focus on commercial aviation, I have excluded flights that do not have an IATA operator code (2-letter) associated with the ICAO aircraft operator code (3-letter); I have excluded flights with a 'ZZZ' aircraft operator code, I have excluded flights that are performing business aviation or cargo only operations. I have limited the data to only flights departing from an airport in Europe.

European countries are defined in the text file, as are EU+ and EU countries.

