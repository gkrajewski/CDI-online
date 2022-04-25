# CDI online
Currently deployed version: v1.7

## Changes
### v1.8
* Changed: color of redirection button (now it's more visible when it's active)
* Added: possibility of adding the run parameter to the redirection URL

### v1.7
* Added: static form has another input file called parameters.csv with parameters for external connections 
* Changed: files in adaptive were divided into translations.csv and parameters.csv
* Added: functionality to display end message based on database query (for vouchers)
* Added: functionality to redirect to a given URL after finishing the test.

### v1.6
* Changed: CAT items displayed without an unnecessary new line and quotes
* Added: Text saying about a need for wait when the CAT is loading (issue #177)
* Changed: Correct CAT settings&translations
* Changed: Question in CAT is displayed above the item, question may be null. Column id added to items
* Changed: CAT items as in static mode, CAT item questions contain category headers
(also notes for lexical items and category questions for gestures)
* Changed: Updated word list in UK:WS

### v1.5
* Added: more informative error for database saving failure (issue #172)
* Changed: app checks for existance of .Renviron file before reading it in (issue #192)
* Changed: end message for adaptive inventory (issue #198)
* Added: Correct Norwegian and English translations


### v1.4
* Changed: general error text (issue #208)
* Changed: sidebar URL errors texts (issue #209)
* Added: instructions for cat forms
* Added: instr, longText and warning message for adaptive tests. Intro placed as LongText
* Added: more logs to adaptive version + main observer function in adaptive set once=True

### v1.3
* Fixed: app is not crashing when double clicking on buttons (issue #55)

### v1.2
* Changed: two sex options for CAT instead of three (issue #174)
* Changed: no endSettings file (issue #138)
* Changed: Start from comment in cat if refreshing during comment (issue #152)

### v1.1
* Fixed: Bad URL params message (issue #150)
* Fixed: Random group order
* Fixed: Saving results in static

### v1.0
* Changed: 'Data urodzenia' not 'data urodzin'
* Changed: Type adaptive not adaptative
* Added: En and No static forms should work
* Added: Default setting (static) for type parameter (issue #164)
* Fixed: Cleaned code for adaptive inventory (issue #162)
* Fixed: End message in CAT before opportunity to give a comment to a last part (issue #168)
* Fixed: Long waiting after confirmation of the last part in the static inventory (issue #110)

earlier deploy from 16.11.2021 had no version tag

