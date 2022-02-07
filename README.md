# CDI online
In order to launch any inventory you have to provide a proper URL query eg. /?id=test&form=wg&lang=pl&type=static
Currently accesible langs and forms are in the www folder. Id could be any string, ids of length 21 mean a person connected with the StarWords app.

Currently deployed version: v1.3

## Changes
### v1.4
* Added: instr, longText and warning message for adaptive tests

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

