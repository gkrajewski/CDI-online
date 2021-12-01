# CDI online
In order to launch any inventory you have to provide a proper URL query eg. /?id=test&form=wg&lang=pl&type=static
Currently accesible langs and forms are in the www folder. Id could be any string, ids of length 21 mean a person connected with the StarWords app.

Currently deployed version: v1.0

## Changes
### v1.1
* Fixed: Bad URL params message (issue #150)

### v1.0
* Changed: 'Data urodzenia' not 'data urodzin'
* Changed: Type adaptive not adaptative
* Added: En and No static forms should work
* Added: Default setting (static) for type parameter (issue #164)
* Fixed: Cleaned code for adaptive inventory (issue #162)
* Fixed: End message in CAT before opportunity to give a comment to a last part (issue #168)
* Fixed: Long waiting after confirmation of the last part in the static inventory (issue #110)

earlier deploy from 16.11.2021 had no version tag

