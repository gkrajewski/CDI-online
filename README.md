# CDI online
In order to launch any inventory you have to provide a proper URL query eg. /?id=test&form=wg&lang=pl&type=static
Currently accesible langs and forms are in the www folder. Id could be any string, ids of length 21 mean a person connected with the StarWords app.

Currently deployed version: v1.2

## Changes
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

# Documentation

## 1. What is CDI online?
It's a web based app that was created to collect data from Communicative Development Inventories (CDIs), however app may be also used for other inventories and psychological tests, due to a wide range of possibilities of customizing the tool. The two main modules are static and adaptive. In static module all items are displayed. In adaptive module only some items are displayed (maximum given number or until minimal error is reached. For more information see Adaptive module below). Tests maybe be defined in multiple languages.

## 2. How you can run CDI online?
Download the code from the repository. Prepare input files (or use the input files that are in the repository). Run the app through rStudio or deploy it on shiny server. The app require a proper URL query with 4 elements defined: id, form (name of test), lang (language) and type (static or adaptive): eg. /?id=test&form=wg&lang=pl&type=static. Form, lang and type are checked for proper input and in case a wrong value is provided, an error is generated.

## 3. Static module

## 4. Adaptive module

Adaptive module starts with a page that asks for age, gender and who's filling the test (in case of CDIs it's a parent, guardian or a relative that is filling the test instead of a child). Currently these questions are obligatory. 

## 5. How to prepare input files?

First we show the file structure, secondly we describe the content and possible values for each input file.

www
|–– functions
|–– languages * in this folder you create subfolders for any language that you will use for testing
    |–– example_language * you can name subfolders however you like, in our case we have pl (for Polish), en-gb (for English), no (for Norwegian)
        |–– preSettings.csv 
        |–– forms
            |–– static
            |–– adaptive * in this folder you can create subfolders for every form
                |–– uniSettings&translations.csv
                |–– example_form * you can name subfolders however you like
                    |–– items.csv
                    |–– settings&translations.csv
                    |–– startThetas.csv

        
        
#### preSettings.csv
This file contains translations for errors that are generated when wrong values are passed in URL query. 
The file contains two columns: text_type and text. Text_type contains error names:
* badType: when type contains other value than "static" or "adaptive"
* noType: when there's no folder with this type name. So type is defined as "static" or "adaptive", but given folder doesn't exist
* badForm: when there's no folder with this form name
* errorInfo: additional message that is displayed with every error. For example a message with contact information.
Text column contains any message you would like to display when given error occurs.


#### uniSettings&translations.csv
Location: www/languages/example_language/forms/adaptive/
This file may contain the same values as settings&translations.csv file in example_form folder. If you have many forms with similar parameters you can define them in uniSettings&translations.csv instead of repeating them in settings&translations files for every individual form. 

! IMPORTANT
In case same parameters are defined in both uniSettings&translations.csv and settings&translations.csv, values from settings&translations.csv are used. Therefore settings&translations.csv takes priority over uniSettings&translations.csv.
Parameters are defined by their name in text_type column. 

#### settings&translations.csv
Location: www/languages/example_language/forms/adaptive/example_form/

This file contains translations and settings for adaptive module. Adaptive forms may be divided into several subtests (groups). In our case we test for both understanding and production. Some parameters may be defined for individual groups, in that case the name of the parameter is preceded with the group name (in this example we will use "ex_group1" as group name). 

The file contains two columns: text_type and text. Text_type contains following parameter names ([o] - means the parameter is optional):

* cdiNamePrefix: main title displayed at the top of the form
* cdiNameSufix: sub title displayed under the title
* birthQuestion: the question that asks for birth date
* genderQuestion: the question that asks for gender
* genders: gender values that will be displayed to choose from, eg. woman, man, other
* fillerQuestion: the question that asks who is filling the test
* fillers: filler values that will be displayed to choose from, eg. mother, father, grandmother,...,other (please write). Currently other value that someone may fill with text of their choice is displayed as default and cannot be turned off.
* btn: name of the start button that is diplayed on the first page (with birth, gender and filler questions)
* noGender: message that is displayed when start button is clicked and genderQuestion is not answered
* noFiller: message that is displayed when start button is clicked and fillerQuestion is not answered
* modalTitle: title of the "noGender"" and "noFiller" messages
* testInstr: text displayed in the sidebar (on left hand side) that contains short instruction
* choiceNames: names of the radio buttons that are clicked to answer the question, eg. 'yes', 'no'
* thanks: message displayed after the test is finished
* end: title of the "thanks" message
* alreadyFilled: message displayed when the test is opened after it was already finished (the same url was used)
* email: possible values "yes" or "no" - whether to send results by email
* error: message to display when some error occurs and app gets disconneted
* refresh: name of the refresh button
* commentLabel: name displayed above the comment field (currently comment field is displayed after every group and cannot be turned off)
* endText: text displayed after all items are answered that is prompting to click end button
* endBtn: end button name
* [o] groups: comma-separated list of group names, eg. ex_group1,ex_group2,ex_group3; If form contains just one group there's no need to define this parameter
* MirtMethod or ex_group1MirtMethod: possible values: "ML", "MAP", see https://cran.r-project.org/web/packages/mirtCAT/mirtCAT.pdf p. 21
* MirtCriteria or ex_group1MirtCriteria: possible values: "seq", "MI", for other see https://cran.r-project.org/web/packages/mirtCAT/mirtCAT.pdf p. 21
* [o] ex_group1Header: Title displayed for given group (it's not the same as main title, it's displayed on right hand side, just above the item)
* [o] ex_group1HeaderColor: color of the group title. If not provided, title has black color.
* [o] MirtSeTheta or ex_group1MirtSeTheta: error rate that acts as stop criterion, ie. if error falls below provided rate the test is finished
* [o] maxItemNr or ex_group1maxItemNr: maximum number of items to display (acts as stop criterion). If maxItemNr is not defined, the number of items is used as maxItemNr.
* [o] minItemNr or ex_group1minItemNr: minimum number of items to display. Error is displayed when minItemNr is greater than maxItemNr or available number of items
* [o] continueBtn: name of the button that appears after end of each group (if there's another group to follow) and after instruction. Obligatory parameter if "groups" are defined.
* [o] ex_group1Instr: Instruction to be displayed before each group items.

!IMPORTANT
1. Currently there's no "noAge"" parameter, that would be displayed when birthQuestion is not aswered. Birth question is followed by a date field with current date chosen. If the date is not changed, current date is considered as birth date.

2. If both stop criterions: MirtSeTheta and maxItemNr are provided, test is finished whenever one of them is met.

3. When "groups" parameter is defined, then following parameters: MirtMethod, MirtCriteria, MirtSeTheta, maxItemNr and minItemNr must be provided for every groups, that is with group name preceding these parameters' names.

4. Currently if "groups" parameter is not defined, there's no option to display a screen with instruction, before the actual test. Instruction is displayed all the time in the sidebar. When "groups" are defined instruction may be displayed both in the sidebar and before the test (two different text may be defined).
