module Handler.Search where

import Import

getSearchR :: Handler RepHtml
getSearchR = defaultLayout [whamlet|<h1>Not implemented yet!|]

postSearchR :: Handler RepHtml
postSearchR = defaultLayout [whamlet|<h1>Not implemented yet!|]

selectSubject :: Handler Widget
selectSubject = return [whamlet|
$newline never
<select name=subject>
    <option>Select a subject
    <option value="AFST">Africana Studies
    <option value="AMST">American Studies
    <option value="APSC">Applied Science
    <option value="ARAB">Arabic
    <option value="ART">Art
    <option value="ARTH">Art History
    <option value="AMES">Asian and Middle Eastern Studies
    <option value="BIOL">Biology
    <option value="BUAD">Business Administration
    <option value="CHEM">Chemistry
    <option value="CHIN">Chinese
    <option value="CLCV">Classical Civilization
    <option value="COLL">College Courses
    <option value="CMST">Community Studies
    <option value="CSCI">Computer Science <3
    <option value="CRWR">Creative Writing
    <option value="CRIN">Curriculum and Instruction
    <option value="DANC">Dance
    <option value="ECON">Economics
    <option value="EPPL">Education Policy Planning Leadership
    <option value="EDUC">Education
    <option value="ENGL">English
    <option value="ENSP">Environmental Science and Policy
    <option value="EURS">European Studies
    <option value="FILM">Film Studies
    <option value="FREN">French and Francophone Studies
    <option value="GSWS">Gender/Sexuality and Women's Studies
    <option value="GEOL">Geology
    <option value="GRMN">German Studies
    <option value="GBST">Global Studies
    <option value="GOVT">Government
    <option value="GRAD">Graduate
    <option value="GREK">Greek
    <option value="HBRW">Hebrew
    <option value="HISP">Hispanic Studies
    <option value="HIST">History
    <option value="HONR">Honors
    <option value="INTR">Interdisciplinary Studies
    <option value="INRL">International Relations
    <option value="ITAL">Italian
    <option value="JAPN">Japanese
    <option value="KINE">Kinesiology and Health Sciences
    <option value="LATN">Latin
    <option value="LAS">Latin American Studies
    <option value="LAW">Law
    <option value="LING">Linguistics
    <option value="LCST">Literary and Cultural Studies
    <option value="MSCI">Marine Science
    <option value="MATH">Mathematics
    <option value="MREN">Medieval and Renaissance Studies
    <option value="MLSC">Military Science
    <option value="MDLL">Modern Languages/Literatures
    <option value="MUSC">Music
    <option value="NSCI">Neuroscience
    <option value="PHIL">Philosophy
    <option value="PHYS">Physics
    <option value="PSYC">Psychology
    <option value="PBHL">Public Health
    <option value="PUBP">Public Policy
    <option value="RELG">Religious Studies
    <option value="RUSN">Russian
    <option value="RPSS">Russian and Post-Soviet Studies
    <option value="SCI">Science
    <option value="SOCL">Sociology
    <option value="SPCH">Speech
    <option value="THEA">Theatre
    <option value="WMST">Women's Studies
    <option value="WRIT">Writing
|]
