# ntnu-gradeinsight

## Update as of 19th of July 2020

Due to changes to the API the app is currrently not working. The code itself is also a huge mess, and therefore hard to maintain. I hope I can find the motivation to make the necessary changes at some point the near future. 

## Overview

This app, implemented in R using Shiny, lets NTNU students upload the HTML file from the "Results" section of StudWeb
and easily gain insight into their performance relative to the rest of the class. This will give the users a more nuanced view of their performance than what can be obtained by purely looking at the grades. A huge thanks to whoever is responsible for https://grades.no/, seeing as this project would've required considerably more work without it.

I'm aware of the privacy concerns related to the upload of files containing information about grades. The information contained in this file is only stored temporarily, and only for the purpose of easily obtaining information about relevant courses. The file is deleted as soon as the user session expires. An option to submit grades in an anonymous fashion will probably be implemented at a later stage. 

Feel free to report any bugs. The app is not well - tested due to the limited access to the HTML - files that serve as input.
